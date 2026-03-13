# Private functions ----

#' Calculate province level population using district pop rollups
#'
#' @param dist_pop_file_path `str` Path to the district pop file.
#' @param edav `logical` If the file is in EDAV. Defaults to `TRUE`.
#'
#' @returns `tibble` District population rollup.
#' @export
#'
load_dist_pop_rollup <- function(dist_pop_file_path = file.path("GID/PEB/SIR/Data/pop",
                                                                "processed_pop_file",
                                                                "dist.pop.long.parquet"),
                                 edav = TRUE) {

  # Load cleaned dist pop
  processed_dist_pop <- sirfunctions::sirfunctions_io("read", NULL, dist_pop_file_path, edav = edav)

  # Roll it up buttercup
  dist_pop_rollup <- processed_dist_pop |>
    dplyr::group_by(adm1guid, year) |>
    # if NA then all districts for a province doesn't have pops and we want
    # the rollups to contain only for provinces where all districts do have pops
    dplyr::summarize(u15pop_rollup = sum(u15pop),
                     u5pop_rollup = sum(u5pop),
                     totpop_rollup = sum(totpop)
                     )

  return(dist_pop_rollup)

}

#' Indonesia province patch
#'
#' @param indonesia_file_path `str` File path to the Indonesia patch for 2023.
#' @param edav `logical` Whether the file is in EDAV.
#'
#' @returns `tibble` Indonesia patch
#' @export
#'
load_indonesia_patch <- function(indonesia_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/Papua Population u15 2023.csv", edav = TRUE) {
  sirfunctions::sirfunctions_io(io = "read", NULL, file_loc = indonesia_file_path, edav = edav) |>
    dplyr::mutate(active.year.01 = 2023,
                  Admin0Name = "INDONESIA",
                  Admin1Name = gsub("_", " ", Province),
                  datasource = "INDONESIA_PAPUA_2023_PATCH") |>
    dplyr::select(-Province) |>
    dplyr::rename(Under15Pop = "Population u15") |>
    dplyr::mutate(Total = NA_real_,
                  Under5Pop = NA_real_) |>
    dplyr::relocate(Admin0Name, Admin1Name, active.year.01, Under15Pop, Under5Pop, Total, .before = datasource)
}

# Public function ----

#' Build province population (Admin1) in wide format
#'
#' Combines POLIS + patches + Jamal; joins to province-year shapes; deduplicates;
#' fills datasource across gaps within GUID; fills missing values using growth rates.
#' Aggregates remaining province-level gaps by summing all district totals per province.
#'
#' @param pop_data `tibble` Province population dataset pulled from the POLIS API.
#' @param pop_dir `str` Default directory to the population folder.
#' @param dist_pop_file_path `str` File path to the cleaned district pop file.
#' @param prov_file_path `str` File path to the global province shapefile.
#' @param growth_rate_file_path `str` File path to the growth rate Excel file.
#' @param output_dir `str` File path to the output directory.
#' @param output_type `str` How the population file should be outputted.
#' @param edav `logical` Whether files are on EDAV. Defaults to `TRUE`.
#'
#' @returns `tibble` Tibble with GUID + province-year rows and final output naming.
#' @export
#' @examples
#' \dontrun{
#' prov_pop_data(pop_data, output_file = "Data/pop/prov_pop_admin1.rds")
#' }
process_prov_pop_data <- function(pop_data,
                                  pop_dir = "GID/PEB/SIR/Data/pop",
                                  dist_pop_file_path = file.path(pop_dir, "processed_pop_file", "dist.pop.long.parquet"),
                                  prov_file_path = "GID/PEB/SIR/Data/spatial/global.prov.rds",
                                  growth_rate_file_path = file.path(pop_dir, "pop raw/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx"),
                                  output_dir = file.path(pop_dir, "processed_pop_file"),
                                  output_type = "parquet",
                                  edav = TRUE) {

  # Remove period if passed in the output type
  output_type <- stringr::str_replace(output_type, stringr::fixed("."), "")

  if (!output_type %in% c("rds", "csv", "parquet")) {
    cli::cli_abort("Please pass only 'rds', 'csv', or 'parquet' in output_type.")
  }

  # Crosswalk and remove forward fills
  pop_data <- crosswalk_pop_cols(pop_data)
  pop_data <- remove_forward_fill_polis_pop(pop_data)

  # Load the district roll-up dataset
  dist_pop_rollup <- load_dist_pop_rollup(dist_pop_file_path, edav)

  # Alert to determine how much of the POLIS API are forward fills
  pop_data_forward_fill_n <- sum(pop_data$is_forward_fill, na.rm = TRUE)
  cli::cli_alert_info(paste0("There are ", pop_data_forward_fill_n, " (", round(pop_data_forward_fill_n / nrow(pop_data) * 100),   "%) records in the POLIS pop API ",
                             "where populations were forward filled. These values were replaced with NAs as they don't account for growth rates."))


  # Input 'province-year shape table + country-level growth rates
  province_long <- sirfunctions::load_clean_prov_sp(fp = prov_file_path, type = "long", edav = edav)
  growth_rates <- load_growth_rates(growth_rate_file_path, edav = edav)

  # Remove unnecessary columns from province_long and reduce file size (from 10GB to 115MB)
  province_long_subset <- province_long |>
    dplyr::tibble() |>
    dplyr::select(dplyr::ends_with("_NAME"), dplyr::ends_with("_GUID"),
      yr.st, yr.end, active.year.01, GUID
    ) |>
    dplyr::select(-dplyr::ends_with("VIZ_NAME"))

  rm(province_long)
  gc()

  # Transform POLIS wide and 0-5Y/U0-15Y/UALL to Under5Pop/Under15Pop/Total
  polis_pop <- pop_data |>
    dplyr::arrange(year) |>
    dplyr::select(-CREATEDDATE, -UPDATEDDATE, -STARTDATE, -ENDDATE, -is_forward_fill) |>
    tidyr::pivot_wider(names_from = AgeGroupCode, values_from = Value) |>
    dplyr::rename(
      ADM0_GUID = adm0guid,
      GUID = adm1guid,
      active.year.01 = year
    )


  # Prefer adm0 guid and names from the shapefile
  polis_pop <- dplyr::left_join(province_long_subset |>
                                  dplyr::rename(sf_adm0_name = ADM0_NAME,
                                                sf_adm1_name = ADM1_NAME,
                                                sf_adm0guid = ADM0_GUID),
                                polis_pop) |>
    dplyr::distinct()

  # Check when the pop names are not matching with the shapefile
  ctry_name_mismatch <- polis_pop |>
    dplyr::filter(sf_adm0_name != ADM0_NAME)
  prov_name_mismatch <- polis_pop |>
    dplyr::filter(sf_adm1_name != ADM1_NAME)

  # Check when adm0guid are not matching the shapefile
  adm0guid_mismatch <- polis_pop |>
    dplyr::filter(sf_adm0guid != ADM0_GUID)

  if (nrow(ctry_name_mismatch) > 0) {
    cli::cli_alert_warning("Country name mismatches between pop and shapefile. See 'errors' folder.")
    sirfunctions::sirfunctions_io("write", NULL, file.path(pop_dir, "errors",
                                                           paste0(Sys.Date(),
                                                                  "_ctry_name_mismatches_in_prov_pop.csv")),
                                  obj = ctry_name_mismatch,
                                  edav = edav)
  } else {
    cli::cli_alert_success("No mismatches in country names between pop and shapefile.")
  }

  if (nrow(prov_name_mismatch) > 0) {
    cli::cli_alert_warning("Province name mismatches between pop and shapefile. See 'errors' folder.")
    sirfunctions::sirfunctions_io("write", NULL, file.path(pop_dir, "errors",
                                                           paste0(Sys.Date(),
                                                                  "_prov_name_mismatches_in_prov_pop.csv")),
                                  obj = prov_name_mismatch,
                                  edav = edav)
  } else {
    cli::cli_alert_success("No mismatches in province names between pop and shapefile.")
  }

  if (nrow(adm0guid_mismatch) > 0) {
    cli::cli_alert_warning("adm0guid mismatches between pop and shapefile. See 'errors' folder.")
    sirfunctions::sirfunctions_io("write", NULL, file.path(pop_dir, "errors",
                                                           paste0(Sys.Date(),
                                                                  "_adm0guid_mismatches_in_prov_pop.csv")),
                                  obj = adm0guid_mismatch,
                                  edav = edav)
  } else {
    cli::cli_alert_success("No mismatches in adm0guid between pop and shapefile.")
  }

  polis_pop <- polis_pop |>
    dplyr::mutate(ADM0_NAME = sf_adm0_name,
                  ADM1_NAME = sf_adm1_name,
                  ADM0_GUID = sf_adm0guid) |>
    dplyr::select(-dplyr::starts_with("sf_"), -FK_DataSetId)

  base_data <- polis_pop |>
    dplyr::mutate(active.year.01 = as.numeric(active.year.01)) |>
    dplyr::rename(ADM1_GUID = "GUID", year = "active.year.01") |>
    dplyr::group_by(ADM1_GUID) |>
    dplyr::arrange(year, .by_group = TRUE) |>
    tidyr::fill(datasource, .direction = "down") |>
    dplyr::ungroup() |>
    dplyr::left_join(growth_rates, by = c("ADM0_NAME" = "Admin0Name", "year" = "year"))

  # Check if there are duplicates in adm1-year combinations
  prov_year_duplicates <- base_data |>
    dplyr::group_by(ADM1_GUID, year) |>
    dplyr::summarize(n = dplyr::n()) |>
    dplyr::filter(n > 1)

  if (nrow(prov_year_duplicates) != 0) {
    cli::cli_alert_warning("There are duplicate adm1guid and year combination in base_data. Check for many-to-many relationships.")
    sirfunctions::sirfunctions_io("write", NULL, file_loc = file.path(pop_dir, "errors", "adm1guid_year_duplicates_polis_api.csv"),
                                  obj = prov_year_duplicates)
  } else {
    cli::cli_alert_success("No duplicate adm1guid-year combinations.")
  }

  # Some years the growth rates are not available. These need to be filled using "downup."
  # We first arrange from oldest to newest year, then fill.
  # For example, if no data in 2024, 2025 but there is data in 2023, then use 2023.
  # In case of gaps, such as 2023, 2024 (blank), 2025 (blank), 2026, then fill using 2023.
  base_data <- base_data |>
    dplyr::group_by(ADM0_NAME) |>
    dplyr::arrange(year, .by_group = TRUE) |>
    tidyr::fill(growth_rate, .direction = "downup") |>
    dplyr::ungroup()

  # Using population roll-ups to fill in the rest
  result <- dplyr::left_join(base_data,
                             dist_pop_rollup |>
                               dplyr::rename(ADM1_GUID = adm1guid)) |>
    dplyr::mutate(dplyr::across(c("u15pop_rollup", "u5pop_rollup", "totpop_rollup"),
                                \(x) ifelse(x == 0, NA, x))) |>
    dplyr::mutate(datasource = dplyr::case_when(
      is.na(`0-15Y`) & !is.na(u15pop_rollup) ~ "DISTRICT ROLLUP",
      is.na(`0-5Y`) & !is.na(u5pop_rollup) ~ "DISTRICT ROLLUP",
      is.na(ALL) & !is.na(totpop_rollup) ~ "DISTRICT ROLLUP",
      .default = datasource
    )) |>
    dplyr::mutate(`0-15Y` = dplyr::coalesce(`0-15Y`, u15pop_rollup),
                  `0-5Y` = dplyr::coalesce(`0-5Y`, u5pop_rollup),
                  ALL = dplyr::coalesce(ALL, totpop_rollup)) |>
    dplyr::select(-dplyr::ends_with("_rollup"))

  # Get how many records filled
  dist_rollup_added_na_n <- sum(is.na(result$`0-15Y`))
  cli::cli_alert_info(paste0("Population roll-up filled in additional ",
                             sum(is.na(base_data$`0-15Y`)) - dist_rollup_added_na_n,
                             " province u15 population."))

  # Apply growth rates
  cli::cli_process_start("Applying growth rate to fill missing populations.")
  result <- result |>
    apply_growth_rate("0-15Y", grouping_col = "ADM1_GUID") |>
    apply_growth_rate("0-5Y", grouping_col = "ADM1_GUID") |>
    apply_growth_rate("ALL", grouping_col = "ADM1_GUID")
  cli::cli_process_done()

  polis_pop_u15_na_n <- sum(is.na(polis_pop$`0-15Y`))
  growth_rate_added_na_n <- sum(is.na(result$`0-15Y`))

  cli::cli_alert_info(paste0("Growth rate filled in additional ",
                             dist_rollup_added_na_n - growth_rate_added_na_n,
                             " province u15 population."))

  # Summary
  cli::cli_alert_info(paste0("District roll-ups and growth rate application ",
                             "filled in ", polis_pop_u15_na_n - growth_rate_added_na_n,
                             " (", round((polis_pop_u15_na_n - growth_rate_added_na_n) / nrow(result) * 100, 2),
                             "%)",
                             " additional province pops in the POLIS province pop files."))

  # Add WHO region to results because the new SF doesn't have that column
  result <- dplyr::left_join(result |>
                               dplyr::select(-WHO_REGION),
                             polis_pop |>
                               dplyr::group_by(ADM0_NAME) |>
                               dplyr::filter(!is.na(`0-15Y`)) |>
                               dplyr::filter(active.year.01 == max(active.year.01)) |>
                               dplyr::select(ADM0_NAME, WHO_REGION) |>
                               dplyr::distinct() |>
                               dplyr::ungroup())

  formatted_result <- result |>
    dplyr::select(-ISO_3_CODE) |>
    tidyr::replace_na(list(used_growth_ALL = FALSE,
                           `used_growth_0-15Y` = FALSE,
                           `used_growth_0-5Y` = FALSE,
                           datasource = "POLIS API")) |>
    dplyr::rename(
      who.region = "WHO_REGION",
      growth.rate = "growth_rate",
      adm0guid = "ADM0_GUID",
      adm1guid = "ADM1_GUID",
      ctry = "ADM0_NAME",
      prov = "ADM1_NAME",
      u15pop = "0-15Y",
      u5pop = "0-5Y",
      totpop = "ALL",
      u15.anchor.year = "0-15Y_anchor_year",
      u5.anchor.year = "0-5Y_anchor_year",
      tot.anchor.year = "ALL_anchor_year",
      used.growth.rate.tot = "used_growth_ALL",
      used.growth.rate.u5 = "used_growth_0-5Y",
      used.growth.rate.u15 = "used_growth_0-15Y"
    ) |>
    dplyr::relocate(who.region, ctry, prov, adm0guid, adm1guid, .after = year) |>
    dplyr::mutate(
      used.growth.rate = dplyr::case_when(
        used_growth_rate_tot & used_growth_rate_u5 & used_growth_rate_u15 ~ "u5, u15, tot",
        used_growth_rate_tot & used_growth_rate_u5 & !used_growth_rate_u15 ~ "u5, tot",
        used_growth_rate_tot & !used_growth_rate_u5 & used_growth_rate_u15 ~ "u15, tot",
        !used_growth_rate_tot & used_growth_rate_u5 & used_growth_rate_u15 ~ "u5, u15",
        used_growth_rate_tot & !used_growth_rate_u5 & !used_growth_rate_u15 ~ "tot",
        !used_growth_rate_tot & used_growth_rate_u5 & !used_growth_rate_u15 ~ "u5",
        !used_growth_rate_tot & !used_growth_rate_u5 & used_growth_rate_u15 ~ "u15",
        .default = "no"
      ),
      miss.u15 = dplyr::if_else(is.na(u15pop), TRUE, FALSE),
      miss.totpop = dplyr::if_else(is.na(totpop), TRUE, FALSE),
      # U15 population category
      pop.cat = dplyr::case_when(
        is.na(u15pop) == T | u15pop == 0 ~ "Missing",
        dplyr::between(u15pop, 0, 99999) ~ "<100,000",
        dplyr::between(u15pop, 100000, 249999) ~ "100,000-249,999",
        dplyr::between(u15pop, 250000, 499999) ~ "250,000-499,999",
        dplyr::between(u15pop, 500000, 999999) ~ "500,000-999,999",
        u15pop >= 1000000 ~ ">=1,000,000")
    ) |>
    dplyr::relocate(u15pop, u5pop, totpop, .after = datasource) |>
    dplyr::relocate(growth.rate, .before = used.growth.rate) |>
    dplyr::mutate(pop.cat = factor(pop.cat,
                                   levels = c("Missing",
                                              "<100,000",
                                              "100,000-249,999",
                                              "250,000-499,999",
                                              "500,000-999,999",
                                              ">=1,000,000"),
                                   ordered = TRUE)) |>
    dplyr::select(-dplyr::contains("used_growth_rate_"))

  # Check if there are GUIDs in the Shapefile that are not in the pop file
  not_in_sf <- setdiff(province_long_subset$GUID, formatted_result$adm1guid)

  if (length(not_in_sf) != 0) {
    cli::cli_alert_warning("There are GUIDs in the shapefile that's not in the population file.")
    sirfunctions::sirfunctions_io("write", NULL, file_loc = file.path(pop_dir,
                                                                      "errors",
                                                                      paste0(Sys.Date(),
                                                                             "_guids_in_prov_sf_not_in_prov_pop.parquet")),
                                  obj = dplyr::tibble(GUID = not_in_sf),
                                  edav = edav)
  } else {
    cli::cli_alert_success("All GUIDs in the province shapefile are present in the province population file.")
  }


  if (!is.null(output_dir)){
    sirfunctions::sirfunctions_io("write", NULL, file_loc = file.path(output_dir, paste0("prov.pop.long.", output_type)),
                                  obj = formatted_result,
                                  edav = edav)
  }

  # Perform diagnostic checks
  prop_missingness_by_ctry_year <- formatted_result |>
    dplyr::group_by(who.region , ctry, year) |>
    dplyr::summarize(missing_u15 = sum(is.na(u15pop)),
                     missing_u5 = sum(is.na(u5pop)),
                     missing_tot = sum(is.na(totpop)),
                     total_provinces = dplyr::n(),
                     missing_u15_pct = round(missing_u15 / total_provinces * 100, 2)) |>
    dplyr::arrange(who.region, dplyr::desc(year), dplyr::desc(missing_u15_pct)) |>
    dplyr::ungroup()

  # Get max year where all district data are present
  last_year_w_complete_data <- prop_missingness_by_ctry_year |>
    dplyr::group_by(ctry) |>
    dplyr::filter(missing_u15_pct == 0) |>
    dplyr::summarize(last_year_with_complete_data = max(year)) |>
    dplyr::ungroup()

  # Join
  prop_missingness_by_ctry_year <- dplyr::left_join(prop_missingness_by_ctry_year, last_year_w_complete_data) |>
    dplyr::arrange(who.region, dplyr::desc(year), dplyr::desc(missing_u15_pct)) |>
    dplyr::ungroup()

  sirfunctions::sirfunctions_io("write", NULL, file_loc = file.path(pop_dir,
                                                                    "pop_diagnostics",
                                                                    paste0(Sys.Date(),
                                                                           "_prop_prov_pop_missing_by_ctry_year.csv")),
                                obj = prop_missingness_by_ctry_year,
                                edav = edav)

  cli::cli_alert_success("Province population created.")

  invisible(formatted_result)
}
