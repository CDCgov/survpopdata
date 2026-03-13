# Public function ----

#' Build country population
#'
#' Cleans the country level population data from the POLIS API and fill in
#' gaps in population counts using the application of growth rates.
#'
#' @param pop_data `tibble` Country population dataset pulled from the POLIS API.
#' @inheritParams process_dist_pop_data
#' @returns `tibble` Cleaned country population.
#'
#' @export
#' @examples
#' \dontrun{
#' pop_data <- load_polis_pop("ctry")
#' ctry_pop <- process_ctry_pop_data(pop_data)
#' }
#'
process_ctry_pop_data <- function(pop_data,
                                  pop_dir = "GID/PEB/SIR/Data/pop",
                                  ctry_file_path = "GID/PEB/SIR/Data/spatial/global.ctry.rds",
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

  # Alert to determine how much of the POLIS API are forward fills
  pop_data_forward_fill_n <- sum(pop_data$is_forward_fill, na.rm = TRUE)
  cli::cli_alert_info(paste0("There are ", pop_data_forward_fill_n, " (", round(pop_data_forward_fill_n / nrow(pop_data) * 100),   "%) records in the POLIS pop API ",
                             "where populations were forward filled. These values were replaced with NAs as they don't account for growth rates."))


  # Input country-year shape table + country-level growth rates
  country_long <- sirfunctions::load_clean_ctry_sp(fp = ctry_file_path, type = "long", edav = edav)
  growth_rates <- load_growth_rates(growth_rate_file_path, edav = edav)

  # Check for duplicated records
  duplicated_records <- pop_data |>
    dplyr::summarise(n = dplyr::n(),
                     .by = c(ISO_3_CODE, WHO_REGION, ADM0_NAME, adm0guid, year,
                             datasource, FK_DataSetId, AgeGroupCode)) |>
    dplyr::filter(n > 1L)

  if (nrow(duplicated_records) != 0) {
    cli::cli_alert_warning("There are duplicated country-level records. Check the 'errors' folder.")
    sirfunctions::sirfunctions_io("write", NULL,
                                  file.path(pop_dir, "errors",
                                            paste0(Sys.Date(),
                                                   "_polis_api_ctry_duplicated_records.csv")),
                                  obj = duplicated_records)
    cli::cli_alert_info("Deduping using the updated date column...")
    pop_data <- pop_data |>
      dplyr::slice_max(UPDATEDDATE, by = c(ISO_3_CODE, WHO_REGION, ADM0_NAME,
                                           adm0guid, year, datasource,
                                           FK_DataSetId, AgeGroupCode))

  } else {
    cli::cli_alert_success("No duplicates in the POLIS ctry level pops.")
  }


  # Remove unnecessary columns from country_long and reduce file size
  country_long_subset <- country_long |>
    dplyr::tibble() |>
    dplyr::select(
      dplyr::ends_with("_NAME"), dplyr::ends_with("_GUID"),
      yr.st, yr.end, active.year.01, GUID
    ) |>
    dplyr::select(-dplyr::ends_with("VIZ_NAME"))

  rm(country_long)
  gc()

  # Transform POLIS wide and 0-5Y/U0-15Y/UALL to Under5Pop/Under15Pop/Total
  polis_pop <- pop_data |>
    dplyr::arrange(year) |>
    dplyr::select(-CREATEDDATE, -UPDATEDDATE, -STARTDATE, -ENDDATE, -is_forward_fill) |>
    tidyr::pivot_wider(names_from = AgeGroupCode, values_from = Value) |>
    dplyr::rename(
      GUID = adm0guid,
      active.year.01 = year
    )

  # Prefer adm0 names from the shapefile
  polis_pop <- dplyr::left_join(country_long_subset |>
                                  dplyr::rename(sf_adm0_name = ADM0_NAME),
                                polis_pop) |>
    dplyr::distinct()

  # Check when the pop names are not matching with the shapefile
  ctry_name_mismatch <- polis_pop |>
    dplyr::filter(sf_adm0_name != ADM0_NAME)

  if (nrow(ctry_name_mismatch) > 0) {
    cli::cli_alert_warning("Country name mismatches between pop and shapefile. See 'errors' folder.")
    sirfunctions::sirfunctions_io("write", NULL, file.path(pop_dir, "errors",
                                                           paste0(Sys.Date(),
                                                                  "_ctry_name_mismatches_in_ctry_pop.csv")),
                                  obj = ctry_name_mismatch,
                                  edav = edav)
  } else {
    cli::cli_alert_success("No mismatches in country names between pop and shapefile.")
  }

  polis_pop <- polis_pop |>
    dplyr::mutate(ADM0_NAME = sf_adm0_name) |>
    dplyr::select(-dplyr::starts_with("sf_"), -FK_DataSetId)

  base_data <- polis_pop |>
    dplyr::mutate(active.year.01 = as.numeric(active.year.01)) |>
    dplyr::rename(ADM0_GUID = "GUID", year = "active.year.01") |>
    dplyr::group_by(ADM0_GUID) |>
    dplyr::arrange(year, .by_group = TRUE) |>
    tidyr::fill(datasource, .direction = "down") |>
    dplyr::ungroup() |>
    dplyr::left_join(growth_rates, by = c("ADM0_NAME" = "Admin0Name", "year" = "year"))

  # Check if there are duplicates in adm1-year combinations
  ctry_year_duplicates <- base_data |>
    dplyr::group_by(ADM0_GUID, year) |>
    dplyr::summarize(n = dplyr::n()) |>
    dplyr::filter(n > 1)

  if (nrow(ctry_year_duplicates) != 0) {
    cli::cli_alert_warning("There are duplicate adm1guid and year combination in base_data. Check for many-to-many relationships.")
    sirfunctions::sirfunctions_io("write", NULL, file_loc = file.path(pop_dir, "errors", "adm0guid_year_duplicates_polis_api.csv"),
                                  obj = ctry_year_duplicates)
  } else {
    cli::cli_alert_success("No duplicate adm0guid-year combinations.")
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

  # Apply growth rates
  cli::cli_process_start("Applying growth rate to fill missing populations.")
  result <- base_data |>
    apply_growth_rate("0-15Y", grouping_col = "ADM0_GUID") |>
    apply_growth_rate("0-5Y", grouping_col = "ADM0_GUID") |>
    apply_growth_rate("ALL", grouping_col = "ADM0_GUID")
  cli::cli_process_done()

  polis_pop_u15_na_n <- sum(is.na(polis_pop$`0-15Y`))
  growth_rate_added_na_n <- sum(is.na(result$`0-15Y`))

  cli::cli_alert_info(paste0("Growth rate filled in additional ",
                             polis_pop_u15_na_n - growth_rate_added_na_n,
                             " country U15 population."))

  # Count countries with missing country pop
  missing_pop_ctry <- result |>
    dplyr::filter(is.na(`0-15Y`)) |>
    dplyr::select(ADM0_NAME) |>
    dplyr::distinct()

  if (nrow(missing_pop_ctry) != 0 ){
    cli::cli_alert_warning("There are countries missing U15 population. See the 'errors' folder.")
    sirfunctions::sirfunctions_io("write", NULL,
                                  file.path(pop_dir, "errors",
                                            paste0(Sys.Date(), "_ctry_missing_pop.csv")),
                                  obj = missing_pop_ctry)
  } else {
    cli::cli_alert_success("No countries missing population.")
  }

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
      ctry = "ADM0_NAME",
      u15pop = "0-15Y",
      u5pop = "0-5Y",
      totpop = "ALL",
      used_growth_rate_tot = "used_growth_ALL",
      used_growth_rate_u5 = "used_growth_0-5Y",
      used_growth_rate_u15 = "used_growth_0-15Y"
    ) |>
    dplyr::relocate(who.region, ctry, adm0guid, .after = year) |>
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
      miss.totpop = dplyr::if_else(is.na(totpop), TRUE, FALSE)
    ) |>
    dplyr::relocate(u15pop, u5pop, totpop, .after = datasource) |>
    dplyr::relocate(growth.rate, .before = used.growth.rate) |>
    dplyr::select(-dplyr::contains("used_growth_rate_"))

  sirfunctions::sirfunctions_io("write", NULL,
                                file.path(pop_dir, "processed_pop_file", "ctry.pop.long.parquet"),
                                obj = formatted_result)

  cli::cli_alert_success("Country population created.")

  invisible(formatted_result)

}
