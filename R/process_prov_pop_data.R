# Private functions ----

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

#' Aggregate district population to province for missing values
#'
#' Fills remaining province-level NAs by summing all districts in that province-year.
#'
#' @param base_data Tibble of province-level population data (may have NA)
#'
#' @return Tibble with remaining NAs filled and aggregation flags
#' @keywords internal
#'
aggregate_districts_to_province <- function(base_data) {

  # Retrieve district-level population and aggregate (sum) all districts to province-year level
  district_data <- process_dist_pop_data()

  province_aggregates <- district_data |>
    dplyr::group_by(ADM0_NAME, ADM1_NAME, year) |>
    dplyr::summarise(
      Total_from_districts = sum(Total, na.rm = TRUE),
      Under5Pop_from_districts = sum(Under5Pop, na.rm = TRUE),
      Under15Pop_from_districts = sum(Under15Pop, na.rm = TRUE),
      .groups = "drop"
    )

  base_data |>
    dplyr::left_join(province_aggregates, by = c("ADM0_NAME", "ADM1_NAME", "year")) |>
    dplyr::mutate(
      used_aggregate_Total = is.na(Total) & !is.na(Total_from_districts),
      used_aggregate_Under5Pop = is.na(Under5Pop) & !is.na(Under5Pop_from_districts),
      used_aggregate_Under15Pop = is.na(Under15Pop) & !is.na(Under15Pop_from_districts),
      Total = dplyr::if_else(used_aggregate_Total, Total_from_districts, Total),
      Under5Pop = dplyr::if_else(used_aggregate_Under5Pop, Under5Pop_from_districts, Under5Pop),
      Under15Pop = dplyr::if_else(used_aggregate_Under15Pop, Under15Pop_from_districts, Under15Pop)
    ) |>
    dplyr::select(-Total_from_districts, -Under5Pop_from_districts, -Under15Pop_from_districts)
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
#' @param output_file Optional .rds output path.
#' @return Tibble with GUID + province-year rows and final output naming.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' prov_pop_data(pop_data, output_file = "Data/pop/prov_pop_admin1.rds")
#' }
process_prov_pop_data <- function(pop_data,
                                  pop_dir = "GID/PEB/SIR/Data/pop",
                                  dist_pop_file_path = file.path(pop_dir, "processed_pop_file", "global.dist.parquet"),
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

  # Only patch for province is the NPAFP rate indicator and potentially indonesia patch
  # will check if we can get data from POLIS only first

  # Prefer adm0 guid and names from the shapefile
  polis_pop <- dplyr::left_join(province_long_subset |>
                                  dplyr::rename(sf_adm0_name = ADM0_NAME,
                                                sf_adm1_name = ADM1_NAME,
                                                sf_adm0guid = ADM0_GUID),
                                polis_pop) |>
    dplyr::distinct() |>
    dplyr::mutate(ADM0_NAME = dplyr::coalesce(sf_adm0_name, ADM0_NAME),
                  ADM1_NAME = dplyr::coalesce(sf_adm1_name, ADM1_NAME),
                  ADM0_GUID = dplyr::coalesce(sf_adm0guid, ADM0_GUID)) |>
    dplyr::select(-dplyr::starts_with("sf_"), -FK_DataSetId)

  # Compare different adm1guids and determine which GUID it represents
  # Count number of GUIDs in non-API sources that belong in the district shapefile

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
    sirfunctions::sirfunctions_io("write", NULL, file_loc = file.path(pop_dir, "errors", "adm1guid_year_duplicates_polis_api.csv"))
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

  # Apply growth rates
  # Output
  cli::cli_process_start("Applying growth rate to fill missing populations.")
  result <- base_data |>
    apply_growth_rate("ALL", grouping_col = "ADM1_GUID") |>
    apply_growth_rate("0-15Y", grouping_col = "ADM1_GUID") |>
    apply_growth_rate("0-5Y", grouping_col = "ADM2_GUID")
  cli::cli_process_done()

  # Output
  result <- base_data |>
    apply_growth_rate("Under5Pop") |>
    apply_growth_rate("Under15Pop") |>
    apply_growth_rate("Total") |>
    aggregate_districts_to_province() |>
    dplyr::mutate(
      Used_Growth_Rate = ifelse(
        used_growth_Under5Pop | used_growth_Under15Pop | used_growth_Total,
        "Yes", "No"
      ),
      Used_District_Aggregation = ifelse(
        used_aggregate_Total | used_aggregate_Under5Pop | used_aggregate_Under15Pop,
        "Yes", ""
      ),
      GUID = ADM1_GUID
    ) |>
    dplyr::rename(
      Country_Name  = ADM0_NAME,
      Province_Name = ADM1_NAME,
      SOURCE        = datasource
    ) |>
    dplyr::select(
      -used_growth_Under5Pop, -used_growth_Under15Pop, -used_growth_Total,
      -used_aggregate_Total, -used_aggregate_Under5Pop, -used_aggregate_Under15Pop,
      -active.year.01, -datasource
    )

  if (!is.null(output_file)) readr::write_rds(result, output_file)
  result
}
