# Private functions ----

# Public function ----

#' Build country population (Admin0) in wide format
#'
#' Combines POLIS + patches + Jamal; joins to country-year shapes; deduplicates;
#' fills datasource across gaps within GUID; fills missing values using growth rates.
#' Aggregates remaining country-level gaps by summing all province totals per country.
#'
#' @param pop_data `tibble` Country population dataset pulled from the POLIS API.
#' @param pop_dir `str` Default directory to the population folder.
#' @param ctry_file_path `str` File path to the global country shapefile.
#' @param growth_rate_file_path `str` File path to the growth rate Excel file.
#' @inheritParams load_all_patches
#' @param output_file Optional .rds output path.
#' @return Tibble with GUID + country-year rows and final output naming.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ctry_pop_data(pop_data, output_file = "Data/pop/ctry_pop_admin0.rds")
#' }
process_ctry_pop_data <- function(pop_data,
                                  pop_dir = "GID/PEB/SIR/Data/pop",
                                  ctry_file_path = "GID/PEB/SIR/Data/spatial/global.ctry.rds",
                                  growth_rate_file_path = file.path(pop_dir, "pop raw/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx"),
                                  output_file = file.path(pop_dir, "processed_ctry_pop_file"),
                                  edav = TRUE) {
  # Crosswalk and remove forward fills
  pop_data <- crosswalk_pop_cols(pop_data)
  pop_data <- remove_forward_fill_polis_pop(pop_data)

  # Input country-year shape table + country-level growth rates
  country_long <- sirfunctions::load_clean_ctry_sp(fp = ctry_file_path, type = "long", edav = edav)
  growth_rates <- load_growth_rates(growth_rate_file_path, edav = edav)

  # Remove unnecessary columns from country_long and reduce file size
  country_long_subset <- country_long |>
    dplyr::tibble() |>
    dplyr::select(
      WHO_REGION, dplyr::ends_with("_NAME"), dplyr::ends_with("_GUID"),
      yr.st, yr.end, active.year.01, GUID
    ) |>
    dplyr::select(-dplyr::ends_with("VIZ_NAME"))

  rm(country_long)
  gc()

  # Transform POLIS wide and 0-5Y/U0-15Y/UALL to Under5Pop/Under15Pop/Total
  polis_pop <- pop_data |>
    dplyr::country(ADM0_NAME, year, AgeGroupCode, datasource, .keep_all = TRUE) |>
    tidyr::pivot_wider(names_from = AgeGroupCode, values_from = Value) |>
    dplyr::mutate(
      Admin0Name = ADM0_NAME,
      year = as.numeric(year),
      Under5Pop = `0-5Y`,
      Under15Pop = `0-15Y`,
      Total = ALL
    ) |>
    dplyr::select(Admin0Name, year, Under5Pop, Under15Pop, Total, datasource) |>
    join_pop_to_country_year_shapes(country_long_subset)

  # Input Non-POLIS data and removal of forward-filled repeats Values
  non_polis_pop <- load_all_patches(
    pakistan_file_path,
    somalia_2022_file_path,
    somalia_2023_file_path,
    somalia_2024_file_path,
    kenya_file_path,
    jamal_pop_file_path,
    edav
  ) |>
    join_pop_to_country_year_shapes(country_long_subset) |>
    remove_forward_fill_non_polis()

  # Combine POLIS + Non-POLIS data
  combined_pop <- dplyr::bind_rows(polis_pop, non_polis_pop) |>
    deduplicate_population()

  # Perform check to make sure ADM0GUIDs are equal to stated admin0guids of non-polis sources
  adm0guid_discrepancies <- combined_pop |>
    dplyr::mutate(
      diff_adm0guid = dplyr::case_when(
        ADM0_GUID != adm0guid ~ TRUE,
        ADM0_GUID == adm0guid ~ FALSE,
        .default = NA
      )
    ) |>
    dplyr::filter(diff_adm0guid) |>
    dplyr::left_join(country_long_subset |> dplyr::select(
      year = active.year.01,
      adm0guid = GUID,
      adm0name_from_non_api_src = ADM0_NAME
    ))

  # Count number of conflicting names
  adm0guid_discrepancies_conflicts <- adm0guid_discrepancies |>
    dplyr::filter(!is.na(adm0name_from_non_api_src)) |>
    dplyr::select(year, Admin0Name, GUID, adm0name_from_non_api_src)

  if (nrow(adm0guid_discrepancies_conflicts) > 0) {
    cli::cli_alert_info(paste0(
      "There are discrepancies in the GUIDs reported for countries in some years. ",
      "Please check the error_log folder."
    ))
    sirfunctions::sirfunctions_io("write", NULL,
                                  file_loc = file.path(
                                    pop_dir, "error_log",
                                    paste0(
                                      "adm0_discrepancies_",
                                      Sys.Date(), ".parquet"
                                    )
                                  ),
                                  edav = edav
    )
  }

  rm(adm0guid_discrepancies, adm0guid_discrepancies_conflicts)

  base_data <- country_long_subset |>
    dplyr::mutate(active.year.01 = as.numeric(active.year.01)) |>
    dplyr::rename(ADM0_GUID = "GUID", year = "active.year.01") |>
    dplyr::distinct(ADM0_GUID, year, .keep_all = TRUE) |>
    dplyr::left_join(
      combined_pop |>
        dplyr::select(ADM0_GUID, year, Under5Pop, Under15Pop, Total, datasource),
      by = c("ADM0_GUID", "year")
    ) |>
    dplyr::group_by(ADM0_GUID) |>
    dplyr::arrange(year, .by_group = TRUE) |>
    tidyr::fill(datasource, .direction = "down") |>
    dplyr::ungroup() |>
    dplyr::left_join(growth_rates, by = c("ADM0_NAME" = "Admin0Name", "year" = "year"))

  # Some years the growth rates are not available. These need to be filled using "downup."
  # We first arrange from oldest to newest year, then fill.
  # For example, if no data in 2024, 2025 but there is data in 2023, then use 2023.
  # In case of gaps, such as 2023, 2024 (blank), 2025 (blank), 2026, then fill using 2023.
  base_data <- base_data |>
    dplyr::group_by(ADM0_NAME) |>
    dplyr::arrange(year, .by_group = TRUE) |>
    tidyr::fill(growth_rate, .direction = "downup") |>
    dplyr::ungroup()

  # Output
  result <- base_data |>
    apply_growth_rate("Under5Pop") |>
    apply_growth_rate("Under15Pop") |>
    apply_growth_rate("Total") |>
    aggregate_provinces_to_country() |>
    dplyr::mutate(
      Used_Growth_Rate = ifelse(
        used_growth_Under5Pop | used_growth_Under15Pop | used_growth_Total,
        "Yes", "No"
      ),
      Used_Province_Aggregation = ifelse(
        used_aggregate_Total | used_aggregate_Under5Pop | used_aggregate_Under15Pop,
        "Yes", ""
      ),
      GUID = ADM0_GUID
    ) |>
    dplyr::rename(
      Country_Name = ADM0_NAME,
      SOURCE       = datasource
    ) |>
    dplyr::select(
      -used_growth_Under5Pop, -used_growth_Under15Pop, -used_growth_Total,
      -used_aggregate_Total, -used_aggregate_Under5Pop, -used_aggregate_Under15Pop,
      -active.year.01, -datasource
    )

  if (!is.null(output_file)) readr::write_rds(result, output_file)
  result
}
