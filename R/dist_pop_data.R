#' District Population Dataset
#'
#' Builds dataset to incorporate all districts within the spatial file and fill
#' missing values using external patches or growth rate projection.
#'
#' @param pop_data `tibble` Population data ingested from POLIS.
#' @param spatial_data `sf` Spatial data file.
#' @param patch_files `list` List of patch tibbles.
#' @param growth_rates `tibble` Growth rates per country.
#'
#' @returns `tibble` Complete district population data.
#'
#' @examples
#' \dontrun{
#' dist_pop_data(pop_data, spatial_districts, patch_files, growth_rates)
#' }
#'
#' @export
dist_pop_data <- function(pop_data, spatial_data,
                          patch_files = list(),
                          growth_rates = NULL) {
  # Spatial completeness
  spatial_attr <- spatial_data |>
    sf::st_drop_geometry()
  spatial_guids <- unique(spatial_attr$GUID)
  pop_guids <- unique(pop_data$adm2guid)
  missing_guids <- setdiff(spatial_guids, pop_guids)
  if (length(missing_guids) > 0) {
    year_age_combos <- pop_data |>
      dplyr::distinct(year, AgeGroupCode)
    missing_spatial <- spatial_attr |>
      dplyr::filter(GUID %in% missing_guids)
    missing_rows <- missing_spatial |>
      tidyr::crossing(year_age_combos) |>
      dplyr::mutate(Value = NA_real_)
    result <- dplyr::bind_rows(pop_data, missing_rows) |>
      dplyr::mutate(used_growth_rate = FALSE)
  } else {
    result <- pop_data |>
      dplyr::mutate(used_growth_rate = FALSE)
  }
  # Patch integration
  if (length(patch_files) > 0) {
    patch_data <- dplyr::bind_rows(patch_files)
    join_keys <- c("adm2guid", "AgeGroupCode", "year")
    result <- result |>
      dplyr::left_join(
        patch_data |>
          dplyr::select(dplyr::all_of(join_keys), patch_value = Value),
        by = join_keys
      ) |>
      dplyr::mutate(
        datasource = dplyr::if_else(is.na(Value) & !is.na(patch_value),
                                    "Patch_file", datasource),
        Value = dplyr::if_else(is.na(Value) & !is.na(patch_value),
                               patch_value, Value)
      ) |>
      dplyr::select(-patch_value)
  }
  # Growth rate integration
  if (!is.null(growth_rates) && any(is.na(result$Value))) {
    result <- result |>
      dplyr::left_join(growth_rates, by = c("adm0guid", "year")) |>
      dplyr::group_by(adm0guid, adm2guid, AgeGroupCode) |>
      dplyr::arrange(year, .by_group = TRUE) |>
      dplyr::mutate(
        anchor_year = min(year[!is.na(Value)], na.rm = TRUE),
        anchor_value = Value[year == anchor_year][1],
        projection_years = year - anchor_year,
        used_growth_rate = dplyr::case_when(
          is.na(Value) & projection_years > 0 & !is.na(growth_rate) ~ TRUE,
          TRUE ~ used_growth_rate
        ),
        Value = dplyr::case_when(
          is.na(Value) & projection_years > 0 & !is.na(growth_rate) ~
            anchor_value * (1 + growth_rate) ^ projection_years,
          TRUE ~ Value
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::select(!c(anchor_year, anchor_value, projection_years))
  }
  return(result)
}
