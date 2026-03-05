#' Create a map to check for population outliers
#'
#' @description
#' Maps the percentage of change in population over years
#' to check for outliers in the population data. This is a
#' diagnostic tool.
#'
#' @param pop_w_outlier_cat `tibble` Output of [detect_processed_pop_outliers()].
#' @param sf_long `sf` Shapefile in long format.
#' Any of the sirfunctions::load_clean_*_sp(type = "long"), as long as the administrative level
#' is the same as the population file.
#' @param pop_cat `str` Population category to map. Either u15, u5, or totpop.
#' @param ctry_name `str` Optional country name
#' @param year_end `int` Year end. Defaults to current year.
#' @param year_start `int` Year start. Defaults to four years ago from the end year.
#'
#' @returns `ggplot2` A map showing population count stability over the year
#' @export
#'
map_pop_outlier <- function(pop_w_outlier_cat,
                            sf_long,
                            pop_cat = "u15",
                            ctry_name = NULL,
                            year_end = lubridate::year(Sys.Date()),
                            year_start = year_end - 3
                            ) {
  adm_level <- NA
  if ("dist" %in% names(pop_w_outlier_cat)) {
    adm_level <- "adm2guid"
  } else if ("prov" %in% names(pop_w_outlier_cat)) {
    adm_level <- "adm1guid"
  } else {
    adm_level <- "adm0guid"
  }

  pop_cat <- switch(cat,
                "u15" = "dec_change_u15_cat",
                "u5" = "dec_change_u5_cat",
                "tot" = "dec_change_tot_cat"
  )

  # Filter the shapefile and pop file
  sf_w_pop <- dplyr::left_join(sf_long |>
                                 dplyr::select(ctry = ADM0_NAME,
                                               GUID,
                                               year = active.year.01),
                               pop_w_outlier_cat |>
                                 dplyr::rename(GUID = adm_level) |>
                                 dplyr::select(dplyr::any_of(c("ctry",
                                                               "GUID",
                                                               "year",
                                                               pop_cat)))) |>
    dplyr::filter(dplyr::between(year, year_start, year_end))

  if (!is.null(ctry_name)) {
    sf_w_pop <- sf_w_pop |>
      dplyr::filter(ctry == ctry_name)
  }

  plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = sf_w_pop, ggplot2::aes(fill = !!dplyr::sym(pop_cat))) +
    ggplot2::scale_fill_manual(
      name = "Outlier category",
      values = c("normal (<3%)" = "#89CFF1",
                 "high (3-4.9%)" = "#FFA630",
                 "rare (5-6.9%)" = "darkorange",
                 "very rare (7-14.9%)" = "red",
                 "likely data error (>15%)" = "darkred")
    ) +
    ggplot2::facet_wrap(~year, ncol = length(year_start:year_end))

}
