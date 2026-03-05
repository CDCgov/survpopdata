#' Map district population
#'
#' @description
#' The function acts as a diagnostic tool be quickly checking which districts are
#' missing population data.
#'
#' @param district_long `sf` District shapefile in long format.
#' @param dist_pop `tibble` District population.
#' @param who_region `str` Region to map.
#' @param .year `int` Year to map.
#' @param country `str` Country to map (optional).
#'
#' @returns `ggplot` A map with U15pop data.
#' @export
create_dist_pop_map <- function(district_long, dist_pop,
                           who_region,
                           .year,
                           country = NULL) {
  combined_sf_pop <- dplyr::left_join(district_long |>
                                        dplyr::select(
                                          ctry = ADM0_NAME,
                                          prov = ADM1_NAME,
                                          dist = ADM2_NAME,
                                          adm0guid = ADM0_GUID,
                                          adm1guid = ADM1_GUID,
                                          adm2guid = GUID,
                                          year = active.year.01
                                        ),
                                      dist_pop)

  # Filter by year and region
  combined_sf_pop <- combined_sf_pop |>
    dplyr::filter(who.region == who_region,
                  year == .year)

  if (!is.null(country)) {
    combined_sf_pop <- combined_sf_pop |>
      dplyr::filter(ctry == country)
  }

  # Map populations
  plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = u15pop), data = combined_sf_pop) +
    ggplot2::scale_fill_distiller(
      palette = "YlOrRd",
      direction = "both",
      labels = scales::comma
    ) +
    sirfunctions::f.plot.looks("epicurve") +
    ggplot2::scale_size_identity() +
    ggplot2::labs(caption = "- Under 15 population is shown at the district level") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 15,
        face = "bold",
        hjust = 0.5
      ),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      legend.position = "right",
      plot.caption = ggplot2::element_text(hjust = 0, size = 11),
      legend.background = ggplot2::element_blank()
    )

  return(plot)

}
