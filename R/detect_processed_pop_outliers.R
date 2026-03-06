#' Detects whether there are issues with the population data
#'
#' @param cleaned_pop_data `tibble` Cleaned population data. Either dist, prov, or country.
#' @param group_col `str` Name of the grouping variable. Recommend adm0guid, adm1guid, or adm2guid.
#'
#' @returns `tibble` Pop data with category of outlier.
#' @export
#'
detect_processed_pop_outliers <- function(cleaned_pop_data, group_col = "adm1guid") {

  outlier_summary <- cleaned_pop_data |>
    dplyr::arrange(!!dplyr::sym(group_col), year) |>
    dplyr::mutate(
      prev_val_u15 = dplyr::lag(u15pop),
      prev_val_u5 = dplyr::lag(u5pop),
      prev_val_tot = dplyr::lag(totpop),
      dec_change_u15 = abs((u15pop - prev_val_u15) / prev_val_u15),
      dec_change_u5 = abs((u5pop - prev_val_u5) / prev_val_u5),
      dec_change_tot = abs((totpop - prev_val_tot) / prev_val_tot),
      , .by = dplyr::all_of(unique(c("adm0guid", group_col))))

  # Categorize the changes
  outlier_summary <- outlier_summary |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("dec_change"),
                    \(x) dplyr::case_when(
                      x < 0.03 ~ "normal (<3%)",
                      dplyr::between(x, 0.03, 0.049) ~ "high (3-4.9%)",
                      dplyr::between(x, 0.05, 0.069) ~ "rare (5-6.9%)",
                      dplyr::between(x, 0.07, 0.149) ~ "very rare (7-14.9%)",
                      dplyr::between(x, 0.15, Inf) ~ "likely data error (>15%)",
                      .default = "no data"
                    ), .names = "{.col}_cat")
    ) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c("dec_change_u15_cat",
                                                "dec_change_u5_cat",
                                                "dec_change_tot_cat")),
                                \(x) factor(x,
                                            levels = c("normal (<3%)",
                                                       "high (3-4.9%)",
                                                       "rare (5-6.9%)",
                                                       "very rare (7-14.9%)",
                                                       "likely data error (>15%)",
                                                       "no data"),
                                            ordered = TRUE))) |>
    dplyr::select(-dplyr::starts_with("prev_val"),
                  -dec_change_u15,
                  -dec_change_u5,
                  -dec_change_tot)

  # Communicate the errors
  cli::cli_alert_info("Under 15 Pop summary")
  print(outlier_summary$dec_change_u15_cat |>
    table())

  cli::cli_alert_info("Under 5 Pop summary")
  print(outlier_summary$dec_change_u5_cat |>
    table())

  cli::cli_alert_info("Total Pop summary")
  print(outlier_summary$dec_change_tot_cat |>
    table())


  invisible(outlier_summary)

}
