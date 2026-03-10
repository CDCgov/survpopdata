#' Remove Forward-Filled POLIS Population Values
#'
#' Identifies forward-filled population records and sets them to NA.
#'
#' @param pop_data `tibble` Population data.
#'
#' @returns `tibble` Population data with forward-filled values set to NA.
#'
#' @examples
#' \dontrun{
#' remove_forward_fill_polis_pop(pop_data)
#' }
#'
#' @export
remove_forward_fill_polis_pop <- function(pop_data) {
  guid_col <- dplyr::case_when(
    "adm2guid" %in% names(pop_data) ~ "adm2guid",
    "adm1guid" %in% names(pop_data) ~ "adm1guid",
    "adm0guid" %in% names(pop_data) ~ "adm0guid"
  )

  pop_data |>
    dplyr::arrange(!!dplyr::sym(guid_col), AgeGroupCode, year) |>
    dplyr::group_by(!!dplyr::sym(guid_col), AgeGroupCode, FK_DataSetId) |>
    dplyr::mutate(
      prev_value = dplyr::lag(Value),
      prev_year = dplyr::lag(year),
      is_forward_fill = !is.na(prev_value) &
        Value == prev_value &
        year == prev_year + 1,
      Value = dplyr::if_else(is_forward_fill, NA_real_, Value)
    ) |>
    dplyr::select(!c(prev_value, prev_year)) |>
    dplyr::ungroup()
}
