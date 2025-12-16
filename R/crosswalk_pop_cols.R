#' Crosswalks column names of the POLIS pop dataset
#'
#' @description
#' Crosswalks the original column names of the POLIS pop dataset so that it is
#' compatible with the names used by the data cleaning scripts developed within
#' CDC. The function also filters for the only columns used in the population
#' datasets and adds new columns, such as the adm{0-2}guids. In addition, the
#' tables get transformed by pivoting wider using the AgeGroupCode.
#'
#' @param pop_data `tibble` The population dataset from the POLIS API. It can be
#' the entire table or a subset of it.
#'
#' @returns `tibble` A tibble with the renamed columns.
#' @export
#'
#' @examples
#' \dontrun{
#' dist_pop <- load_polis_pop("dist")
#' dist_pop <- crosswalk_pop_cols(dist_pop)
#'
#' }
crosswalk_pop_cols <- function(pop_data) {

  formatted_pop <- pop_data |>
    dplyr::mutate(Value = as.numeric(Value)) |> # in case Value is a string
    dplyr::rename_with(dplyr::recode,
                       CountryISO3Code = "ISO_3_CODE",
                       WHORegion = "WHO_REGION",
                       Admin0Name = "ADM0_NAME",
                       Admin1Name = "ADM1_NAME",
                       Admin2Name = "ADM2_NAME",
                       StartDate = "STARTDATE",
                       EndDate = "ENDDATE",
                       Year = "active.year.01") |>
    dplyr::mutate(adm0guid = paste0("{", stringr::str_to_upper(Admin0GUID), "}"),
                  adm1guid = paste0("{", stringr::str_to_upper(Admin1GUID), "}"),
                  adm2guid = paste0("{", stringr::str_to_upper(Admin2GUID), "}"),
                  CreatedDate = lubridate::as_date(CreatedDate),
                  UpdatedDate = lubridate::as_date(UpdatedDate),
                  STARTDATE = lubridate::as_date(STARTDATE),
                  ENDDATE = lubridate::as_date(ENDDATE)
                  )

  formatted_pop <- formatted_pop |>
    dplyr::select(dplyr::any_of(c("ISO_3_CODE", "WHO_REGION",
                                  "ADM0_NAME", "ADM1_NAME", "ADM2_NAME",
                                  "adm0guid", "adm1guid", "adm2guid",
                                  "STARTDATE", "ENDDATE", "year", "AgeGroupCode",
                                  "Value", "datasource")))
  return(formatted_pop)
}
