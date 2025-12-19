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
                       CreatedDate = "CREATEDDATE",
                       UpdatedDate = "UPDATEDDATE",
                       StartDate = "STARTDATE",
                       EndDate = "ENDDATE",
                       Year = "year") |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c("Admin0GUID", "Admin1GUID", "Admin2GUID")),
                                \(x) paste0("{", stringr::str_to_upper(x), "}"))) |>
    dplyr::rename_with(dplyr::recode,
                       Admin0GUID = "adm0guid",
                       Admin1GUID = "adm1guid",
                       Admin2GUID = "adm2guid")

  formatted_pop <- formatted_pop |>
    dplyr::select(dplyr::any_of(c("ISO_3_CODE", "WHO_REGION",
                                  "ADM0_NAME", "ADM1_NAME", "ADM2_NAME",
                                  "adm0guid", "adm1guid", "adm2guid",
                                  "STARTDATE", "ENDDATE",
                                  "CREATEDDATE", "UPDATEDDATE",
                                  "year", "AgeGroupCode",
                                  "Value", "datasource")))
  return(formatted_pop)
}
