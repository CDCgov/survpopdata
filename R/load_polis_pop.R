#' Load POLIS Population Data
#'
#' @param spatial_scale Geographic level: "ctry", "prov", or "dist"
#' @param edav Load data from EDAV
#' @param azcontainer Azure storage connection
#' @param file_loc Load from local file (path)
#'
#' @return Tibble filtered to spatial_scale with datasource column
#'
#' @examples
#' \dontrun{
#' load_polis_pop(spatial_scale = "dist")
#' load_polis_pop(spatial_scale = "prov", file_loc = "path/to/file.rds")
#' }
#'
#' @export
load_polis_pop <- function(spatial_scale,
                           edav = TRUE,
                           file_loc = "GID/PEB/SIR/POLIS/data/pop.rds",
                           azcontainer = sirfunctions::get_azure_storage_connection()) {
  # Auto-detect edav when file_loc is not specified
  if (is.null(edav)) {
    edav <- !file.exists(file_loc)
  }

  polis_data <- sirfunctions::edav_io(
    io = "read",
    NULL,
    file_loc = file_loc,
    azcontainer = azcontainer,
    edav = edav
  )

  # Validate spatial_scale
  if (!spatial_scale %in% c("ctry", "prov", "dist")) {
    stop("spatial_scale must be 'ctry', 'prov', or 'dist'")
  }

  # Column check
  col_check <- list(
    ctry = "Admin0GUID",
    prov = "Admin1GUID",
    dist = "Admin2GUID"
  )

  required_col <- col_check[[spatial_scale]]

  if (!required_col %in% colnames(polis_data)) {
    stop("Column '", required_col, "' not found in data")
  }

  # Filter and include source column
  polis_data <- polis_data |>
    dplyr::filter(!is.na(.data[[required_col]])) |>
    dplyr::mutate(datasource = "POLIS API")

  # Check if empty data
  if (nrow(polis_data) == 0) {
    warning("POLIS data is empty after filtering to '", spatial_scale, "'")
    return(polis_data)
  } else {
    message("Loaded ", nrow(polis_data), " ", spatial_scale, "-level records from POLIS")
  }

  # Validate Data_Source before returning
  polis_data |>
    assertr::verify(assertr::has_all_names("datasource")) |>
    assertr::assert(assertr::not_na, datasource) |>
    assertr::assert(function(x) x != "", datasource) |>
    assertr::assert(assertr::in_set("POLIS API"), datasource)
}
