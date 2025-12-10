#' Load POLIS Population Data
#'
#' @param spatial_scale Geographic level: "ctry", "prov", or "dist"
#' @param edav Load data from EDAV
#' @param azcontainer Azure storage connection
#' @param file_loc Load from local file (path)
#'
#' @return Tibble filtered to spatial_scale with Data_Source column
#'
#' @examples
#' \dontrun{
#' load_polis_pop(spatial_scale = "dist")
#' load_polis_pop(spatial_scale = "prov", file_loc = "path/to/file.rds")
#' }
#'
#' @export
load_polis_pop <- function(spatial_scale,
                           edav = NULL,
                           file_loc = "pop.rds",
                           azcontainer = NULL) {
  # Auto-detect edav when file_loc is not specified
  if (is.null(edav)) {
    edav <- !file.exists(file_loc)
  }

  # Validate spatial_scale
  if (!spatial_scale %in% c("ctry", "prov", "dist")) {
    stop("spatial_scale must be 'ctry', 'prov', or 'dist'")
  }

  # Check file exists if local
  if (!edav && !file.exists(file_loc)) {
    stop("File not found: ", file_loc)
  }

  # Load data
  polis_data <- if (edav) {
    if (is.null(azcontainer)) {
      azcontainer <- sirfunctions::get_azure_storage_connection()
    }
    sirfunctions::edav_io(io = "read", file_loc = file_loc, azcontainer = azcontainer)
  } else {
    readRDS(file_loc)
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
    dplyr::mutate(Data_Source = "POLIS API")

  # Check if empty data
  if (nrow(polis_data) == 0) {
    warning("POLIS data is empty after filtering to '", spatial_scale, "'")
    return(polis_data)
  } else {
    message("Loaded ", nrow(polis_data), " ", spatial_scale, "-level records from POLIS")
  }

  # Validate Data_Source before returning
  polis_data |>
    assertr::verify(assertr::has_all_names("Data_Source")) |>
    assertr::assert(assertr::not_na, Data_Source) |>
    assertr::assert(function(x) x != "", Data_Source) |>
    assertr::assert(assertr::in_set("POLIS API"), Data_Source)
}
