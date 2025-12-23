#' Check if all required age groups are present
#'
#' @description
#' Uses the AgeGroupCode column of the POLIS pop file.
#'
#' @param age_col `str` A string or vector of age group codes.
#'
#' @returns A list of missing group codes
#' @keywords internal
#'
missing_required_agegroups <- function(age_col) {
  required <- c("0-5Y", "0-15Y", "ALL")
  missing_groups <- setdiff(required, age_col)

  if (length(missing_groups) == 0) {
    NA_character_
  } else {
    paste(missing_groups, collapse = ", ")
  }
}

#' Find conflicting admin mappings
#' @noRd
find_conflicting_admins <- function(data, admin_check, upper_admin_check, return_details = FALSE) {
  sym <- dplyr::sym

  conflicts <- data |>
    dplyr::filter(!is.na(!!sym(admin_check)), !!sym(admin_check) != "") |>
    dplyr::distinct(!!sym(admin_check), !!sym(upper_admin_check)) |>
    dplyr::group_by(!!sym(admin_check)) |>
    dplyr::filter(dplyr::n_distinct(!!sym(upper_admin_check)) > 1)

  if (return_details) {
    conflicts |>
      dplyr::summarise(
        conflicting_parents = paste(unique(!!sym(upper_admin_check)), collapse = ", "),
        .groups = "drop"
      )
  } else {
    conflicts |>
      dplyr::pull(!!sym(admin_check)) |>
      unique()
  }
}

#' Create age group predicate generator
#' @noRd
create_agegroup_predicate <- function(missing_guids) {
  function(x) {
    function(y) !(y %in% missing_guids) | is.na(y)
  }
}

#' Create unique parent predicate generator
#' @noRd
create_parent_predicate <- function(guid_col, data) {
  if (guid_col == "Admin2GUID") {
    conflicting_dist_prov <- find_conflicting_admins(data, "Admin2GUID", "Admin1GUID")
    conflicting_dist_ctry <- find_conflicting_admins(data, "Admin2GUID", "Admin0GUID")
    all_conflicting_admins <- unique(c(conflicting_dist_prov, conflicting_dist_ctry))
    function(x) {
      function(y) !(y %in% all_conflicting_admins) | is.na(y)
    }
  } else if (guid_col == "Admin1GUID") {
    conflicting_prov_ctry <- find_conflicting_admins(data, "Admin1GUID", "Admin0GUID")
    function(x) {
      function(y) !(y %in% conflicting_prov_ctry) | is.na(y)
    }
  } else {
    function(x) {
      function(y) TRUE
    }
  }
}

#' Compute conflicting parent admins per GUID
#' @noRd
compute_conflicting_parents_df <- function(guid_col, data) {
  case_when <- dplyr::case_when

  if (guid_col == "Admin2GUID") {
    conflicts_prov <- find_conflicting_admins(data, "Admin2GUID", "Admin1GUID", return_details = TRUE) |>
      dplyr::rename(conflict_prov = conflicting_parents)
    conflicts_ctry <- find_conflicting_admins(data, "Admin2GUID", "Admin0GUID", return_details = TRUE) |>
      dplyr::rename(conflict_ctry = conflicting_parents)

    data |>
      dplyr::distinct(Admin2GUID) |>
      dplyr::left_join(conflicts_prov, by = "Admin2GUID") |>
      dplyr::left_join(conflicts_ctry, by = "Admin2GUID") |>
      dplyr::mutate(
        duplicated_parents = case_when(
          !is.na(conflict_prov) & !is.na(conflict_ctry) ~ paste("Prov:", conflict_prov, "; Ctry:", conflict_ctry),
          !is.na(conflict_prov) ~ paste("Prov:", conflict_prov),
          !is.na(conflict_ctry) ~ paste("Ctry:", conflict_ctry),
          TRUE ~ NA_character_
        )
      ) |>
      dplyr::filter(!is.na(duplicated_parents)) |>
      dplyr::select(Admin2GUID, duplicated_parents)
  } else if (guid_col == "Admin1GUID") {
    find_conflicting_admins(data, "Admin1GUID", "Admin0GUID", return_details = TRUE) |>
      dplyr::rename(Admin1GUID = 1, duplicated_parents = conflicting_parents)
  } else {
    dplyr::tibble(!!dplyr::sym(guid_col) := character(), duplicated_parents = character())
  }
}

#' Check Population Data Quality
#'
#' Runs validation checks on population data from POLIS and returns flagged rows.
#'
#' @param pop_rds Tibble containing population data from POLIS.
#' @param spatial_scale Geographic level: "ctry", "prov", or "dist".
#'
#' @return A tibble of flagged rows with validation columns appended, or NULL if all checks pass.
#'
#' Validation checks include:
#' \itemize{
#'   \item Required columns exist
#'   \item No missing values in Value, StartDate, AgeGroupCode, PlaceId
#'   \item Value is non-negative
#'   \item StartDate is not after EndDate
#'   \item CreatedDate and UpdatedDate are not in the future
#'   \item Year matches the year in StartDate
#'   \item Each Admin GUID has all three age groups (U5, U15, TotalPop)
#'   \item Each Admin GUID maps to a single parent admin
#'   \item Value is within 2 standard deviations of the mean
#' }
#'
#' @examples
#' \dontrun{
#' check_pop_data_quality(raw_dist_pop, spatial_scale = "dist")
#' check_pop_data_quality(raw_prov_pop, spatial_scale = "prov")
#' check_pop_data_quality(raw_prov_pop, spatial_scale = "ctry")
#' }
#'
#' @export
check_pop_data_quality <- function(pop_rds, spatial_scale) {
  sym <- dplyr::sym
  case_when <- dplyr::case_when
  row_number <- dplyr::row_number

  # Spatial scale filter
  pop_rds <- switch(spatial_scale,
                    "ctry" = dplyr::filter(pop_rds, !is.na(Admin0GUID) & is.na(Admin1GUID) & is.na(Admin2GUID)),
                    "prov" = dplyr::filter(pop_rds, !is.na(Admin0GUID) & !is.na(Admin1GUID) & is.na(Admin2GUID)),
                    "dist" = dplyr::filter(pop_rds, !is.na(Admin0GUID) & !is.na(Admin1GUID) & !is.na(Admin2GUID))
  )

  # Set GUID column
  guid_col <- switch(spatial_scale,
                     "ctry" = "Admin0GUID",
                     "prov" = "Admin1GUID",
                     "dist" = "Admin2GUID"
  )

  # Compute missing age groups per GUID
  missing_agegroups_df <- pop_rds |>
    dplyr::filter(!is.na(!!sym(guid_col))) |>
    dplyr::group_by(!!sym(guid_col)) |>
    dplyr::summarise(
      present = list(unique(AgeGroupCode)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      missing = sapply(present, missing_required_agegroups)
    ) |>
    dplyr::filter(!is.na(missing)) |>
    dplyr::select(dplyr::all_of(guid_col), missing_agegroups = missing)

  missing_guids <- missing_agegroups_df |>
    dplyr::pull(!!sym(guid_col)) |>
    unique()

  # Create predicate generators
  has_all_agegroups <- create_agegroup_predicate(missing_guids)
  has_unique_parent <- create_parent_predicate(guid_col, pop_rds)

  # Compute conflicting parent admins per GUID (for detail column later)
  conflicting_parents_df <- compute_conflicting_parents_df(guid_col, pop_rds)

  # Run validation chain
  validated <- pop_rds |>
    assertr::chain_start() |>
    assertr::verify(assertr::has_all_names(
      "Admin0GUID", "Admin0Id", "Admin0Name",
      "Admin1GUID", "Admin1Id", "Admin1Name",
      "Admin2GUID", "Admin2Id", "Admin2Name",
      "AgeGroupCode", "CountryISO3Code",
      "CreatedDate", "EndDate", "StartDate", "UpdatedDate",
      "Value", "WHORegion", "Year", "datasource", "PlaceId"
    )) |>
    assertr::assert(assertr::not_na, Value, StartDate, AgeGroupCode, PlaceId) |>
    assertr::assert(assertr::within_bounds(0, Inf), Value) |>
    assertr::verify(is.na(EndDate) | StartDate <= EndDate) |>
    assertr::verify(is.na(CreatedDate) | CreatedDate <= Sys.Date()) |>
    assertr::verify(is.na(UpdatedDate) | UpdatedDate <= Sys.Date()) |>
    assertr::verify(is.na(Year) | is.na(StartDate) | Year == lubridate::year(StartDate)) |>
    assertr::insist(has_all_agegroups, !!sym(guid_col)) |>
    assertr::insist(has_unique_parent, !!sym(guid_col)) |>
    assertr::insist(assertr::within_n_sds(2), Value) |>
    assertr::chain_end(error_fun = assertr::error_df_return)

  # Check if errors were returned
  has_errors <- nrow(validated) > 0

  # All checks passed
  if (!has_errors) {
    message("All checks passed")
    return(invisible(NULL))
  }

  # Extract errors into table
  error_rows <- validated |>
    dplyr::filter(!is.na(index)) |>
    dplyr::mutate(
      col_name = case_when(
        verb == "assert" & column %in% c("Value", "StartDate", "AgeGroupCode", "PlaceId") ~ "missing_required_values",
        verb == "assert" & grepl("within_bounds", predicate, ignore.case = TRUE) ~ "negative_value",
        verb == "verify" & grepl("EndDate.*StartDate|StartDate.*EndDate", predicate) ~ "startdate_after_enddate",
        verb == "verify" & grepl("CreatedDate", predicate) ~ "created_date_future",
        verb == "verify" & grepl("UpdatedDate", predicate) ~ "updated_date_future",
        verb == "verify" & grepl("Year.*StartDate|StartDate.*Year", predicate) ~ "year_startdate_mismatch",
        verb == "insist" & grepl("has_all_agegroups", predicate) ~ "agegroup_missing",
        verb == "insist" & grepl("has_unique_parent", predicate) ~ "duplicated_parent_admin",
        verb == "insist" & grepl("within_n_sds", predicate, ignore.case = TRUE) ~ "pop_outlier",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(col_name)) |>
    dplyr::select(row_num = index, validation_col = col_name)

  # Skip filter if no rows
  if (nrow(error_rows) == 0) {
    message("No validation flag detected")
    return(invisible(NULL))
  }

  # Pivot to wide format
  flags_wide <- error_rows |>
    dplyr::mutate(flag = "FLAG") |>
    tidyr::pivot_wider(names_from = validation_col, values_from = flag, values_fill = NA_character_)

  # Join with original data
  result <- pop_rds |>
    dplyr::mutate(row_num = row_number()) |>
    dplyr::left_join(flags_wide, by = "row_num")

  # Add detail columns for agegroup_missing
  if ("agegroup_missing" %in% names(result)) {
    result <- result |>
      dplyr::left_join(missing_agegroups_df, by = guid_col)
  }

  # Add detail columns for duplicated_parent_admin
  if ("duplicated_parent_admin" %in% names(result)) {
    result <- result |>
      dplyr::left_join(conflicting_parents_df, by = guid_col)
  }

  # Filter and select
  val_cols <- setdiff(names(flags_wide), "row_num")
  flagged_cols <- val_cols[sapply(val_cols, function(c) any(result[[c]] == "FLAG", na.rm = TRUE))]

  # Add detail columns if they exist
  detail_cols <- intersect(c("missing_agegroups", "duplicated_parents"), names(result))

  result <- result |>
    dplyr::filter(dplyr::if_any(dplyr::all_of(flagged_cols), ~ . == "FLAG")) |>
    dplyr::select(row_num, dplyr::all_of(names(pop_rds)), dplyr::all_of(flagged_cols), dplyr::all_of(detail_cols))

  message(paste0(nrow(result), " rows flagged"))
  return(result)
}
