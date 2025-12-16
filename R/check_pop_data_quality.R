#' Check Population Data Quality
#'
#' Runs validation checks on population data from POLIS and returns flagged rows.
#'
#' @param pop.rds Tibble containing population data from POLIS.
#' @param spatial_scale Geographic level: "ctry", "prov", or "dist".
#'
#' @return A tibble of flagged rows with validation columns appended, or NULL if all checks pass.
#'
#' Validation checks include:
#' \itemize{
#'   \item Required columns exist
#'   \item No missing values in Value, StartDate, AgeGroupName, PlaceID
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
check_pop_data_quality <- function(pop.rds, spatial_scale) {
  guid_col <- dplyr::case_when(
    spatial_scale == "dist" ~ "Admin2GUID",
    spatial_scale == "prov" ~ "Admin1GUID",
    spatial_scale == "ctry" ~ "Admin0GUID"
  )

  # Compute missing age groups per GUID (for detail column later)
  missing_agegroups_df <- pop.rds |>
    filter(!is.na(.data[[guid_col]])) |>
    group_by(.data[[guid_col]]) |>
    summarise(
      present = list(unique(AgeGroupName)),
      .groups = "drop"
    ) |>
    mutate(
      missing = sapply(present, function(x) {
        required <- c("U5", "U15", "TotalPop")
        missing_groups <- setdiff(required, x)
        if (length(missing_groups) == 0) {
          NA_character_
        } else {
          paste(missing_groups, collapse = ", ")
        }
      })
    ) |>
    filter(!is.na(missing)) |>
    select(all_of(guid_col), missing_agegroups = missing)

  # Predicate generator: checks if AdminGUID has all 3 age groups
  has_all_agegroups <- function(guid_vector) {
    complete_guids <- pop.rds |>
      filter(!is.na(.data[[guid_col]])) |>
      group_by(.data[[guid_col]]) |>
      summarise(has_all = setequal(unique(AgeGroupName), c("U5", "U15", "TotalPop")), .groups = "drop") |>
      filter(has_all) |>
      pull(.data[[guid_col]])

    function(x) x %in% complete_guids | is.na(x)
  }

  # Helper: finds conflicting admin mappings (used by both predicate and detail)
  find_conflicting_admins <- function(admin_check, upper_admin_check, return_details = FALSE) {
    conflicts <- pop.rds |>
      filter(!is.na(.data[[admin_check]]), .data[[admin_check]] != "") |>
      distinct(.data[[admin_check]], .data[[upper_admin_check]]) |>
      group_by(.data[[admin_check]]) |>
      filter(n_distinct(.data[[upper_admin_check]]) > 1)

    if (return_details) {
      conflicts |>
        summarise(
          conflicting_parents = paste(unique(.data[[upper_admin_check]]), collapse = ", "),
          .groups = "drop"
        )
    } else {
      conflicts |>
        pull(.data[[admin_check]]) |>
        unique()
    }
  }

  # Predicate generator: checks relational integrity based on spatial scale
  has_unique_parent <- function(guid_vector) {
    if (guid_col == "Admin2GUID") {
      conflicting_dist_prov <- find_conflicting_admins("Admin2GUID", "Admin1GUID")
      conflicting_dist_ctry <- find_conflicting_admins("Admin2GUID", "Admin0GUID")
      all_conflicting_admins <- unique(c(conflicting_dist_prov, conflicting_dist_ctry))
      function(x) !(x %in% all_conflicting_admins) | is.na(x)
    } else if (guid_col == "Admin1GUID") {
      conflicting_prov_ctry <- find_conflicting_admins("Admin1GUID", "Admin0GUID")
      function(x) !(x %in% conflicting_prov_ctry) | is.na(x)
    } else {
      function(x) TRUE
    }
  }

  # Compute conflicting parent admins per GUID (for detail column later)
  if (guid_col == "Admin2GUID") {
    conflicts_prov <- find_conflicting_admins("Admin2GUID", "Admin1GUID", return_details = TRUE) |>
      rename(conflict_prov = conflicting_parents)
    conflicts_ctry <- find_conflicting_admins("Admin2GUID", "Admin0GUID", return_details = TRUE) |>
      rename(conflict_ctry = conflicting_parents)

    conflicting_parents_df <- pop.rds |>
      distinct(Admin2GUID) |>
      left_join(conflicts_prov, by = "Admin2GUID") |>
      left_join(conflicts_ctry, by = "Admin2GUID") |>
      mutate(
        duplicated_parents = case_when(
          !is.na(conflict_prov) & !is.na(conflict_ctry) ~ paste("Prov:", conflict_prov, "; Ctry:", conflict_ctry),
          !is.na(conflict_prov) ~ paste("Prov:", conflict_prov),
          !is.na(conflict_ctry) ~ paste("Ctry:", conflict_ctry),
          TRUE ~ NA_character_
        )
      ) |>
      filter(!is.na(duplicated_parents)) |>
      select(Admin2GUID, duplicated_parents)
  } else if (guid_col == "Admin1GUID") {
    conflicting_parents_df <- find_conflicting_admins("Admin1GUID", "Admin0GUID", return_details = TRUE) |>
      rename(Admin1GUID = 1, duplicated_parents = conflicting_parents)
  } else {
    conflicting_parents_df <- tibble(!!sym(guid_col) := character(), duplicated_parents = character())
  }

  # Run validation chain
  validated <- pop.rds |>
    chain_start() |>
    verify(has_all_names(
      "Admin0GUID", "Admin0Id", "Admin0Name",
      "Admin1GUID", "Admin1Id", "Admin1Name",
      "Admin2GUID", "Admin2Id", "Admin2Name",
      "AgeGroupName", "CountryISO3Code",
      "CreatedDate", "EndDate", "StartDate", "UpdatedDate",
      "Value", "WHORegion", "Year", "Data_Source", "PlaceID"
    )) |>
    assert(not_na, Value, StartDate, AgeGroupName, PlaceID) |>
    assert(within_bounds(0, Inf), Value) |>
    verify(is.na(EndDate) | StartDate <= EndDate) |>
    verify(is.na(CreatedDate) | CreatedDate <= Sys.Date()) |>
    verify(is.na(UpdatedDate) | UpdatedDate <= Sys.Date()) |>
    verify(is.na(Year) | is.na(StartDate) | Year == lubridate::year(StartDate)) |>
    insist(has_all_agegroups, !!sym(guid_col)) |>
    insist(has_unique_parent, !!sym(guid_col)) |>
    insist(within_n_sds(2), Value) |>
    chain_end(error_fun = error_return)

  # Check if errors were returned
  has_errors <- is.list(validated) &&
    length(validated) > 0 &&
    any(sapply(validated, function(x) inherits(x, "assertr_error")))

  # All checks passed
  if (!has_errors) {
    message("All checks passed")
    return(invisible(NULL))
  }

  # Extract errors into table
  error_rows <- bind_rows(lapply(validated, function(err) {
    if (is.null(err$error_df) || !"index" %in% names(err$error_df)) {
      return(NULL)
    }

    edf <- err$error_df
    idx <- edf$index[!is.na(edf$index)]

    if (length(idx) == 0) {
      return(NULL)
    }

    # Get verb and predicate from error_df
    err_verb <- unique(edf$verb)[1]
    err_predicate <- unique(edf$predicate)[1]
    err_column <- if ("column" %in% names(edf) && !all(is.na(edf$column))) {
      unique(edf$column[!is.na(edf$column)])[1]
    } else {
      ""
    }

    # Determine validation column name based on error type
    col_name <- case_when(
      err_verb == "assert" && err_column %in% c("Value", "StartDate", "AgeGroupName", "PlaceID") ~ "missing_required_values",
      err_verb == "assert" && grepl("within_bounds", err_predicate, ignore.case = TRUE) ~ "negative_value",
      err_verb == "verify" && grepl("EndDate.*StartDate|StartDate.*EndDate", err_predicate) ~ "start_after_end",
      err_verb == "verify" && grepl("CreatedDate", err_predicate) ~ "created_date_future",
      err_verb == "verify" && grepl("UpdatedDate", err_predicate) ~ "updated_date_future",
      err_verb == "verify" && grepl("Year.*StartDate|StartDate.*Year", err_predicate) ~ "year_start_mismatch",
      err_verb == "insist" && grepl("has_all_agegroups", err_predicate) ~ "agegroup_missing",
      err_verb == "insist" && grepl("has_unique_parent", err_predicate) ~ "duplicated_parent_admin",
      err_verb == "insist" && grepl("within_n_sds", err_predicate, ignore.case = TRUE) ~ "pop_outlier",
      TRUE ~ NA_character_
    )

    if (is.na(col_name)) {
      return(NULL)
    }

    tibble(row_num = idx, validation_col = col_name)
  }))

  # Skip filter if no rows
  if (nrow(error_rows) == 0) {
    message("No validation flag detected")
    return(invisible(NULL))
  }

  # Pivot to wide format
  flags_wide <- error_rows |>
    mutate(flag = "FLAG") |>
    pivot_wider(names_from = validation_col, values_from = flag, values_fill = NA_character_)

  # Join with original data
  result <- pop.rds |>
    mutate(row_num = row_number()) |>
    left_join(flags_wide, by = "row_num")

  # Add detail columns for agegroup_missing
  if ("agegroup_missing" %in% names(result)) {
    result <- result |>
      left_join(missing_agegroups_df, by = guid_col)
  }

  # Add detail columns for duplicated_parent_admin
  if ("duplicated_parent_admin" %in% names(result)) {
    result <- result |>
      left_join(conflicting_parents_df, by = guid_col)
  }

  # Filter and select
  val_cols <- setdiff(names(flags_wide), "row_num")
  flagged_cols <- val_cols[sapply(val_cols, function(c) any(result[[c]] == "FLAG", na.rm = TRUE))]

  # Add detail columns if they exist
  detail_cols <- intersect(c("missing_agegroups", "duplicated_parents"), names(result))

  result <- result |>
    filter(if_any(all_of(flagged_cols), ~ . == "FLAG")) |>
    select(row_num, all_of(names(pop.rds)), all_of(flagged_cols), all_of(detail_cols))

  message(sprintf("%d rows flagged with validation flags", nrow(result)))
  return(result)
}
