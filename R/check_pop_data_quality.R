# Private functions ----
#' Check if all required age groups are present
#'
#' @param present_agegroups `str` Character vector of age group codes.
#'
#' @returns `str` `NA` if all present, otherwise a string of missing age groups.
#'
#' @keywords internal
missing_required_agegroups <- function(present_agegroups) {
  required <- c("0-5Y", "0-15Y", "ALL")
  missing_groups <- setdiff(required, present_agegroups)

  if (length(missing_groups) == 0) {
    return(NA_character_)
  } else {
    return(paste(missing_groups, collapse = ", "))
  }
}

#' Find conflicting admin mappings
#'
#' @description
#' Identifies child admin units that map to multiple parent admin units.
#'
#' @param pop_data `tibble` Population dataset.
#' @param child_admin_col `str` Column name of the next lower admin column. For example,
#' "Admin2GUID" if `pop_data` contains the province population dataset.
#' @param parent_admin_col `str` Parent admin column. For example, "Admin1GUID"
#' if `pop_data` contains the province population dataset.
#' @param return_details `logical`
#'   If TRUE, returns a tibble with conflicting parent GUIDs per child.
#'   If FALSE, returns the child GUIDs with multiple parents.
#'
#' @returns `tibble` Summary tibble with conflict details or vector of conflicting GUIDs.
#'
#' @keywords internals
find_conflicting_admins <- function(pop_data, child_admin_col, parent_admin_col,
                                    return_details = FALSE) {

  conflicts <- pop_data |>
    dplyr::filter(!is.na(!!dplyr::sym(child_admin_col)),
                  !!dplyr::sym(child_admin_col) != "") |>
    dplyr::distinct(!!dplyr::sym(child_admin_col), !!dplyr::sym(parent_admin_col)) |>
    dplyr::group_by(!!dplyr::sym(child_admin_col)) |>
    dplyr::filter(dplyr::n_distinct(!!dplyr::sym(parent_admin_col)) > 1)

  if (return_details) {
    error_summary <- conflicts |>
      dplyr::summarise(
        conflicting_parents = paste(
          unique(!!dplyr::sym(parent_admin_col)),
          collapse = ", "
        ),
        .groups = "drop"
      )
  } else {
    error_summary <- conflicts |>
      dplyr::pull(!!dplyr::sym(child_admin_col)) |>
      unique()
  }

  return(error_summary)
}

#' Summarize conflicting parent admins per GUID
#'
#' @description
#' Returns the admin GUIDs that map to multiple parent admin units, with the
#' conflicting parent GUIDs listed.
#'
#'  - For "Admin2GUID", reports conflicts against Admin1GUID and Admin0GUID.
#'  - For "Admin1GUID", reports conflicts against Admin0GUID.
#'
#' @param pop_data `tibble` Population data.
#' @param guid_col `str` Name of the GUID column to summarize.
#'
#' @returns `tibble` Summary dataset of GUIDs with conflicting parent mappings.
#'
#' @keywords internal
summarize_conflicting_parents <- function(pop_data, guid_col) {

  conflicts <- switch(guid_col,
                      "Admin2GUID" = {
                        admin1_conflicts <- find_conflicting_admins(
                          pop_data, "Admin2GUID", "Admin1GUID",
                          return_details = TRUE
                        ) |>
                          dplyr::rename(conflicting_admin1 = conflicting_parents)
                        admin0_conflicts <- find_conflicting_admins(
                          pop_data, "Admin2GUID", "Admin0GUID",
                          return_details = TRUE
                        ) |>
                          dplyr::rename(conflicting_admin0 = conflicting_parents)

                        pop_data |>
                          dplyr::distinct(Admin2GUID) |>
                          dplyr::left_join(admin1_conflicts, by = "Admin2GUID") |>
                          dplyr::left_join(admin0_conflicts, by = "Admin2GUID") |>
                          dplyr::filter(!is.na(conflicting_admin1) | !is.na(conflicting_admin0)) |>
                          dplyr::select(Admin2GUID, conflicting_admin1, conflicting_admin0)
                      },
                      "Admin1GUID" = {
                        find_conflicting_admins(
                          pop_data, "Admin1GUID", "Admin0GUID",
                          return_details = TRUE
                        ) |>
                          dplyr::rename(conflicting_admin0 = conflicting_parents) |>
                          dplyr::mutate(conflicting_admin1 = NA_character_) |>
                          dplyr::select(Admin1GUID, conflicting_admin1, conflicting_admin0)
                      },
                      {
                        dplyr::tibble(
                          Admin0GUID = character(),
                          conflicting_admin1 = character(),
                          conflicting_admin0 = character()
                        )
                      }
                      )

  return(conflicts)

}

#' Create unique parent predicate per admin GUID for assertr validation
#'
#' @description
#' Predicate that checks whether each admin GUID maps to a single parent admin.
#'
#' - For "Admin2GUID", checks uniqueness against Admin1GUID and Admin0GUID.
#' - For "Admin1GUID", checks uniqueness against Admin0GUID.
#'
#' @param pop_data Tibble containing population data.
#' @param guid_col String name of the GUID column to validate.
#'
#' @returns `fun` A predicate function for use with assertr.
#'
#' @keywords internal
create_parent_predicate <- function(pop_data, guid_col) {

  predicate_function <- switch(guid_col,
         "Admin2GUID" = {
           conflicting_with_admin1 <- find_conflicting_admins(
             pop_data, "Admin2GUID", "Admin1GUID"
           )
           conflicting_with_admin0 <- find_conflicting_admins(
             pop_data, "Admin2GUID", "Admin0GUID"
           )
           all_conflicting_guids <- unique(c(
             conflicting_with_admin1,
             conflicting_with_admin0
           ))
           function(column_values) {
             function(value) !(value %in% all_conflicting_guids) | is.na(value)
           }
         },
         "Admin1GUID" = {
           conflicting_with_admin0 <- find_conflicting_admins(
             pop_data, "Admin1GUID", "Admin0GUID"
           )
           function(column_values) {
             function(value) !(value %in% conflicting_with_admin0) | is.na(value)
           }
         },
         {
           function(column_values) {
             function(value) TRUE
           }
         }
         )

  return(predicate_function)

}

#' Create age group predicate per GUID for assertr validation
#'
#' @description
#' Predicate that checks whether each GUID has required age group data.
#'
#' @param guids_with_missing_agegroups `str` GUIDs with missing age groups.
#'
#' @returns `fun` A predicate function for use with assertr.
#'
#' @keywords internal
create_agegroup_predicate <- function(guids_with_missing_agegroups) {
  function(column_values) {
    function(value) !(value %in% guids_with_missing_agegroups) | is.na(value)
  }
}

#' Obtains GUID with missing age group
#'
#' @description
#' Obtains the GUIDs missing age groups.
#'
#' @inheritParams create_parent_predicate
#'
#' @returns `tibble` A summary table of GUIDs and their missing age groups.
#' @keywords internal
#'
get_guid_missing_age_groups <- function(pop_data, guid_col) {

  missing_agegroups_df <- pop_data |>
    dplyr::filter(!is.na(!!dplyr::sym(guid_col))) |>
    dplyr::group_by(!!dplyr::sym(guid_col)) |>
    dplyr::summarise(
      present_agegroups = list(unique(AgeGroupCode)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      missing = sapply(present_agegroups, missing_required_agegroups)
    ) |>
    dplyr::filter(!is.na(missing)) |>
    dplyr::select(dplyr::all_of(guid_col), missing_agegroups = missing)

  return(missing_agegroups_df)
}

## Outlier checking mini functions ----

#' Filters data to a single (guid, age_group) slice and adds an index
#'
#' @inheritParams get_guid_missing_age_groups
#' @param guid `str` GUID to search for.
#' @param age_group `str` Age group to search for.
#'
#' @returns `str` Pop data filtered to a particular GUID and age group
#' @keywords internal
#'
filter_guid_age_data <- function(pop_data, guid_col, guid, age_group) {
  pop_data |>
    dplyr::filter(
      !!dplyr::sym(guid_col) == guid,
      AgeGroupCode == age_group,
      !is.na(Value),
      Value >= 0
    ) |>
    dplyr::mutate(index = dplyr::row_number())
}

#' Checks whether outliers exist among age groups
#'
#' @param guid_age_data `tibble` Output of [filter_guid_age_data].
#'
#' @returns `tibble` Summary of outlier detection
#' @keywords internal
#'
compute_outliers_df <- function(guid_age_data, guid_col) {

  # Need at least 2 points to compute MAD-based outliers
  if (nrow(guid_age_data) < 2 |
      # MAD cannot be zero
      # Using .Machine$double.eps as a tolerance threshold to because
      # of floating point errors
      mad(guid_age_data$Value, na.rm = T) <= .Machine$double.eps) {
    return(NULL)
  }

  res <- guid_age_data |>
    assertr::insist(
      assertr::within_n_mads(2),
      Value,
      error_fun = assertr::error_df_return)

  # If the result has a "verb" column, it's an assertr error dataframe
  if ("verb" %in% names(res)) {
    # Join back helpful context (guid, year)
    dplyr::left_join(
      res,
      guid_age_data |>
        dplyr::select(
          index,
          dplyr::any_of(guid_col),
          Year
        ),
      by = "index"
    )

  } else {

    return(NULL)

  }
}


#' Orchestrates per guid + age group
#'
#' @description
#' Filters and compute outliers for population values in the pop data
#'
#' @inheritParams filter_guid_age_data
#'
#' @returns `tibble` Summary containing outliers per age group.
#' @keywords internal
#'
outliers_for_guid_age <- function(pop_data, guid_col, guid, age_group) {
  guid_age_data <- filter_guid_age_data(pop_data, guid_col, guid, age_group)

  return(compute_outliers_df(guid_age_data, guid_col))
}

# Public function ----

#' Check Population Data Quality
#'
#' @description
#' Validation checks on population data from POLIS and returns flagged rows.
#'
#' @param pop_rds `tibble` Population data from POLIS. Output of [load_polis_pop].
#' @param dataset_source `int` FKDataSet_ID, defaults to 2.
#'
#' @returns `tibble` Summary table with one row per GUID that has validation issues.
#' Returns empty tibble with correct structure if all checks pass.
#'
#' @details
#' Validation checks include:
#' \itemize{
#'   \item Required columns exist
#'   \item No missing values in Value, StartDate, AgeGroupCode, PlaceId
#'   \item Value is non-negative
#'   \item StartDate is not after EndDate
#'   \item CreatedDate and UpdatedDate are not in the future
#'   \item Each Admin GUID has all three age groups (0-5Y, 0-15Y, ALL)
#'   \item Each Admin GUID maps to a single parent admin
#'   \item Value is within 2 median absolute deviation per admin and age group
#' }
#'
#' There are several data sources (from the `FK_DataSetId` column).They include:
#' \itemize{
#' \item 1: Unknown
#'  \item 2: POLIS default
#'  \item 17: Unknown
#'  \item 19: country population data
#'  \item 41: LandScan data
#' }
#'
#' @examples
#' \dontrun{
#' check_pop_data_quality(raw_dist_pop)
#' check_pop_data_quality(raw_prov_pop)
#' check_pop_data_quality(raw_prov_pop)
#' }
#'
#' @export
check_pop_data_quality <- function(pop_data, dataset_source = 2) {

  spatial_scale <- dplyr::case_when(
    "Admin2GUID" %in% names(pop_data) ~ "dist",
    "Admin1GUID" %in% names(pop_data) ~ "prov",
    "Admin0GUID" %in% names(pop_data) ~ "ctry"
  )

  cli::cli_alert_info(paste0("Detected that pop data is at the ", spatial_scale, " spatial scale."))

  # Set GUID column
  guid_col <- switch(spatial_scale,
    "ctry" = "Admin0GUID",
    "prov" = "Admin1GUID",
    "dist" = "Admin2GUID"
  )

  # Name columns
  guid_name_col <- switch(spatial_scale,
                          "ctry" = "Admin0Name",
                          "prov" = c("Admin0Name", "Admin1Name"),
                          "dist" = c("Admin0Name", "Admin1Name", "Admin2Name"))

  # Filter POLIS source for validation and spatial scale filter
  pop_data <- pop_data |>
    dplyr::filter(FK_DataSetId == dataset_source) |>
    dplyr::filter(!is.na(!!dplyr::sym(guid_col)))

  # Capture all POLIS available sources per GUID before filtering
  sources_df <- pop_data |>
    dplyr::group_by(dplyr::across(dplyr::any_of(c(
      "Admin0GUID", "Admin1GUID", "Admin2GUID")))) |>
    dplyr::summarize(sources = paste(sort(unique(FK_DataSetId)),
                                     collapse = ", "
    ), .groups = "drop")

  # Compute missing age groups per GUID
  missing_agegroups_df <- get_guid_missing_age_groups(pop_data, guid_col)
  guids_with_missing_agegroups <- missing_agegroups_df |> dplyr::pull(guid_col)

  # Create predicate generators
  has_all_agegroups <- create_agegroup_predicate(guids_with_missing_agegroups)
  has_unique_parent <- create_parent_predicate(pop_data, guid_col)

  # Compute conflicting parent admins
  conflicting_parents_df <- summarize_conflicting_parents(pop_data, guid_col)

  # Run validation chain
  validated <- pop_data |>
    assertr::chain_start() |>
    assertr::verify(assertr::has_all_names(
      "Admin0GUID", "Admin0Id", "Admin0Name",
      "Admin1GUID", "Admin1Id", "Admin1Name",
      "Admin2GUID", "Admin2Id", "Admin2Name",
      "AgeGroupCode", "CountryISO3Code",
      "CreatedDate", "EndDate", "StartDate", "UpdatedDate",
      "Value", "WHORegion", "Year", "datasource", "PlaceId",
      "FK_DataSetId"
    )) |>
    assertr::assert(assertr::not_na, Value, StartDate, AgeGroupCode, PlaceId) |>
    assertr::assert(assertr::within_bounds(0, Inf), Value) |>
    assertr::verify(is.na(EndDate) | StartDate <= EndDate) |>
    assertr::verify(is.na(CreatedDate) | CreatedDate <= Sys.Date()) |>
    assertr::verify(is.na(UpdatedDate) | UpdatedDate <= Sys.Date()) |>
    assertr::insist(has_all_agegroups, !!dplyr::sym(guid_col)) |>
    assertr::insist(has_unique_parent, !!dplyr::sym(guid_col)) |>
    assertr::chain_end(error_fun = assertr::error_df_return)

  # Outlier validation per age group
  age_groups <- c("0-5Y", "0-15Y", "ALL")
  unique_guids <- unique(pop_data[[guid_col]])

  outlier_results <- setNames(
    purrr::map(age_groups, \(age_group) {
      purrr::map(unique_guids, \(guid) {
        outliers_for_guid_age(pop_data, guid_col, guid, age_group)
      }) |>
        dplyr::bind_rows()
    }),
    age_groups
  )

  if (any(sapply(outlier_results, nrow) == 0)) {
    cli::cli_alert_success("No outliers in population values across age groups.")
  } else {
    cli::cli_alert_success("Outliers detected in population values.")
  }

  # Check if errors were returned
  has_errors <- nrow(validated) > 0 ||
    any(sapply(outlier_results, nrow) > 0)

  if (!has_errors) {
    message("All checks passed")
    return(invisible(NULL))
  }

  # Extract errors and applicable year(s) data per validation type
  error_rows <- validated |>
    dplyr::filter(!is.na(index)) |>
    dplyr::mutate(
      validation_type = dplyr::case_when(
        verb == "assert" & column == "Value" & grepl(
          "not_na", predicate,
          ignore.case = TRUE
        ) ~ "null_value",
        verb == "assert" & column == "StartDate" & grepl(
          "not_na", predicate,
          ignore.case = TRUE
        ) ~ "null_startdate",
        verb == "assert" & column == "AgeGroupCode" & grepl(
          "not_na", predicate,
          ignore.case = TRUE
        ) ~ "null_agegroup",
        verb == "assert" & column == "PlaceId" & grepl(
          "not_na", predicate,
          ignore.case = TRUE
        ) ~ "null_placeid",
        verb == "assert" & grepl(
          "within_bounds", predicate,
          ignore.case = TRUE
        ) ~ "negative_value",
        verb == "verify" & grepl(
          "EndDate.*StartDate|StartDate.*EndDate", predicate
        ) ~ "startdate_after_enddate",
        verb == "verify" & grepl(
          "CreatedDate", predicate
        ) ~ "future_created_date",
        verb == "verify" & grepl(
          "UpdatedDate", predicate
        ) ~ "future_updated_date"
      )
    ) |>
    dplyr::filter(!is.na(validation_type))

  # Summarize errors by GUID
  error_summary <- error_rows |>
    dplyr::left_join(
      pop_data |> dplyr::mutate(index = dplyr::row_number()) |>
        dplyr::select(index, !!dplyr::sym(guid_col), Year),
      by = "index"
    ) |>
    dplyr::group_by(!!dplyr::sym(guid_col), validation_type) |>
    dplyr::summarise(years = paste(sort(unique(Year)),
                                   collapse = ", "
    ), .groups = "drop") |>
    dplyr::mutate(validation_type = factor(
      validation_type,
      levels = unique(error_rows$validation_type)
    )) |>
    tidyr::pivot_wider(names_from = validation_type, values_from = years)

  # Summarize outliers by GUID and age group
  outlier_summaries <- lapply(age_groups, \(age_group) {
    col_name <- paste0("outside_2_mad_", age_group)
    outlier_data <- outlier_results[[age_group]]

    if (is.null(outlier_data) || nrow(outlier_data) == 0) {
      return(dplyr::tibble(
        !!dplyr::sym(guid_col) := character(), !!col_name := character()
      ))
    }

    outlier_data |>
      dplyr::group_by(!!dplyr::sym(guid_col)) |>
      dplyr::summarise(!!col_name :=
                  paste(sort(unique(Year)), collapse = ", "), .groups = "drop")
  })

  outlier_summary <- outlier_summaries[[1]] |>
    dplyr::full_join(outlier_summaries[[2]], by = guid_col) |>
    dplyr::full_join(outlier_summaries[[3]], by = guid_col)

  # Combine all summaries
  result <- sources_df |>
    dplyr::left_join(missing_agegroups_df, by = guid_col) |>
    dplyr::left_join(conflicting_parents_df, by = guid_col) |>
    dplyr::left_join(error_summary, by = guid_col) |>
    dplyr::left_join(outlier_summary, by = guid_col) |>
    dplyr::filter(dplyr::if_any(-dplyr::any_of(c(
      "Admin0GUID", "Admin1GUID", "Admin2GUID", "sources"
    )), ~ !is.na(.))) |>
    dplyr::select(
      dplyr::any_of(c("Admin0GUID", "Admin1GUID", "Admin2GUID", "sources")),
      dplyr::where(~ any(!is.na(.)))
    )

  # Add names
  join_cols <- switch(spatial_scale,
                      "ctry" = "Admin0GUID",
                      "prov" = c("Admin0GUID","Admin1GUID"),
                      "dist" = c("Admin0GUID", "Admin1GUID", "Admin2GUID")
                      )

  result <- dplyr::inner_join(pop_data |>
                       dplyr::select(dplyr::any_of(guid_name_col),
                                     dplyr::any_of(c("Admin0GUID", "Admin1GUID",
                                                     "Admin2GUID"))) |>
                       dplyr::distinct()
                     , result, by = join_cols)

  # Output
  message(if (nrow(result) == 0) {
    "All checks passed"
  } else {
    paste0(nrow(result), " GUIDs flagged with issues")
  })
  return(result)
}
