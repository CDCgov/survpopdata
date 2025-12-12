#' Check Population Data Quality
#'
#' Runs validation checks on population data from POLIS and reports whether all checks passed.
#'
#' @param pop.rds Tibble containing population data from POLIS.
#' @param spatial_scale Geographic level: "ctry", "prov", or "dist".
#'
#' @return Validation outcome indicating whether all checks passed or not all checks passed.
#'
#' @details
#' If not all checks passed, the function reports detected issues and prompts the user
#' to view details for all issues, no issues, or selected issue numbers.
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
  all_issues <- list()

  add_issue <- function(check, msg, data, processed, failed) {
    processed <- as.numeric(ifelse(is.na(processed) | is.null(processed), 0, processed))
    failed    <- as.numeric(ifelse(is.na(failed)    | is.null(failed),    0, failed))
    pct <- ifelse(processed > 0, round(failed / processed * 100, 1), 0)
    full_msg <- paste0(msg, " (", ifelse(pct == 0 && failed > 0,
                                         paste0(failed, " of ", processed),
                                         paste0(pct, "% of ", processed)),
                       " records processed)")
    all_issues[[length(all_issues) + 1]] <<- list(check = check, message = full_msg, data = data)
  }

  error_collect_verify <- function(check_label, total_processed, fail_condition_expr) {
    function(data, error_df) {
      failing_rows <- data |> dplyr::filter(!!fail_condition_expr)
      if (nrow(failing_rows) > 0) add_issue(check_label, "", failing_rows, total_processed, nrow(failing_rows))
      data
    }
  }

  error_collect_assert <- function(check_label, total_processed) {
    function(data, error_df) {
      idx <- error_df$index; idx <- idx[!is.na(idx)]
      failing_rows <- if (length(idx) > 0) data[idx, , drop = FALSE] else data[0, , drop = FALSE]
      if (nrow(failing_rows) > 0) add_issue(check_label, "", failing_rows, total_processed, nrow(failing_rows))
      data
    }
  }

  geo_col <- switch(spatial_scale, "dist" = "Admin2GUID", "prov" = "Admin1GUID", "Admin0GUID")
  pop_with_rownum <- pop.rds |> dplyr::mutate(row_num = dplyr::row_number(), .before = 1)

  # Step 1: Required columns
  required_cols <- c("Admin0GUID","Admin0Id","Admin0Name","Admin1GUID","Admin1Id","Admin1Name",
                     "Admin2GUID","Admin2Id","Admin2Name","AgeGroupName","CountryISO3Code",
                     "CreatedDate","EndDate","StartDate","UpdatedDate","Value","WHORegion",
                     "Year","Data_Source","PlaceID")
  tryCatch({
    pop.rds |> assertr::verify(assertr::has_all_names(!!!required_cols))
  }, error = function(e) {
    missing_cols <- setdiff(required_cols, names(pop.rds))
    add_issue("Required Columns", paste0(length(missing_cols), " columns missing"),
              tibble::tibble(missing_columns = missing_cols), length(required_cols), length(missing_cols))
  })

  # Step 2: Relational integrity
  check_relational <- function(child_col, parent_col, child_name, parent_name) {
    all_pairs <- pop.rds |> dplyr::filter(!is.na(!!rlang::sym(child_col)), !!rlang::sym(child_col) != "") |>
      dplyr::distinct(!!rlang::sym(child_col), !!rlang::sym(parent_col))
    violations <- all_pairs |>
      dplyr::group_by(!!rlang::sym(child_col)) |>
      dplyr::summarise(parent_count = dplyr::n_distinct(!!rlang::sym(parent_col)),
                       parents = paste(unique(!!rlang::sym(parent_col)), collapse = ", "), .groups = "drop") |>
      dplyr::filter(parent_count > 1)
    if (nrow(violations) > 0)
      add_issue("Relational Integrity", paste0(nrow(violations), " ", child_name, " in multiple ", parent_name),
                violations, dplyr::n_distinct(all_pairs[[child_col]]), nrow(violations))
  }
  if (spatial_scale == "dist") { check_relational("Admin2GUID","Admin1GUID","districts","provinces")
    check_relational("Admin2GUID","Admin0GUID","districts","countries") }
  if (spatial_scale == "prov") { check_relational("Admin1GUID","Admin0GUID","provinces","countries") }

  # Step 3: NULL/empty values in key columns
  n_total <- nrow(pop.rds)
  pop_with_rownum |>
    assertr::assert(assertr::not_na, Value, error_fun = error_collect_assert("NULL Value", n_total)) |>
    assertr::assert(function(x) !is.na(x) & x != "", PlaceID, error_fun = error_collect_assert("NULL/Empty PlaceID", n_total)) |>
    assertr::assert(assertr::not_na, StartDate, error_fun = error_collect_assert("NULL StartDate", n_total)) |>
    assertr::assert(assertr::not_na, AgeGroupName, error_fun = error_collect_assert("NULL AgeGroupName", n_total))

  # Step 4: Age group completeness
  data_for_pop_check <- pop.rds |> dplyr::filter(!is.na(Value), !is.na(PlaceID), PlaceID != "", !is.na(AgeGroupName))
  pop_issues <- data_for_pop_check |>
    dplyr::group_by(PlaceID, Year) |>
    dplyr::summarise(present_groups = paste(sort(unique(AgeGroupName)), collapse = ", "), .groups = "drop") |>
    dplyr::filter(present_groups != "TotalPop, U15, U5") |>
    dplyr::mutate(missing_groups = sapply(present_groups, function(x) {
      missing <- setdiff(c("U5","U15","TotalPop"), strsplit(x, ", ")[[1]])
      if (length(missing) == 0) "duplicate groups" else paste(missing, collapse = ", ")
    })) |>
    dplyr::select(PlaceID, Year, missing_groups)
  if (nrow(pop_issues) > 0)
    add_issue("Age Group Completeness", "locations with incomplete age group representation",
              pop_issues, dplyr::n_distinct(data_for_pop_check$PlaceID), nrow(pop_issues))

  # Step 5: Date validations (filter once per check)
  start_end_df <- pop_with_rownum |> dplyr::filter(!is.na(StartDate), !is.na(EndDate))
  created_df   <- pop_with_rownum |> dplyr::filter(!is.na(CreatedDate))
  updated_df   <- pop_with_rownum |> dplyr::filter(!is.na(UpdatedDate))
  year_df      <- pop_with_rownum |> dplyr::filter(!is.na(StartDate), !is.na(Year))
  start_end_df |> assertr::verify(StartDate <= EndDate,
                                  error_fun = error_collect_verify("StartDate After EndDate", nrow(start_end_df), rlang::quo(StartDate > EndDate)))
  created_df   |> assertr::verify(CreatedDate <= Sys.Date(),
                                  error_fun = error_collect_verify("CreatedDate In Future", nrow(created_df), rlang::quo(CreatedDate > Sys.Date())))
  updated_df   |> assertr::verify(UpdatedDate <= Sys.Date(),
                                  error_fun = error_collect_verify("UpdatedDate In Future", nrow(updated_df), rlang::quo(UpdatedDate > Sys.Date())))
  year_df      |> assertr::verify(Year == lubridate::year(StartDate),
                                  error_fun = error_collect_verify("Year-StartDate Mismatch", nrow(year_df), rlang::quo(Year != lubridate::year(StartDate))))

  # Step 6: Non-negative population values (filter once)
  non_na_value_df <- pop_with_rownum |> dplyr::filter(!is.na(Value))
  non_na_value_df |> assertr::assert(assertr::within_bounds(0, Inf), Value,
                                     error_fun = error_collect_assert("Negative Population Values", nrow(non_na_value_df)))

  # Step 7: Temporal outliers (base filter once)
  outlier_base <- pop.rds |> dplyr::filter(!is.na(Value), Value >= 0, !is.na(PlaceID), PlaceID != "", !is.na(AgeGroupName))
  outlier_data <- outlier_base |>
    dplyr::group_by(!!rlang::sym(geo_col), AgeGroupName) |>
    dplyr::filter(dplyr::n() >= 2, stats::sd(Value, na.rm = TRUE) > 0) |>
    dplyr::mutate(z_score = abs((Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE))) |>
    dplyr::filter(z_score > 2) |>
    dplyr::ungroup() |>
    dplyr::select(!!rlang::sym(geo_col), AgeGroupName, Year, Value, z_score) |>
    dplyr::arrange(!!rlang::sym(geo_col), AgeGroupName, Year)
  if (nrow(outlier_data) > 0)
    add_issue("Temporal Outliers", "population values with z-score > 2", outlier_data, nrow(outlier_base), nrow(outlier_data))

  # Display results
  is_outlier <- \(x) x$check == "Temporal Outliers"
  has_outliers <- any(sapply(all_issues, is_outlier))
  has_failures <- any(sapply(all_issues, \(x) !is_outlier(x)))

  if (length(all_issues) == 0) { message("✓ All checks passed"); return(invisible(pop.rds)) }
  if (has_outliers && !has_failures) {
    message("⚠ Outliers detected\n"); print(all_issues[[which(sapply(all_issues, is_outlier))]]$data); return(invisible(pop.rds))
  }
  if (length(all_issues) == 1) {
    message("✗ Not all checks passed\n\n1 issue found:\n")
    message("1. [", all_issues[[1]]$check, "] ", all_issues[[1]]$message, "\n")
    print(all_issues[[1]]$data); return(invisible(NULL))
  }

  message("✗ Not all checks passed\n\n", length(all_issues), " issues found:\n")
  for (i in seq_along(all_issues)) message(i, ". [", all_issues[[i]]$check, "] ", all_issues[[i]]$message)

  message("")
  input <- tolower(readline("View details? (Enter number(s) - separate with comma for multiple numbers, 'all', or 'none'): "))
  if (input != "" && input != "none") {
    nums <- if (input == "all") seq_along(all_issues) else suppressWarnings(as.integer(strsplit(input, ",")[[1]]))
    for (i in nums[!is.na(nums) & nums >= 1 & nums <= length(all_issues)]) {
      message("\nIssue ", i, ": [", all_issues[[i]]$check, "] ", all_issues[[i]]$message); print(all_issues[[i]]$data)
    }
  }
  invisible(NULL)
}
