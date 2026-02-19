# Private functions ----
#' Load Pakistan province population patch
#'
#' @param pakistan_file_path Path to Pakistan patch file.
#'
#' @return Tibble with Admin names, year, age columns, datasource.
#'
#' @export
load_pakistan_patch <- function(
  pakistan_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/2022_2023 Population Pakistan.csv",
  edav = TRUE
) {
  sirfunctions::sirfunctions_io("read", NULL, pakistan_file_path, edav = edav) |>
    dplyr::filter(!is.na(Country)) |>
    dplyr::select(-dplyr::any_of("...12")) |>
    dplyr::rename(
      Province = "Procince / governorate",
      `2022` = "2022.00",
      `2023` = "2023.00"
    ) |>
    dplyr::mutate(
      Country = "PAKISTAN",
    ) |>
    dplyr::select(Country, Province, `2022`, `2023`) |>
    tidyr::pivot_longer(c(`2022`, `2023`), names_to = "year", values_to = "Total") |>
    dplyr::mutate(
      year = as.numeric(year),
      Total = as.numeric(Total),
      Under5Pop = NA_real_,
      Under15Pop = NA_real_,
      datasource = "PATCH_PAKISTAN"
    ) |>
    dplyr::rename(
      Admin0Name = Country,
      Admin1Name = Province
    ) |>
    dplyr::select(Admin0Name, Admin1Name, year, Under5Pop, Under15Pop, Total, datasource)
}

#' Load Somalia province population patch
#'
#' @param somalia_2022_file_path Path to Somalia 2022 file.
#' @param somalia_2023_file_path Path to Somalia 2023 file.
#' @param somalia_2024_file_path Path to Somalia 2024 file.
#'
#' @return Tibble with Admin names, year, age columns, datasource.
#'
#' @export
load_somalia_patch <- function(
  somalia_2022_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/AFPPOP_22.csv",
  somalia_2023_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/AFPPOP_23.csv",
  somalia_2024_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/AFPPOP_24.csv",
  edav = TRUE
) {
  # Parse Somalia header rows
  read_somalia_year <- function(somalia_file, year_value) {
    somalia_raw <- sirfunctions::sirfunctions_io("read", NULL, somalia_file, edav = edav)
    somalia_raw <- somalia_raw[8:nrow(somalia_raw), ]
    colnames(somalia_raw) <- as.character(unlist(somalia_raw[1, ]))
    somalia_raw <- somalia_raw[-1, ]
    somalia_raw$year <- year_value
    somalia_raw
  }

  # Stack yearly extracts and harmonize admin names
  somalia_patches <- dplyr::bind_rows(
    read_somalia_year(somalia_2022_file_path, 2022),
    read_somalia_year(somalia_2023_file_path, 2023),
    read_somalia_year(somalia_2024_file_path, 2024)
  ) |>
    dplyr::filter(!is.na(PROVINCE)) |>
    dplyr::mutate(
      Admin0Name = "SOMALIA",
      datasource = "PATCH_SOMALIA",
      PROVINCE = dplyr::case_when(
        PROVINCE == "BAKOOL" ~ "BAKOL",
        PROVINCE == "SANAQ" ~ "SANAG",
        PROVINCE == "MUDUG" & DISTRICT %in% c("GOLDOGOB", "JARIBAN", "GALKAYU NORTH") ~ "MUDUG PL",
        PROVINCE == "MUDUG" & DISTRICT %in% c("HARA DHERE", "HOBYO") ~ "MUDUG GM",
        PROVINCE == "SOUTH MUDUG" & DISTRICT == "GALKAYU SOUTH" ~ "MUDUG GM",
        DISTRICT %in% c("GARDO", "BENDER BAYLA", "HAFUN", "RAKO", "WACIYA") ~ "KARKAR",
        TRUE ~ PROVINCE
      ),
      Total = as.numeric(gsub(",", "", Total))
    )

  somalia_formatted <- somalia_patches |>
    dplyr::select(
      Admin0Name,
      Admin1Name = PROVINCE,
      year,
      Total,
      datasource
    ) |>
    dplyr::mutate(
      year = as.numeric(year),
      Under5Pop = NA_real_,
      Under15Pop = NA_real_
    )

  return(somalia_formatted)
}

#' Load Kenya province population patch
#'
#' @param kenya_file_path Path to Kenya 2018 file.
#'
#' @return Tibble with Admin names, year, age columns, datasource.
#'
#' @export
load_kenya_patch <- function(
  kenya_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/Kenya_SubCounty_pop_2018.csv",
  edav = TRUE
) {
  sirfunctions::sirfunctions_io("read", NULL, kenya_file_path, edav = edav) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), toupper)) |>
    dplyr::rename(
      Admin1Name = County,
      Under15Pop = Under_15_Prop
    ) |>
    dplyr::mutate(
      Under15Pop = as.numeric(Under15Pop),
      Admin1Name = dplyr::case_when(
        Admin1Name == "KIKIFI" ~ "KILIFI",
        Admin1Name == "MURANGA" ~ "MURANG'A",
        Admin1Name == "MWINGI" ~ "KITUI",
        Admin1Name == "THARAKA" ~ "THARAKA-NITHI",
        Admin1Name == "TRANSZOIA" ~ "TRANS NZOIA",
        TRUE ~ Admin1Name
      ),
      Admin0Name = "KENYA",
      year = 2018,
      datasource = "KENYA 2018 PATCH",
      Under5Pop = NA_real_,
      Total = NA_real_
    ) |>
    dplyr::select(Admin0Name, Admin1Name, year, Under5Pop, Under15Pop, Total, datasource)
}

#' Load Jamal province under-15 population
#'
#' @param jamal_pop_file_path Path to Jamal file.
#'
#' @return Tibble with Admin names, year, age columns, datasource.
#'
#' @export
load_jamal_pop <- function(
  jamal_pop_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/POPU15.csv",
  edav = TRUE
) {
  jamal_pop <- sirfunctions::sirfunctions_io("read", NULL, jamal_pop_file_path,
    edav = edav
  )

  jamal_pop_formatted <- jamal_pop |>
    dplyr::mutate(
      Admin0Name = toupper(Admin0),
      Admin1Name = toupper(Admin1),
      adm1guid = paste0("{", toupper(PlaceId), "}"),
      Admin0Name = ifelse(stringr::str_detect(Admin0Name, "IVOIRE"), "COTE D IVOIRE", Admin0Name)
    ) |>
    dplyr::select(ends_with("Name"), starts_with("1"), adm1guid) |>
    tidyr::pivot_longer(
      cols = starts_with("1"),
      names_to = "year",
      values_to = "Under15Pop",
      values_drop_na = TRUE
    ) |>
    dplyr::mutate(
      Under5Pop = NA_real_,
      Total = NA_real_,
      year = as.integer(year),
      datasource = "JAMAL POP"
    )

  return(jamal_pop_formatted)
}

#' Combine all district patch sources
#'
#' @inheritParams load_pakistan_patch
#' @inheritParams load_somalia_patch
#' @inheritParams load_kenya_patch
#' @inheritParams load_jamal_pop
#'
#' @return `tibble` Dataset with all the patch files.
#'
#' @export
load_all_patches <- function(pakistan_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/2022_2023 Population Pakistan.csv",
                             somalia_2022_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/AFPPOP_22.csv",
                             somalia_2023_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/AFPPOP_23.csv",
                             somalia_2024_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/AFPPOP_24.csv",
                             kenya_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/Kenya_SubCounty_pop_2018.csv",
                             jamal_pop_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/POPU15.csv",
                             edav = TRUE) {
  pak_patch <- load_pakistan_patch(pakistan_file_path, edav)
  som_patch <- load_somalia_patch(somalia_2022_file_path, somalia_2023_file_path, somalia_2024_file_path, edav)
  ken_patch <- load_kenya_patch(kenya_file_path, edav)
  jamal_pop <- load_jamal_pop(jamal_pop_file_path, edav)

  return(dplyr::bind_rows(pak_patch, som_patch, ken_patch, jamal_pop))
}

#' Load population growth rates
#'
#' @details
#' Loads and formats the population growth rate file for use in R.
#'
#'
#' @param file_loc `str` Path to WPP Excel file.
#' @details
#' The Excel file uses the "Estimates" tab. The first few rows (16 rows or so) are deleted for ease of loading into
#' R. The source of this dataset is in the [World Population Prospects](https://population.un.org/wpp/downloads?folder=Standard%20Projections&group=Most%20used)
#' website, using the "Compact" file.
#'
#' @return `tibble` Growth rates for each country by year.
#' @export
load_growth_rates <- function(
  file_loc = "GID/PEB/SIR/Data/pop/pop raw/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx",
  edav = TRUE
) {
  wpp_raw <- sirfunctions::sirfunctions_io("read", NULL, file_loc, edav = edav)
  wpp_raw <- wpp_raw$Estimates

  # Select and standardize output
  wpp_raw |>
    dplyr::select(
      Admin0Name = `Region, subregion, country or area *`,
      year = Year,
      growth_rate = `Population Growth Rate (percentage)`
    ) |>
    dplyr::filter(
      !is.na(Admin0Name),
      Admin0Name != "REGION, SUBREGION, COUNTRY OR AREA *"
    ) |>
    dplyr::mutate(
      Admin0Name = toupper(Admin0Name),
      Admin0Name = dplyr::case_when(
        stringr::str_detect(Admin0Name, "IVOIRE") ~
          "COTE D IVOIRE",
        Admin0Name == "UNITED KINGDOM" ~
          "THE UNITED KINGDOM",
        Admin0Name == "DEM. PEOPLE'S REPUBLIC OF KOREA" ~
          "DEMOCRATIC PEOPLE'S REPUBLIC OF KOREA",
        Admin0Name == "STATE OF PALESTINE" ~
          "OCCUPIED PALESTINIAN TERRITORY, INCLUDING EAST JERUSALEM",
        TRUE ~ Admin0Name
      ),
      year = as.numeric(year),
      growth_rate = as.numeric(growth_rate) / 100
    ) |> # convert to decimal
    dplyr::arrange(Admin0Name, year)
}

#' Join named population rows to province-year shapes
#'
#' @param pop_named Tibble with Admin names, year, age columns, datasource.
#' @param province_long Province-year long shapes table.
#'
#' @return Tibble joined to year-valid province shapes.
#'
#' @export
join_pop_to_province_year_shapes <- function(pop_named, province_long) {
  pop_named |>
    dplyr::left_join(
      province_long,
      by = c(
        "Admin0Name" = "ADM0_NAME",
        "Admin1Name" = "ADM1_NAME",
        "year" = "active.year.01"
      )
    ) |>
    dplyr::filter(!is.na(GUID)) |>
    dplyr::mutate(
      ADM1_GUID = GUID,
      year = as.numeric(year),
      Under5Pop = as.numeric(Under5Pop),
      Under15Pop = as.numeric(Under15Pop),
      Total = as.numeric(Total)
    )
}

#' Remove forward-filled values (non-POLIS only)
#'
#' @param non_polis_pop Tibble with ADM1_GUID, year, age columns, datasource.
#'
#' @return Tibble with forward-filled values set to NA.
#'
#' @export
remove_forward_fill_non_polis <- function(non_polis_pop) {
  non_polis_pop |>
    dplyr::filter(datasource != "POLIS") |>
    dplyr::arrange(ADM1_GUID, datasource, year) |>
    dplyr::group_by(ADM1_GUID, datasource) |>
    dplyr::mutate(
      Total = dplyr::if_else(
        !is.na(dplyr::lag(Total)) & Total == dplyr::lag(Total) &
          year == dplyr::lag(year) + 1,
        NA_real_, Total
      ),
      Under15Pop = dplyr::if_else(
        !is.na(dplyr::lag(Under15Pop)) & Under15Pop == dplyr::lag(Under15Pop) &
          year == dplyr::lag(year) + 1,
        NA_real_, Under15Pop
      ),
      Under5Pop = dplyr::if_else(
        !is.na(dplyr::lag(Under5Pop)) & Under5Pop == dplyr::lag(Under5Pop) &
          year == dplyr::lag(year) + 1,
        NA_real_, Under5Pop
      )
    ) |>
    dplyr::ungroup()
}

#' Deduplicate population rows by source priority (one row per GUID-year)
#'
#' @param pop_with_guid Tibble with ADM1_GUID, year, age columns, datasource.
#'
#' @return Deduplicated tibble (one row per ADM1_GUID-year).
#'
#' @export
deduplicate_population <- function(pop_with_guid) {
  pop_with_guid |>
    dplyr::mutate(
      source_rank = dplyr::case_when(
        datasource == "POLIS" ~ 1L,
        grepl("^PATCH_", datasource) ~ 2L,
        datasource == "KENYA 2018 PATCH" ~ 2L,
        datasource == "JAMAL POP" ~ 3L,
        TRUE ~ 99L
      ),
      has_any_value = !(is.na(Total) & is.na(Under15Pop) & is.na(Under5Pop))
    ) |>
    dplyr::group_by(ADM1_GUID, year) |>
    dplyr::arrange(!has_any_value, source_rank, .by_group = TRUE) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(-source_rank, -has_any_value)
}

#' Applies the growth rate to each population columns
#'
#' @param base_data `tibble` Combined population data with the growth rate columns.
#' @param pop_column `str` Name of the population column to apply the growth rate to.
#'
#' @returns `tibble` Population data with population filled based on growth rates
#' @keywords internal
#'
apply_growth_rate <- function(base_data, pop_column) {
  flag_column <- paste0("used_growth_", pop_column)

  # Create anchor year vars
  base_data_formatted_1 <- base_data |>
    dplyr::group_by(ADM1_GUID) |>
    dplyr::arrange(year, .by_group = TRUE) |>
    dplyr::mutate(
      anchor_year = year,
      anchor_value = !!dplyr::sym(pop_column)
    ) |>
    dplyr::mutate(dplyr::across(
      dplyr::any_of(c("anchor_year", "anchor_value")),
      \(x) {
        ifelse(!is.na(!!dplyr::sym(pop_column)), x, NA)
      }
    )) |>
    tidyr::fill(anchor_year, anchor_value, .direction = "downup") |>
    dplyr::ungroup() |>
    dplyr::mutate(
      used_growth = is.na(!!dplyr::sym(pop_column)) & !is.na(anchor_year) &
        !is.na(anchor_value) & !is.na(growth_rate),
      pop_using_growth_rate = round(anchor_value * (((growth_rate / 100) + 1)^(year - anchor_year)))
    )

  # Create anchor years
  base_data_formatted <- base_data |>
    dplyr::group_by(ADM1_GUID) |>
    dplyr::arrange(year, .by_group = TRUE) |>
    dplyr::mutate(
      anchor_year = tidyr::fill(
        tibble::tibble(anchor_year = dplyr::if_else(
          is.na(!!dplyr::sym(pop_column)), NA_real_, year
        )),
        anchor_year,
        .direction = "down"
      )$anchor_year,
      anchor_value = tidyr::fill(
        tibble::tibble(anchor_value = dplyr::if_else(
          is.na(!!dplyr::sym(pop_column)), NA_real_, !!dplyr::sym(pop_column)
        )),
        anchor_value,
        .direction = "down"
      )$anchor_value,
      used_growth = is.na(!!dplyr::sym(pop_column)) &
        !is.na(anchor_year) & !is.na(anchor_value) &
        !is.na(growth_rate),
      "{pop_column}" := dplyr::if_else(
        used_growth,
        round(anchor_value * (((growth_rate / 100) + 1)^(year - anchor_year))),
        !!dplyr::sym(pop_column)
      ),
      "{flag_column}" := used_growth
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-anchor_year, -anchor_value)
}

#' Aggregate district population to province for missing values
#'
#' Fills remaining province-level NAs by summing all districts in that province-year.
#'
#' @param base_data Tibble of province-level population data (may have NA)
#'
#' @return Tibble with remaining NAs filled and aggregation flags
#' @export
aggregate_districts_to_province <- function(base_data) {
  # Retrieve district-level population and aggregate (sum) all districts to province-year level
  province_aggregates <- process_dist_pop_data() |>
    dplyr::group_by(ADM0_NAME, ADM1_NAME, year) |>
    dplyr::summarise(
      Total_from_districts = sum(Total, na.rm = TRUE),
      Under5Pop_from_districts = sum(Under5Pop, na.rm = TRUE),
      Under15Pop_from_districts = sum(Under15Pop, na.rm = TRUE),
      .groups = "drop"
    )

  base_data |>
    dplyr::left_join(province_aggregates, by = c("ADM0_NAME", "ADM1_NAME", "year")) |>
    dplyr::mutate(
      used_aggregate_Total = is.na(Total) & !is.na(Total_from_districts),
      used_aggregate_Under5Pop = is.na(Under5Pop) & !is.na(Under5Pop_from_districts),
      used_aggregate_Under15Pop = is.na(Under15Pop) & !is.na(Under15Pop_from_districts),
      Total = dplyr::if_else(used_aggregate_Total, Total_from_districts, Total),
      Under5Pop = dplyr::if_else(used_aggregate_Under5Pop, Under5Pop_from_districts, Under5Pop),
      Under15Pop = dplyr::if_else(used_aggregate_Under15Pop, Under15Pop_from_districts, Under15Pop)
    ) |>
    dplyr::select(-Total_from_districts, -Under5Pop_from_districts, -Under15Pop_from_districts)
}

# Public function ----

#' Build province population (Admin1) in wide format
#'
#' Combines POLIS + patches + Jamal; joins to province-year shapes; deduplicates;
#' fills datasource across gaps within GUID; fills missing values using growth rates.
#' Aggregates remaining province-level gaps by summing all district totals per province.
#'
#' @param pop_data `tibble` Province population dataset pulled from the POLIS API.
#' @param pop_dir `str` Default directory to the population folder.
#' @param prov_file_path `str` File path to the global province shapefile.
#' @param growth_rate_file_path `str` File path to the growth rate Excel file.
#' @inheritParams load_all_patches
#' @param output_file Optional .rds output path.
#' @return Tibble with GUID + province-year rows and final output naming.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' prov_pop_data(pop_data, output_file = "Data/pop/prov_pop_admin1.rds")
#' }
process_prov_pop_data <- function(pop_data,
                                  pop_dir = "GID/PEB/SIR/Data/pop",
                                  prov_file_path = "GID/PEB/SIR/Data/spatial/global.dist.rds",
                                  growth_rate_file_path = file.path(pop_dir, "pop raw/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx"),
                                  pakistan_file_path = file.path(pop_dir, "pop raw/csv files/2022_2023 Population Pakistan.csv"),
                                  somalia_2022_file_path = file.path(pop_dir, "pop raw/csv files/AFPPOP_22.csv"),
                                  somalia_2023_file_path = file.path(pop_dir, "pop raw/csv files/AFPPOP_23.csv"),
                                  somalia_2024_file_path = file.path(pop_dir, "pop raw/csv files/AFPPOP_24.csv"),
                                  kenya_file_path = file.path(pop_dir, "pop raw/csv files/Kenya_SubCounty_pop_2018.csv"),
                                  jamal_pop_file_path = file.path(pop_dir, "pop raw/csv files/POPU15.csv"),
                                  output_file = file.path(pop_dir, "processed_pop_file"),
                                  edav = TRUE) {
  # Crosswalk and remove forward fills
  pop_data <- crosswalk_pop_cols(pop_data)
  pop_data <- remove_forward_fill_polis_pop(pop_data)

  # Input 'province-year shape table + country-level growth rates
  province_long <- sirfunctions::load_clean_prov_sp(fp = prov_file_path, type = "long", edav = edav)
  growth_rates <- load_growth_rates(growth_rate_file_path, edav = edav)

  # Remove unnecessary columns from province_long and reduce file size (from 10GB to 115MB)
  province_long_subset <- province_long |>
    dplyr::tibble() |>
    dplyr::select(
      WHO_REGION, dplyr::ends_with("_NAME"), dplyr::ends_with("_GUID"),
      yr.st, yr.end, active.year.01, GUID
    ) |>
    dplyr::select(-dplyr::ends_with("VIZ_NAME"))

  rm(district_long)
  gc()

  # Transform POLIS wide and 0-5Y/U0-15Y/UALL to Under5Pop/Under15Pop/Total
  polis_pop <- pop_data |>
    dplyr::province(ADM0_NAME, ADM1_NAME, year, AgeGroupCode, datasource, .keep_all = TRUE) |>
    tidyr::pivot_wider(names_from = AgeGroupCode, values_from = Value) |>
    dplyr::mutate(
      Admin0Name = ADM0_NAME,
      Admin1Name = ADM1_NAME,
      year = as.numeric(year),
      Under5Pop = `0-5Y`,
      Under15Pop = `0-15Y`,
      Total = ALL
    ) |>
    dplyr::select(Admin0Name, Admin1Name, year, Under5Pop, Under15Pop, Total, datasource) |>
    join_pop_to_province_year_shapes(province_long_subset)

  # Input Non-POLIS data and removal of forward-filled repeats Values
  non_polis_pop <- load_all_patches(
    pakistan_file_path,
    somalia_2022_file_path,
    somalia_2023_file_path,
    somalia_2024_file_path,
    kenya_file_path,
    jamal_pop_file_path,
    edav
  ) |>
    join_pop_to_province_year_shapes(province_long_subset) |>
    remove_forward_fill_non_polis()

  # Combine POLIS + Non-POLIS data
  combined_pop <- dplyr::bind_rows(polis_pop, non_polis_pop) |>
    deduplicate_population()

  # Perform check to make sure ADM1GUIDs are equal to stated admin1guids of non-polis sources
  adm1guid_discrepancies <- combined_pop |>
    dplyr::mutate(
      diff_adm1guid = dplyr::case_when(
        ADM1_GUID != adm2guid ~ TRUE,
        ADM1_GUID == adm2guid ~ FALSE,
        .default = NA
      )
    ) |>
    dplyr::filter(diff_adm1guid) |>
    dplyr::left_join(district_long_subset |> dplyr::select(
      year = active.year.01,
      adm1guid = GUID,
      adm1name_from_non_api_src = ADM1_NAME
    ))

  # Count number of conflicting names
  adm1guid_discrepancies_conflicts <- adm1guid_discrepancies |>
    dplyr::filter(!is.na(adm1name_from_non_api_src)) |>
    dplyr::select(year, Admin1Name, GUID, adm1name_from_non_api_src)

  if (nrow(adm1guid_discrepancies_conflicts) > 0) {
    cli::cli_alert_info(paste0(
      "There are discrepancies in the GUIDs reported for provinces in some years. ",
      "Please check the error_log folder."
    ))
    sirfunctions::sirfunctions_io("write", NULL,
      file_loc = file.path(
        pop_dir, "error_log",
        paste0(
          "adm1_discrepancies_",
          Sys.Date(), ".parquet"
        )
      ),
      edav = edav
    )
  }

  rm(adm1guid_discrepancies, adm1guid_discrepancies_conflicts)

  # Compare different adm1guids and determine which GUID it represents
  # Count number of GUIDs in non-API sources that belong in the district shapefile

  base_data <- province_long_subset |>
    dplyr::mutate(active.year.01 = as.numeric(active.year.01)) |>
    dplyr::rename(ADM1_GUID = "GUID", year = "active.year.01") |>
    dplyr::distinct(ADM1_GUID, year, .keep_all = TRUE) |>
    dplyr::left_join(
      combined_pop |>
        dplyr::select(ADM1_GUID, year, Under5Pop, Under15Pop, Total, datasource),
      by = c("ADM1_GUID", "year")
    ) |>
    dplyr::group_by(ADM1_GUID) |>
    dplyr::arrange(year, .by_group = TRUE) |>
    tidyr::fill(datasource, .direction = "down") |>
    dplyr::ungroup() |>
    dplyr::left_join(growth_rates, by = c("ADM0_NAME" = "Admin0Name", "year" = "year"))

  # Note that there are mismatches in names

  # Some years the growth rates are not available. These need to be filled using "downup."
  # We first arrange from oldest to newest year, then fill.
  # For example, if no data in 2024, 2025 but there is data in 2023, then use 2023.
  # In case of gaps, such as 2023, 2024 (blank), 2025 (blank), 2026, then fill using 2023.
  base_data <- base_data |>
    dplyr::group_by(ADM0_NAME) |>
    dplyr::arrange(year, .by_group = TRUE) |>
    tidyr::fill(growth_rate, .direction = "downup") |>
    dplyr::ungroup()

  # Output
  result <- base_data |>
    apply_growth_rate("Under5Pop") |>
    apply_growth_rate("Under15Pop") |>
    apply_growth_rate("Total") |>
    aggregate_districts_to_province() |>
    dplyr::mutate(
      Used_Growth_Rate = ifelse(
        used_growth_Under5Pop | used_growth_Under15Pop | used_growth_Total,
        "Yes", "No"
      ),
      Used_District_Aggregation = ifelse(
        used_aggregate_Total | used_aggregate_Under5Pop | used_aggregate_Under15Pop,
        "Yes", ""
      ),
      GUID = ADM1_GUID
    ) |>
    dplyr::rename(
      Country_Name  = ADM0_NAME,
      Province_Name = ADM1_NAME,
      SOURCE        = datasource
    ) |>
    dplyr::select(
      -used_growth_Under5Pop, -used_growth_Under15Pop, -used_growth_Total,
      -used_aggregate_Total, -used_aggregate_Under5Pop, -used_aggregate_Under15Pop,
      -active.year.01, -datasource
    )

  if (!is.null(output_file)) readr::write_rds(result, output_file)
  result
}
