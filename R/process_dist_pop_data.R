# Private functions ----
#' Load Pakistan district population patch
#'
#' @param pakistan_file_path Path to Pakistan patch file.
#'
#' @return Tibble with Admin names, year, age columns, datasource.
#'
#' @export
load_pakistan_patch <- function(
    pakistan_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/2022_2023 Population Pakistan.csv",
    edav = TRUE) {
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
      District = dplyr::case_when(
        Province == "KPAKHTUNKHWA" & District == "CHITRALLOWER" ~ "CHITRAL LOWER",
        Province == "KPAKHTUNKHWA" & District == "CHITRALUPPER" ~ "CHITRAL UPPER",
        Province == "KPAKHTUNKHWA" & District == "KOHISTANLOWER" ~ "KOHISTAN LOWER",
        Province == "KPAKHTUNKHWA" & District == "KOHISTANUPPER" ~ "KOHISTAN UPPER",
        Province == "KPAKHTUNKHWA" & District == "KOLAIPALAS" ~ "KOLAI PALAS",
        Province == "SINDH" & District == "KHIKEAMARI" ~ "KHIKAMARI",
        TRUE ~ District
      )
    ) |>
    dplyr::select(Country, Province, District, `2022`, `2023`) |>
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
      Admin1Name = Province,
      Admin2Name = District
    ) |>
    dplyr::select(Admin0Name, Admin1Name, Admin2Name, year, Under5Pop, Under15Pop, Total, datasource)
}

#' Load Somalia district population patch
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
        PROVINCE == "SANAQ"  ~ "SANAG",
        PROVINCE == "MUDUG" & DISTRICT %in% c("GOLDOGOB", "JARIBAN", "GALKAYU NORTH") ~ "MUDUG PL",
        PROVINCE == "MUDUG" & DISTRICT %in% c("HARA DHERE", "HOBYO") ~ "MUDUG GM",
        PROVINCE == "SOUTH MUDUG" & DISTRICT == "GALKAYU SOUTH" ~ "MUDUG GM",
        DISTRICT %in% c("GARDO", "BENDER BAYLA", "HAFUN", "RAKO", "WACIYA") ~ "KARKAR",
        TRUE ~ PROVINCE
      ),
      DISTRICT = dplyr::case_when(
        DISTRICT == "MADINA"     ~ "MEDINA",
        DISTRICT == "HAMAR WEYN" ~ "HAMARWEYNE",
        TRUE ~ DISTRICT
      ),
      Total = as.numeric(gsub(",", "", Total))
    )

  somalia_formatted <- somalia_patches |>
    dplyr::select(
      Admin0Name,
      Admin1Name = PROVINCE,
      Admin2Name = DISTRICT,
      year,
      Total,
      datasource
    ) |>
    dplyr::mutate(year = as.numeric(year),
                  Under5Pop  = NA_real_,
                  Under15Pop = NA_real_)

  return(somalia_formatted)

}

#' Load Kenya district population patch
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
      Admin2Name = SubCounty_Name,
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
      Admin2Name = dplyr::case_when(
        Admin2Name == "HOMA BAY" ~ "HOMA BAY TOWN",
        Admin2Name == "BURET" ~ "BURETI",
        Admin2Name == "CENTAL IMENTI" ~ "CENTRAL IMENTI",
        Admin2Name == "MUKURWENI" ~ "MUKURWEINI",
        Admin2Name == "NITHI" ~ "CHUKA-IGAMBANG'OMBE",
        Admin2Name == "DAGORETTI" ~ "DAGORETTI SOUTH",
        Admin2Name == "KILIMANI" ~ "DAGORETTI NORTH",
        Admin2Name == "DUJIS" ~ "GARISSA TOWNSHIP",
        Admin2Name == "SIAKAGO" ~ "MBEERE SOUTH",
        Admin2Name == "GACHOKA" ~ "MBEERE NORTH",
        Admin2Name == "SUBA SOUTH" ~ "SUBA",
        Admin2Name == "SUBA NORTH" ~ "MBITA",
        Admin2Name == "MWINGI EAST" ~ "MWINGI CENTRAL",
        TRUE ~ Admin2Name
      ),
      Admin0Name = "KENYA",
      year = 2018,
      datasource = "KENYA 2018 PATCH",
      Under5Pop = NA_real_,
      Total = NA_real_
    ) |>
    dplyr::select(Admin0Name, Admin1Name, Admin2Name, year, Under5Pop, Under15Pop, Total, datasource)
}

#' Load Jamal district under-15 population
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
                                             edav = edav)

  jamal_pop_formatted <- jamal_pop |>
    dplyr::mutate(
      Admin0Name = toupper(Admin0),
      Admin1Name = toupper(Admin1),
      Admin2Name = toupper(Admin2),
      adm2guid = paste0("{", toupper(PlaceId), "}"),
      Admin0Name = ifelse(stringr::str_detect(Admin0Name, "IVOIRE"),"COTE D IVOIRE", Admin0Name)
    ) |>
    dplyr::select(ends_with("Name"), starts_with("2"), adm2guid) |>
    tidyr::pivot_longer(cols = starts_with("2"),
                        names_to = "year",
                        values_to = "Under15Pop",
                        values_drop_na = TRUE) |>
    dplyr::mutate(Under5Pop = NA_real_,
                  Total = NA_real_,
                  year = as.integer(year),
                  datasource = "JAMAL POP")

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
#' @param file_loc Path to WPP Excel file.
#'
#' @return Tibble with Admin0Name, year, growth_rate.
#' @export
load_growth_rates <- function(
    file_loc = "GID/PEB/SIR/Data/pop/pop raw/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx",
    edav = TRUE
) {
  wpp_raw <- sirfunctions::sirfunctions_io("read", NULL, file_loc, edav = edav)
  wpp_raw <- wpp_raw$Estimates

  # Find growth-rate table header row
  header_row <- which(apply(
    wpp_raw, 1,
    \(row) any(grepl("Population Growth Rate", row))
  ))[1]

  wpp_body <- wpp_raw[(header_row + 1):nrow(wpp_raw), ]
  colnames(wpp_body) <- wpp_raw[header_row, ]

  # Select and standardize output
  wpp_body |>
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
      growth_rate = as.numeric(growth_rate),
      growth_rate = dplyr::if_else(
        !is.na(growth_rate) & growth_rate < 0,
        1 / (-1 * growth_rate),
        growth_rate
      )
    ) |>
    dplyr::arrange(Admin0Name, year)
}

#' Join named population rows to district-year shapes
#'
#' @param pop_named Tibble with Admin names, year, age columns, datasource.
#' @param district_long District-year long shapes table.
#'
#' @return Tibble joined to year-valid district shapes.
#'
#' @export
join_pop_to_district_year_shapes <- function(pop_named, district_long) {
  pop_named |>
    dplyr::left_join(
      district_long,
      by = c(
        "Admin0Name" = "ADM0_NAME",
        "Admin1Name" = "ADM1_NAME",
        "Admin2Name" = "ADM2_NAME",
        "year" = "active.year.01"
      )
    ) |>
    dplyr::filter(!is.na(GUID)) |>
    dplyr::mutate(
      ADM2_GUID = GUID,
      year = as.numeric(year),
      Under5Pop = as.numeric(Under5Pop),
      Under15Pop = as.numeric(Under15Pop),
      Total = as.numeric(Total)
    )
}

#' Remove forward-filled values (non-POLIS only)
#'
#' @param non_polis_pop Tibble with ADM2_GUID, year, age columns, datasource.
#'
#' @return Tibble with forward-filled values set to NA.
#'
#' @export
remove_forward_fill_non_polis <- function(non_polis_pop) {
  non_polis_pop |>
    dplyr::filter(datasource != "POLIS") |>
    dplyr::arrange(ADM2_GUID, datasource, year) |>
    dplyr::group_by(ADM2_GUID, datasource) |>
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
#' @param pop_with_guid Tibble with ADM2_GUID, year, age columns, datasource.
#'
#' @return Deduplicated tibble (one row per ADM2_GUID-year).
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
    dplyr::group_by(ADM2_GUID, year) |>
    dplyr::arrange(!has_any_value, source_rank, .by_group = TRUE) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(-source_rank, -has_any_value)
}

# Public function ----

#' Build district population (Admin2) in wide format
#'
#' Combines POLIS + patches + Jamal; joins to district-year shapes; deduplicates;
#' fills datasource across gaps within GUID; fills missing values using growth rates.
#'
#' @param pop_data Tibble POLIS datasource
#' @param dist_file_path `str` File path to the global district shapefile.
#' @param growth_rate_file_path `str` File path to the growth rate Excel file.
#' @inheritParams load_all_patches
#' @param output_file Optional .rds output path.
#' @return Tibble with GUID + district-year rows and final output naming.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dist_pop_data(pop_data, output_file = "Data/pop/dist_pop_admin2.rds")
#' }
process_dist_pop_data <- function(pop_data,
                          dist_file_path = "GID/PEB/SIR/Data/spatial/global.dist.rds",
                          growth_rate_file_path = "GID/PEB/SIR/Data/pop/pop raw/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx",
                          pakistan_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/2022_2023 Population Pakistan.csv",
                          somalia_2022_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/AFPPOP_22.csv",
                          somalia_2023_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/AFPPOP_23.csv",
                          somalia_2024_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/AFPPOP_24.csv",
                          kenya_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/Kenya_SubCounty_pop_2018.csv",
                          jamal_pop_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/POPU15.csv",
                          output_file = getwd(),
                          edav = TRUE) {

  # Crosswalk and remove forward fills
  pop_data <- crosswalk_pop_cols(pop_data)
  pop_data <- remove_forward_fill_polis_pop(pop_data)

  # Input district-year shape table + country-level growth rates
  district_long <- sirfunctions::load_clean_dist_sp(fp = dist_file_path, type = "long", edav = edav)
  growth_rates <- load_growth_rates(growth_rate_file_path, edav = edav)

  # Transform POLIS wide and 0-5Y/U0-15Y/UALL to Under5Pop/Under15Pop/Total
  polis_pop <- pop_data |>
    dplyr::distinct(ADM0_NAME, ADM1_NAME, ADM2_NAME, year, AgeGroupCode, datasource, .keep_all = TRUE) |>
    tidyr::pivot_wider(names_from = AgeGroupCode, values_from = Value) |>
    dplyr::mutate(
      Admin0Name = ADM0_NAME,
      Admin1Name = ADM1_NAME,
      Admin2Name = ADM2_NAME,
      year = as.numeric(year),
      Under5Pop = `0-5Y`,
      Under15Pop = `0-15Y`,
      Total = ALL
    ) |>
    dplyr::select(Admin0Name, Admin1Name, Admin2Name, year, Under5Pop, Under15Pop, Total, datasource) |>
    join_pop_to_district_year_shapes(district_long)

  # Input Non-POLIS data and removal of forward-filled repeats Values
  non_polis_pop <- load_all_patches(pakistan_file_path,
                                    somalia_2022_file_path,
                                    somalia_2023_file_path,
                                    somalia_2024_file_path,
                                    kenya_file_path,
                                    jamal_pop_file_path,
                                    edav) |>
    join_pop_to_district_year_shapes(district_long) |>
    remove_forward_fill_non_polis()

  # Combine POLIS + Non-POLIS data
  combined_pop <- dplyr::bind_rows(polis_pop, non_polis_pop) |>
    deduplicate_population()

  base_data <- district_long |>
    dplyr::mutate(ADM2_GUID = GUID, year = as.numeric(active.year.01)) |>
    dplyr::distinct(ADM2_GUID, year, .keep_all = TRUE) |>
    dplyr::left_join(
      combined_pop |>
        dplyr::select(ADM2_GUID, year, Under5Pop, Under15Pop, Total, datasource),
      by = c("ADM2_GUID", "year")
    ) |>
    dplyr::group_by(ADM2_GUID) |>
    dplyr::arrange(year, .by_group = TRUE) |>
    tidyr::fill(datasource, .direction = "downup") |>
    dplyr::ungroup() |>
    dplyr::left_join(growth_rates, by = c("ADM0_NAME" = "Admin0Name", "year" = "year"))

  # Apply Growth Rate to Fill NAs
  apply_growth <- function(base_data, pop_column) {
    flag_column <- paste0("used_growth_", pop_column)

    base_data |>
      dplyr::group_by(ADM2_GUID) |>
      dplyr::arrange(year, .by_group = TRUE) |>
      dplyr::mutate(
        anchor_year = tidyr::fill(
          tibble::tibble(anchor_year = dplyr::if_else(
            is.na(.data[[pop_column]]), NA_real_, year
          )),
          anchor_year,
          .direction = "down"
        )$anchor_year,

        anchor_value = tidyr::fill(
          tibble::tibble(anchor_value = dplyr::if_else(
            is.na(.data[[pop_column]]), NA_real_, .data[[pop_column]]
          )),
          anchor_value,
          .direction = "down"
        )$anchor_value,

        used_growth = is.na(.data[[pop_column]]) &
          !is.na(anchor_year) & !is.na(anchor_value) &
          !is.na(growth_rate),

        "{pop_column}" := dplyr::if_else(
          used_growth,
          round(anchor_value * (((growth_rate / 100) + 1) ^ (year - anchor_year))),
          .data[[pop_column]]
        ),
        "{flag_column}" := used_growth
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-anchor_year, -anchor_value)
  }

  # Output
  result <- base_data |>
    apply_growth("Under5Pop") |>
    apply_growth("Under15Pop") |>
    apply_growth("Total") |>
    dplyr::mutate(
      Used_Growth_Rate = ifelse(
        used_growth_Under5Pop | used_growth_Under15Pop | used_growth_Total,
        "Yes", "No"
      ),
      GUID = ADM2_GUID
    ) |>
    dplyr::rename(
      Country_Name  = ADM0_NAME,
      Province_Name = ADM1_NAME,
      District_Name = ADM2_NAME,
      SOURCE        = datasource
    ) |>
    dplyr::select(
      -used_growth_Under5Pop, -used_growth_Under15Pop, -used_growth_Total,
      -active.year.01, -datasource
    )

  if (!is.null(output_file)) readr::write_rds(result, output_file)
  result
}
