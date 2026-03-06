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
    tidyr::pivot_longer(c(`2022`, `2023`), names_to = "year", values_to = "Under15Pop") |>
    dplyr::mutate(
      year = as.numeric(year),
      Total = NA_real_,
      Under5Pop = NA_real_,
      Under15Pop = as.numeric(Under15Pop),
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
      Under15Pop = as.numeric(gsub(",", "", Total))
    )

  somalia_formatted <- somalia_patches |>
    dplyr::select(
      Admin0Name,
      Admin1Name = PROVINCE,
      Admin2Name = DISTRICT,
      year,
      Under15Pop,
      datasource
    ) |>
    dplyr::mutate(year = as.numeric(year),
                  Under5Pop  = NA_real_,
                  Total = NA_real_)

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
      datasource = "KENYA_2018_PATCH",
      Under5Pop = NA_real_,
      Total = NA_real_
    ) |>
    dplyr::select(Admin0Name, Admin1Name, Admin2Name, year, Under5Pop, Under15Pop, Total, datasource)
}

#' Load Jamal district under-15 population
#'
#' @param jamal_pop_file_path Path to Jamal file.
#' @param edav `logical` Whether to use EDAV or not to load the file.
#'
#' @returns `tibble` Population data with Admin names, year, age columns, datasource.
#'
#' @export
load_jamal_pop <- function(
    jamal_pop_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/POPU15.csv",
    edav = TRUE
) {
  jamal_pop <- sirfunctions::sirfunctions_io("read", NULL, jamal_pop_file_path,
                                             edav = edav)

  # Dedup
  jamal_pop <- jamal_pop |>
    dplyr::mutate(dplyr::across(dplyr::everything(), \(x) stringr::str_to_upper(x))) |>
    dplyr::distinct()

  jamal_pop_formatted <- jamal_pop |>
    # Some of the PlaceIds already have the curly braces
    dplyr::mutate(PlaceId = stringr::str_replace_all(PlaceId, "\\{", ""),
                  PlaceId = stringr::str_replace_all(PlaceId, "\\}", ""),
                  PlaceId = stringr::str_trim(PlaceId),
                  PlaceId = stringr::str_to_upper(PlaceId)) |>
    dplyr::mutate(
      Admin0Name = toupper(Admin0),
      Admin1Name = toupper(Admin1),
      Admin2Name = toupper(Admin2),
      adm2guid = paste0("{", PlaceId, "}"),
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
                  Under15Pop = as.numeric(Under15Pop),
                  datasource = "JAMAL_POP")

  jamal_pop_formatted <- jamal_pop_formatted |>
    # Remove invalid ADM2GUIDs from Jamal Pop
    # {C7B58BAE-7125-41F8-8A19-E8F4378DCCE5} North Gondar was only active in 2020 based on district shapefile, so remove for 2019, 2021, 2022
    # {2BA264D0-ABCA-4EC5-89F0-794744688AF1} North Gondar was only active in 2021 based on district shapefile, so remove for 2019, 2020
    # {A56970BF-2D44-4FD6-A3D4-6B351339B4AB} BENCH MAJI was only active in 2019, 2020 based on district shapefile, so remove for 2021 and 2022
    dplyr::filter(!(adm2guid == "{A56970BF-2D44-4FD6-A3D4-6B351339B4AB}" & year %in% c(2018, 2021, 2022)),
                  !(adm2guid == "{2BA264D0-ABCA-4EC5-89F0-794744688AF1}" & year %in% c(2018, 2019, 2020)),
                  !(adm2guid == "{C7B58BAE-7125-41F8-8A19-E8F4378DCCE5}" & year %in% c(2018, 2019, 2021, 2022))) |>
    # Filter out duplicated GUIDs that have incorrect counts
    # Keep the u15pops that closely matches the pop counts in the POLIS API pop dataset
    dplyr::filter(
      !(adm2guid == "{2BA264D0-ABCA-4EC5-89F0-794744688AF1}" & Under15Pop < 300000),
      !(adm2guid == "{A56970BF-2D44-4FD6-A3D4-6B351339B4AB}" & Under15Pop >= 410000)
    )

  return(jamal_pop_formatted)
}


#' Load 2015 World Pop data
#'
#' @description
#' World pop data obtained for 2015, to fill in missing data before 2016.
#'
#' @param world_pop_file_path `str` Path to the world pop file path.
#' @param edav `logical` Whether to use EDAV or not to load the file.
#'
#' @returns `tibble` 2015 World Pop data.
#' @export
#'
load_world_pop_patch <- function(world_pop_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/adm2_2015_pop.csv",
                                 edav = TRUE) {
  pop15 <- sirfunctions::sirfunctions_io(io = "read", NULL, file_loc = world_pop_file_path, edav = edav) |>
    dplyr::rename(
      Under15Pop = all_0014,
      Under5Pop = all_0004,
      adm1guid = ADM1_GUID,
      adm2guid = GUID,
      Admin0Name = ADM0_NAME,
      Admin1Name = ADM1_NAME, Admin2Name = ADM2_NAME) |>
    dplyr::mutate(year = 2015, datasource = "WORLD_POP",
                  Admin0Name = ifelse(stringr::str_detect(Admin0Name, "IVOIRE"), "COTE D IVOIRE", Admin0Name),
                  Admin1Name = dplyr::case_when(
                    Admin0Name == "TIMOR-LESTE" & adm1guid == "{A1F50BC9-EC0A-4979-845C-98DF1A352AEF}" ~ "LIQUIÇÁ",
                    Admin0Name == "COOK ISLANDS" & adm1guid == "{3313FFF5-9113-49D3-AC57-473B9F28FDE8}" ~ "MANUAE AND TEAUOTU",
                    Admin0Name == "GUAM" & adm1guid == "{9CF7AE57-7FD9-4B44-B15A-4D9873AF509F}"~ "HAGATÑA",
                    Admin0Name == "KIRIBATI" & adm1guid == "{8EF854F2-CEBF-494A-B447-B2EC11450B96}" ~ "TABITEUEA NORTH",
                    Admin0Name == "KIRIBATI" & adm1guid == "{0977BB72-8A91-403F-A4F6-27C910DF1168}" ~ "TABITEUEA SOUTH",
                    Admin0Name == "COMOROS" & adm1guid == "{B940CAF0-1C95-409D-A39D-1AEDC153B17B}" ~ "NJAZÍDJA",
                    Admin0Name == "MAURITIUS" & adm1guid == "{3A214917-2F6F-4181-9DE1-B2C8A8842FE3}" ~ "RIVIERE DU REMPART",
                    Admin0Name == "SAO TOME AND PRINCIPE" & adm1guid == "{63B0EAFE-817E-43A0-97A8-6163F6FED504}" ~ "SAO_TOME",
                    Admin0Name == "SAO TOME AND PRINCIPE" & adm1guid == "{504267D4-D657-4AC7-B680-36364E09DEDD}" ~ "PRINCIPE",
                    .default = Admin1Name),
                  Admin2Name = dplyr::case_when(
                    Admin0Name == "TIMOR-LESTE" & adm2guid == "{5E239A7A-0101-47B5-A890-C8E6368D6D93}" ~ "LIQUIÇÁ",
                    Admin0Name == "COOK ISLANDS" & adm2guid == "{C44D12CA-0BBB-4230-A4A0-169136005BDD}" ~ "MANUAE AND TEAUOTU",
                    Admin0Name == "GUAM" & adm2guid == "{26D9B778-BCF2-4943-A8C0-5BFDE1068EFD}" ~ "HAGATÑA",
                    Admin0Name == "KIRIBATI" & adm2guid == "{EEC7BF01-1431-4C3B-B110-542F5A20271F}" ~ "TABITEUEA NORTH",
                    Admin0Name == "KIRIBATI" & adm2guid == "{BD995045-5442-4CE6-8D9F-D2343AB8FC01}" ~ "TABITEUEA SOUTH",
                    Admin0Name == "COMOROS" & adm2guid == "{4019BF0F-2C8A-4ED7-90ED-3D31BA03C55B}" ~ "NJAZÍDJA",
                    Admin0Name == "MAURITIUS" & adm2guid == "{B9E68FEE-6B19-4806-AB7C-89432B4E0744}" ~ "RIVIERE_DU_REMPART",
                    .default = Admin2Name)
                  ) |>
    dplyr::mutate(Admin1Name = gsub(" ", "_", Admin1Name),
                  Admin1Name = gsub("-", "_", Admin1Name),
                  Total = NA_real_) |>
    dplyr::select(Admin0Name, Admin1Name, Admin2Name, year, Under15Pop, Under5Pop, Total, datasource, adm2guid)

}

#' Combine all district patch sources
#'
#' @inheritParams load_pakistan_patch
#' @inheritParams load_somalia_patch
#' @inheritParams load_kenya_patch
#' @inheritParams load_jamal_pop
#' @inheritParams load_world_pop_patch
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
                             world_pop_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/adm2_2015_pop.csv",
                             edav = TRUE) {

  pak_patch <- load_pakistan_patch(pakistan_file_path, edav)
  som_patch <- load_somalia_patch(somalia_2022_file_path, somalia_2023_file_path, somalia_2024_file_path, edav)
  ken_patch <- load_kenya_patch(kenya_file_path, edav)
  jamal_pop <- load_jamal_pop(jamal_pop_file_path, edav)
  world_pop <- load_world_pop_patch(world_pop_file_path, edav)

  return(dplyr::bind_rows(pak_patch, som_patch, ken_patch, jamal_pop, world_pop))
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
  wpp_raw <- sirfunctions::sirfunctions_io("read", NULL, file_loc, edav = edav, sheet = 1)
  wpp_raw <- wpp_raw$Estimates

  # Select and standardize output
  growth_rates <- wpp_raw |>
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
      growth_rate = as.numeric(growth_rate) / 100) |> # convert to decimal
    dplyr::arrange(Admin0Name, year)

  cli::cli_alert_info("Note: Growth rates are in decimal form, not percentages!")

  return(growth_rates)

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
    dplyr::arrange(GUID, datasource, active.year.01) |>
    dplyr::group_by(GUID, datasource) |>
    dplyr::mutate(

      # This flag was written with AI
      # Microsoft 365 Copilot version bizchat.20260217.44.1
      ff_total   = !is.na(dplyr::lag(Total)) & Total == dplyr::lag(Total) &
        active.year.01 == dplyr::lag(active.year.01) + 1,
      ff_under15 = !is.na(dplyr::lag(Under15Pop)) & Under15Pop == dplyr::lag(Under15Pop) &
        active.year.01 == dplyr::lag(active.year.01) + 1,
      ff_under5  = !is.na(dplyr::lag(Under5Pop))  & Under5Pop  == dplyr::lag(Under5Pop)  &
        active.year.01 == dplyr::lag(active.year.01) + 1,
      # Combined flag: TRUE if any variable was forward filled on this row
      is_forward_fill = ff_total | ff_under15 | ff_under5,

      Total = dplyr::if_else(
        !is.na(dplyr::lag(Total)) & Total == dplyr::lag(Total) &
          active.year.01 == dplyr::lag(active.year.01) + 1,
        NA_real_, Total
      ),
      Under15Pop = dplyr::if_else(
        !is.na(dplyr::lag(Under15Pop)) & Under15Pop == dplyr::lag(Under15Pop) &
          active.year.01 == dplyr::lag(active.year.01) + 1,
        NA_real_, Under15Pop
      ),
      Under5Pop = dplyr::if_else(
        !is.na(dplyr::lag(Under5Pop)) & Under5Pop == dplyr::lag(Under5Pop) &
          active.year.01 == dplyr::lag(active.year.01) + 1,
        NA_real_, Under5Pop
      )
    ) |>
    dplyr::ungroup()
}

#' Applies the growth rate to each population columns
#'
#' @param base_data `tibble` Combined population data with the growth rate columns.
#' @param pop_column `str` Name of the population column to apply the growth rate to.
#' @param grouping_col `str` Column to use for grouping.
#'
#' @returns `tibble` Population data with population filled based on growth rates
#' @keywords internal
#'
apply_growth_rate <- function(base_data, pop_column, grouping_col = "ADM2_GUID") {

  # Create anchor year vars
  base_data_formatted <- base_data |>
    dplyr::group_by(!!dplyr::sym(grouping_col)) |>
    dplyr::arrange(year, .by_group = TRUE) |>
    dplyr::mutate(anchor_year = year,
                  anchor_value = !!dplyr::sym(pop_column)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c("anchor_year", "anchor_value")),
                                \(x) {
                                  ifelse(!is.na(!!dplyr::sym(pop_column)), x, NA)
                                })) |>
    tidyr::fill(anchor_year, anchor_value, .direction = "downup") |>
    dplyr::ungroup() |>
    dplyr::mutate(growth_factor_applied = dplyr::if_else(year != anchor_year, TRUE, FALSE)) |>
    tidyr::replace_na(list(growth_factor_applied = FALSE))

  base_data_formatted <- base_data_formatted |>
    dplyr::rename_with(dplyr::recode,
                       growth_factor_applied = paste0("used_growth_", pop_column))

  # forward fill
  forward_fill <- base_data_formatted |>
    dplyr::filter(year > anchor_year) |>
    dplyr::group_by(!!dplyr::sym(grouping_col), anchor_year) |>
    dplyr::mutate(cp = cumprod((1+growth_rate)),
           computed_value = cp * anchor_value) |>
    dplyr::ungroup()

  # backfill
  backward_fill <- base_data_formatted |>
    dplyr::arrange(dplyr::desc(year), !!dplyr::sym(grouping_col), anchor_year) |>
    dplyr::filter(year <= anchor_year) |>
    dplyr::group_by(!!dplyr::sym(grouping_col), anchor_year) |>
    dplyr::mutate(lag_gr = dplyr::lag(growth_rate)) |>
    dplyr::filter(year != anchor_year) |> # remove anchor year values
    dplyr::mutate(cp = cumprod((1+lag_gr)),
           computed_value = anchor_value / cp) |>
    dplyr::ungroup()

  # no fill
  no_fill <- base_data_formatted |>
    dplyr::filter((year == anchor_year | is.na(anchor_year)))

  # combine
  base_data_growth_rate_filled <- dplyr::bind_rows(forward_fill, backward_fill, no_fill)

  # replace
  base_data_growth_rate_filled <- base_data_growth_rate_filled |>
    dplyr::mutate(dplyr::across(dplyr::any_of(pop_column), \(x) dplyr::if_else(is.na(x), computed_value, x))) |>
    dplyr::select(-cp, -computed_value, -anchor_value, -lag_gr) |>
    dplyr::rename_with(dplyr::recode,
                       anchor_year = paste0(pop_column,"_anchor_year")) |>
    dplyr::arrange(ADM0_NAME, year)

  return(base_data_growth_rate_filled)

}

#' Patch missing data in POLIS pop with non-polis data
#'
#' @param polis_pop `tibble` Population data from POLIS API.
#' @param non_polis_pop `tibble` Population data fron non-POLIS API sources.
#' @param patch_file `str` Name of the patch file datasource. Valid values include:
#' "JAMAL POP", "PATCH_PAKISTAN", "KENYA 2018 PATCH", "PATCH_SOMALIA".
#'
#' @returns `tibble` Population data patched.
#' @keywords internal
#'
patch_polis_with_non_polis_pop <- function(polis_pop, non_polis_pop, patch_file) {

  if (!patch_file %in% c("JAMAL_POP", "PATCH_PAKISTAN",
                         "KENYA_2018_PATCH", "PATCH_SOMALIA",
                         "WORLD_POP")) {
    cli::cli_abort("Invalid patch file datasource.")
  }

  combined_pop <- dplyr::left_join(polis_pop,
                                   non_polis_pop |>
                                     dplyr::filter(datasource == patch_file) |>
                                     dplyr::rename(patch_u15pop = "0-15Y",
                                                   patch_u5pop = "0-5Y",
                                                   patch_totpop = "ALL") |>
                                     dplyr::select(-datasource)) |>
    dplyr::mutate(datasource = dplyr::case_when(
      !is.na(patch_u15pop) & is.na(`0-15Y`) ~ patch_file,
      !is.na(patch_u5pop) & is.na(`0-5Y`) ~ patch_file,
      !is.na(patch_totpop) & is.na(ALL) ~ patch_file,
      .default = datasource
    )) |>
    dplyr::mutate(ALL = dplyr::coalesce(ALL, patch_totpop),
                  `0-5Y` = dplyr::coalesce(`0-5Y`, patch_u5pop),
                  `0-15Y` = dplyr::coalesce(`0-15Y`, patch_u15pop)
    ) |>
    dplyr::select(-dplyr::starts_with("patch_"))

  return(combined_pop)
}

# Public function ----

#' Build district population (Admin2) in wide format
#'
#' Combines POLIS + patches + Jamal; joins to district-year shapes; deduplicates;
#' fills datasource across gaps within GUID; fills missing values using growth rates.
#'
#' @param pop_data `tibble` District population dataset pulled from the POLIS API.
#' @param pop_dir `str` Default directory to the population folder.
#' @param dist_file_path `str` File path to the global district shapefile.
#' @param growth_rate_file_path `str` File path to the growth rate Excel file.
#' @inheritParams load_all_patches
#' @param output_dir `str` Where to output the cleaned district population dataset.
#' @param output_type `str` Output types. Valid values are 'rds', 'csv', and 'parquet'. Default
#' is `rds`.
#' @returns `tibble` Cleaned district population dataset.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dist_pop_data(pop_data, output_file = "Data/pop/dist_pop_admin2.rds")
#' }
process_dist_pop_data <- function(pop_data,
                                  pop_dir = "GID/PEB/SIR/Data/pop",
                                  dist_file_path = "GID/PEB/SIR/Data/spatial/global.dist.rds",
                                  growth_rate_file_path = file.path(pop_dir, "pop raw/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx"),
                                  pakistan_file_path = file.path(pop_dir, "pop raw/csv files/2022_2023 Population Pakistan.csv"),
                                  somalia_2022_file_path = file.path(pop_dir, "pop raw/csv files/AFPPOP_22.csv"),
                                  somalia_2023_file_path = file.path(pop_dir, "pop raw/csv files/AFPPOP_23.csv"),
                                  somalia_2024_file_path = file.path(pop_dir, "pop raw/csv files/AFPPOP_24.csv"),
                                  kenya_file_path = file.path(pop_dir, "pop raw/csv files/Kenya_SubCounty_pop_2018.csv"),
                                  jamal_pop_file_path = file.path(pop_dir, "pop raw/csv files/POPU15.csv"),
                                  world_pop_file_path = file.path(pop_dir, "pop raw/csv files/adm2_2015_pop.csv"),
                                  output_dir = file.path(pop_dir, "processed_pop_file"),
                                  output_type = "parquet",
                                  edav = TRUE) {

  # Remove period if passed in the output type
  output_type <- stringr::str_replace(output_type, stringr::fixed("."), "")

  if (!output_type %in% c("rds", "csv", "parquet")) {
    cli::cli_abort("Please pass only 'rds', 'csv', or 'parquet' in output_type.")
  }

  # Crosswalk and remove forward fills
  pop_data <- crosswalk_pop_cols(pop_data)
  pop_data <- remove_forward_fill_polis_pop(pop_data)

  # Alert to determine how much of the POLIS API are forward fills
  pop_data_forward_fill_n <- sum(pop_data$is_forward_fill, na.rm = TRUE)
  cli::cli_alert_info(paste0("There are ", pop_data_forward_fill_n, " (", round(pop_data_forward_fill_n / nrow(pop_data) * 100),   "%) records in the POLIS pop API ",
                             "where populations were forward filled. These values were replaced with NAs as they don't account for growth rates."))

  # Input district-year shape table + country-level growth rates
  district_long <- sirfunctions::load_clean_dist_sp(fp = dist_file_path, type = "long", edav = edav)
  growth_rates <- load_growth_rates(growth_rate_file_path, edav = edav)

  # Remove unnecessary columns from district_long and reduce file size (from 10GB to 115MB)
  district_long_subset <- district_long |>
    dplyr::tibble() |>
    dplyr::select(#WHO_REGION, apparently missing in the new SF
                  dplyr::ends_with("_NAME"), dplyr::ends_with("_GUID"),
                  yr.st, yr.end, active.year.01, GUID) |>
    dplyr::select(-dplyr::ends_with("VIZ_NAME"))

  rm(district_long)
  gc()

  # Transform POLIS wide and 0-5Y/U0-15Y/UALL to Under5Pop/Under15Pop/Total
  polis_pop <- pop_data |>
    dplyr::arrange(year) |>
    dplyr::select(-CREATEDDATE, -UPDATEDDATE, -STARTDATE, -ENDDATE, -is_forward_fill) |>
    tidyr::pivot_wider(names_from = AgeGroupCode, values_from = Value) |>
    dplyr::rename(
      ADM0_GUID = adm0guid,
      ADM1_GUID = adm1guid,
      GUID = adm2guid,
      active.year.01 = year
    )

  # Prefer adm0-1 guid and names from the shapefile
  # Sometimes the province are not matched correctly between the POLIS API pop file and
  # the shapefile :(
  polis_pop <- dplyr::left_join(district_long_subset |>
                                  dplyr::rename(sf_adm0_name = ADM0_NAME,
                                                sf_adm1_name = ADM1_NAME,
                                                sf_adm2_name = ADM2_NAME,
                                                sf_adm0guid = ADM0_GUID,
                                                sf_adm1guid = ADM1_GUID),
                                polis_pop) |>
    dplyr::distinct() |>
    dplyr::mutate(ADM0_NAME = dplyr::coalesce(sf_adm0_name, ADM0_NAME),
                  ADM1_NAME = dplyr::coalesce(sf_adm1_name, ADM1_NAME),
                  ADM2_NAME = dplyr::coalesce(sf_adm2_name, ADM2_NAME),
                  ADM0_GUID = dplyr::coalesce(sf_adm0guid, ADM0_GUID),
                  ADM1_GUID = dplyr::coalesce(sf_adm1guid, ADM1_GUID)) |>
    dplyr::select(-dplyr::starts_with("sf_"))

  # Input Non-POLIS data and removal of forward-filled repeats Values
  non_polis_pop <- load_all_patches(pakistan_file_path,
                                    somalia_2022_file_path,
                                    somalia_2023_file_path,
                                    somalia_2024_file_path,
                                    kenya_file_path,
                                    jamal_pop_file_path,
                                    world_pop_file_path,
                                    edav)

  # Jamal pop, Kenya, Pakistan, Somalia contains only U15
  #
  # Format to match the district shapefile
  non_polis_pop <- non_polis_pop |>
    dplyr::rename(ADM0_NAME = Admin0Name,
                  ADM1_NAME = Admin1Name,
                  ADM2_NAME = Admin2Name,
                  active.year.01 = year,
                  GUID = adm2guid) |>
    dplyr::distinct()

  # Ensure the Jamal pops match the names with the GUIDs
  non_polis_jamal_wp <- non_polis_pop |>
    dplyr::filter(datasource %in% c("JAMAL_POP", "WORLD_POP")) |>
    dplyr::right_join(district_long_subset |>
                        dplyr::rename(sf_adm0_name = ADM0_NAME,
                                      sf_adm1_name = ADM1_NAME,
                                      sf_adm2_name = ADM2_NAME)) |>
    dplyr::distinct() |>
    dplyr::mutate(ADM0_NAME = dplyr::coalesce(sf_adm0_name, ADM0_NAME),
                  ADM1_NAME = dplyr::coalesce(sf_adm1_name, ADM1_NAME)) |>
    dplyr::select(-dplyr::starts_with("sf_"))

  # Fill GUIDs based on names for the Pakistan, Somalia, and Kenya patch since
  # There don't have GUIDs
  non_polis_pop_non_jamal_wp <- dplyr::left_join(district_long_subset,
                                    non_polis_pop |>
                                      dplyr::filter(!datasource %in% c("JAMAL_POP", "WORLD_POP")) |>
                                      dplyr::select(-GUID))

  non_polis_pop_combined <- dplyr::bind_rows(non_polis_pop_non_jamal_wp, non_polis_jamal_wp) |>
    dplyr::filter(!is.na(datasource)) |>
    dplyr::distinct()

  non_polis_pop_combined <- non_polis_pop_combined |>
    remove_forward_fill_non_polis() |>
    dplyr::rename(`0-5Y` = Under5Pop,
                  `0-15Y` = Under15Pop,
                  ALL = Total)

  forward_filled_non_polis_pop_n <- sum(non_polis_pop_combined$is_forward_fill, na.rm = TRUE)
  cli::cli_alert_info(paste0("There were ", forward_filled_non_polis_pop_n, " forward-filled pop data in the patch files."))

  non_polis_pop_combined <- non_polis_pop_combined |>
    dplyr::select(-dplyr::starts_with("ff"), -is_forward_fill)

  # Fill using the following step:
  # PAK and SOM Patch > KENYA Patch > Jamal Pop Patch |> World Pop
  u15_missingness_before <- sum(is.na(polis_pop$`0-15Y`))
  cli::cli_alert_info(paste0(round(u15_missingness_before),
                             " adm2guid-year combination missing populations in POLIS API (forward-fills removed)."))
  combined_pop <- patch_polis_with_non_polis_pop(polis_pop, non_polis_pop_combined, "PATCH_PAKISTAN")
  u15_missingness <- sum(is.na(combined_pop$`0-15Y`))
  cli::cli_alert_info(paste0(round(u15_missingness),
                             " adm2guid-year combination missing populations after patching with PATCH_PAKISTAN."))

  for (i in c("PATCH_SOMALIA", "KENYA_2018_PATCH", "JAMAL_POP", "WORLD_POP")) {
    combined_pop <- patch_polis_with_non_polis_pop(combined_pop, non_polis_pop_combined, i)
    u15_missingness <- sum(is.na(combined_pop$`0-15Y`))
    cli::cli_alert_info(paste0(round(u15_missingness),
                               " adm2guid-year combination missing populations after patching with ",
                               i, "."))
  }

  cli::cli_alert_info(paste0("Number of records filled using patches: ", u15_missingness_before - u15_missingness))

  if (nrow(combined_pop) != nrow(polis_pop)) {
    cli::cli_alert_warning("combined_pop != polis_pop counts. Please check for a many-to-many join!!!")
  } else {
    cli::cli_alert_success("No new or lost records added after implementing patches.")
  }

  # Change column order so it's neater to look at
  combined_pop <- combined_pop |>
    dplyr::relocate(ADM1_GUID, .after = ADM0_GUID) |>
    dplyr::relocate(GUID, .after = ADM1_GUID) |>
    dplyr::distinct()

  # Check to make sure there are no duplicated guid - year populations
  combined_pop_dup <- combined_pop |>
    dplyr::group_by(GUID, active.year.01) |>
    dplyr::summarize(n = dplyr::n()) |>
    dplyr::filter(n > 1)

  if (nrow(combined_pop_dup) != 0) {
    cli::cli_alert_warning("There are duplicates in combined population dataset. Check the pop errors folder.")
    sirfunctions::sirfunctions_io("write", NULL,
                                  file_loc = file.path(pop_dir, "errors", paste0(Sys.Date(), "_combined_pop_duplicates.parquet")),
                                  obj = combined_pop_dup,
                                  edav = edav)
  } else {
    cli::cli_alert_success("No duplicates in combined population dataset")
  }

  # Check for GUIDs in the pop file that are not in the district shapefile
  mismatches <- dplyr::anti_join(combined_pop |>
                                   dplyr::select(GUID, active.year.01, datasource),
                                 district_long_subset |>
                                   dplyr::select(GUID, active.year.01))

  if (nrow(mismatches) != 0) {
    cli::cli_alert_info("There are GUIDs present in the population dataset not in the district shapefile. Check the pop errors folder.")
    sirfunctions::sirfunctions_io("write", NULL,
                                  file_loc = file.path(pop_dir, "errors", paste0(Sys.Date(), "_combined_pop_duplicates.parquet")),
                                  obj = mismatches,
                                  edav = edav)
  } else {
    cli::cli_alert_success("All GUIDs in the population dataset are present in the district shapefile.")
  }

  # Add growth rates
  base_data <- combined_pop |>
    dplyr::rename(ADM2_GUID = "GUID", year = "active.year.01") |>
    dplyr::group_by(ADM2_GUID) |>
    dplyr::arrange(year, .by_group = TRUE) |>
    tidyr::fill(datasource, .direction = "downup") |>
    dplyr::ungroup() |>
    dplyr::left_join(growth_rates, by = c("ADM0_NAME" = "Admin0Name", "year" = "year"))

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
  cli::cli_process_start("Applying growth rate to fill missing populations.")
  result <- base_data |>
    apply_growth_rate("ALL") |>
    apply_growth_rate("0-15Y") |>
    apply_growth_rate("0-5Y")
  cli::cli_process_done()

  # Compare missingness after growth rate added to population data
  cli::cli_alert_info(paste0(sum(is.na(base_data$`0-15Y`)) - sum(is.na(result$`0-15Y`)),
                             " additional records filled using growth rates."))

  # Add WHO region to results because the new SF doesn't have that column
  result <- dplyr::left_join(result |>
                                    dplyr::select(-WHO_REGION),
                             polis_pop |>
                               dplyr::group_by(ADM0_NAME) |>
                               dplyr::filter(!is.na(`0-15Y`)) |>
                               dplyr::filter(active.year.01 == max(active.year.01)) |>
                               dplyr::select(ADM0_NAME, WHO_REGION) |>
                               dplyr::distinct() |>
                               dplyr::ungroup())

  formatted_result <- result |>
    dplyr::select(-FK_DataSetId, -ISO_3_CODE) |>
    tidyr::replace_na(list(used_growth_ALL = FALSE,
                           `used_growth_0-15Y` = FALSE,
                           `used_growth_0-5Y` = FALSE,
                           datasource = "POLIS API")) |>
    dplyr::rename(
      who.region = "WHO_REGION",
      growth.rate = "growth_rate",
      adm0guid = "ADM0_GUID",
      adm1guid = "ADM1_GUID",
      adm2guid = "ADM2_GUID",
      ctry = "ADM0_NAME",
      prov = "ADM1_NAME",
      dist = "ADM2_NAME",
      u15pop = "0-15Y",
      u5pop = "0-5Y",
      totpop = "ALL",
      used_growth_rate_tot = "used_growth_ALL",
      used_growth_rate_u5 = "used_growth_0-5Y",
      used_growth_rate_u15 = "used_growth_0-15Y"
    ) |>
    dplyr::relocate(who.region, ctry, prov, dist, adm0guid, adm1guid, adm2guid, .after = year) |>
    dplyr::mutate(
      used.growth.rate = dplyr::case_when(
        used_growth_rate_tot & used_growth_rate_u5 & used_growth_rate_u15 ~ "u5, u15, tot",
        used_growth_rate_tot & used_growth_rate_u5 & !used_growth_rate_u15 ~ "u5, tot",
        used_growth_rate_tot & !used_growth_rate_u5 & used_growth_rate_u15 ~ "u15, tot",
        !used_growth_rate_tot & used_growth_rate_u5 & used_growth_rate_u15 ~ "u5, u15",
        used_growth_rate_tot & !used_growth_rate_u5 & !used_growth_rate_u15 ~ "tot",
        !used_growth_rate_tot & used_growth_rate_u5 & !used_growth_rate_u15 ~ "u5",
        !used_growth_rate_tot & !used_growth_rate_u5 & used_growth_rate_u15 ~ "u15",
        .default = "no"
      ),
      miss.u15 = dplyr::if_else(is.na(u15pop), TRUE, FALSE),
      miss.totpop = dplyr::if_else(is.na(totpop), TRUE, FALSE),
      # U15 population category
      pop.cat = dplyr::case_when(
        is.na(u15pop) == T | u15pop == 0 ~ "Missing",
        dplyr::between(u15pop, 0, 24999) ~ "<25,000",
        dplyr::between(u15pop, 25000, 49999) ~ "25,000-49,999",
        dplyr::between(u15pop, 50000, 99999) ~ "50,000-99,999",
        dplyr::between(u15pop, 100000, 499999) ~ "100,000-499,999",
        u15pop >= 500000 ~ ">=500,000")
    ) |>
    dplyr::mutate(pop.cat = factor(pop.cat,
                                   levels = c("Missing", "<25,000",
                                              "25,000-49,999", "50,000-99,999",
                                              "100,000-499,999", ">=500,000"),
                                   ordered = TRUE)) |>
    dplyr::select(-dplyr::contains("used_growth_rate_"))

  # Check if there are GUIDs in the Shapefile that are not in the pop file
  not_in_sf <- setdiff(district_long_subset$GUID, formatted_result$adm2guid)

  if (length(not_in_sf) != 0) {
    cli::cli_alert_warning("There are GUIDs in the shapefile that's not in the population file.")
    sirfunctions::sirfunctions_io("write", NULL, file_loc = file.path(pop_dir,
                                                                      "errors",
                                                                      paste0(Sys.Date(),
                                                                             "_guids_in_dist_sf_not_in_dist_pop.parquet")),
                                  obj = dplyr::tibble(GUID = not_in_sf),
                                  edav = edav)
  } else {
    cli::cli_alert_success("All GUIDs in the district shapefile are present in the district population file.")
  }


  if (!is.null(output_dir)){
    sirfunctions::sirfunctions_io("write", NULL, file_loc = file.path(output_dir, paste0("dist.pop.long.", output_type)),
                                  obj = formatted_result,
                                  edav = edav)
  }

  # Perform diagnostic checks
  prop_missingness_by_ctry_year <- formatted_result |>
    dplyr::group_by(who.region , ctry, year) |>
    dplyr::summarize(missing_u15 = sum(is.na(u15pop)),
                     missing_u5 = sum(is.na(u5pop)),
                     missing_tot = sum(is.na(totpop)),
                     total_districts = dplyr::n(),
                     missing_u15_pct = round(missing_u15 / total_districts * 100, 2)) |>
    dplyr::arrange(who.region, dplyr::desc(year), dplyr::desc(missing_u15_pct)) |>
    dplyr::ungroup()

  # Get max year where all district data are present
  last_year_w_complete_data <- prop_missingness_by_ctry_year |>
    dplyr::group_by(ctry) |>
    dplyr::filter(missing_u15_pct == 0) |>
    dplyr::summarize(last_year_with_complete_data = max(year)) |>
    dplyr::ungroup()

  # Join
  prop_missingness_by_ctry_year <- dplyr::left_join(prop_missingness_by_ctry_year, last_year_w_complete_data) |>
    dplyr::arrange(who.region, dplyr::desc(year), dplyr::desc(missing_u15_pct)) |>
    dplyr::ungroup()

  sirfunctions::sirfunctions_io("write", NULL, file_loc = file.path(pop_dir,
                                                                    "pop_diagnostics",
                                                                    paste0(Sys.Date(),
                                                                           "_prop_dist_pop_missing_by_ctry_year.csv")),
                                obj = prop_missingness_by_ctry_year,
                                edav = edav)

  invisible(formatted_result)

}
