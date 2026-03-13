# Build district population

Cleans the district level population data from the POLIS API and fill in
gaps in population counts using several patch files and application of
growth rates.

## Usage

``` r
process_dist_pop_data(
  pop_data,
  pop_dir = "GID/PEB/SIR/Data/pop",
  dist_file_path = "GID/PEB/SIR/Data/spatial/global.dist.rds",
  growth_rate_file_path = file.path(pop_dir,
    "pop raw/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx"),
  pakistan_file_path = file.path(pop_dir,
    "pop raw/csv files/2022_2023 Population Pakistan.csv"),
  somalia_2022_file_path = file.path(pop_dir, "pop raw/csv files/AFPPOP_22.csv"),
  somalia_2023_file_path = file.path(pop_dir, "pop raw/csv files/AFPPOP_23.csv"),
  somalia_2024_file_path = file.path(pop_dir, "pop raw/csv files/AFPPOP_24.csv"),
  kenya_file_path = file.path(pop_dir, "pop raw/csv files/Kenya_SubCounty_pop_2018.csv"),
  jamal_pop_file_path = file.path(pop_dir, "pop raw/csv files/POPU15.csv"),
  world_pop_file_path = file.path(pop_dir, "pop raw/csv files/adm2_2015_pop.csv"),
  output_dir = file.path(pop_dir, "processed_pop_file"),
  output_type = "parquet",
  edav = TRUE
)
```

## Arguments

- pop_data:

  `tibble` District population dataset pulled from the POLIS API.

- pop_dir:

  `str` Default directory to the population folder.

- dist_file_path:

  `str` File path to the global district shapefile.

- growth_rate_file_path:

  `str` File path to the growth rate Excel file.

- pakistan_file_path:

  Path to Pakistan patch file.

- somalia_2022_file_path:

  Path to Somalia 2022 file.

- somalia_2023_file_path:

  Path to Somalia 2023 file.

- somalia_2024_file_path:

  Path to Somalia 2024 file.

- kenya_file_path:

  Path to Kenya 2018 file.

- jamal_pop_file_path:

  Path to Jamal file.

- world_pop_file_path:

  `str` Path to the world pop file path.

- output_dir:

  `str` Where to output the cleaned district population dataset.

- output_type:

  `str` Output types. Valid values are 'rds', 'csv', and 'parquet'.
  Default is `rds`.

- edav:

  `logical` Whether to use EDAV or not to load the file.

## Value

`tibble` Cleaned district population dataset.

## Examples

``` r
if (FALSE) { # \dontrun{
pop_data <- load_polis_pop("dist")
dist_pop <- process_dist_pop_data(pop_data)
} # }
```
