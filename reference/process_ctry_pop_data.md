# Build country population

Cleans the country level population data from the POLIS API and fill in
gaps in population counts using the application of growth rates.

## Usage

``` r
process_ctry_pop_data(
  pop_data,
  pop_dir = "GID/PEB/SIR/Data/pop",
  ctry_file_path = "GID/PEB/SIR/Data/spatial/global.ctry.rds",
  growth_rate_file_path = file.path(pop_dir,
    "pop raw/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx"),
  output_dir = file.path(pop_dir, "processed_pop_file"),
  output_type = "parquet",
  edav = TRUE
)
```

## Arguments

- pop_data:

  `tibble` Country population dataset pulled from the POLIS API.

- pop_dir:

  `str` Default directory to the population folder.

- growth_rate_file_path:

  `str` File path to the growth rate Excel file.

- output_dir:

  `str` Where to output the cleaned district population dataset.

- output_type:

  `str` Output types. Valid values are 'rds', 'csv', and 'parquet'.
  Default is `rds`.

- edav:

  `logical` Whether to use EDAV or not to load the file.

## Value

`tibble` Cleaned country population.

## Examples

``` r
if (FALSE) { # \dontrun{
pop_data <- load_polis_pop("ctry")
ctry_pop <- process_ctry_pop_data(pop_data)
} # }
```
