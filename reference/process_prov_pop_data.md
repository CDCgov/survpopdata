# Build province population

Cleans the province level population data from the POLIS API and fill in
gaps in population counts using district roll-ups where applicable and
application of growth rates.

## Usage

``` r
process_prov_pop_data(
  pop_data,
  pop_dir = "GID/PEB/SIR/Data/pop",
  dist_pop_file_path = file.path(pop_dir, "processed_pop_file", "dist.pop.long.parquet"),
  prov_file_path = "GID/PEB/SIR/Data/spatial/global.prov.rds",
  growth_rate_file_path = file.path(pop_dir,
    "pop raw/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx"),
  output_dir = file.path(pop_dir, "processed_pop_file"),
  output_type = "parquet",
  edav = TRUE
)
```

## Arguments

- pop_data:

  `tibble` Province population dataset pulled from the POLIS API.

- pop_dir:

  `str` Default directory to the population folder.

- dist_pop_file_path:

  `str` File path to the cleaned district pop file.

- prov_file_path:

  `str` File path to the global province shapefile.

- growth_rate_file_path:

  `str` File path to the growth rate Excel file.

- output_dir:

  `str` File path to the output directory.

- output_type:

  `str` How the population file should be outputted.

- edav:

  `logical` Whether files are on EDAV. Defaults to `TRUE`.

## Value

`tibble` Tibble with GUID + province-year rows and final output naming.

## Examples

``` r
if (FALSE) { # \dontrun{
pop_data <- load_polis_pop("prov")
prov_pop <- process_prov_pop_data(pop_data)
} # }
```
