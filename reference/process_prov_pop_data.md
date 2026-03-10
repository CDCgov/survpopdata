# Build province population (Admin1) in wide format

Combines POLIS + patches + Jamal; joins to province-year shapes;
deduplicates; fills datasource across gaps within GUID; fills missing
values using growth rates. Aggregates remaining province-level gaps by
summing all district totals per province.

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
prov_pop_data(pop_data, output_file = "Data/pop/prov_pop_admin1.rds")
} # }
```
