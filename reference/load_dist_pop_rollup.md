# Calculate province level population using district pop rollups

Calculate province level population using district pop rollups

## Usage

``` r
load_dist_pop_rollup(
  dist_pop_file_path = file.path("GID/PEB/SIR/Data/pop", "processed_pop_file",
    "dist.pop.long.parquet"),
  edav = TRUE
)
```

## Arguments

- dist_pop_file_path:

  `str` Path to the district pop file.

- edav:

  `logical` If the file is in EDAV. Defaults to `TRUE`.

## Value

`tibble` District population rollup.
