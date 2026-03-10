# Load POLIS Population Data

Load POLIS Population Data

## Usage

``` r
load_polis_pop(
  spatial_scale,
  edav = TRUE,
  file_loc = "GID/PEB/SIR/POLIS/data/pop.parquet",
  azcontainer = sirfunctions::get_azure_storage_connection()
)
```

## Arguments

- spatial_scale:

  Geographic level: "ctry", "prov", or "dist"

- edav:

  Load data from EDAV

- file_loc:

  Load from local file (path)

- azcontainer:

  Azure storage connection

## Value

Tibble filtered to spatial_scale with datasource column

## Examples

``` r
if (FALSE) { # \dontrun{
load_polis_pop(spatial_scale = "dist")
load_polis_pop(spatial_scale = "prov", file_loc = "path/to/file.rds")
} # }
```
