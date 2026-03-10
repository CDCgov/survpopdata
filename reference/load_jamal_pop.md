# Load Jamal district under-15 population

Load Jamal district under-15 population

## Usage

``` r
load_jamal_pop(
  jamal_pop_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/POPU15.csv",
  edav = TRUE
)
```

## Arguments

- jamal_pop_file_path:

  Path to Jamal file.

- edav:

  `logical` Whether to use EDAV or not to load the file.

## Value

`tibble` Population data with Admin names, year, age columns,
datasource.
