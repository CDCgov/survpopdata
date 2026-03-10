# Load 2015 World Pop data

World pop data obtained for 2015, to fill in missing data before 2016.

## Usage

``` r
load_world_pop_patch(
  world_pop_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/adm2_2015_pop.csv",
  edav = TRUE
)
```

## Arguments

- world_pop_file_path:

  `str` Path to the world pop file path.

- edav:

  `logical` Whether to use EDAV or not to load the file.

## Value

`tibble` 2015 World Pop data.
