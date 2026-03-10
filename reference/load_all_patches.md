# Combine all district patch sources

Combine all district patch sources

## Usage

``` r
load_all_patches(
  pakistan_file_path =
    "GID/PEB/SIR/Data/pop/pop raw/csv files/2022_2023 Population Pakistan.csv",
  somalia_2022_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/AFPPOP_22.csv",
  somalia_2023_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/AFPPOP_23.csv",
  somalia_2024_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/AFPPOP_24.csv",
  kenya_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/Kenya_SubCounty_pop_2018.csv",
  jamal_pop_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/POPU15.csv",
  world_pop_file_path = "GID/PEB/SIR/Data/pop/pop raw/csv files/adm2_2015_pop.csv",
  edav = TRUE
)
```

## Arguments

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

- edav:

  `logical` Whether to use EDAV or not to load the file.

## Value

`tibble` Dataset with all the patch files.
