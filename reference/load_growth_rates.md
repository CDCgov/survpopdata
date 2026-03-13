# Load population growth rates

Load population growth rates

## Usage

``` r
load_growth_rates(
  .file_loc =
    "GID/PEB/SIR/Data/pop/pop raw/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx",
  edav = TRUE
)
```

## Arguments

- .file_loc:

  `str` Path to WPP Excel file.

## Value

`tibble` Growth rates for each country by year.

## Details

Loads and formats the population growth rate file for use in R.

The Excel file uses the "Estimates" tab. The first few rows (16 rows or
so) are deleted for ease of loading into R. The source of this dataset
is in the [World Population
Prospects](https://population.un.org/wpp/downloads?folder=Standard%20Projections&group=Most%20used)
website, using the "Compact" file.
