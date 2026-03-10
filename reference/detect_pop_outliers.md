# Detect population outliers

Detects the outliers in population counts for each age group and GUID.

## Usage

``` r
detect_pop_outliers(pop_data, guid_col, age_groups = c("0-5Y", "0-15Y", "ALL"))
```

## Arguments

- pop_data:

  Tibble containing population data.

- guid_col:

  String name of the GUID column to validate.

- age_groups:

  `str` Age groups to analyze. By default, analyzes for the under 5,
  under 15, and total population.

## Value

`list` A list containing outliers per age group analyzed.
