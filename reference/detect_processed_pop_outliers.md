# Detects whether there are issues with the population data

Detects whether there are issues with the population data

## Usage

``` r
detect_processed_pop_outliers(cleaned_pop_data, group_col = "adm1guid")
```

## Arguments

- cleaned_pop_data:

  `tibble` Cleaned population data. Either dist, prov, or country.

- group_col:

  `str` Name of the grouping variable. Recommend adm0guid, adm1guid, or
  adm2guid.

## Value

`tibble` Pop data with category of outlier.
