# Applies the growth rate to each population columns

Applies the growth rate to each population columns

## Usage

``` r
apply_growth_rate(base_data, pop_column, grouping_col = "ADM2_SHAPE_ID")
```

## Arguments

- base_data:

  `tibble` Combined population data with the growth rate columns.

- pop_column:

  `str` Name of the population column to apply the growth rate to.

- grouping_col:

  `str` Column to use for grouping.

## Value

`tibble` Population data with population filled based on growth rates
