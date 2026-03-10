# Create a map to check for population outliers

Maps the percentage of change in population over years to check for
outliers in the population data. This is a diagnostic tool.

## Usage

``` r
map_pop_outlier(
  pop_w_outlier_cat,
  sf_long,
  pop_cat = "u15",
  ctry_name = NULL,
  who_region = NULL,
  year_end = lubridate::year(Sys.Date()),
  year_start = year_end - 3
)
```

## Arguments

- pop_w_outlier_cat:

  `tibble` Output of
  [`detect_processed_pop_outliers()`](https://cdcgov.github.io/survpopdata/reference/detect_processed_pop_outliers.md).

- sf_long:

  `sf` Shapefile in long format. Any of the
  sirfunctions::load_clean\_\*\_sp(type = "long"), as long as the
  administrative level is the same as the population file.

- pop_cat:

  `str` Population category to map. Either u15, u5, or totpop.

- ctry_name:

  `str` Optional country name.

- who_region:

  `str` Optional region name.

- year_end:

  `int` Year end. Defaults to current year.

- year_start:

  `int` Year start. Defaults to four years ago from the end year.

## Value

`ggplot2` A map showing population count stability over the year
