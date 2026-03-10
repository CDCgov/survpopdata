# Map district population

The function acts as a diagnostic tool be quickly checking which
districts are missing population data.

## Usage

``` r
create_dist_pop_map(district_long, dist_pop, who_region, .year, country = NULL)
```

## Arguments

- district_long:

  `sf` District shapefile in long format.

- dist_pop:

  `tibble` District population.

- who_region:

  `str` Region to map.

- .year:

  `int` Year to map.

- country:

  `str` Country to map (optional).

## Value

`ggplot` A map with U15pop data.
