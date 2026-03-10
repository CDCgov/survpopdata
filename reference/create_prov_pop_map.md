# Map province population

The function acts as a diagnostic tool be quickly checking which
provinces are missing population data.

## Usage

``` r
create_prov_pop_map(prov_long, prov_pop, who_region, .year, country = NULL)
```

## Arguments

- prov_long:

  `sf` Province shapefile in long (year) format.

- prov_pop:

  `tibble` Cleaned province population.

- who_region:

  `str` Region to map.

- .year:

  `int` Year to map.

- country:

  `str` Country to map (optional).

## Value

`ggplot` A map with U15pop data.
