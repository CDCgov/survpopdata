# Remove forward-filled values (non-POLIS only)

Remove forward-filled values (non-POLIS only)

## Usage

``` r
remove_forward_fill_non_polis(non_polis_pop)
```

## Arguments

- non_polis_pop:

  Tibble with ADM2_GUID, year, age columns, datasource.

## Value

Tibble with forward-filled values set to NA.
