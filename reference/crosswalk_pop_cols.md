# Crosswalks column names of the POLIS pop dataset

Crosswalks the original column names of the POLIS pop dataset so that it
is compatible with the names used by the data cleaning scripts developed
within CDC. The function also filters for the only columns used in the
population datasets and adds new columns, such as the adm0-2guids. In
addition, the tables get transformed by pivoting wider using the
AgeGroupCode.

## Usage

``` r
crosswalk_pop_cols(pop_data)
```

## Arguments

- pop_data:

  `tibble` The population dataset from the POLIS API. It can be the
  entire table or a subset of it.

## Value

`tibble` A tibble with the renamed columns.

## Examples

``` r
if (FALSE) { # \dontrun{
dist_pop <- load_polis_pop("dist")
dist_pop <- crosswalk_pop_cols(dist_pop)

} # }
```
