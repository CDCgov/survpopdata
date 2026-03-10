# Obtains GUID with missing age group

Obtains the GUIDs missing age groups.

## Usage

``` r
get_guid_missing_age_groups(pop_data, guid_col)
```

## Arguments

- pop_data:

  Tibble containing population data.

- guid_col:

  String name of the GUID column to validate.

## Value

`tibble` A summary table of GUIDs and their missing age groups.
