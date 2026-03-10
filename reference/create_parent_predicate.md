# Create unique parent predicate per admin GUID for assertr validation

Predicate that checks whether each admin GUID maps to a single parent
admin.

- For "Admin2GUID", checks uniqueness against Admin1GUID and Admin0GUID.

- For "Admin1GUID", checks uniqueness against Admin0GUID.

## Usage

``` r
create_parent_predicate(pop_data, guid_col)
```

## Arguments

- pop_data:

  Tibble containing population data.

- guid_col:

  String name of the GUID column to validate.

## Value

`fun` A predicate function for use with assertr.
