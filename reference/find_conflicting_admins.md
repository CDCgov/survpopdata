# Find conflicting admin mappings

Identifies child admin units that map to multiple parent admin units.

## Usage

``` r
find_conflicting_admins(
  pop_data,
  child_admin_col,
  parent_admin_col,
  return_details = FALSE
)
```

## Arguments

- pop_data:

  `tibble` Population dataset.

- child_admin_col:

  `str` Column name of the next lower admin column. For example,
  "Admin2GUID" if `pop_data` contains the province population dataset.

- parent_admin_col:

  `str` Parent admin column. For example, "Admin1GUID" if `pop_data`
  contains the province population dataset.

- return_details:

  `logical` If TRUE, returns a tibble with conflicting parent GUIDs per
  child. If FALSE, returns the child GUIDs with multiple parents.

## Value

`tibble` Summary tibble with conflict details or vector of conflicting
GUIDs.
