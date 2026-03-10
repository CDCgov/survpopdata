# Summarize conflicting parent admins per GUID

Returns the admin GUIDs that map to multiple parent admin units, with
the conflicting parent GUIDs listed.

- For "Admin2GUID", reports conflicts against Admin1GUID and Admin0GUID.

- For "Admin1GUID", reports conflicts against Admin0GUID.

## Usage

``` r
summarize_conflicting_parents(pop_data, guid_col)
```

## Arguments

- pop_data:

  `tibble` Population data.

- guid_col:

  `str` Name of the GUID column to summarize.

## Value

`tibble` Summary dataset of GUIDs with conflicting parent mappings.
