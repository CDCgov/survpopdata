# Create age group predicate per GUID for assertr validation

Predicate that checks whether each GUID has required age group data.

## Usage

``` r
create_agegroup_predicate(guids_with_missing_agegroups)
```

## Arguments

- guids_with_missing_agegroups:

  `str` GUIDs with missing age groups.

## Value

`fun` A predicate function for use with assertr.
