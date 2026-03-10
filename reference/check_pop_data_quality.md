# Check Population Data Quality

Validation checks on population data from POLIS and returns flagged
rows.

## Usage

``` r
check_pop_data_quality(pop_data, dataset_source = 2)
```

## Arguments

- dataset_source:

  `int` FKDataSet_ID, defaults to 2.

- pop_rds:

  `tibble` Population data from POLIS. Output of
  [load_polis_pop](https://cdcgov.github.io/survpopdata/reference/load_polis_pop.md).

## Value

`tibble` Summary table with one row per GUID that has validation issues.
Returns empty tibble with correct structure if all checks pass.

## Details

Validation checks include:

- Required columns exist

- No missing values in Value, StartDate, AgeGroupCode, PlaceId

- Value is non-negative

- StartDate is not after EndDate

- CreatedDate and UpdatedDate are not in the future

- Each Admin GUID has all three age groups (0-5Y, 0-15Y, ALL)

- Each Admin GUID maps to a single parent admin

- Value is within 2 median absolute deviation per admin and age group

There are several data sources (from the `FK_DataSetId` column).They
include:

- 1: Unknown

- 2: POLIS default

- 17: Unknown

- 19: country population data

- 41: LandScan data

## Examples

``` r
if (FALSE) { # \dontrun{
check_pop_data_quality(raw_dist_pop)
check_pop_data_quality(raw_prov_pop)
check_pop_data_quality(raw_prov_pop)
} # }
```
