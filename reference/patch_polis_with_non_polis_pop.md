# Patch missing data in POLIS pop with non-polis data

Patch missing data in POLIS pop with non-polis data

## Usage

``` r
patch_polis_with_non_polis_pop(polis_pop, non_polis_pop, patch_file)
```

## Arguments

- polis_pop:

  `tibble` Population data from POLIS API.

- non_polis_pop:

  `tibble` Population data fron non-POLIS API sources.

- patch_file:

  `str` Name of the patch file datasource. Valid values include: "JAMAL
  POP", "PATCH_PAKISTAN", "KENYA 2018 PATCH", "PATCH_SOMALIA".

## Value

`tibble` Population data patched.
