# Compact mock data - ONE TABLE with all levels
create_mock <- function() {
  tibble::tibble(
    Admin0GUID = c("{C-1}", "{C-1}", "{C-1}", "{C-2}", "{C-2}", "{C-2}"),
    Admin1GUID = c(NA, "{P-1}", "{P-1}", NA, "{P-2}", "{P-2}"),
    Admin2GUID = c(NA, NA, "{D-1}", NA, NA, "{D-2}"),
    Admin0Name = c("NIGERIA", "NIGERIA", "NIGERIA", "PAKISTAN", "PAKISTAN", "PAKISTAN"),
    Year = 2023,
    Value = c(50000000, 5000000, 500000, 40000000, 4000000, 400000),
    AgeGroupCode = "ALL",
    WHORegion = "AFRO"
  )
}

mock_data <- create_mock()

# Input validation tests
test_that("Invalid spatial_scale fails", {
  temp <- tempfile(fileext = ".rds")
  saveRDS(mock_data, temp)
  expect_error(load_polis_pop(spatial_scale = "invalid", file_loc = temp))
  expect_error(load_polis_pop(spatial_scale = NULL, file_loc = temp))
  unlink(temp)
})

test_that("Missing local file fails", {
  expect_error(load_polis_pop(spatial_scale = "dist", edav = FALSE, file_loc = "fake.rds"))
})

# Local file tests
test_that("ctry spatial filter works correctly", {
  temp <- tempfile(fileext = ".rds")
  saveRDS(mock_data, temp)

  ctry <- suppressMessages(load_polis_pop(spatial_scale = "ctry", edav = FALSE, file_loc = temp))
  expect_true(all(!is.na(ctry$Admin0GUID),
                  is.na(ctry$Admin1GUID),
                  is.na(ctry$Admin2GUID)))
  expect_equal(nrow(ctry), 2)
  unlink(temp)
})

test_that("prov spatial filter works correctly", {
  temp <- tempfile(fileext = ".rds")
  saveRDS(mock_data, temp)

  prov <- suppressMessages(load_polis_pop(spatial_scale = "prov", edav = FALSE, file_loc = temp))
  expect_true(all(!is.na(prov$Admin0GUID),
                  !is.na(prov$Admin1GUID),
                  is.na(prov$Admin2GUID)))
  expect_equal(nrow(prov), 4)
  unlink(temp)
})

test_that("dist spatial filter works correctly", {
  temp <- tempfile(fileext = ".rds")
  saveRDS(mock_data, temp)

  dist <- suppressMessages(load_polis_pop(spatial_scale = "dist", edav = FALSE, file_loc = temp))
  expect_true(all(!is.na(dist$Admin0GUID),
                  !is.na(dist$Admin1GUID),
                  !is.na(dist$Admin2GUID)))
  expect_equal(nrow(dist), 2)
  unlink(temp)
})

test_that("Data_Source column added", {
  temp <- tempfile(fileext = ".rds")
  saveRDS(mock_data, temp)
  result <- suppressMessages(load_polis_pop(spatial_scale = "dist", edav = FALSE, file_loc = temp))
  expect_true(all(result$Data_Source == "POLIS API"))
  expect_false(any(is.na(result$Data_Source)))
  unlink(temp)
})

test_that("Empty data warns", {
  empty <- mock_data |>
    dplyr::filter(Year == 9999)
  temp <- tempfile(fileext = ".rds")
  saveRDS(empty, temp)
  expect_warning(load_polis_pop(spatial_scale = "dist", edav = FALSE, file_loc = temp), "empty")
  unlink(temp)
})

test_that("Missing required columns fail", {
  temp <- tempfile(fileext = ".rds")

  saveRDS(
    mock_data |>
      dplyr::select(-Admin2GUID),
    temp
  )
  expect_error(load_polis_pop(spatial_scale = "dist", edav = FALSE, file_loc = temp), "Admin2GUID")

  saveRDS(
    mock_data |>
      dplyr::select(-Admin1GUID),
    temp
  )
  expect_error(load_polis_pop(spatial_scale = "prov", edav = FALSE, file_loc = temp), "Admin1GUID")

  saveRDS(
    mock_data |>
      dplyr::select(-Admin0GUID),
    temp
  )
  expect_error(load_polis_pop(spatial_scale = "ctry", edav = FALSE, file_loc = temp), "Admin0GUID")

  unlink(temp)
})

test_that("Local file doesn't call Azure connection", {
  temp <- tempfile(fileext = ".rds")
  saveRDS(mock_data, temp)

  testthat::local_mocked_bindings(
    get_azure_storage_connection = function() stop("Should not be called!"),
    .package = "sirfunctions"
  )

  result <- suppressMessages(load_polis_pop(spatial_scale = "dist", edav = FALSE, file_loc = temp))
  expect_gt(nrow(result), 0)

  unlink(temp)
})

# EDAV/Azure tests
test_that("Invalid azcontainer fails", {
  testthat::local_mocked_bindings(
    get_azure_storage_connection = function() stop("Invalid Azure credentials"),
    .package = "sirfunctions"
  )
  expect_error(load_polis_pop(spatial_scale = "dist"))
})

test_that("EDAV loads successfully", {
  testthat::local_mocked_bindings(
    edav_io = function(io, ...) if (io == "read") mock_data else stop("Invalid"),
    get_azure_storage_connection = function() "mock_connection",
    .package = "sirfunctions"
  )
  result <- suppressMessages(load_polis_pop(spatial_scale = "dist"))
  expect_gt(nrow(result), 0)
  expect_true(all(result$Data_Source == "POLIS API"))
})

test_that("EDAV connection failure", {
  testthat::local_mocked_bindings(
    edav_io = function(...) stop("Connection failed"),
    get_azure_storage_connection = function() "mock_connection",
    .package = "sirfunctions"
  )
  expect_error(load_polis_pop(spatial_scale = "dist"))
})
