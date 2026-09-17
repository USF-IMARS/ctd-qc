test_that("a cast with a time_elapsed column loads correctly", {
  path <- test_path("fixtures", "cast_with_time_elapsed.csv")
  ctd_raw <- read_erddap_tabledap_csv(path)
  expect_true("time_elapsed" %in% names(ctd_raw))

  cast <- ctd_load_from_csv(path, ctd_raw = ctd_raw)
  expect_s4_class(cast, "ctd")
  expect_equal(length(cast[["temperature"]]), nrow(ctd_raw))
})

test_that("a cast WITHOUT a time_elapsed column still loads (regression test)", {
  # sfer-mbon-oxygen's original R/ctd_load_from_csv.R unconditionally used
  # `time_elapsed`, which errors/produces no time data on ERDDAP datasets
  # that only publish `time`. sfer-mbon-ctd-viz fixed this with a fallback
  # that was never ported back to oxygen. This package ships the fixed
  # version as canonical - this test pins that fix in place.
  path <- test_path("fixtures", "cast_without_time_elapsed.csv")
  ctd_raw <- read_erddap_tabledap_csv(path)
  expect_false("time_elapsed" %in% names(ctd_raw))
  expect_true("time" %in% names(ctd_raw))

  cast <- expect_no_error(ctd_load_from_csv(path, ctd_raw = ctd_raw))
  expect_s4_class(cast, "ctd")
  expect_equal(length(cast[["temperature"]]), nrow(ctd_raw))
  expect_false(any(is.na(cast[["time"]])))
})

test_that("additional (non-core) columns survive onto the oce object", {
  path <- test_path("fixtures", "cast_with_time_elapsed.csv")
  ctd_raw <- read_erddap_tabledap_csv(path)
  cast <- ctd_load_from_csv(path, ctd_raw = ctd_raw)
  expect_equal(cast[["dissolved_oxygen"]], ctd_raw$dissolved_oxygen)
})
