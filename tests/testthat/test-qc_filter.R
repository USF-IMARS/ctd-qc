test_that("apply_qc_filter blanks failing values and keeps passing rows", {
  path <- test_path("fixtures", "cast_with_time_elapsed.csv")
  ctd_raw <- read_erddap_tabledap_csv(path)

  filtered <- apply_qc_filter(ctd_raw)
  expect_lte(nrow(filtered), nrow(ctd_raw))
  expect_true(all(c("sea_water_pressure", "sea_water_temperature", "sea_water_salinity") %in% names(filtered)))
  expect_false(any(is.na(filtered$sea_water_pressure)))
})

test_that("is_qc_good treats only flags 1 and 2 as good by default", {
  expect_equal(is_qc_good(c(1L, 2L, 3L, 4L, 9L, NA)), c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
})

test_that("data_columns_with_qc is column-driven, not a fixed list", {
  df <- data.frame(foo = 1:3, foo_qc = c(1, 1, 4), bar = 1:3)
  expect_equal(data_columns_with_qc(df), "foo")
})

test_that("remove_qc_columns drops every *_qc* column", {
  df <- data.frame(foo = 1, foo_qc = 1, foo_qc_agg = 1, bar = 1)
  expect_equal(names(remove_qc_columns(df)), c("foo", "bar"))
})
