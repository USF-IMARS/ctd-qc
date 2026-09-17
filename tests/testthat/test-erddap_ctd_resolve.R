test_that("parse_sfer_ctd_id decodes the SFER_CTD_{cruise}_{station} convention", {
  parsed <- parse_sfer_ctd_id("SFER_CTD_WS17086_62")
  expect_equal(parsed$cruise_id, "WS17086")
  expect_equal(parsed$station_id, "62")
  expect_equal(parsed$erddap_id, "SFER_CTD_WS17086_62")
})

test_that("parse_sfer_ctd_id returns NULL for non-matching IDs", {
  expect_null(parse_sfer_ctd_id("not_a_valid_id"))
  expect_null(parse_sfer_ctd_id(NA_character_))
  expect_null(parse_sfer_ctd_id(""))
})

test_that("read_erddap_tabledap_csv strips a units row when present", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c("time,value", "UTC,unitless", "2020-01-01T00:00:00Z,1.23"), tmp)
  df <- read_erddap_tabledap_csv(tmp)
  expect_equal(nrow(df), 1)
  expect_equal(df$value, "1.23")
})

test_that("read_erddap_tabledap_csv leaves data alone when there's no units row", {
  path <- test_path("fixtures", "cast_with_time_elapsed.csv")
  df <- read_erddap_tabledap_csv(path)
  expect_equal(nrow(df), 3L)
})
