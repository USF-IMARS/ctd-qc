test_that("get_metadata_from_cast_id handles SFER_CTD_ ids", {
  meta <- get_metadata_from_cast_id("SFER_CTD_WS17086_62")
  expect_equal(meta$cruise_id, "WS17086")
  expect_equal(meta$station_id, "62")
})

test_that("get_metadata_from_cast_id cross-checks the station against the raw file", {
  path <- test_path("fixtures", "cast_with_time_elapsed.csv")
  meta <- get_metadata_from_cast_id("SFER_CTD_WS17086_62", raw_file = path)
  expect_equal(meta$station_id, "62")
})

test_that("get_metadata_from_cast_id handles the generic {cruise}_..._STN{station} convention", {
  meta <- get_metadata_from_cast_id("WS21093_WS21093_STN_10")
  expect_equal(meta$cruise_id, "WS21093")
  expect_equal(meta$station_id, "10")
})
