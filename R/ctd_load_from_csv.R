#' Build an `oce` CTD object from a raw or QC'd ERDDAP CTD data frame.
#'
#' Either `file` (a path to a raw ERDDAP CSV, read via
#' [read_erddap_tabledap_csv()]) or `ctd_raw` (an already-loaded data frame,
#' e.g. after [apply_qc_filter()]) must be supplied. Every column in the
#' input beyond the core CTD variables is attached to the resulting object
#' via [oce::oceSetData()], so downstream variables (e.g. `dissolved_oxygen`)
#' survive into the `oce` object for `ctdTrim`/`ctdDecimate` filtering.
#'
#' @param file Optional path to a raw ERDDAP CTD CSV.
#' @param cast_id Optional cast ID; derived from `file`'s basename if omitted.
#' @param cruise_id Optional cruise ID override, passed through to
#'   [get_metadata_from_cast_id()].
#' @param ctd_raw Optional pre-loaded data frame to use instead of reading
#'   `file`.
#' @return An `oce` `ctd` object.
#' @export
ctd_load_from_csv <- function(file = NULL, cast_id = NULL, cruise_id = NULL, ctd_raw = NULL) {
  if (is.null(ctd_raw)) {
    if (is.null(file)) {
      stop("Either file or ctd_raw must be provided.")
    }
    if (is.null(cast_id)) {
      cast_id <- sub("\\.csv$", "", basename(file))
    }
    ctd_raw <- read_erddap_tabledap_csv(file)
  } else if (is.null(cast_id) && !is.null(file)) {
    cast_id <- sub("\\.csv$", "", basename(file))
  } else if (is.null(cast_id)) {
    stop("cast_id is required when loading from ctd_raw.")
  }

  metadata <- get_metadata_from_cast_id(cast_id, cruise_id = cruise_id)
  station_id <- metadata$station_id
  cruise_id <- metadata$cruise_id

  lat <- ctd_raw$latitude[[1]]
  lon <- ctd_raw$longitude[[1]]

  # Not all ERDDAP datasets publish a per-scan `time_elapsed` column; fall
  # back to the cast-start `time` column when it's absent. (This is the fix
  # that was previously only applied in sfer-mbon-ctd-viz's copy of this
  # file and never ported back to sfer-mbon-oxygen's - see the package's
  # regression test for this specific case.)
  time_col <- if ("time_elapsed" %in% names(ctd_raw)) "time_elapsed" else "time"

  ctd_temp <- oce::as.ctd(
    salinity = ctd_raw$sea_water_salinity,
    temperature = ctd_raw$sea_water_temperature,
    pressure = ctd_raw$sea_water_pressure,
    station = station_id,
    cruise = cruise_id,
    longitude = lon,
    latitude = lat,
    time = ctd_raw[[time_col]]
  )

  core_columns <- c(
    "sea_water_salinity", "sea_water_temperature", "sea_water_pressure",
    "latitude", "longitude", time_col
  )
  additional_columns <- setdiff(names(ctd_raw), core_columns)

  for (param_name in additional_columns) {
    ctd_temp <- oce::oceSetData(
      object = ctd_temp,
      name = param_name,
      value = ctd_raw[[param_name]],
      originalName = param_name
    )
  }

  ctd_temp
}
