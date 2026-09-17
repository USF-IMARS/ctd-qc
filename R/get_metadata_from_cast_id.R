#' Derive cruise/station metadata from a CTD cast ID.
#'
#' Handles both the `SFER_CTD_{cruise}_{station}` ERDDAP naming convention
#' (via [parse_sfer_ctd_id()], with the station ID cross-checked against the
#' cast's own `station` column when `raw_file` is supplied — ERDDAP's parsed
#' station name isn't always authoritative) and a more general
#' `{cruise}_{...}_{STN|STA}{station}` convention used by non-SFER cast IDs.
#'
#' @param cast_id The cast/dataset ID to parse.
#' @param cruise_id Optional cruise ID override; if supplied, it's used
#'   as-is instead of the one parsed from `cast_id`.
#' @param raw_file Optional path to the cast's raw ERDDAP CSV, used to
#'   cross-check the parsed station ID against the file's own `station`
#'   column (SFER path only).
#' @return A list with `cast_id`, `cruise_id`, `station_id`.
#' @export
get_metadata_from_cast_id <- function(cast_id, cruise_id = NULL, raw_file = NULL) {
  if (grepl("^SFER_CTD_", cast_id)) {
    parsed <- parse_sfer_ctd_id(cast_id)
    if (is.null(parsed)) {
      stop("Could not parse SFER CTD cast id: ", cast_id)
    }

    station_id <- parsed$station_id
    if (!is.null(raw_file) && file.exists(raw_file)) {
      station_from_file <- read_station_from_erddap_csv(raw_file)
      if (!is.na(station_from_file) && nzchar(station_from_file)) {
        station_id <- station_from_file
      }
    }

    return(list(
      cast_id = cast_id,
      cruise_id = if (is.null(cruise_id)) parsed$cruise_id else cruise_id,
      station_id = station_id
    ))
  }

  cruise_id <- sub("_.*", "", cast_id)
  end_of_cast_id <- sub("^[^_]+_[^_]+_", "", cast_id)
  station_id_as_entered <- sub("^[A-Za-z]{1,3}[0-9]{4,5}_?", "", end_of_cast_id)
  station_id <- sub("(?i)(stn|sta)_?", "", station_id_as_entered, perl = TRUE)

  list(
    cast_id = cast_id,
    cruise_id = cruise_id,
    station_id = station_id
  )
}
