#' Has a cast already been cleaned, and is the clean output newer than the
#' raw input?
#' @param raw_file Path to the raw cast CSV.
#' @param output_file Path the cleaned cast CSV would be written to.
#' @return `TRUE` if `output_file` exists and is at least as new as `raw_file`.
#' @export
is_cast_already_processed <- function(raw_file, output_file) {
  file.exists(output_file) && file.mtime(output_file) >= file.mtime(raw_file)
}

#' Clean one raw CTD cast: QC filter, trim, decimate, and write the result.
#'
#' Pipeline: [apply_qc_filter()] on the raw ERDDAP data -> [ctd_load_from_csv()]
#' to build an `oce` object -> `oce::ctdTrim()` (upcast/downcast trimming) ->
#' `oce::ctdDecimate()` (pressure-bin decimation) -> map the retained scan
#' indices back onto the QC'd data frame and write it to `output_file`.
#'
#' @param raw_file Path to the raw ERDDAP cast CSV.
#' @param output_file Path to write the cleaned CSV to. Defaults to
#'   `here::here("data", "02_clean", "{cast_id}.csv")`, matching this
#'   package's original callers' directory convention - pass an explicit
#'   path for a different layout.
#' @param cruise_id Optional cruise ID override; derived from `raw_file`'s
#'   parent directory name if omitted.
#' @param decimate_p Pressure bin size (dbar) passed to `oce::ctdDecimate()`.
#' @param overwrite If `FALSE` (default), skip casts already cleaned and up
#'   to date per [is_cast_already_processed()].
#' @return A list describing the outcome: `status` (`"skipped"`, `"empty"`,
#'   or `"cleaned"`), `cast_id`, `cruise_id`, `output_file`, and row counts.
#' @export
clean_ctd_cast <- function(
    raw_file,
    output_file = NULL,
    cruise_id = NULL,
    decimate_p = 0.1,
    overwrite = FALSE
) {
  cast_id <- sub("\\.csv$", "", basename(raw_file))

  if (is.null(cruise_id)) {
    cruise_id <- basename(dirname(raw_file))
  }

  if (is.null(output_file)) {
    output_file <- here::here("data", "02_clean", paste0(cast_id, ".csv"))
  }

  if (!overwrite && is_cast_already_processed(raw_file, output_file)) {
    return(list(
      status = "skipped",
      cast_id = cast_id,
      cruise_id = cruise_id,
      output_file = output_file
    ))
  }

  metadata <- get_metadata_from_cast_id(cast_id, cruise_id = cruise_id, raw_file = raw_file)
  ctd_raw <- read_erddap_tabledap_csv(raw_file)
  raw_rows <- count_qc_pressure_rows(ctd_raw)
  ctd_qc <- apply_qc_filter(ctd_raw)
  qc_rows <- count_qc_pressure_rows(ctd_qc)

  cast <- ctd_load_from_csv(
    raw_file,
    cast_id = cast_id,
    cruise_id = cruise_id,
    ctd_raw = ctd_qc
  )

  trimmed_cast <- oce::ctdTrim(cast)
  decimated_cast <- oce::ctdDecimate(trimmed_cast, p = decimate_p)
  cleaned_scans <- decimated_cast@data$scan

  cast_df <- ctd_qc %>%
    dplyr::filter(!is.na(sea_water_pressure)) %>%
    dplyr::mutate(row_num = dplyr::row_number()) %>%
    dplyr::filter(row_num %in% cleaned_scans) %>%
    dplyr::select(-row_num) %>%
    dplyr::mutate(
      station = metadata$station_id,
      cruise_id = metadata$cruise_id
    ) %>%
    remove_qc_columns()

  if (nrow(cast_df) == 0) {
    return(list(
      status = "empty",
      cast_id = cast_id,
      cruise_id = cruise_id,
      output_file = output_file,
      raw_rows = raw_rows,
      qc_rows = qc_rows,
      clean_rows = 0L
    ))
  }

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  write.csv(cast_df, output_file, row.names = FALSE)

  list(
    status = "cleaned",
    cast_id = cast_id,
    cruise_id = cruise_id,
    rows = nrow(cast_df),
    output_file = output_file,
    raw_rows = raw_rows,
    qc_rows = qc_rows,
    clean_rows = nrow(cast_df)
  )
}
