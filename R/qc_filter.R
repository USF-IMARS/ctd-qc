#' IOOS QARTOD aggregate QC flags treated as usable data (1 = good, 2 = not
#' evaluated).
#' @export
QC_GOOD_FLAGS <- c(1L, 2L)

#' Data columns whose QC flag failing means the whole scan/row is dropped
#' (not just that one column blanked to NA), in [apply_qc_filter()].
#' @export
QC_CORE_COLUMNS <- c(
  "sea_water_pressure",
  "sea_water_temperature",
  "sea_water_salinity"
)

#' Find the QC-flag column name for a data column, if one exists.
#'
#' Prefers an aggregate QC column (`{data_col}_qc_agg`) over a plain one
#' (`{data_col}_qc`), matching IOOS QARTOD naming conventions.
#'
#' @param data_col Name of a data column, e.g. `"sea_water_temperature"`.
#' @param column_names Character vector of column names present in the data.
#' @return The matching QC column name, or `NULL` if none exists.
#' @export
qc_column_for <- function(data_col, column_names) {
  agg <- paste0(data_col, "_qc_agg")
  if (agg %in% column_names) {
    return(agg)
  }

  plain <- paste0(data_col, "_qc")
  if (plain %in% column_names) {
    return(plain)
  }

  NULL
}

#' Find every data column in a data frame that has a matching QC column.
#'
#' Column-driven rather than a fixed list of expected variables, so a new CTD
#' variable (e.g. chlorophyll) needs no code change here as long as ERDDAP
#' exposes its QC column with the standard `_qc`/`_qc_agg` naming.
#'
#' @param df A data frame, typically a raw ERDDAP tabledap CSV already read
#'   via [read_erddap_tabledap_csv()].
#' @return Character vector of data column names that have a QC column.
#' @export
data_columns_with_qc <- function(df) {
  qc_cols <- grep("_qc(_agg)?$", names(df), value = TRUE)
  if (length(qc_cols) == 0) {
    return(character())
  }

  data_cols <- unique(sub("_qc(_agg)?$", "", qc_cols, perl = TRUE))
  data_cols[data_cols %in% names(df)]
}

#' Is a QC flag value in the good-flags set?
#' @param qc_values Vector of raw QC flag values (coerced to integer).
#' @param good_flags Flags considered "good" (default [QC_GOOD_FLAGS]).
#' @return Logical vector, `TRUE` where the flag is good.
#' @export
is_qc_good <- function(qc_values, good_flags = QC_GOOD_FLAGS) {
  qc_num <- suppressWarnings(as.integer(qc_values))
  !is.na(qc_num) & qc_num %in% good_flags
}

#' Apply IOOS QARTOD QC filtering to a raw ERDDAP CTD data frame.
#'
#' For every data column with a matching QC column (see
#' [data_columns_with_qc()]), values failing QC are set to `NA`. Rows failing
#' QC (or missing a value) on any of `core_columns` are dropped entirely,
#' since a cast scan with no valid pressure/temperature/salinity isn't usable
#' at all.
#'
#' @param df Raw ERDDAP tabledap data frame (already units-row-stripped via
#'   [read_erddap_tabledap_csv()]).
#' @param good_flags Flags considered "good" (default [QC_GOOD_FLAGS]).
#' @param core_columns Columns whose QC failure drops the whole row (default
#'   [QC_CORE_COLUMNS]).
#' @param drop_qc_columns If `TRUE`, remove all `*_qc`/`*_qc_agg` columns from
#'   the result via [remove_qc_columns()].
#' @return The QC-filtered data frame.
#' @export
apply_qc_filter <- function(
    df,
    good_flags = QC_GOOD_FLAGS,
    core_columns = QC_CORE_COLUMNS,
    drop_qc_columns = FALSE
) {
  if (nrow(df) == 0) {
    return(df)
  }

  out <- df
  data_cols <- data_columns_with_qc(out)

  for (col in data_cols) {
    qc_col <- qc_column_for(col, names(out))
    if (is.null(qc_col)) {
      next
    }
    good <- is_qc_good(out[[qc_col]], good_flags = good_flags)
    out[[col]][!good] <- NA
  }

  core_present <- intersect(core_columns, names(out))
  if (length(core_present) > 0) {
    keep <- rep(TRUE, nrow(out))
    for (col in core_present) {
      qc_col <- qc_column_for(col, names(out))
      if (!is.null(qc_col)) {
        keep <- keep & is_qc_good(out[[qc_col]], good_flags = good_flags)
      }
      keep <- keep & !is.na(out[[col]])
    }
    out <- out[keep, , drop = FALSE]
  }

  if (drop_qc_columns) {
    out <- remove_qc_columns(out)
  }

  out
}

#' Drop every `*_qc`/`*_qc_agg`/etc. QC-flag column from a data frame.
#' @param df A data frame.
#' @return `df` with all columns matching `"_qc"` removed.
#' @export
remove_qc_columns <- function(df) {
  qc_cols <- grep("_qc", names(df), value = TRUE)
  if (length(qc_cols) == 0) {
    return(df)
  }
  df[, setdiff(names(df), qc_cols), drop = FALSE]
}

#' Count rows with a non-`NA` `sea_water_pressure` value.
#'
#' Used as a simple before/after QC row-count metric (pressure is present on
#' essentially every cast, so it's a reasonable proxy for "usable scans").
#'
#' @param df A data frame.
#' @return Integer count.
#' @export
count_qc_pressure_rows <- function(df) {
  if (!"sea_water_pressure" %in% names(df)) {
    return(0L)
  }
  sum(!is.na(df$sea_water_pressure))
}
