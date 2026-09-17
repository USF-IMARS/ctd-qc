#' GCOOS ERDDAP base URL used for SFER MBON CTD datasets.
#' @export
ERDDAP_BASE <- "https://gcoos5.geos.tamu.edu/erddap/"

#' Dataset ID prefix for SFER MBON CTD casts on GCOOS ERDDAP.
#' @export
SFER_CTD_PREFIX <- "SFER_CTD_"

#' Prefix searches used to discover cruises without hitting ERDDAP's ~1000-row
#' search-result cap.
#' @export
DISCOVERY_SEARCH_PREFIXES <- c(
  "SFER_CTD_SV",
  "SFER_CTD_WB",
  paste0("SFER_CTD_WS0", 6:9),
  paste0("SFER_CTD_WS", 10:24)
)

#' Parse a `SFER_CTD_{cruise_id}_{station_id}` ERDDAP dataset ID.
#'
#' This is the single place that decodes the SFER CTD dataset-ID naming
#' convention; other functions in this package call it rather than
#' re-deriving cruise/station IDs themselves.
#'
#' @param erddap_id A single ERDDAP dataset ID string.
#' @return A list with `erddap_id`, `cast_id`, `cruise_id`, `station_id`, or
#'   `NULL` if `erddap_id` doesn't match the expected naming convention.
#' @export
parse_sfer_ctd_id <- function(erddap_id) {
  if (length(erddap_id) != 1L || is.na(erddap_id) || !nzchar(erddap_id)) {
    return(NULL)
  }
  if (!grepl(paste0("^", SFER_CTD_PREFIX), erddap_id)) {
    return(NULL)
  }

  rest <- sub(paste0("^", SFER_CTD_PREFIX), "", erddap_id)
  cruise_match <- regexpr("^[A-Za-z]{1,3}[0-9]{4,5}", rest, perl = TRUE)
  if (cruise_match[1] == -1) {
    return(NULL)
  }

  cruise_id <- regmatches(rest, cruise_match)
  station_id <- sub(paste0("^", cruise_id, "_?"), "", rest, perl = TRUE)
  if (!nzchar(station_id)) {
    return(NULL)
  }

  list(
    erddap_id = erddap_id,
    cast_id = erddap_id,
    cruise_id = cruise_id,
    station_id = station_id
  )
}

#' Search the ERDDAP catalog for dataset IDs matching a search term.
#'
#' @param search_term Free-text search term passed to ERDDAP's catalog search.
#' @param base_url ERDDAP server base URL.
#' @param dataset_prefix Only IDs starting with this prefix are returned.
#' @return Character vector of matching dataset IDs (possibly empty).
#' @export
fetch_erddap_search_ids <- function(
    search_term,
    base_url = ERDDAP_BASE,
    dataset_prefix = SFER_CTD_PREFIX
) {
  search_url <- paste0(
    base_url,
    "search/index.csv?searchFor=",
    utils::URLencode(search_term, reserved = TRUE)
  )

  result <- tryCatch(
    utils::read.csv(search_url, stringsAsFactors = FALSE),
    error = function(e) {
      warning(
        "ERDDAP catalog search failed for ", search_term, ": ",
        conditionMessage(e)
      )
      return(NULL)
    }
  )

  if (is.null(result) || nrow(result) == 0) {
    return(character())
  }

  id_col <- if ("Dataset.ID" %in% names(result)) {
    "Dataset.ID"
  } else {
    names(result)[ncol(result)]
  }

  ids <- unique(result[[id_col]])
  ids <- ids[ids != "Dataset ID" & nzchar(ids)]
  ids[grepl(paste0("^", dataset_prefix), ids)]
}

#' Fetch all ERDDAP dataset IDs (casts) belonging to one cruise.
#' @param cruise_id Cruise ID, e.g. `"SV18067"`.
#' @param base_url ERDDAP server base URL.
#' @return Character vector of matching dataset IDs.
#' @export
fetch_cruise_erddap_ids <- function(cruise_id, base_url = ERDDAP_BASE) {
  ids <- fetch_erddap_search_ids(
    paste0(SFER_CTD_PREFIX, cruise_id),
    base_url = base_url
  )
  ids[grepl(paste0("^", SFER_CTD_PREFIX, cruise_id, "_"), ids)]
}

#' Discover every SFER CTD dataset ID currently published on ERDDAP.
#'
#' Splits discovery across [DISCOVERY_SEARCH_PREFIXES] to dodge ERDDAP's
#' ~1000-row search-result cap, which a single unprefixed search would hit.
#'
#' @param base_url ERDDAP server base URL.
#' @return Sorted, deduplicated character vector of dataset IDs.
#' @export
discover_sfer_erddap_ids <- function(base_url = ERDDAP_BASE) {
  all_ids <- unique(unlist(lapply(
    DISCOVERY_SEARCH_PREFIXES,
    fetch_erddap_search_ids,
    base_url = base_url
  )))
  sort(unique(all_ids[grepl(paste0("^", SFER_CTD_PREFIX), all_ids)]))
}

#' Discover every SFER cruise ID with CTD casts currently published on ERDDAP.
#' @param base_url ERDDAP server base URL.
#' @return Sorted, deduplicated character vector of cruise IDs.
#' @export
discover_sfer_cruise_ids <- function(base_url = ERDDAP_BASE) {
  ids <- discover_sfer_erddap_ids(base_url = base_url)
  parsed <- lapply(ids, parse_sfer_ctd_id)
  parsed <- Filter(Negate(is.null), parsed)
  if (length(parsed) == 0) {
    return(character())
  }
  sort(unique(vapply(parsed, function(x) x$cruise_id, character(1L))))
}

#' Read an ERDDAP tabledap CSV, stripping the units row ERDDAP prepends.
#'
#' ERDDAP's `.csv` tabledap responses put a units row (e.g. `"UTC"` under a
#' `time` column) directly under the header, before the data rows. Cached raw
#' files inherit this. This is the single place that strips it.
#'
#' @param file_path Path to a `.csv` file (ERDDAP response or a cached copy).
#' @return A data frame with the units row removed, if present.
#' @export
read_erddap_tabledap_csv <- function(file_path) {
  df <- utils::read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
  if (
    nrow(df) > 0 &&
      "time" %in% names(df) &&
      identical(as.character(df$time[[1]]), "UTC")
  ) {
    df <- df[-1, , drop = FALSE]
  }
  df
}

#' Read the `station` value from a cached ERDDAP CTD CSV, if present.
#' @param file_path Path to a cached ERDDAP CSV.
#' @return The station ID as a string, or `NA_character_` if unavailable.
#' @export
read_station_from_erddap_csv <- function(file_path) {
  if (!file.exists(file_path)) {
    return(NA_character_)
  }

  df <- tryCatch(
    read_erddap_tabledap_csv(file_path),
    error = function(e) NULL
  )
  if (is.null(df) || !"station" %in% names(df) || nrow(df) == 0) {
    return(NA_character_)
  }

  station <- as.character(df$station[[1]])
  if (!nzchar(station) || is.na(station)) {
    return(NA_character_)
  }
  station
}

#' Download one ERDDAP dataset's tabledap CSV and cache it locally.
#' @param erddap_id ERDDAP dataset ID to download.
#' @param file_path Local path to write the (units-row-stripped) CSV to.
#' @param base_url ERDDAP server base URL.
#' @return `file_path`, invisibly.
#' @export
download_erddap_dataset_csv <- function(erddap_id, file_path, base_url = ERDDAP_BASE) {
  url <- paste0(
    base_url,
    "tabledap/",
    utils::URLencode(erddap_id, reserved = TRUE),
    ".csv"
  )
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
  df <- read_erddap_tabledap_csv(tmp)
  utils::write.csv(df, file_path, row.names = FALSE)
  invisible(file_path)
}
