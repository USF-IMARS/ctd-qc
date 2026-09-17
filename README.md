# ctdqc

Shared utilities for discovering and downloading SFER MBON CTD casts from
GCOOS ERDDAP, applying IOOS QARTOD quality-control flags, and cleaning casts
with the [`oce`](https://cran.r-project.org/package=oce) package
(`ctdTrim`/`ctdDecimate`).

Extracted from the previously byte-for-byte-duplicated implementations in
[sfer-mbon-oxygen](https://github.com/marinebon/sfer-mbon-oxygen) and
[sfer-mbon-ctd-viz](https://github.com/USF-IMARS/sfer-mbon-ctd-viz), whose
copies had already drifted (`ctd_load_from_csv()`'s `time_elapsed`/`time`
column fallback existed only in ctd-viz's copy) — this package exists so
that fix, and future ones, only need to happen once.

## Install

```r
remotes::install_github("USF-IMARS/ctd-qc")
```

## What's here

- `parse_sfer_ctd_id()`, `discover_sfer_cruise_ids()`, `fetch_cruise_erddap_ids()`,
  `read_erddap_tabledap_csv()`, `download_erddap_dataset_csv()` — ERDDAP
  dataset discovery/download for the `SFER_CTD_{cruise}_{station}` naming
  convention.
- `apply_qc_filter()` — column-driven IOOS QARTOD QC filtering (works for any
  variable ERDDAP exposes a `*_qc`/`*_qc_agg` column for, not a fixed list).
- `clean_ctd_cast()`, `ctd_load_from_csv()` — build an `oce` CTD object and
  run it through `ctdTrim`/`ctdDecimate`.
- `get_metadata_from_cast_id()` — cruise/station ID parsing.

## Not included

Anything downstream of cleaned CTD data (interpolation, plotting, report
generation) stays in each consuming repo — this package is QC/ingestion
only.
