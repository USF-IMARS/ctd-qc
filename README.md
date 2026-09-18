# ctdqc

Helper package for working with SFER MBON CTD cast data.

Include utilities for:

* discovering and downloading SFER MBON CTD casts from GCOOS ERDDAP
* applying IOOS QARTOD quality-control flags that are included with the data
* cleaning casts with the [`oce`](https://cran.r-project.org/package=oce) package


Extracted from the previously byte-for-byte-duplicated implementations in
[sfer-mbon-oxygen](https://github.com/marinebon/sfer-mbon-oxygen) and
[sfer-mbon-ctd-viz](https://github.com/USF-IMARS/sfer-mbon-ctd-viz).


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


