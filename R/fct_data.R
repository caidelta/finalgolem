#' Load all application data
#'
#' Loads RTL continuous futures and FRED CMT yields. Checks for a pre-built
#' cache at inst/data-cache/app_data.rds first; falls back to live fetch.
#'
#' @return A named list: futures (long), futures_wide, cmt (long), series_meta
#' @export
fct_load_all_data <- function() {
  cache_path <- app_sys("data-cache", "app_data.rds")

  if (nchar(cache_path) > 0 && file.exists(cache_path)) {
    message("[fin451app] Loading data from cache: ", cache_path)
    return(readRDS(cache_path))
  }

  message("[fin451app] Cache not found — fetching live data...")
  fct_fetch_live_data()
}

#' Fetch live data from RTL and FRED
#'
#' @return Same structure as fct_load_all_data()
#' @noRd
fct_fetch_live_data <- function() {
  # ── RTL futures ────────────────────────────────────────────────────────────
  message("  -> Loading RTL::dflong ...")
  futures_raw <- RTL::dflong

  # Inspect available series — keep only commodity futures (exclude indices/fx)
  all_series  <- unique(futures_raw$series)

  # Identify our 5 markets by prefix matching
  target_prefixes <- c("CL", "BRN", "RB", "HO", "NG")

  futures_raw <- futures_raw |>
    dplyr::mutate(
      date   = as.Date(date),
      market = dplyr::case_when(
        grepl("^CL",  series) ~ "CL",
        grepl("^BRN", series) ~ "BRN",
        grepl("^RB",  series) ~ "RB",
        grepl("^HO",  series) ~ "HO",
        grepl("^NG",  series) ~ "NG",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(market))

  # Extract contract number (tenor position) from series name
  # RTL series are typically "CL01", "CL02", ... or "CL_001", "CL_002"
  futures_raw <- futures_raw |>
    dplyr::mutate(
      tenor = as.integer(gsub("[^0-9]", "", gsub(paste(target_prefixes, collapse = "|"), "", series)))
    ) |>
    dplyr::filter(!is.na(tenor), tenor >= 1, tenor <= 12)

  # Long format: date, market, tenor, series, value
  futures <- futures_raw |>
    dplyr::select(date, market, tenor, series, value) |>
    dplyr::arrange(market, date, tenor)

  # Front-month prices wide (one column per market)
  futures_front <- futures |>
    dplyr::filter(tenor == 1) |>
    dplyr::select(date, market, value) |>
    tidyr::pivot_wider(names_from = market, values_from = value) |>
    dplyr::arrange(date)

  # Returns wide (for co-dynamics, hedge ratios)
  # Replace non-positive prices with NA before log to avoid NaN
  futures_returns <- futures_front |>
    dplyr::mutate(
      dplyr::across(-date, ~ {
        x <- dplyr::if_else(.x <= 0, NA_real_, .x)
        c(NA_real_, diff(log(x)))
      })
    )

  # ── FRED CMT yields ────────────────────────────────────────────────────────
  fred_series <- c(
    "DGS1MO", "DGS3MO", "DGS6MO",
    "DGS1",   "DGS2",   "DGS3",
    "DGS5",   "DGS7",   "DGS10",
    "DGS20",  "DGS30"
  )
  tenor_labels <- c(
    "1M", "3M", "6M", "1Y", "2Y", "3Y", "5Y", "7Y", "10Y", "20Y", "30Y"
  )
  tenor_years <- c(1/12, 3/12, 6/12, 1, 2, 3, 5, 7, 10, 20, 30)

  cmt <- tryCatch({
    api_key <- Sys.getenv("FRED_API_KEY")
    if (nchar(api_key) == 0) stop("FRED_API_KEY not set")
    fredr::fredr_set_key(api_key)
    message("  -> Fetching FRED CMT data ...")
    purrr::map2_dfr(fred_series, tenor_labels, function(s, lbl) {
      fredr::fredr(series_id = s, observation_start = as.Date("2007-01-01")) |>
        dplyr::select(date, value) |>
        dplyr::mutate(series = s, tenor_label = lbl)
    })
  }, error = function(e) {
    message("  -> FRED fetch failed: ", conditionMessage(e), " — using empty CMT data.")
    data.frame(date = as.Date(character()), value = numeric(),
               series = character(), tenor_label = character())
  })

  # CMT wide (yields matrix)
  cmt_wide <- if (nrow(cmt) > 0) {
    cmt |>
      dplyr::select(date, tenor_label, value) |>
      tidyr::pivot_wider(names_from = tenor_label, values_from = value) |>
      dplyr::arrange(date)
  } else {
    data.frame(date = as.Date(character()))
  }

  list(
    futures         = futures,          # long: date, market, tenor, series, value
    futures_front   = futures_front,    # wide front-month prices
    futures_returns = futures_returns,  # wide front-month log returns
    cmt             = cmt,              # long: date, value, series, tenor_label
    cmt_wide        = cmt_wide,         # wide: date + one col per tenor
    tenor_labels    = tenor_labels,
    tenor_years     = tenor_years,
    markets         = c("CL", "BRN", "RB", "HO", "NG"),
    market_names    = c(
      CL  = "WTI Crude Oil",
      BRN = "Brent Crude",
      RB  = "RBOB Gasoline",
      HO  = "Heating Oil",
      NG  = "Natural Gas"
    )
  )
}

#' Cache application data to inst/data-cache/app_data.rds
#'
#' Run this once locally (with FRED_API_KEY set) to pre-populate the cache
#' so the Docker image can serve data without hitting APIs at runtime.
#'
#' @export
cache_app_data <- function() {
  dir_path <- system.file("data-cache", package = "fin451app")
  if (nchar(dir_path) == 0) {
    # Running from source (not installed) — use inst/ directly
    dir_path <- file.path("inst", "data-cache")
  }
  dir.create(dir_path, showWarnings = FALSE, recursive = TRUE)

  message("Fetching data for cache...")
  data <- fct_fetch_live_data()

  out_path <- file.path(dir_path, "app_data.rds")
  saveRDS(data, out_path)
  message("Saved cache to: ", out_path)
  invisible(out_path)
}

#' Filter futures data to a specific market and date range
#'
#' @param data The full data list from fct_load_all_data()
#' @param market Character, e.g. "CL"
#' @param dates Date vector of length 2 (start, end)
#' @param tenors Integer vector of contract months to keep (default 1:6)
#' @noRd
fct_filter_futures <- function(data, market, dates, tenors = 1:6) {
  data$futures |>
    dplyr::filter(
      market == !!market,
      tenor  %in% tenors,
      date   >= dates[1],
      date   <= dates[2]
    )
}

#' Compute log returns for a numeric vector
#' @noRd
fct_log_returns <- function(x) c(NA_real_, diff(log(x)))
