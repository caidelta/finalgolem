#' Load all application data
#'
#' Reads pre-cached Feather files from inst/app/data/.
#' Run cache_app_data() once (with FRED_API_KEY set) to generate the files.
#' The app never calls any external API at runtime.
#'
#' @return A named list: futures, futures_front, futures_returns, cmt, cmt_wide, etc.
#' @export
fct_load_all_data <- function() {
  futures_path <- app_sys("app/data/futures.feather")
  cmt_path     <- app_sys("app/data/cmt.feather")

  if (nchar(futures_path) == 0 || !file.exists(futures_path)) {
    # Fallback: try source-tree paths (devtools::load_all() context)
    futures_path <- file.path("inst", "app", "data", "futures.feather")
    cmt_path     <- file.path("inst", "app", "data", "cmt.feather")
  }

  if (!file.exists(futures_path)) {
    stop(
      "Feather cache not found. Run fin451app::cache_app_data() with FRED_API_KEY set ",
      "to generate inst/app/data/futures.feather and cmt.feather."
    )
  }

  message("[fin451app] Loading futures from feather...")
  futures <- arrow::read_feather(futures_path)

  message("[fin451app] Loading CMT from feather...")
  cmt <- if (file.exists(cmt_path)) {
    arrow::read_feather(cmt_path)
  } else {
    message("  -> cmt.feather not found — CMT tab will show placeholder.")
    data.frame(date = as.Date(character()), value = numeric(),
               series = character(), tenor_label = character())
  }

  # Derive wide tables in memory (fast — already filtered/clean)
  futures_front <- futures |>
    dplyr::filter(tenor == 1) |>
    dplyr::select(date, market, value) |>
    tidyr::pivot_wider(names_from = market, values_from = value) |>
    dplyr::arrange(date)

  futures_returns <- futures_front |>
    dplyr::mutate(
      dplyr::across(-date, ~ {
        x <- dplyr::if_else(.x <= 0, NA_real_, .x)
        c(NA_real_, diff(log(x)))
      })
    )

  cmt_wide <- if (nrow(cmt) > 0) {
    cmt |>
      dplyr::select(date, tenor_label, value) |>
      tidyr::pivot_wider(names_from = tenor_label, values_from = value) |>
      dplyr::arrange(date)
  } else {
    data.frame(date = as.Date(character()))
  }

  tenor_labels <- c("1M","3M","6M","1Y","2Y","3Y","5Y","7Y","10Y","20Y","30Y")

  list(
    futures         = futures,
    futures_front   = futures_front,
    futures_returns = futures_returns,
    cmt             = cmt,
    cmt_wide        = cmt_wide,
    tenor_labels    = tenor_labels,
    tenor_years     = c(1/12, 3/12, 6/12, 1, 2, 3, 5, 7, 10, 20, 30),
    markets         = c("CL", "BRN", "RB", "HO", "NG", "HTT"),
    market_names    = c(CL="WTI Crude Oil", BRN="Brent Crude",
                        RB="RBOB Gasoline", HO="Heating Oil",
                        NG="Natural Gas", HTT="WTI Houston (HTT)")
  )
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

#' Cache application data as Feather files
#'
#' Fetches RTL futures and FRED CMT data once, saves to inst/app/data/ as
#' Feather files. After running this, the app needs no internet connection.
#' Requires FRED_API_KEY environment variable for CMT data.
#'
#' @export
cache_app_data <- function() {
  # Always prefer source tree inst/app/data when running from the project root.
  # system.file() resolves to the installed library path when the package is
  # installed, so feather files would land there instead of the source tree.
  src_path <- file.path("inst", "app", "data")
  dir_path <- if (dir.exists("inst")) {
    src_path
  } else {
    p <- system.file("app/data", package = "fin451app")
    if (nchar(p) == 0) src_path else p
  }
  dir.create(dir_path, showWarnings = FALSE, recursive = TRUE)

  raw <- fct_fetch_live_data()

  futures_path <- file.path(dir_path, "futures.feather")
  cmt_path     <- file.path(dir_path, "cmt.feather")

  message("Writing futures.feather ...")
  arrow::write_feather(raw$futures, futures_path)

  message("Writing cmt.feather ...")
  arrow::write_feather(raw$cmt, cmt_path)

  message("Cache written to: ", dir_path)
  invisible(dir_path)
}

#' Filter futures data to a specific market and date range
#'
#' @param data The full data list from fct_load_all_data()
#' @param market Character, e.g. "CL"
#' @param dates Date vector of length 2 (start, end)
#' @param tenors Integer vector of contract months to keep (default 1:6)
#' @noRd
fct_filter_futures <- function(data, mkt, dates, tenors = 1:6) {
  data$futures |>
    dplyr::filter(
      market == mkt,
      tenor  %in% tenors,
      date   >= dates[1],
      date   <= dates[2]
    )
}

#' Compute log returns for a numeric vector
#' @noRd
fct_log_returns <- function(x) c(NA_real_, diff(log(x)))

#' Compute annualized roll yield time series for a market
#'
#' Roll yield = (M1 - M2) / M1, annualized by *12 (monthly roll).
#' Positive = backwardation (rolling earns). Negative = contango (rolling costs).
#'
#' @param data Full data list from fct_load_all_data()
#' @param mkt  Market code e.g. "CL"
#' @param dates Date range vector length 2
#' @noRd
fct_roll_yield <- function(data, mkt, dates) {
  data$futures |>
    dplyr::filter(market == mkt, tenor %in% c(1, 2),
                  date >= dates[1], date <= dates[2]) |>
    dplyr::select(date, tenor, value) |>
    tidyr::pivot_wider(names_from = tenor, values_from = value,
                       names_prefix = "M") |>
    dplyr::filter(!is.na(M1), !is.na(M2), M1 > 0) |>
    dplyr::mutate(
      roll_yield     = (M1 - M2) / M1 * 12,   # annualized (monthly roll)
      regime         = dplyr::if_else(roll_yield > 0, "Backwardation", "Contango")
    )
}
