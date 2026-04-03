# CLAUDE.md — AI Context File

## Project Overview

**FIN451 Final Project — Commodity Analytics Golem App**

This is an R Shiny application built as a Golem package (`fin451app`) for the University of Alberta FIN451 course. It serves as a market dynamics analytics tool for a senior Risk Management or Trading Management leader with limited experience in the commodity markets.

The app tells the story of market dynamics in **individual markets and across markets** using energy futures (from the RTL package) and fixed income rates (from FRED).

---

## Architecture

### Golem "Small r" Strategy

All modules receive a shared `r <- shiny::reactiveValues()` object plus reactive expressions for the selected market and date range. Data is **loaded once at startup** from `fct_load_all_data()` and stored in `r$data`. Modules never independently fetch data — they filter from `r$data`.

```
app_server.R
  r <- reactiveValues(data = NULL, loaded = FALSE)
  r$data <- fct_load_all_data()  # once at startup
  selected_market    <- reactive(input$market)    # top-level, not in any module
  selected_daterange <- reactive(input$date_range)
  mod_*_server("id", r = r, market = selected_market, dates = selected_daterange)
```

### Module Registry

| Module | UI file | Server file | Purpose |
|--------|---------|-------------|---------|
| `fwd` | `mod_forward_curve_ui.R` | `mod_forward_curve_server.R` | Forward curve shape, contango/backwardation regime, curve heatmap |
| `vol` | `mod_volatility_ui.R` | `mod_volatility_server.R` | Rolling realized vol (Rcpp), term structure of vol, vol surface |
| `cody` | `mod_codynamics_ui.R` | `mod_codynamics_server.R` | Correlation matrix, rolling correlations, PCA |
| `seas` | `mod_seasonality_ui.R` | `mod_seasonality_server.R` | Monthly patterns, calendar heatmap, STL decomposition |
| `hedge` | `mod_hedge_ratios_ui.R` | `mod_hedge_ratios_server.R` | Cross-market hedge ratios, term structure betas, OLS summary |

### Rcpp

Two C++ functions in `src/roll_vol.cpp`:
- `rolling_vol_cpp(x, window)` — rolling annualized realized volatility (Welford's algorithm)
- `rolling_beta_cpp(x, y, window)` — rolling OLS beta (hedge ratio)

Both are generated into `R/RcppExports.R` by `Rcpp::compileAttributes()`.

---

## Data Sources

### RTL::dflong (Commodity Futures)
- Install dev version: `remotes::install_github("risktoollib/RTL")`
- Markets: CL (WTI Crude), BRN (Brent), RB (RBOB Gasoline), HO (Heating Oil), NG (Natural Gas)
- Series naming convention: prefix + 2-digit contract month number (e.g., "CL01", "CL02")
- Date range: 2007-01-01 onward

### FRED Constant Maturity Treasuries
- Series: DGS1MO, DGS3MO, DGS6MO, DGS1, DGS2, DGS3, DGS5, DGS7, DGS10, DGS20, DGS30
- Package: `fredr` — requires `FRED_API_KEY` environment variable
- Fetch start: 2007-01-01

### Data Cache
- Location: `inst/data-cache/app_data.rds`
- Generate locally: `fin451app::cache_app_data()` (requires FRED_API_KEY)
- The app checks for the cache file first; falls back to live fetch
- Commit `app_data.rds` to git to avoid API dependency in Docker

---

## Running Locally

```r
# Install dependencies
remotes::install_github("risktoollib/RTL")
install.packages(c("golem","shiny","bslib","dplyr","tidyr","lubridate","purrr",
                   "plotly","ggplot2","fredr","Rcpp","zoo","slider",
                   "FactoMineR","factoextra","DT","shinycssloaders","scales","rlang"))

# Load and run (from project root)
devtools::load_all()
run_app()
```

Or build+install first:
```r
devtools::install()
fin451app::run_app()
```

---

## Docker

### Build locally

```bash
# Without data cache (fetches live at runtime)
docker build -t fin451app .

# With data pre-cached at build time
docker build --build-arg FRED_API_KEY=your_key_here -t fin451app .
```

### Run

```bash
docker run -p 3838:3838 -e FRED_API_KEY=your_key_here fin451app
```

Visit: http://localhost:3838

---

## GitHub Actions

On push to `main`/`master`, the workflow in `.github/workflows/deploy.yml`:
1. Builds the Docker image with `FRED_API_KEY` from GitHub secrets
2. Pushes to `ghcr.io/<your-username>/<repo-name>:latest`

Required GitHub secret: `FRED_API_KEY`

---

## Environment Variables

| Variable | Required | Description |
|----------|----------|-------------|
| `FRED_API_KEY` | Yes (if no cache) | FRED API key for CMT yield data |

Get a free FRED API key at: https://fred.stlouisfed.org/docs/api/api_key.html

---

## Key Design Decisions

1. **bslib page_navbar** with `header =` argument for the market selector — this keeps the selector above all tabs without being inside any module, avoiding input duplication
2. **Cache-first data loading** — `inst/data-cache/app_data.rds` prevents API timeouts during app startup; the cache is generated once and committed
3. **Rcpp for rolling vol/beta** — pure R nested loops over 200K+ rows are too slow; C++ gives ~50x speedup
4. **All 5 markets interactive** — user switches market in the header; all 5 tabs update simultaneously via the shared `r` object and reactive expressions
5. **No Plumber** — pure Golem/Shiny as per assignment requirements; the cache strategy solves the same performance problem without inter-process complexity
