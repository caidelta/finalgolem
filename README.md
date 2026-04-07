# Energy Commodity Analytics Dashboard

**FIN451 Final Project — University of Alberta**

A Golem-based R Shiny application for analyzing energy commodity futures markets and fixed income rates. Built for a senior Risk Management or Trading Management leader to understand market dynamics across individual markets and across markets.

---

## Live App

```bash
docker run -p 3838:3838 cainaidoo/finalgolem:latest
```

Open **http://localhost:3838**

---

## What It Does

Six analysis modules, each with an auto-computed narrative insight card:

| Tab | What it answers |
|-----|----------------|
| **Forward Curve** | What shape is the curve? Contango or backwardation? What does rolling cost? |
| **Volatility** | How risky is this market now vs history? When were the stress events? |
| **Co-Dynamics** | How do markets move relative to each other? What drives the common factor? |
| **Seasonality** | Are there predictable calendar patterns? Which months are structurally weak? |
| **Hedge Ratios** | How many units of Y offset 1 unit of X? Is the relationship stable? |
| **Rates (CMT)** | What is the yield curve doing? Is it inverted? What does that mean for commodities? |
| **Methodology** | How is every number computed? |

**Markets:** WTI Crude (CL), Brent Crude (BRN), RBOB Gasoline (RB), Heating Oil (HO), Natural Gas (NG)

**Rates:** FRED Constant Maturity Treasuries — 1M to 30Y

---

## Architecture

- **Golem package** (`fin451app`) with the small-r strategy — data loads once at startup, flows to all modules via shared `reactiveValues`
- **Rcpp C++** for rolling realized volatility (Welford's algorithm) and rolling OLS hedge ratios — ~50× faster than pure R
- **Feather cache** — futures and CMT data pre-cached as binary files in `inst/app/data/`, no API calls at runtime
- **bslib Bootstrap 5** UI with persistent market + date selector above all tabs

---

## Data Sources

- **RTL::dflong** (dev version) — continuous energy futures, 2007–present
- **FRED CMT** — DGS1MO through DGS30, 2007–present

---

## Run Locally (from source)

```r
# Prerequisites
remotes::install_github("risktoollib/RTL")
devtools::install_deps()

# Cache data (one time — requires FRED API key)
Sys.setenv(FRED_API_KEY = "your_key")
devtools::load_all()
fin451app::cache_app_data()

# Run
devtools::load_all()
fin451app::run_app()
```

---

## Docker

```bash
# Build
docker build --platform linux/amd64 -t finalgolem .

# Run
docker run -p 3838:3838 finalgolem
```

---

## Deployment

GitHub Actions automatically builds and pushes to Docker Hub on every push to `main`.

Required GitHub secrets:
- `DOCKERHUB_USERNAME`
- `DOCKERHUB_TOKEN`
