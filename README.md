# Energy Commodity Analytics Dashboard

**FIN451 Final Project — University of Alberta**

A Golem-based R Shiny application for analyzing energy commodity futures markets and fixed income rates. Built for a senior Risk Management or Trading Management leader to understand market dynamics across individual markets and across markets.

---

## Live App

```bash
docker pull --platform linux/amd64 cainaidoo/finalgolem:latest
docker run --platform linux/amd64 -p 3838:3838 cainaidoo/finalgolem:latest
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
- **Rcpp C++** for rolling realized volatility (Welford's algorithm) and rolling OLS hedge ratios 
- **Feather cache** — futures and CMT data pre-cached as binary files in `inst/app/data/`, no API calls at runtime
- **bslib Bootstrap 5** UI with persistent market + date selector above all tabs, kept simple yet somewhat sleek

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


## Difficulties and what I learned

I found overall the entire project being more of a planning ended situation with the use of Ai being permitted in this assignment, so most of this was simply just planning, running code, returning to planning to see what I liked and I didn't like, repeat. The part I found the most difficult was incuding the use of C++, although I have used C++ in the past I found it tricky figuring out how exactly to include it in our modularized build of the app. I did find UI build to be quite simple with Ai as it pretty much explained exactly how to go about making a nice Ui and getting everything to fix although I wish I could figure out how to efficiently fix the section at the top where you click on the market and you have to scroll down, because I do see that as quite a fatal design flaw.

I learned a lot more about finance than I honestly expected to with the introduction of Ai in this class especially with the correlation matrix and the Roll yield PNL section where the charts had to include the stress yield and the charts had to somewhat support that information there. 

If I were to do this project again from the very start though, I would want to setup a data cache pipeline on the very first day , as I spent a lot of time fixing the API call bugs and the feather app problems, and also work on the dockerfile early as although it seems to last step of the assignment it can very well be done first a lot of the time. Not only that but I should have stayed focus on one module at a time because at times I got frustrated with how the module was looking and if a specific part of it wasnt working (originally I had houston WTI and it wasnt rendering properly) I should have just either fixed it or removed it before constantly swapping to another module because I had a few problems with modules and instead of fixing them there I ended up just quickly moving to the next leaving me with a lto of problems at the end. 

Also I think adding the methodology tab early on as my own personal notes as I learned more about the finance would ahve been a smarter more efficient way of doing this as I was constantly glancing through my computer to find the notes when I could have just kept it all in the app.

## Thank you message

Thank you Professor for a fun learning class it was an difficult yet enjoyable experience and I will be looking forward to FIN-440 next term!
