#' About / Methodology Tab UI (static — no server needed)
#' @noRd
mod_about_ui <- function() {
  meth_card <- function(title, formula = NULL, plain, detail) {
    bslib::card(
      class = "mb-3",
      bslib::card_header(class = "bg-primary text-white fw-semibold py-2", title),
      bslib::card_body(
        if (!is.null(formula))
          shiny::tags$p(class = "font-monospace text-muted small mb-1", formula),
        shiny::tags$p(shiny::HTML(plain)),
        shiny::tags$p(class = "small text-muted mb-0", shiny::HTML(detail))
      )
    )
  }

  shiny::tagList(
    bslib::layout_columns(
      col_widths = c(6, 6),
      gap = "1rem",

      # Left column
      shiny::tagList(
        meth_card(
          "Roll Yield",
          formula = "Roll Yield = (M1 - M2) / M1 \u00d7 12   (annualized monthly roll)",
          plain = paste0(
            "<strong>What it measures:</strong> the gain or loss from rolling a futures position ",
            "forward each month (selling the expiring M1 contract, buying M2). ",
            "<strong>Backwardation</strong> (M1 > M2) earns a positive yield \u2014 near-term scarcity. ",
            "<strong>Contango</strong> (M1 < M2) costs money to roll \u2014 storage cost is priced in."
          ),
          detail = paste0(
            "Multiplying by 12 converts the one-month spread into an annualized rate, ",
            "making it comparable to interest rates and other carry measures. ",
            "A backwardated crude market at +8% roll yield means a passive long futures ",
            "investor earns ~8% per year from the roll alone, before any spot price change."
          )
        ),
        meth_card(
          "Realized Volatility (Rcpp)",
          formula = "\u03c3\u2090\u2099\u2099 = \u221a(252) \u00d7 StdDev(log(P\u209c / P\u209c\u208b\u2081)) over rolling window",
          plain = paste0(
            "Daily log returns are computed as ln(P\u209c / P\u209c\u208b\u2081). ",
            "A rolling standard deviation over <em>w</em> days is annualized by multiplying by \u221a252 ",
            "(trading days per year). Implemented in <strong>C++ via Rcpp</strong> using ",
            "Welford\u2019s one-pass algorithm for numerical stability and speed (\u223c50\u00d7 faster than pure R)."
          ),
          detail = paste0(
            "Welford\u2019s algorithm updates the running mean and variance in a single pass, ",
            "avoiding catastrophic cancellation that can occur when squaring large numbers. ",
            "Front-month contracts are typically more volatile than deferred contracts ",
            "because near-term supply/demand uncertainty is higher."
          )
        ),
        meth_card(
          "PCA (Principal Component Analysis)",
          formula = "X = U \u03a3 V\u1d40  \u2014  PC loadings = columns of V",
          plain = paste0(
            "PCA decomposes the return correlation matrix into orthogonal factors. ",
            "<strong>PC1</strong> (usually 60\u201380% of variance) captures the <em>overall market level</em> \u2014 ",
            "all energy markets moving together (macro / USD / risk-on/off). ",
            "<strong>PC2</strong> captures <em>relative value</em> \u2014 crude vs gas spreads, ",
            "or liquid markets vs natural gas."
          ),
          detail = paste0(
            "The biplot shows each market\u2019s loading on PC1 (x-axis) vs PC2 (y-axis). ",
            "Markets clustered together move in sync; markets on opposite sides of the origin ",
            "are negatively correlated. The scree plot shows how many factors are needed to ",
            "explain most of the variance \u2014 in commodity energy markets, PC1 alone usually ",
            "explains over 60%, reflecting the dominant oil price factor."
          )
        )
      ),

      # Right column
      shiny::tagList(
        meth_card(
          "Hedge Ratios (OLS Beta)",
          formula = "R\u2093 = \u03b1 + \u03b2 \u00d7 R\u1d67 + \u03b5   \u2014   \u03b2\u0302 = Cov(R\u2093, R\u1d67) / Var(R\u1d67)",
          plain = paste0(
            "The hedge ratio \u03b2 estimates how many units of the hedging instrument (Y) ",
            "offset one unit of the exposure (X). ",
            "For example, \u03b2 = 0.87 between WTI and Brent means: sell 0.87 barrels of Brent ",
            "futures for every 1 barrel of WTI exposure to minimize basis risk. ",
            "<strong>Rolling \u03b2</strong> (computed in C++) shows how the relationship evolves over time."
          ),
          detail = paste0(
            "R\u00b2 measures how much of X\u2019s variance is explained by Y \u2014 higher R\u00b2 = better hedge effectiveness. ",
            "When rolling \u03b2 drifts significantly from the long-run average, the hedge ratio ",
            "should be recalibrated. This is particularly important in stress events ",
            "when correlations break down."
          )
        ),
        meth_card(
          "Seasonality & STL Decomposition",
          formula = "Y\u209c = Trend\u209c + Seasonal\u209c + Remainder\u209c",
          plain = paste0(
            "STL (Seasonal-Trend decomposition using LOESS) separates a price series into three components. ",
            "The <strong>seasonal component</strong> captures recurring calendar patterns ",
            "(e.g., heating oil demand peaks in winter, gasoline demand in summer). ",
            "The <strong>trend</strong> captures multi-year directional moves. ",
            "The <strong>remainder</strong> is idiosyncratic noise."
          ),
          detail = paste0(
            "A large seasonal component relative to the remainder confirms that calendar patterns ",
            "are statistically meaningful and can be used for position timing. ",
            "Natural gas has the strongest seasonality of the five markets due to heating/cooling demand cycles."
          )
        ),
        meth_card(
          "Data Sources",
          plain = paste0(
            "<strong>Futures:</strong> RTL::dflong \u2014 continuous front-month and deferred contract ",
            "prices for CL (WTI Crude), BRN (Brent), RB (RBOB Gasoline), HO (Heating Oil), NG (Natural Gas). ",
            "Series naming: prefix + 2-digit contract number (e.g., CL01 = front month). ",
            "Date range: 2007\u2013present.<br><br>",
            "<strong>Interest Rates:</strong> FRED CMT (Constant Maturity Treasuries) via fredr package. ",
            "Series: DGS1MO through DGS30 (1-month to 30-year). ",
            "Data cached as Feather binary files at startup \u2014 no API call at runtime."
          ),
          detail = "RTL is maintained by Phil Cote at the University of Alberta. FRED is the Federal Reserve Bank of St. Louis."
        )
      )
    )
  )
}
