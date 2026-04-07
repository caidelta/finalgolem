#' Rates (CMT) Module UI
#' @param id Module namespace id
#' @noRd
mod_rates_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("rates_insight")),
    # Controls
    bslib::card(
      class = "border-0 bg-light mb-2",
      bslib::card_body(
        class = "py-2",
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          shiny::sliderInput(
            ns("rates_date"),
            "Yield Curve Snapshot Date",
            min        = as.Date("2007-01-01"),
            max        = Sys.Date(),
            value      = Sys.Date() - 30,
            step       = 30,
            timeFormat = "%Y-%m-%d",
            width      = "100%"
          ),
          shiny::checkboxInput(
            ns("rates_overlay"),
            "Compare to 1 year ago",
            value = TRUE
          ),
          shiny::div(
            shiny::helpText("CMT = Constant Maturity Treasury yields from FRED. Maturities: 1M to 30Y.")
          )
        )
      )
    ),
    bslib::navset_card_tab(
      bslib::nav_panel(
        "Yield Curve",
        bslib::card_body(
          class = "p-2",
          spinner_plot(ns("yield_curve_plot"), height = "400px"),
          shiny::helpText(
            "Normal (upward slope) = healthy growth expectations. ",
            "Inverted (downward slope) = recession signal. ",
            "Flat = transition or uncertainty."
          )
        )
      ),
      bslib::nav_panel(
        "10Y Over Time",
        bslib::card_body(
          class = "p-2",
          spinner_plot(ns("yield_ts_plot"), height = "400px"),
          shiny::helpText(
            "Rising rates increase the cost of carry for commodity futures and ",
            "compete with risk assets for capital."
          )
        )
      ),
      bslib::nav_panel(
        "Term Spread (10Y\u22123M)",
        bslib::card_body(
          class = "p-2",
          spinner_plot(ns("spread_ts_plot"), height = "400px"),
          shiny::helpText(
            "10Y minus 3M yield. Negative (inverted) = historically reliable US recession signal. ",
            "Each inversion since 1970 has preceded a recession within 6\u201318 months."
          )
        )
      )
    )
  )
}
