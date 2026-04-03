#' Seasonality Module UI
#' @param id Module namespace id
#' @noRd
mod_seasonality_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_columns(
      col_widths = c(12),
      gap = "1rem",
      # Controls
      bslib::card(
        class = "border-0 bg-light",
        bslib::card_body(
          class = "py-2",
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            shiny::radioButtons(
              ns("seas_type"),
              "Measure",
              choices  = c("Log Returns" = "returns", "Price Level" = "price"),
              selected = "returns",
              inline   = TRUE
            ),
            shiny::selectInput(
              ns("seas_stat"),
              "Summary Statistic",
              choices  = c("Mean" = "mean", "Median" = "median"),
              selected = "mean"
            ),
            shiny::checkboxInput(
              ns("seas_overlay"),
              "Year-over-year overlay",
              value = FALSE
            )
          )
        )
      ),
      # Row 1: Monthly averages + Calendar heatmap
      bslib::layout_columns(
        col_widths = c(5, 7),
        analytics_card(
          "Average by Month",
          spinner_plot(ns("seas_monthly_bar"), height = "360px"),
          shiny::helpText("Average return (or price) for each calendar month across all years.")
        ),
        analytics_card(
          "Calendar Heatmap (Year \u00d7 Month)",
          spinner_plot(ns("seas_heatmap"), height = "360px"),
          shiny::helpText("Each cell = monthly average return. Reveals recurring seasonal patterns.")
        )
      ),
      # Row 2: STL decomposition
      analytics_card(
        "STL Decomposition: Trend + Seasonal + Remainder",
        spinner_plot(ns("seas_stl"), height = "420px"),
        shiny::helpText(
          "STL decomposes the front-month price series into trend, seasonal cycle, and remainder.",
          "Strong seasonal component confirms predictable supply/demand patterns."
        )
      )
    )
  )
}
