#' Forward Curve Module UI
#' @param id Module namespace id
#' @noRd
mod_forward_curve_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_columns(
      col_widths = c(12),
      gap = "1rem",
      # Row 1: controls
      bslib::card(
        class = "border-0 bg-light",
        bslib::card_body(
          class = "py-2",
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            shiny::sliderInput(
              ns("fwd_date"),
              "Curve Snapshot Date",
              min   = as.Date("2007-01-01"),
              max   = Sys.Date(),
              value = Sys.Date() - 365,
              step  = 30,
              timeFormat = "%Y-%m-%d",
              width = "100%"
            ),
            shiny::sliderInput(
              ns("n_tenors"),
              "Contracts to Show",
              min = 2, max = 12, value = 6, step = 1
            ),
            shiny::checkboxInput(
              ns("show_history"),
              "Overlay multiple snapshot dates",
              value = FALSE
            )
          )
        )
      ),
      # Row 2: main charts
      bslib::layout_columns(
        col_widths = c(6, 6),
        analytics_card(
          "Forward Curve Shape",
          spinner_plot(ns("fwd_curve_plot"), height = "380px"),
          shiny::helpText(
            "Each point is a futures contract month. Upward slope = contango (normal),",
            "downward slope = backwardation (typically signals supply tightness)."
          )
        ),
        analytics_card(
          "Contango / Backwardation Regime",
          spinner_plot(ns("fwd_spread_plot"), height = "380px"),
          shiny::helpText(
            "Spread = M1 minus M2 price. Positive = backwardation; negative = contango."
          )
        )
      ),
      # Row 3: curve surface over time
      analytics_card(
        "Forward Curve Heatmap (Tenor × Time)",
        spinner_plot(ns("fwd_heatmap"), height = "360px"),
        shiny::helpText(
          "Rows = contract months (M1..M6). Color = settlement price.",
          "Reveals how curve shape evolves over time."
        )
      )
    )
  )
}
