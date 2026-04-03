#' Volatility Module UI
#' @param id Module namespace id
#' @noRd
mod_volatility_ui <- function(id) {
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
            shiny::selectInput(
              ns("vol_window"),
              "Rolling Window (days)",
              choices  = c("20" = 20, "60" = 60, "90" = 90, "252" = 252),
              selected = 60
            ),
            shiny::selectInput(
              ns("vol_tenor"),
              "Contract Month",
              choices  = setNames(1:6, paste0("M", 1:6)),
              selected = 1
            ),
            shiny::checkboxInput(
              ns("show_all_tenors"),
              "Compare all contract months",
              value = FALSE
            )
          )
        )
      ),
      # Charts row 1
      bslib::layout_columns(
        col_widths = c(8, 4),
        analytics_card(
          "Rolling Realized Volatility Over Time",
          spinner_plot(ns("vol_ts_plot"), height = "360px"),
          shiny::helpText("Annualized realized volatility from log returns. Computed via Rcpp for speed.")
        ),
        analytics_card(
          "Volatility Term Structure",
          spinner_plot(ns("vol_term_plot"), height = "360px"),
          shiny::helpText("Average vol by contract month over the selected date range.")
        )
      ),
      # Vol surface
      analytics_card(
        "Volatility Surface (Tenor × Time)",
        spinner_plot(ns("vol_surface"), height = "400px"),
        shiny::helpText(
          "Color = rolling realized vol. Horizontal bands reveal term structure;",
          "vertical spikes reveal market stress events."
        )
      )
    )
  )
}
