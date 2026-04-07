#' Volatility Module UI
#' @param id Module namespace id
#' @noRd
mod_volatility_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("vol_insight")),
    # Controls — persist above all tabs
    bslib::card(
      class = "border-0 bg-light mb-2",
      bslib::card_body(
        class = "py-2",
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          shiny::selectInput(ns("vol_window"), "Rolling Window (days)",
            choices = c("20" = 20, "60" = 60, "90" = 90, "252" = 252),
            selected = 60),
          shiny::selectInput(ns("vol_tenor"), "Contract Month",
            choices = setNames(1:6, paste0("M", 1:6)), selected = 1),
          shiny::checkboxInput(ns("show_all_tenors"),
            "Compare all contract months", value = FALSE)
        )
      )
    ),
    bslib::navset_card_tab(
      bslib::nav_panel(
        "Time Series",
        bslib::card_body(
          class = "p-2",
          spinner_plot(ns("vol_ts_plot"), height = "400px"),
          shiny::helpText("Annualized realized volatility from log returns. Computed via Rcpp.")
        )
      ),
      bslib::nav_panel(
        "Term Structure",
        bslib::card_body(
          class = "p-2",
          spinner_plot(ns("vol_term_plot"), height = "400px"),
          shiny::helpText(
            "Average volatility by contract month. Front-month contracts are typically",
            "more volatile — immediate supply/demand uncertainty is higher than long-term."
          )
        )
      ),
      bslib::nav_panel(
        "Surface",
        bslib::card_body(
          class = "p-2",
          spinner_plot(ns("vol_surface"), height = "400px"),
          shiny::helpText(
            "Color = rolling realized vol by tenor and time.",
            "Vertical spikes = market stress events; horizontal bands = term structure."
          )
        )
      )
    )
  )
}
