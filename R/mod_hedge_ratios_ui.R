#' Hedge Ratios Module UI
#' @param id Module namespace id
#' @noRd
mod_hedge_ratios_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("hedge_insight")),
    bslib::navset_card_tab(
      # ── Tab 1: Cross-Market Hedge Ratios ─────────────────────────────────
      bslib::nav_panel(
        "Cross-Market",
        bslib::card_body(
          class = "p-2",
          bslib::layout_columns(
            col_widths = c(3, 9),
            shiny::div(
              shiny::selectInput(
                ns("hedge_instrument"),
                "Hedge with",
                choices  = c("CL", "BRN", "RB", "HO", "NG"),
                selected = "BRN"
              ),
              shiny::sliderInput(
                ns("hedge_window"),
                "Rolling Window (days)",
                min = 30, max = 252, value = 90, step = 10
              ),
              shiny::helpText(
                "Hedge ratio \u03b2 = cov(\u0394X, \u0394Y) / var(\u0394Y).",
                "Computed via Rcpp rolling OLS.",
                "\u03b2 = 1 means 1-for-1 hedge. Deviations signal basis risk."
              ),
              shiny::hr(),
              shiny::uiOutput(ns("hedge_static_table"))
            ),
            shiny::div(
              spinner_plot(ns("hedge_cross_plot"), height = "400px")
            )
          )
        )
      ),
      # ── Tab 2: Term Structure Hedge Ratios ───────────────────────────────
      bslib::nav_panel(
        "Term Structure",
        bslib::card_body(
          class = "p-2",
          bslib::layout_columns(
            col_widths = c(3, 9),
            shiny::div(
              shiny::selectInput(
                ns("ts_hedge_ref"),
                "Reference contract",
                choices  = setNames(1:3, paste0("M", 1:3)),
                selected = 1
              ),
              shiny::sliderInput(
                ns("ts_hedge_window"),
                "Rolling Window (days)",
                min = 30, max = 252, value = 90, step = 10
              ),
              shiny::helpText(
                "Rolling hedge ratio of M1 vs M2, M3, M4.",
                "Divergence from 1.0 = term structure basis risk."
              )
            ),
            shiny::div(
              spinner_plot(ns("hedge_term_plot"), height = "400px")
            )
          )
        )
      ),
      # ── Tab 3: Summary Table ─────────────────────────────────────────────
      bslib::nav_panel(
        "Summary Table",
        bslib::card_body(
          class = "p-2",
          shiny::helpText(
            "Full-sample OLS hedge ratios with R\u00b2 and standard errors.",
            "Use to set initial hedge ratios before applying rolling estimates."
          ),
          spinner_dt(ns("hedge_summary_table"))
        )
      )
    )
  )
}
