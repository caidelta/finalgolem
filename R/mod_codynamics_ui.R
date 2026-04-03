#' Co-Dynamics Module UI
#' @param id Module namespace id
#' @noRd
mod_codynamics_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::navset_card_tab(
      # ── Tab 1: Correlation Matrix ─────────────────────────────────────────
      bslib::nav_panel(
        "Correlation Matrix",
        bslib::card_body(
          class = "p-2",
          bslib::layout_columns(
            col_widths = c(3, 9),
            shiny::div(
              shiny::selectInput(
                ns("cor_period"),
                "Sample Period",
                choices = c(
                  "Full history"     = "full",
                  "Last 1 year"      = "1y",
                  "Last 3 years"     = "3y",
                  "Last 5 years"     = "5y"
                ),
                selected = "full"
              ),
              shiny::radioButtons(
                ns("cor_type"),
                "Data type",
                choices  = c("Log Returns" = "returns", "Price Levels" = "prices"),
                selected = "returns"
              ),
              shiny::helpText(
                "All five markets shown. Correlation based on daily log returns",
                "removes price-level trends, isolating co-movement."
              )
            ),
            shiny::div(
              spinner_plot(ns("cor_heatmap"), height = "420px")
            )
          )
        )
      ),
      # ── Tab 2: Rolling Correlations ───────────────────────────────────────
      bslib::nav_panel(
        "Rolling Correlations",
        bslib::card_body(
          class = "p-2",
          bslib::layout_columns(
            col_widths = c(3, 9),
            shiny::div(
              shiny::selectInput(
                ns("roll_cor_pair1"),
                "Market 1",
                choices  = c("CL", "BRN", "RB", "HO", "NG"),
                selected = "CL"
              ),
              shiny::selectInput(
                ns("roll_cor_pair2"),
                "Market 2",
                choices  = c("CL", "BRN", "RB", "HO", "NG"),
                selected = "BRN"
              ),
              shiny::sliderInput(
                ns("roll_cor_window"),
                "Window (days)",
                min = 30, max = 252, value = 90, step = 10
              )
            ),
            shiny::div(
              spinner_plot(ns("rolling_cor_plot"), height = "420px")
            )
          )
        )
      ),
      # ── Tab 3: PCA ────────────────────────────────────────────────────────
      bslib::nav_panel(
        "PCA",
        bslib::card_body(
          class = "p-2",
          bslib::layout_columns(
            col_widths = c(6, 6),
            analytics_card(
              "Scree Plot — Variance Explained",
              spinner_plot(ns("pca_scree"), height = "320px")
            ),
            analytics_card(
              "Biplot — Market Factor Loadings",
              spinner_plot(ns("pca_biplot"), height = "320px")
            )
          )
        )
      )
    )
  )
}
