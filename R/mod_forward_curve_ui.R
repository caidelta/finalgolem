#' Forward Curve Module UI
#' @param id Module namespace id
#' @noRd
mod_forward_curve_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("fwd_insight")),
    # Controls — persist above all tabs
    bslib::card(
      class = "border-0 bg-light mb-2",
      bslib::card_body(
        class = "py-2",
        bslib::layout_columns(
          col_widths = c(3, 3, 3, 3),
          shiny::sliderInput(ns("fwd_date"), "Curve Snapshot Date",
            min = as.Date("2007-01-01"), max = Sys.Date(),
            value = Sys.Date() - 365, step = 30,
            timeFormat = "%Y-%m-%d", width = "100%"),
          shiny::sliderInput(ns("n_tenors"), "Contracts to Show",
            min = 2, max = 12, value = 6, step = 1),
          shiny::checkboxInput(ns("show_history"),
            "Overlay multiple snapshot dates", value = FALSE),
          shiny::div(
            class = "d-flex align-items-end pb-1",
            shiny::downloadButton(ns("export_csv"), "Export CSV",
              class = "btn-sm btn-outline-primary w-100",
              icon  = shiny::icon("file-csv"))
          )
        )
      )
    ),
    bslib::navset_card_tab(
      bslib::nav_panel(
        "Structure",
        bslib::card_body(
          class = "p-2",
          spinner_plot(ns("fwd_curve_plot"), height = "400px"),
          shiny::helpText(
            "Upward slope = contango (cost to hold); downward = backwardation (tight near-term supply)."
          )
        )
      ),
      bslib::nav_panel(
        "Roll Yield",
        bslib::card_body(
          class = "p-2",
          spinner_plot(ns("roll_yield_plot"), height = "400px"),
          shiny::helpText(
            "Roll yield = (M1 \u2212 M2) / M1 \u00d7 12. Green = backwardation (earn by rolling);",
            "red = contango (pay to roll). This is the real P&L cost of holding futures."
          )
        )
      ),
      bslib::nav_panel(
        "Heatmap",
        bslib::card_body(
          class = "p-2",
          spinner_plot(ns("fwd_heatmap"), height = "400px"),
          shiny::helpText(
            "Rows = contract months. Color = settlement price. Reveals how curve shape evolves over time."
          )
        )
      )
    )
  )
}
