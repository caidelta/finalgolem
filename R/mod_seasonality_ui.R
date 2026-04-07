#' Seasonality Module UI
#' @param id Module namespace id
#' @noRd
mod_seasonality_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("seas_insight")),
    # Controls — persist above all tabs
    bslib::card(
      class = "border-0 bg-light mb-2",
      bslib::card_body(
        class = "py-2",
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          shiny::radioButtons(ns("seas_type"), "Measure",
            choices = c("Log Returns" = "returns", "Price Level" = "price"),
            selected = "returns", inline = TRUE),
          shiny::selectInput(ns("seas_stat"), "Summary Statistic",
            choices = c("Mean" = "mean", "Median" = "median"), selected = "mean"),
          shiny::checkboxInput(ns("seas_overlay"), "Year-over-year overlay", value = FALSE)
        )
      )
    ),
    bslib::navset_card_tab(
      bslib::nav_panel(
        "Monthly",
        bslib::card_body(
          class = "p-2",
          spinner_plot(ns("seas_monthly_bar"), height = "400px"),
          shiny::helpText("Average return (or price) for each calendar month across all years.")
        )
      ),
      bslib::nav_panel(
        "Calendar",
        bslib::card_body(
          class = "p-2",
          spinner_plot(ns("seas_heatmap"), height = "400px"),
          shiny::helpText("Each cell = monthly average return. Recurring patterns confirm seasonal cycles.")
        )
      ),
      bslib::nav_panel(
        "Decomposition",
        bslib::card_body(
          class = "p-2",
          spinner_plot(ns("seas_stl"), height = "420px"),
          shiny::helpText(
            "STL separates the price series into trend, seasonal cycle, and remainder.",
            "A strong seasonal component confirms predictable supply/demand patterns."
          )
        )
      )
    )
  )
}
