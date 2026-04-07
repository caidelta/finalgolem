#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}. DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # ── "Small r" shared reactive state ──────────────────────────────────────
  r <- shiny::reactiveValues(
    data   = NULL,   # populated once at startup by fct_load_all_data()
    loaded = FALSE
  )

  # ── Load ALL data once at startup ─────────────────────────────────────────
  shiny::observe({
    shiny::withProgress(message = "Loading market data, please wait...", value = 0, {
      shiny::setProgress(0.2, detail = "Reading futures data...")
      r$data   <- fct_load_all_data()
      r$loaded <- TRUE
      shiny::setProgress(1.0, detail = "Done")
    })
  }) |> shiny::bindEvent(TRUE, once = TRUE)

  # ── Status indicator ──────────────────────────────────────────────────────
  output$data_status <- shiny::renderUI({
    if (r$loaded) {
      shiny::span(
        shiny::icon("circle-check", class = "text-success"),
        " Data loaded",
        class = "small text-success"
      )
    } else {
      shiny::span(
        shinycssloaders::withSpinner(shiny::span(), type = 4, size = 0.4),
        " Loading...",
        class = "small text-muted"
      )
    }
  })

  # ── Top-level reactive expressions (shared across all modules) ───────────
  selected_market    <- shiny::reactive(input$market)
  selected_daterange <- shiny::reactive(input$date_range)

  # ── Call all module servers ───────────────────────────────────────────────
  mod_forward_curve_server("fwd",   r = r, market = selected_market, dates = selected_daterange)
  mod_volatility_server(   "vol",   r = r, market = selected_market, dates = selected_daterange)
  mod_codynamics_server(   "cody",  r = r, market = selected_market, dates = selected_daterange)
  mod_seasonality_server(  "seas",  r = r, market = selected_market, dates = selected_daterange)
  mod_hedge_ratios_server( "hedge", r = r, market = selected_market, dates = selected_daterange)
  mod_rates_server(        "rates", r = r,                           dates = selected_daterange)
}
