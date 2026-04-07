#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`. DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib page_navbar nav_panel bs_theme card card_body layout_columns
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    bslib::page_navbar(
      id    = "main_navbar",
      title = tags$span(
        tags$img(src = "www/logo.png", height = "30px", style = "margin-right:8px;",
                 onerror = "this.style.display='none'"),
        "Commodity Analytics"
      ),
      theme = bslib::bs_theme(
        version  = 5,
        bootswatch = "flatly",
        primary  = "#1a3a5c",
        "navbar-bg" = "#1a3a5c"
      ),
      # ── Persistent header: market + date selectors ──────────────────────
      header = shiny::conditionalPanel(
        "input.main_navbar !== 'about'",
        bslib::card(
        class = "mb-0 border-0 rounded-0",
        bslib::card_body(
          class = "py-2 bg-light",
          bslib::layout_columns(
            col_widths = c(3, 5, 4),
            shiny::selectInput(
              inputId  = "market",
              label    = tags$strong("Market"),
              choices  = c(
                "WTI Crude Oil (CL)"  = "CL",
                "Brent Crude (BRN)"   = "BRN",
                "RBOB Gasoline (RB)"  = "RB",
                "Heating Oil (HO)"    = "HO",
                "Natural Gas (NG)"    = "NG"
              ),
              selected = "CL"
            ),
            shiny::dateRangeInput(
              inputId = "date_range",
              label   = tags$strong("Date Range"),
              start   = "2010-01-01",
              end     = Sys.Date(),
              min     = "2007-01-01",
              max     = Sys.Date()
            ),
            shiny::div(
              class = "d-flex align-items-end pb-1",
              shiny::uiOutput("data_status")
            )
          )
        )
        )  # end conditionalPanel
      ),
      # ── Tabs ─────────────────────────────────────────────────────────────
      bslib::nav_panel(
        title = shiny::icon("chart-line", class = "me-1") |> tagList("Forward Curve"),
        value = "fwd",
        mod_forward_curve_ui("fwd")
      ),
      bslib::nav_panel(
        title = shiny::icon("fire-flame-curved", class = "me-1") |> tagList("Volatility"),
        value = "vol",
        mod_volatility_ui("vol")
      ),
      bslib::nav_panel(
        title = shiny::icon("diagram-project", class = "me-1") |> tagList("Co-Dynamics"),
        value = "cody",
        mod_codynamics_ui("cody")
      ),
      bslib::nav_panel(
        title = shiny::icon("calendar", class = "me-1") |> tagList("Seasonality"),
        value = "seas",
        mod_seasonality_ui("seas")
      ),
      bslib::nav_panel(
        title = shiny::icon("scale-balanced", class = "me-1") |> tagList("Hedge Ratios"),
        value = "hedge",
        mod_hedge_ratios_ui("hedge")
      ),
      bslib::nav_panel(
        title = shiny::icon("landmark", class = "me-1") |> tagList("Rates (CMT)"),
        value = "rates",
        mod_rates_ui("rates")
      ),
      bslib::nav_panel(
        title = shiny::icon("book-open", class = "me-1") |> tagList("Methodology"),
        value = "about",
        mod_about_ui()
      ),
      footer = tags$div(
        class = "text-muted text-center py-1 small border-top bg-light",
        "FIN451 Final Project — Data: RTL continuous futures + FRED CMT"
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))

  tags$head(
    favicon(),
    bundle_resources(
      path      = app_sys("app/www"),
      app_title = "Energy Commodity Analytics"
    ),
    tags$link(rel = "stylesheet", href = "www/custom.css")
  )
}
