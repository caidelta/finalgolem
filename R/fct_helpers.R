#' Standard plotly time-series line chart
#'
#' @param df Data frame with date column and one or more value columns
#' @param x  Column name (unquoted) for x axis
#' @param y  Column name (unquoted) for y axis
#' @param color Column name for color grouping (optional)
#' @param title Chart title
#' @param ylab  Y-axis label
#' @param hoverformat Plotly hover format string
#' @noRd
plotly_ts <- function(df, x = date, y = value, color = NULL,
                      title = "", ylab = "", hoverformat = ".4f") {
  x_col <- rlang::enquo(x)
  y_col <- rlang::enquo(y)
  c_col <- rlang::enquo(color)

  if (rlang::quo_is_null(c_col)) {
    p <- plotly::plot_ly(df,
      x    = x_col,
      y    = y_col,
      type = "scatter",
      mode = "lines",
      line = list(width = 1.5)
    )
  } else {
    p <- plotly::plot_ly(df,
      x     = x_col,
      y     = y_col,
      color = c_col,
      type  = "scatter",
      mode  = "lines",
      line  = list(width = 1.5)
    )
  }

  p |>
    plotly::layout(
      title  = list(text = title, font = list(size = 14)),
      xaxis  = list(title = "", showgrid = FALSE),
      yaxis  = list(title = ylab, tickformat = hoverformat),
      legend = list(orientation = "h", y = -0.15),
      margin = list(t = 40, r = 10, b = 40, l = 60),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)"
    ) |>
    plotly::config(displayModeBar = TRUE, displaylogo = FALSE)
}

#' Plotly correlation heatmap
#'
#' @param cor_mat A numeric correlation matrix with named rows/cols
#' @param title   Chart title
#' @noRd
plotly_corr_heatmap <- function(cor_mat, title = "Correlation Matrix") {
  plotly::plot_ly(
    x         = colnames(cor_mat),
    y         = rownames(cor_mat),
    z         = cor_mat,
    type      = "heatmap",
    colorscale = list(
      c(0,   "#d73027"),
      c(0.5, "#ffffbf"),
      c(1,   "#1a9641")
    ),
    zmin = -1, zmax = 1,
    text = round(cor_mat, 2),
    texttemplate = "%{text}",
    showscale = TRUE
  ) |>
    plotly::layout(
      title  = list(text = title, font = list(size = 14)),
      xaxis  = list(title = ""),
      yaxis  = list(title = "", autorange = "reversed"),
      margin = list(t = 50, r = 10, b = 60, l = 80),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)"
    ) |>
    plotly::config(displayModeBar = FALSE)
}

#' Consistent bslib analytics card wrapper
#'
#' @param title Card header title
#' @param ... Contents for card_body
#' @param height Optional height string, e.g. "400px"
#' @noRd
analytics_card <- function(title, ..., height = NULL) {
  bslib::card(
    full_screen = TRUE,
    height      = height,
    bslib::card_header(
      class = "bg-primary text-white fw-semibold py-2",
      title
    ),
    bslib::card_body(
      class = "p-2",
      ...
    )
  )
}

#' Spinner-wrapped plotly output
#'
#' @param outputId The plotlyOutput id
#' @param height   Height string
#' @noRd
spinner_plot <- function(outputId, height = "380px") {
  shinycssloaders::withSpinner(
    plotly::plotlyOutput(outputId, height = height),
    type  = 6,
    color = "#1a3a5c"
  )
}

#' Spinner-wrapped DT table output
#' @noRd
spinner_dt <- function(outputId) {
  shinycssloaders::withSpinner(
    DT::dataTableOutput(outputId),
    type  = 6,
    color = "#1a3a5c"
  )
}

#' Market display name helper
#' @noRd
market_label <- function(market_code) {
  labels <- c(
    CL  = "WTI Crude Oil (CL)",
    BRN = "Brent Crude (BRN)",
    RB  = "RBOB Gasoline (RB)",
    HO  = "Heating Oil (HO)",
    NG  = "Natural Gas (NG)"
  )
  labels[[market_code]] %||% market_code
}

# Re-export %||% from rlang
`%||%` <- function(a, b) if (!is.null(a)) a else b
