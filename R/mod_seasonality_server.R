#' Seasonality Module Server
#' @noRd
mod_seasonality_server <- function(id, r, market, dates) {
  shiny::moduleServer(id, function(input, output, session) {

    # ── Front-month series for selected market ───────────────────────────────
    front_month <- shiny::reactive({
      shiny::req(r$loaded, r$data)
      dr <- dates()
      r$data$futures |>
        dplyr::filter(market == !!market(), tenor == 1,
                      date >= dr[1], date <= dr[2]) |>
        dplyr::arrange(date) |>
        dplyr::mutate(
          log_ret = fct_log_returns(value),
          year    = lubridate::year(date),
          month   = lubridate::month(date, label = TRUE, abbr = TRUE),
          month_n = lubridate::month(date)
        )
    })

    # ── Monthly average bar chart ────────────────────────────────────────────
    output$seas_monthly_bar <- plotly::renderPlotly({
      shiny::req(front_month())
      stat_fn <- if (input$seas_stat == "median") median else mean
      y_col   <- if (input$seas_type == "price") "value" else "log_ret"

      df <- front_month() |>
        dplyr::filter(!is.na(.data[[y_col]])) |>
        dplyr::group_by(month, month_n) |>
        dplyr::summarise(stat = stat_fn(.data[[y_col]], na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(month_n)

      y_label <- if (input$seas_type == "price") "Avg Price" else "Avg Log Return"

      plotly::plot_ly(df,
        x = ~month, y = ~stat,
        type   = "bar",
        marker = list(
          color = ~ifelse(stat >= 0, "#1a9641", "#d73027")
        )) |>
        plotly::add_hlines(y = 0, line = list(color = "grey50", dash = "dot")) |>
        plotly::layout(
          title  = list(text = paste(market_label(market()), "— Seasonal Pattern"),
                        font = list(size = 13)),
          xaxis  = list(title = "", categoryorder = "array",
                        categoryarray = month.abb),
          yaxis  = list(title = y_label),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        plotly::config(displaylogo = FALSE)
    })

    # ── Calendar heatmap (year × month) ─────────────────────────────────────
    output$seas_heatmap <- plotly::renderPlotly({
      shiny::req(front_month())
      y_col <- if (input$seas_type == "price") "value" else "log_ret"

      df <- front_month() |>
        dplyr::filter(!is.na(.data[[y_col]])) |>
        dplyr::group_by(year, month, month_n) |>
        dplyr::summarise(stat = mean(.data[[y_col]], na.rm = TRUE), .groups = "drop")

      shiny::validate(shiny::need(nrow(df) > 0, "No data."))

      years <- sort(unique(df$year))
      months_ord <- month.abb

      z_mat <- matrix(NA, nrow = length(years), ncol = 12,
                      dimnames = list(as.character(years), months_ord))
      for (i in seq_len(nrow(df))) {
        yr <- as.character(df$year[i])
        mn <- as.character(df$month[i])
        if (yr %in% rownames(z_mat) && mn %in% colnames(z_mat)) {
          z_mat[yr, mn] <- df$stat[i]
        }
      }

      plotly::plot_ly(
        x         = months_ord,
        y         = as.character(years),
        z         = z_mat,
        type      = "heatmap",
        colorscale = list(
          c(0, "#d73027"), c(0.5, "#ffffbf"), c(1, "#1a9641")
        ),
        colorbar = list(title = if (input$seas_type == "returns") "Log Ret" else "Price")
      ) |>
        plotly::layout(
          title  = list(text = paste(market_label(market()), "— Calendar Heatmap"),
                        font = list(size = 13)),
          xaxis  = list(title = ""),
          yaxis  = list(title = "Year", autorange = "reversed"),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        plotly::config(displaylogo = FALSE)
    })

    # ── STL decomposition ────────────────────────────────────────────────────
    output$seas_stl <- plotly::renderPlotly({
      shiny::req(front_month())

      df <- front_month() |>
        dplyr::filter(!is.na(value)) |>
        dplyr::group_by(year, month_n) |>
        dplyr::summarise(price = mean(value, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(year, month_n)

      shiny::validate(shiny::need(nrow(df) >= 24, "Need at least 2 years for STL."))

      ts_obj  <- stats::ts(df$price, frequency = 12)
      stl_res <- stats::stl(ts_obj, s.window = "periodic")

      n     <- nrow(df)
      dates <- seq(as.Date(paste0(df$year[1], "-", df$month_n[1], "-01")),
                   by = "month", length.out = n)

      stl_df <- dplyr::tibble(
        date      = dates,
        observed  = as.numeric(stl_res$time.series[, "trend"] +
                                stl_res$time.series[, "seasonal"] +
                                stl_res$time.series[, "remainder"]),
        trend     = as.numeric(stl_res$time.series[, "trend"]),
        seasonal  = as.numeric(stl_res$time.series[, "seasonal"]),
        remainder = as.numeric(stl_res$time.series[, "remainder"])
      )

      plotly::subplot(
        plotly::plot_ly(stl_df, x = ~date, y = ~observed,
          type = "scatter", mode = "lines", name = "Observed",
          line = list(color = "#333333", width = 1)) |>
          plotly::layout(yaxis = list(title = "Observed")),

        plotly::plot_ly(stl_df, x = ~date, y = ~trend,
          type = "scatter", mode = "lines", name = "Trend",
          line = list(color = "#1a3a5c", width = 2)) |>
          plotly::layout(yaxis = list(title = "Trend")),

        plotly::plot_ly(stl_df, x = ~date, y = ~seasonal,
          type = "scatter", mode = "lines", name = "Seasonal",
          line = list(color = "#2ca02c", width = 1.5)) |>
          plotly::layout(yaxis = list(title = "Seasonal")),

        plotly::plot_ly(stl_df, x = ~date, y = ~remainder,
          type = "scatter", mode = "lines", name = "Remainder",
          line = list(color = "#d73027", width = 1)) |>
          plotly::layout(yaxis = list(title = "Remainder")),

        nrows      = 4,
        shareX     = TRUE,
        titleY     = TRUE,
        heights    = c(0.3, 0.3, 0.2, 0.2)
      ) |>
        plotly::layout(
          title  = list(text = paste(market_label(market()), "— STL Decomposition"),
                        font = list(size = 13)),
          showlegend = FALSE,
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        plotly::config(displaylogo = FALSE)
    })

  })
}
