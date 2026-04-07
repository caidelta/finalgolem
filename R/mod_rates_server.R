#' Rates (CMT) Module Server
#' @noRd
mod_rates_server <- function(id, r, dates) {
  shiny::moduleServer(id, function(input, output, session) {

    # ── Insight card ─────────────────────────────────────────────────────────
    output$rates_insight <- shiny::renderUI({
      shiny::req(r$loaded, r$data)
      cmt <- r$data$cmt
      shiny::validate(shiny::need(nrow(cmt) > 10, "CMT data not available. Set FRED_API_KEY and run cache_app_data()."))

      # Latest available date
      latest_date <- max(cmt$date, na.rm = TRUE)
      latest <- cmt |>
        dplyr::filter(date == latest_date) |>
        dplyr::select(tenor_label, value) |>
        tidyr::drop_na()

      shiny::validate(shiny::need(nrow(latest) >= 3, ""))

      # Get short and long end
      short_end <- latest |> dplyr::filter(tenor_label %in% c("3M","6M","1Y")) |>
        dplyr::summarise(v = mean(value, na.rm=TRUE)) |> dplyr::pull(v)
      long_end  <- latest |> dplyr::filter(tenor_label %in% c("10Y","20Y","30Y")) |>
        dplyr::summarise(v = mean(value, na.rm=TRUE)) |> dplyr::pull(v)

      spread <- round(long_end - short_end, 2)
      shape  <- dplyr::case_when(
        spread >  0.5 ~ "upward sloping",
        spread < -0.1 ~ "inverted",
        TRUE          ~ "flat"
      )
      meaning <- dplyr::case_when(
        spread >  0.5 ~ "indicating normal growth expectations and healthy risk appetite.",
        spread < -0.1 ~ "a historically reliable recession signal that pressures risk assets including commodities.",
        TRUE          ~ "reflecting economic transition and rate uncertainty."
      )
      y10 <- latest |> dplyr::filter(tenor_label == "10Y") |> dplyr::pull(value)
      y10_txt <- if (length(y10) > 0) paste0(" The 10Y yield is <strong>", round(y10,2), "%</strong>.") else ""

      color <- if (spread < -0.1) "#d73027" else if (spread > 0.5) "#1a9641" else "#f0a500"

      shiny::div(class="p-2 mb-2 rounded", style=paste0("border-left:4px solid ",color,"; background:#f8f9fa;"),
        shiny::HTML(paste0(
          "As of <strong>", format(latest_date, "%b %d, %Y"), "</strong>, the US Treasury yield curve is ",
          "<strong>", shape, "</strong> (10Y–3M spread: ",
          if(spread >= 0) "+" else "", spread, "%), ", meaning, y10_txt,
          " Higher rates increase the cost of carry for commodity futures and compete with risk assets for capital."
        ))
      )
    })

    # ── Yield curve snapshot plot ─────────────────────────────────────────────
    output$yield_curve_plot <- plotly::renderPlotly({
      shiny::req(r$loaded, r$data)
      cmt <- r$data$cmt
      shiny::validate(shiny::need(nrow(cmt) > 10, "CMT data not available. Ensure FRED_API_KEY is set."))

      snap   <- input$rates_date
      levels_order <- c("1M","3M","6M","1Y","2Y","3Y","5Y","7Y","10Y","20Y","30Y")

      get_snap <- function(d) {
        avail <- cmt |> dplyr::filter(date <= d) |> dplyr::pull(date) |> max()
        cmt |>
          dplyr::filter(date == avail) |>
          dplyr::mutate(tenor_label = factor(tenor_label, levels = levels_order)) |>
          dplyr::arrange(tenor_label) |>
          tidyr::drop_na(value)
      }

      df_now  <- get_snap(snap) |> dplyr::mutate(period = format(snap, "%b %d, %Y"))

      p <- plotly::plot_ly() |>
        plotly::add_trace(data = df_now,
          x = ~tenor_label, y = ~value, name = ~period,
          type = "scatter", mode = "lines+markers",
          line = list(color = "#1a3a5c", width = 2.5),
          marker = list(size = 8, color = "#1a3a5c"))

      if (isTRUE(input$rates_overlay)) {
        snap_1y <- snap - 365
        df_1y   <- tryCatch(get_snap(snap_1y) |> dplyr::mutate(period = format(snap_1y, "%b %d, %Y")),
                            error = function(e) NULL)
        if (!is.null(df_1y) && nrow(df_1y) > 0) {
          p <- p |> plotly::add_trace(data = df_1y,
            x = ~tenor_label, y = ~value, name = ~period,
            type = "scatter", mode = "lines+markers",
            line = list(color = "#d73027", width = 1.5, dash = "dash"),
            marker = list(size = 6, color = "#d73027"))
        }
      }

      p |> plotly::layout(
        title  = list(text = "US Treasury Yield Curve (CMT)", font = list(size = 13)),
        xaxis  = list(title = "Maturity", categoryorder = "array",
                      categoryarray = levels_order),
        yaxis  = list(title = "Yield (%)", tickformat = ".2f"),
        legend = list(orientation = "h", y = -0.2),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
      ) |>
      plotly::config(displaylogo = FALSE)
    })

    # ── 10Y yield time series ─────────────────────────────────────────────────
    output$yield_ts_plot <- plotly::renderPlotly({
      shiny::req(r$loaded, r$data)
      cmt <- r$data$cmt
      shiny::validate(shiny::need(nrow(cmt) > 10, "CMT data not available."))
      dr  <- dates()

      df <- cmt |>
        dplyr::filter(tenor_label == "10Y", date >= dr[1], date <= dr[2]) |>
        tidyr::drop_na(value)

      shiny::validate(shiny::need(nrow(df) > 5, "No 10Y data in this date range."))

      plotly::plot_ly(df, x = ~date, y = ~value,
        type = "scatter", mode = "lines",
        line = list(color = "#1a3a5c", width = 1.5),
        name = "10Y Yield",
        hovertemplate = paste0(
          "<b>%{x|%b %d, %Y}</b><br>",
          "10Y Yield: <b>%{y:.2f}%</b><br>",
          "<i style='color:#888'>Higher rates raise cost of carry for futures<br>",
          "and shift capital away from commodities.</i>",
          "<extra>10Y Treasury</extra>"
        )) |>
        plotly::layout(
          title  = list(text = "US 10Y Treasury Yield", font = list(size = 13)),
          xaxis  = list(title = ""),
          yaxis  = list(title = "Yield (%)", tickformat = ".2f"),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        plotly::config(displaylogo = FALSE)
    })

    # ── Term spread (10Y minus 3M) — recession indicator ─────────────────────
    output$spread_ts_plot <- plotly::renderPlotly({
      shiny::req(r$loaded, r$data)
      cmt <- r$data$cmt
      shiny::validate(shiny::need(nrow(cmt) > 10, "CMT data not available."))
      dr <- dates()

      spread_df <- cmt |>
        dplyr::filter(tenor_label %in% c("10Y", "3M"),
                      date >= dr[1], date <= dr[2]) |>
        tidyr::drop_na(value) |>
        dplyr::select(date, tenor_label, value) |>
        tidyr::pivot_wider(names_from = tenor_label, values_from = value) |>
        dplyr::filter(!is.na(`10Y`), !is.na(`3M`)) |>
        dplyr::mutate(
          spread  = `10Y` - `3M`,
          inverted = spread < 0
        )

      shiny::validate(shiny::need(nrow(spread_df) > 5, "Not enough data for term spread."))

      plotly::plot_ly(spread_df, x = ~date, y = ~spread,
        type = "scatter", mode = "lines",
        color = ~inverted,
        colors = c("FALSE" = "#1a9641", "TRUE" = "#d73027"),
        line = list(width = 1.5),
        hovertemplate = paste0(
          "<b>%{x|%b %d, %Y}</b><br>",
          "10Y\u22123M Spread: <b>%{y:.2f}%</b><br>",
          "<i style='color:#888'>Negative = yield curve inverted<br>",
          "Every US recession since 1970 was preceded by inversion.</i>",
          "<extra>%{fullData.name}</extra>"
        )) |>
        plotly::layout(
          shapes = list(list(type = "line", x0 = 0, x1 = 1, xref = "paper",
                             y0 = 0, y1 = 0,
                             line = list(color = "grey50", dash = "dot"))),
          title  = list(text = "US Treasury Term Spread (10Y \u2212 3M)",
                        font = list(size = 13)),
          xaxis  = list(title = ""),
          yaxis  = list(title = "Spread (%)", tickformat = ".2f"),
          legend = list(orientation = "h", y = -0.2,
                        title = list(text = "Inverted")),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        plotly::config(displaylogo = FALSE)
    })

  })
}
