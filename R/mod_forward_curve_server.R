#' Forward Curve Module Server
#' @param id     Module namespace id
#' @param r      Shared reactiveValues (contains r$data)
#' @param market Reactive returning the selected market code (e.g. "CL")
#' @param dates  Reactive returning c(start_date, end_date)
#' @noRd
mod_forward_curve_server <- function(id, r, market, dates) {
  shiny::moduleServer(id, function(input, output, session) {

    # ── Reactive: filtered futures for selected market + date range ──────────
    fwd_data <- shiny::reactive({
      shiny::req(r$loaded, r$data)
      n_t <- as.integer(input$n_tenors)
      fct_filter_futures(r$data, market(), dates(), tenors = seq_len(n_t))
    })

    # ── Forward curve cross-section at snapshot date ─────────────────────────
    output$fwd_curve_plot <- plotly::renderPlotly({
      shiny::req(fwd_data())
      snap <- input$fwd_date

      df <- fwd_data() |>
        dplyr::filter(date == snap) |>
        dplyr::arrange(tenor)

      # If exact date not available, use nearest prior date
      if (nrow(df) == 0) {
        available <- fwd_data() |>
          dplyr::filter(date <= snap) |>
          dplyr::pull(date) |>
          max()
        df <- fwd_data() |>
          dplyr::filter(date == available) |>
          dplyr::arrange(tenor)
      }

      shiny::validate(shiny::need(nrow(df) > 0, "No data for this date."))

      if (isTRUE(input$show_history)) {
        # Overlay 4 quarterly snapshots
        snap_dates <- seq(snap - 270, snap, by = 90) |> as.Date()
        df_all <- purrr::map_dfr(snap_dates, function(d) {
          available <- fwd_data() |>
            dplyr::filter(date <= d) |>
            dplyr::pull(date)
          if (length(available) == 0) return(NULL)
          fwd_data() |>
            dplyr::filter(date == max(available)) |>
            dplyr::mutate(snap_label = as.character(max(available)))
        })
        plotly::plot_ly(df_all,
          x     = ~tenor, y = ~value, color = ~snap_label,
          type  = "scatter", mode = "lines+markers",
          marker = list(size = 8)) |>
          plotly::layout(
            title  = list(text = paste("Forward Curves —", market_label(market())),
                          font = list(size = 13)),
            xaxis  = list(title = "Contract Month", tickvals = seq_len(as.integer(input$n_tenors))),
            yaxis  = list(title = "Settlement Price"),
            legend = list(orientation = "h", y = -0.2),
            paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
          ) |>
          plotly::config(displaylogo = FALSE)
      } else {
        plotly::plot_ly(df,
          x    = ~tenor, y = ~value,
          type = "scatter", mode = "lines+markers",
          line   = list(color = "#1a3a5c", width = 2),
          marker = list(size = 9, color = "#1a3a5c")) |>
          plotly::layout(
            title  = list(text = paste(market_label(market()), "—", format(snap, "%b %d, %Y")),
                          font = list(size = 13)),
            xaxis  = list(title = "Contract Month",
                          tickvals = df$tenor, ticktext = paste0("M", df$tenor)),
            yaxis  = list(title = "Settlement Price"),
            paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
          ) |>
          plotly::config(displaylogo = FALSE)
      }
    })

    # ── M1 minus M2 spread over time ─────────────────────────────────────────
    output$fwd_spread_plot <- plotly::renderPlotly({
      shiny::req(fwd_data())
      spread_df <- fwd_data() |>
        dplyr::filter(tenor %in% c(1, 2)) |>
        dplyr::select(date, tenor, value) |>
        tidyr::pivot_wider(names_from = tenor, values_from = value,
                           names_prefix = "M") |>
        dplyr::filter(!is.na(M1), !is.na(M2)) |>
        dplyr::mutate(spread = M1 - M2)

      shiny::validate(shiny::need(nrow(spread_df) > 0, "Spread data unavailable."))

      plotly::plot_ly(spread_df, x = ~date, y = ~spread,
        type = "scatter", mode = "lines",
        line = list(width = 1.2),
        color = ~ifelse(spread > 0, "Backwardation", "Contango"),
        colors = c("Backwardation" = "#d73027", "Contango" = "#1a9641")) |>
        plotly::add_hlines(y = 0, line = list(color = "grey", dash = "dot")) |>
        plotly::layout(
          title  = list(text = paste(market_label(market()), "— M1–M2 Spread"),
                        font = list(size = 13)),
          xaxis  = list(title = ""),
          yaxis  = list(title = "M1 minus M2 Price"),
          legend = list(orientation = "h", y = -0.2),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        plotly::config(displaylogo = FALSE)
    })

    # ── Forward curve heatmap (tenor × time) ────────────────────────────────
    output$fwd_heatmap <- plotly::renderPlotly({
      shiny::req(fwd_data())

      df <- fwd_data() |>
        dplyr::filter(tenor <= 6) |>
        dplyr::mutate(tenor_label = paste0("M", tenor))

      # Monthly aggregate to keep heatmap readable
      df_monthly <- df |>
        dplyr::mutate(ym = lubridate::floor_date(date, "month")) |>
        dplyr::group_by(ym, tenor_label) |>
        dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

      heatmap_mat <- df_monthly |>
        tidyr::pivot_wider(names_from = tenor_label, values_from = value)

      shiny::validate(shiny::need(nrow(heatmap_mat) > 0, "Not enough data."))

      tenor_cols <- paste0("M", 1:6)
      tenor_cols <- intersect(tenor_cols, names(heatmap_mat))
      z_mat <- t(as.matrix(heatmap_mat[, tenor_cols]))

      plotly::plot_ly(
        x         = heatmap_mat$ym,
        y         = tenor_cols,
        z         = z_mat,
        type      = "heatmap",
        colorscale = "RdBu",
        colorbar   = list(title = "Price")
      ) |>
        plotly::layout(
          title  = list(text = paste(market_label(market()), "— Curve Heatmap"),
                        font = list(size = 13)),
          xaxis  = list(title = ""),
          yaxis  = list(title = "Contract Month"),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        plotly::config(displaylogo = FALSE)
    })

  })
}
