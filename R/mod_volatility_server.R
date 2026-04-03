#' Volatility Module Server
#' @noRd
mod_volatility_server <- function(id, r, market, dates) {
  shiny::moduleServer(id, function(input, output, session) {

    # ── Reactive: returns for selected market ────────────────────────────────
    vol_returns <- shiny::reactive({
      shiny::req(r$loaded, r$data)
      window  <- as.integer(input$vol_window)
      tenor   <- as.integer(input$vol_tenor)
      show_all <- isTRUE(input$show_all_tenors)

      tenors_to_use <- if (show_all) 1:6 else tenor

      fct_filter_futures(r$data, market(), dates(), tenors = tenors_to_use) |>
        dplyr::group_by(tenor) |>
        dplyr::arrange(date) |>
        dplyr::mutate(
          log_ret = fct_log_returns(value),
          roll_vol = rolling_vol_cpp(tidyr::replace_na(log_ret, 0), window)
        ) |>
        dplyr::ungroup()
    })

    # ── Rolling vol time-series ──────────────────────────────────────────────
    output$vol_ts_plot <- plotly::renderPlotly({
      shiny::req(vol_returns())
      df <- vol_returns() |>
        dplyr::filter(!is.na(roll_vol)) |>
        dplyr::mutate(tenor_label = paste0("M", tenor))

      shiny::validate(shiny::need(nrow(df) > 0, "Insufficient data for window size."))

      plotly::plot_ly(df,
        x = ~date, y = ~roll_vol, color = ~tenor_label,
        type = "scatter", mode = "lines",
        line = list(width = 1.5)) |>
        plotly::layout(
          title  = list(text = paste(market_label(market()),
                                     "— Rolling Vol (", input$vol_window, "d)"),
                        font = list(size = 13)),
          xaxis  = list(title = ""),
          yaxis  = list(title = "Annualized Volatility", tickformat = ".1%"),
          legend = list(orientation = "h", y = -0.2),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        plotly::config(displaylogo = FALSE)
    })

    # ── Volatility term structure (avg vol by tenor) ─────────────────────────
    output$vol_term_plot <- plotly::renderPlotly({
      shiny::req(vol_returns())

      df_term <- vol_returns() |>
        dplyr::filter(!is.na(roll_vol)) |>
        dplyr::group_by(tenor) |>
        dplyr::summarise(
          avg_vol = mean(roll_vol, na.rm = TRUE),
          med_vol = median(roll_vol, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::mutate(tenor_label = paste0("M", tenor))

      shiny::validate(shiny::need(nrow(df_term) > 0, "No term structure data."))

      plotly::plot_ly(df_term,
        x = ~tenor_label, y = ~avg_vol,
        type = "bar",
        marker = list(color = "#1a3a5c"),
        error_y = list(
          type       = "data",
          array      = df_term$med_vol - df_term$avg_vol,
          visible    = FALSE
        )) |>
        plotly::layout(
          title  = list(text = "Vol Term Structure (average)", font = list(size = 13)),
          xaxis  = list(title = "Contract Month", categoryorder = "array",
                        categoryarray = paste0("M", 1:6)),
          yaxis  = list(title = "Avg Annualized Vol", tickformat = ".1%"),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        plotly::config(displaylogo = FALSE)
    })

    # ── Volatility surface heatmap ───────────────────────────────────────────
    output$vol_surface <- plotly::renderPlotly({
      shiny::req(r$loaded, r$data)
      window <- as.integer(input$vol_window)

      df <- fct_filter_futures(r$data, market(), dates(), tenors = 1:6) |>
        dplyr::group_by(tenor) |>
        dplyr::arrange(date) |>
        dplyr::mutate(
          log_ret  = fct_log_returns(value),
          roll_vol = rolling_vol_cpp(tidyr::replace_na(log_ret, 0), window)
        ) |>
        dplyr::ungroup() |>
        dplyr::filter(!is.na(roll_vol)) |>
        dplyr::mutate(
          ym           = lubridate::floor_date(date, "month"),
          tenor_label  = paste0("M", tenor)
        ) |>
        dplyr::group_by(ym, tenor_label) |>
        dplyr::summarise(vol = mean(roll_vol, na.rm = TRUE), .groups = "drop")

      surface_wide <- df |>
        tidyr::pivot_wider(names_from = tenor_label, values_from = vol)

      tenor_cols <- intersect(paste0("M", 1:6), names(surface_wide))
      z_mat <- t(as.matrix(surface_wide[, tenor_cols]))

      plotly::plot_ly(
        x = surface_wide$ym, y = tenor_cols, z = z_mat,
        type      = "heatmap",
        colorscale = "YlOrRd",
        colorbar   = list(title = "Ann. Vol"),
        zauto      = TRUE
      ) |>
        plotly::layout(
          title  = list(text = paste(market_label(market()), "— Volatility Surface"),
                        font = list(size = 13)),
          xaxis  = list(title = ""),
          yaxis  = list(title = "Contract Month"),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        plotly::config(displaylogo = FALSE)
    })

  })
}
