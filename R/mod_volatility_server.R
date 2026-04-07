#' Volatility Module Server
#' @noRd
mod_volatility_server <- function(id, r, market, dates) {
  shiny::moduleServer(id, function(input, output, session) {

    # ── Insight card ─────────────────────────────────────────────────────────
    output$vol_insight <- shiny::renderUI({
      shiny::req(r$loaded, r$data)
      w <- as.integer(input$vol_window)
      df <- fct_filter_futures(r$data, mkt = market(), dates(), tenors = 1) |>
        dplyr::arrange(date) |>
        dplyr::mutate(lr = fct_log_returns(value),
                      rv = rolling_vol_cpp(tidyr::replace_na(lr, 0), w)) |>
        dplyr::filter(!is.na(rv))
      shiny::validate(shiny::need(nrow(df) > 20, ""))
      cur  <- tail(df$rv, 1)
      med  <- median(df$rv, na.rm = TRUE)
      pct  <- round(mean(df$rv <= cur, na.rm = TRUE) * 100)
      hi   <- df$date[which.max(df$rv)]
      color <- if (cur > med) "#d73027" else "#1a9641"
      shiny::div(class="p-2 mb-2 rounded", style=paste0("border-left:4px solid ",color,"; background:#f8f9fa;"),
        shiny::HTML(paste0(
          "<strong>", market_label(market()), "</strong> front-month realized vol is currently <strong>",
          scales::percent(cur, accuracy=0.1), "</strong> (", w, "-day window), ",
          "at the <strong>", pct, "th percentile</strong> of history. ",
          "Median historical vol is ", scales::percent(med, accuracy=0.1), ". ",
          "Peak vol occurred around <strong>", format(hi, "%b %Y"), "</strong>."
        ))
      )
    })

    # ── Reactive: returns for selected market ────────────────────────────────
    vol_returns <- shiny::reactive({
      shiny::req(r$loaded, r$data)
      window  <- as.integer(input$vol_window)
      tenor   <- as.integer(input$vol_tenor)
      show_all <- isTRUE(input$show_all_tenors)

      tenors_to_use <- if (show_all) 1:6 else tenor

      fct_filter_futures(r$data, mkt = market(), dates(), tenors = tenors_to_use) |>
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

      # Compute spike threshold (90th percentile of front-month vol)
      m1_vol    <- df |> dplyr::filter(tenor == min(tenor, na.rm = TRUE))
      threshold <- quantile(m1_vol$roll_vol, 0.90, na.rm = TRUE)
      date_min  <- min(m1_vol$date); date_max <- max(m1_vol$date)

      # Auto-shade contiguous periods above threshold
      spike_shapes <- if (nrow(m1_vol) > 0) {
        is_spike <- m1_vol$roll_vol >= threshold
        runs <- rle(is_spike)
        ends   <- cumsum(runs$lengths)
        starts <- c(1L, head(ends, -1L) + 1L)
        purrr::map(which(runs$values), function(i) list(
          type = "rect", xref = "x", yref = "paper",
          x0 = m1_vol$date[starts[i]], x1 = m1_vol$date[ends[i]],
          y0 = 0, y1 = 1,
          fillcolor = "rgba(215,48,39,0.10)", line = list(width = 0), layer = "below"
        ))
      } else list()

      # Named market events — vertical lines + labels
      known_events <- list(
        list(date = as.Date("2008-09-15"), label = "GFC '08"),
        list(date = as.Date("2010-04-20"), label = "Deepwater Horizon"),
        list(date = as.Date("2011-03-01"), label = "Arab Spring"),
        list(date = as.Date("2014-11-27"), label = "OPEC No Cut"),
        list(date = as.Date("2018-11-12"), label = "Iran Sanctions"),
        list(date = as.Date("2020-03-09"), label = "COVID / Oil War"),
        list(date = as.Date("2022-02-24"), label = "Russia-Ukraine"),
        list(date = as.Date("2025-04-01"), label = "US-Iran '25")
      )
      events_in_range <- purrr::keep(known_events,
        ~ .x$date >= date_min & .x$date <= date_max)

      event_shapes <- purrr::map(events_in_range, function(e) list(
        type = "line", xref = "x", yref = "paper",
        x0 = e$date, x1 = e$date, y0 = 0, y1 = 1,
        line = list(color = "rgba(60,60,60,0.35)", dash = "dashdot", width = 1.2)
      ))
      event_annotations <- purrr::imap(events_in_range, function(e, i) list(
        x = e$date, xref = "x",
        y = if (i %% 2 == 1) 0.99 else 0.88,   # alternate height to avoid overlap
        yref = "paper",
        text = paste0("<b>", e$label, "</b>"),
        showarrow = FALSE, textangle = -45,
        xanchor = "left", yanchor = "top",
        font = list(size = 9, color = "#444"),
        bgcolor = "rgba(255,255,255,0.7)",
        borderpad = 2
      ))

      all_shapes <- c(spike_shapes, event_shapes)

      plotly::plot_ly(df,
        x = ~date, y = ~roll_vol, color = ~tenor_label,
        type = "scatter", mode = "lines",
        line = list(width = 1.5),
        hovertemplate = paste0(
          "<b>%{x|%b %d, %Y}</b><br>",
          "Realized Vol: <b>%{y:.1%}</b> annualized<br>",
          "<i style='color:#888'>Rolling ", input$vol_window, "-day window on log returns, ",
          "scaled by \u221a252 to annualize.<br>",
          "High vol = larger daily price swings, wider bid-ask spreads.</i>",
          "<extra>%{fullData.name}</extra>"
        )) |>
        plotly::layout(
          title  = list(text = paste(market_label(market()),
                                     "— Rolling Vol (", input$vol_window, "d)"),
                        font = list(size = 13)),
          xaxis  = list(title = ""),
          yaxis  = list(title = "Annualized Volatility", tickformat = ".1%"),
          shapes      = all_shapes,
          annotations = event_annotations,
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

      df <- fct_filter_futures(r$data, mkt = market(), dates(), tenors = 1:6) |>
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
