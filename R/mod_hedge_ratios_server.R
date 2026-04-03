#' Hedge Ratios Module Server
#' @noRd
mod_hedge_ratios_server <- function(id, r, market, dates) {
  shiny::moduleServer(id, function(input, output, session) {

    # ── Returns for all markets in date range ────────────────────────────────
    all_returns <- shiny::reactive({
      shiny::req(r$loaded, r$data)
      dr <- dates()
      r$data$futures_returns |>
        dplyr::filter(date >= dr[1], date <= dr[2]) |>
        tidyr::drop_na()
    })

    # ── Rolling cross-market hedge ratio ────────────────────────────────────
    output$hedge_cross_plot <- plotly::renderPlotly({
      shiny::req(all_returns())
      m    <- market()
      inst <- input$hedge_instrument
      w    <- as.integer(input$hedge_window)

      shiny::validate(
        shiny::need(m != inst, "Select a different hedging instrument."),
        shiny::need(m %in% names(all_returns()), paste(m, "not available.")),
        shiny::need(inst %in% names(all_returns()), paste(inst, "not available."))
      )

      df <- all_returns() |>
        dplyr::select(date, x = dplyr::all_of(m), y = dplyr::all_of(inst)) |>
        tidyr::drop_na()

      shiny::validate(shiny::need(nrow(df) > w * 2, "Not enough data for this window."))

      roll_b <- rolling_beta_cpp(df$x, df$y, w)

      plot_df <- dplyr::tibble(date = df$date, beta = roll_b) |>
        dplyr::filter(!is.na(beta))

      plotly::plot_ly(plot_df, x = ~date, y = ~beta,
        type = "scatter", mode = "lines",
        line = list(color = "#1a3a5c", width = 1.5)) |>
        plotly::add_hlines(y = 1, line = list(color = "grey50", dash = "dot")) |>
        plotly::layout(
          title  = list(
            text = paste("Rolling", w, "d Hedge Ratio:", m, "\u2192", inst),
            font = list(size = 13)
          ),
          xaxis  = list(title = ""),
          yaxis  = list(title = paste("\u03b2 (hedge ratio)")),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        plotly::config(displaylogo = FALSE)
    })

    # ── Static cross-market hedge ratio panel ────────────────────────────────
    output$hedge_static_table <- shiny::renderUI({
      shiny::req(all_returns())
      m      <- market()
      others <- setdiff(c("CL", "BRN", "RB", "HO", "NG"), m)

      rows <- purrr::map(others, function(inst) {
        if (!inst %in% names(all_returns())) return(NULL)
        df <- all_returns() |>
          dplyr::select(x = dplyr::all_of(m), y = dplyr::all_of(inst)) |>
          tidyr::drop_na()
        if (nrow(df) < 30) return(NULL)
        fit  <- lm(x ~ y, data = df)
        beta <- round(coef(fit)[["y"]], 3)
        r2   <- round(summary(fit)$r.squared, 3)
        shiny::tags$li(
          shiny::strong(inst), ": \u03b2 = ", beta, ", R\u00b2 = ", r2
        )
      })

      shiny::tagList(
        shiny::tags$p(shiny::strong("Full-sample \u03b2 vs ", m)),
        shiny::tags$ul(rows)
      )
    })

    # ── Term structure hedge ratios ──────────────────────────────────────────
    output$hedge_term_plot <- plotly::renderPlotly({
      shiny::req(r$loaded, r$data)
      dr   <- dates()
      m    <- market()
      ref  <- as.integer(input$ts_hedge_ref)
      w    <- as.integer(input$ts_hedge_window)
      compare_tenors <- setdiff(1:5, ref)

      futures_wide_ts <- r$data$futures |>
        dplyr::filter(market == m, tenor %in% c(ref, compare_tenors),
                      date >= dr[1], date <= dr[2]) |>
        dplyr::group_by(tenor) |>
        dplyr::arrange(date) |>
        dplyr::mutate(log_ret = fct_log_returns(value)) |>
        dplyr::ungroup() |>
        dplyr::select(date, tenor, log_ret) |>
        tidyr::pivot_wider(names_from = tenor, values_from = log_ret,
                           names_prefix = "T") |>
        tidyr::drop_na()

      shiny::validate(shiny::need(nrow(futures_wide_ts) > w * 2, "Not enough data."))

      ref_col <- paste0("T", ref)
      shiny::validate(shiny::need(ref_col %in% names(futures_wide_ts), "Reference tenor not available."))

      plots <- purrr::map(compare_tenors, function(t) {
        t_col <- paste0("T", t)
        if (!t_col %in% names(futures_wide_ts)) return(NULL)
        roll_b <- rolling_beta_cpp(
          futures_wide_ts[[ref_col]],
          futures_wide_ts[[t_col]],
          w
        )
        dplyr::tibble(
          date  = futures_wide_ts$date,
          beta  = roll_b,
          label = paste0("M", ref, " vs M", t)
        )
      }) |> purrr::compact() |> dplyr::bind_rows()

      shiny::validate(shiny::need(nrow(plots) > 0 && any(!is.na(plots$beta)),
                                   "Could not compute term structure betas."))

      plotly::plot_ly(plots,
        x = ~date, y = ~beta, color = ~label,
        type = "scatter", mode = "lines",
        line = list(width = 1.5)) |>
        plotly::add_hlines(y = 1, line = list(color = "grey50", dash = "dot")) |>
        plotly::layout(
          title  = list(
            text = paste(market_label(m), "— Term Structure Hedge Ratios (ref: M", ref, ")"),
            font = list(size = 13)
          ),
          xaxis  = list(title = ""),
          yaxis  = list(title = "\u03b2 (hedge ratio)"),
          legend = list(orientation = "h", y = -0.2),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        plotly::config(displaylogo = FALSE)
    })

    # ── Full-sample OLS summary table ────────────────────────────────────────
    output$hedge_summary_table <- DT::renderDataTable({
      shiny::req(all_returns())
      m      <- market()
      others <- setdiff(c("CL", "BRN", "RB", "HO", "NG"), m)

      rows <- purrr::map_dfr(others, function(inst) {
        if (!inst %in% names(all_returns())) return(NULL)
        df <- all_returns() |>
          dplyr::select(x = dplyr::all_of(m), y = dplyr::all_of(inst)) |>
          tidyr::drop_na()
        if (nrow(df) < 30) return(NULL)
        fit  <- lm(x ~ y, data = df)
        sm   <- summary(fit)
        dplyr::tibble(
          Market     = inst,
          Beta       = round(coef(fit)[["y"]], 4),
          Std_Error  = round(sm$coefficients["y", "Std. Error"], 4),
          R_Squared  = round(sm$r.squared, 4),
          N_obs      = nrow(df)
        )
      })

      shiny::validate(shiny::need(nrow(rows) > 0, "No data for summary table."))

      DT::datatable(rows,
        rownames  = FALSE,
        options   = list(dom = "t", pageLength = 10),
        caption   = paste("Full-sample OLS hedge ratios: X =", m, "/ Y = hedging instrument")
      ) |>
        DT::formatRound(c("Beta", "Std_Error", "R_Squared"), digits = 4)
    })

  })
}
