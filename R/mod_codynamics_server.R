#' Co-Dynamics Module Server
#' @noRd
mod_codynamics_server <- function(id, r, market, dates) {
  shiny::moduleServer(id, function(input, output, session) {

    # ── Insight card ─────────────────────────────────────────────────────────
    output$cody_insight <- shiny::renderUI({
      shiny::req(r$loaded, r$data)
      m   <- market()
      dr  <- dates()
      ret <- r$data$futures_returns |>
        dplyr::filter(date >= dr[1], date <= dr[2]) |>
        dplyr::select(-date) |> tidyr::drop_na()
      shiny::validate(shiny::need(nrow(ret) > 50 && m %in% names(ret), ""))
      others <- setdiff(names(ret), m)
      cors   <- sapply(others, function(o) cor(ret[[m]], ret[[o]], use="complete.obs"))
      top    <- names(which.max(cors)); low <- names(which.min(cors))
      shiny::div(class="p-2 mb-2 rounded", style="border-left:4px solid #1a3a5c; background:#f8f9fa;",
        shiny::HTML(paste0(
          "<strong>", market_label(m), "</strong> is most correlated with <strong>",
          top, "</strong> (r = ", round(max(cors),2), ") and least with <strong>",
          low, "</strong> (r = ", round(min(cors),2), ") over the selected period. ",
          "Low correlation markets offer diversification; high correlation markets are natural hedge candidates."
        ))
      )
    })

    # ── Update rolling-corr pair1 when selected market changes ──────────────
    shiny::observe({
      m      <- market()
      others <- setdiff(c("CL", "BRN", "RB", "HO", "NG"), m)
      shiny::updateSelectInput(session, "roll_cor_pair1", selected = m)
      shiny::updateSelectInput(session, "roll_cor_pair2", selected = others[1])
    })

    # ── Reactive: returns wide within date range ─────────────────────────────
    returns_wide <- shiny::reactive({
      shiny::req(r$loaded, r$data)
      dr <- dates()

      df <- r$data$futures_returns |>
        dplyr::filter(date >= dr[1], date <= dr[2])

      # Period filter for correlation tab
      if (!is.null(input$cor_period) && input$cor_period != "full") {
        n_years <- as.numeric(sub("y", "", input$cor_period))
        df <- df |> dplyr::filter(date >= Sys.Date() - n_years * 365)
      }
      df
    })

    prices_wide <- shiny::reactive({
      shiny::req(r$loaded, r$data)
      dr <- dates()
      r$data$futures_front |>
        dplyr::filter(date >= dr[1], date <= dr[2])
    })

    # ── Correlation heatmap ──────────────────────────────────────────────────
    output$cor_heatmap <- plotly::renderPlotly({
      shiny::req(returns_wide())
      use_df <- if (isTRUE(input$cor_type == "prices")) {
        prices_wide() |> dplyr::select(-date)
      } else {
        returns_wide() |> dplyr::select(-date)
      }

      # Keep only markets with enough data
      use_df <- use_df[, colSums(!is.na(use_df)) > 100, drop = FALSE]
      shiny::validate(shiny::need(ncol(use_df) >= 2, "Need at least 2 markets."))

      cor_mat <- cor(use_df, use = "pairwise.complete.obs")
      plotly_corr_heatmap(cor_mat,
        title = paste0("Return Correlations (", input$cor_period, ")"))
    })

    # ── Rolling correlations ─────────────────────────────────────────────────
    output$rolling_cor_plot <- plotly::renderPlotly({
      shiny::req(returns_wide(), input$roll_cor_pair1, input$roll_cor_pair2)
      shiny::validate(
        shiny::need(input$roll_cor_pair1 != input$roll_cor_pair2, "Select two different markets.")
      )

      m1 <- input$roll_cor_pair1
      m2 <- input$roll_cor_pair2
      w  <- as.integer(input$roll_cor_window)

      df <- returns_wide() |>
        dplyr::select(date, dplyr::all_of(c(m1, m2))) |>
        tidyr::drop_na()

      shiny::validate(shiny::need(nrow(df) > w * 2, "Not enough data for this window."))

      x_vec <- df[[m1]]
      y_vec <- df[[m2]]

      roll_cor <- slider::slide_dbl(
        seq_len(nrow(df)),
        ~ {
          i <- .x
          if (length(i) < 10) return(NA_real_)
          cor(x_vec[i], y_vec[i], use = "complete.obs")
        },
        .before = w - 1, .complete = TRUE
      )

      plot_df <- dplyr::tibble(date = df$date, roll_cor = roll_cor)

      plotly::plot_ly(plot_df, x = ~date, y = ~roll_cor,
        type = "scatter", mode = "lines",
        line = list(color = "#1a3a5c", width = 1.5)) |>
        plotly::layout(shapes = list(list(type="line",x0=0,x1=1,xref="paper",y0=0,y1=0,line=list(color="grey50",dash="dot",width=1)))) |>
        plotly::layout(
          title  = list(text = paste("Rolling", w, "d Correlation:", m1, "vs", m2),
                        font = list(size = 13)),
          xaxis  = list(title = ""),
          yaxis  = list(title = "Correlation", range = c(-1, 1)),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        plotly::config(displaylogo = FALSE)
    })

    # ── PCA factor interpretation ────────────────────────────────────────────
    output$pca_interpretation <- shiny::renderUI({
      shiny::req(returns_wide())
      mat <- returns_wide() |> dplyr::select(-date) |> tidyr::drop_na()
      shiny::validate(shiny::need(nrow(mat) > 10, ""))

      pca  <- prcomp(mat, scale. = TRUE)
      pvar <- round((pca$sdev^2 / sum(pca$sdev^2)) * 100, 1)

      top_pc1 <- names(sort(abs(pca$rotation[, 1]), decreasing = TRUE))[1:2]
      top_pc2 <- names(sort(abs(pca$rotation[, 2]), decreasing = TRUE))[1:2]

      pc1_same_sign <- mean(pca$rotation[, 1] > 0) > 0.8 || mean(pca$rotation[, 1] < 0) > 0.8
      pc1_label <- if (pc1_same_sign) "Overall Market Level" else "Long-Short Spread"

      shiny::div(
        class = "p-2 mt-2 rounded",
        style = "background:#f8f9fa; border-left:4px solid #1a3a5c; font-size:0.85rem;",
        shiny::HTML(paste0(
          "<strong>PC1</strong> explains <strong>", pvar[1], "%</strong> of variance \u2014 ",
          "the <em>", pc1_label, "</em> factor. ",
          "Dominant markets: <strong>", paste(top_pc1, collapse = ", "), "</strong>. ",
          "When PC1 shifts, most energy markets move together (macro/USD/risk-sentiment driven).<br>",
          "<strong>PC2</strong> explains <strong>", pvar[2], "%</strong> \u2014 ",
          "the <em>Relative Value</em> factor. ",
          "Dominant markets: <strong>", paste(top_pc2, collapse = ", "), "</strong>. ",
          "PC2 captures divergence between crude and gas \u2014 useful for spread trades and cross-market hedging."
        ))
      )
    })

    # ── PCA scree plot ───────────────────────────────────────────────────────
    output$pca_scree <- plotly::renderPlotly({
      shiny::req(returns_wide())
      mat <- returns_wide() |>
        dplyr::select(-date) |>
        tidyr::drop_na()
      shiny::validate(shiny::need(nrow(mat) > 10, "Not enough data for PCA."))

      pca  <- prcomp(mat, scale. = TRUE)
      pvar <- (pca$sdev^2 / sum(pca$sdev^2)) * 100
      df_scree <- dplyr::tibble(
        PC = paste0("PC", seq_along(pvar)),
        var_pct = pvar,
        cum_pct = cumsum(pvar)
      )

      plotly::plot_ly() |>
        plotly::add_bars(data = df_scree, x = ~PC, y = ~var_pct,
                         name = "Individual", marker = list(color = "#1a3a5c")) |>
        plotly::add_lines(data = df_scree, x = ~PC, y = ~cum_pct,
                          name = "Cumulative", yaxis = "y2",
                          line = list(color = "#d73027", width = 2)) |>
        plotly::layout(
          title   = list(text = "PCA Scree Plot", font = list(size = 13)),
          xaxis   = list(title = "Principal Component"),
          yaxis   = list(title = "Variance Explained (%)"),
          yaxis2  = list(title = "Cumulative (%)", overlaying = "y", side = "right",
                         range = c(0, 105)),
          legend  = list(orientation = "h", y = -0.2),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        plotly::config(displaylogo = FALSE)
    })

    # ── PCA biplot ───────────────────────────────────────────────────────────
    output$pca_biplot <- plotly::renderPlotly({
      shiny::req(returns_wide())
      mat <- returns_wide() |>
        dplyr::select(-date) |>
        tidyr::drop_na()
      shiny::validate(shiny::need(nrow(mat) > 10, "Not enough data for PCA."))

      pca      <- prcomp(mat, scale. = TRUE)
      loadings <- as.data.frame(pca$rotation[, 1:2])
      loadings$market <- rownames(loadings)

      plotly::plot_ly(loadings,
        x = ~PC1, y = ~PC2, text = ~market,
        type = "scatter", mode = "markers+text",
        textposition = "top center",
        marker = list(size = 14, color = "#1a3a5c")) |>
        plotly::add_annotations(
          x = 0, y = 0, xref = "x", yref = "y",
          text = "", showarrow = FALSE) |>
        plotly::layout(
          title  = list(text = "PCA Factor Loadings (PC1 vs PC2)", font = list(size = 13)),
          xaxis  = list(title = paste0("PC1 (", round((pca$sdev[1]^2 / sum(pca$sdev^2)) * 100, 1), "% var)")),
          yaxis  = list(title = paste0("PC2 (", round((pca$sdev[2]^2 / sum(pca$sdev^2)) * 100, 1), "% var)")),
          shapes = list(
            list(type = "line", x0 = -1, x1 = 1, y0 = 0, y1 = 0,
                 line = list(dash = "dot", color = "grey")),
            list(type = "line", x0 = 0, x1 = 0, y0 = -1, y1 = 1,
                 line = list(dash = "dot", color = "grey"))
          ),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        plotly::config(displaylogo = FALSE)
    })

  })
}
