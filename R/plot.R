# Utils
check_class <- function(x, object) {
  if (any(!x %in% class(object))) stop(sprintf("Input object must be class %s", x))
}

# Enable parallelisation of main modelling loop for MacOS and Linux only
check_parallel <- function() "unix" %in% .Platform$OS.type
# ggplot doesn't work with process forking on MacOS; however it works fine on Linux and Windows
check_parallel_plot <- function() !"Darwin" %in% Sys.info()["sysname"]

# Suggestions
####################################################################
#' [Suggestion included] robyn_onepagers
#'
#' Suggestion included in (63:105). Original version is in plot.R
#' Suggestion: Message of Convergence on plot.
#' Reason: Practical suggestion for checking convergence
#' on the resulted plots, (not only logs)
#'
#' @param InputCollect TODO::Type. TODO::Description
#' @param OutputCollect TODO::Type. TODO::Description
#' @param select_model = Null TODO::Type. TODO::Description
#' @param quiet = False TODO::Type. TODO::Description
#' @param export = True TODO::Type. TODO::Description
#' @return List(). Contains list of hyperparameters.
#' @export
robyn_onepagers <- function(InputCollect,
                            OutputCollect,
                            select_model = NULL,
                            quiet = FALSE,
                            export = TRUE) {
  check_class("robyn_outputs", OutputCollect)
  if (TRUE) {
    pareto_fronts <- OutputCollect$pareto_fronts
    hyper_fixed <- OutputCollect$hyper_fixed
    resultHypParam <- as_tibble(OutputCollect$resultHypParam)
    xDecompAgg <- as_tibble(OutputCollect$xDecompAgg)
    sid <- NULL # for parallel loops
  }
  if (!is.null(select_model)) {
    if ("clusters" %in% select_model) select_model <- OutputCollect$clusters$models$solID
    resultHypParam <- resultHypParam[resultHypParam$solID %in% select_model, ]
    xDecompAgg <- xDecompAgg[xDecompAgg$solID %in% select_model, ]
    if (!quiet) message(">> Generating only cluster results one-pagers (", nrow(resultHypParam), ")...")
  }

  # Prepare for parallel plotting
  if (check_parallel_plot()) registerDoParallel(OutputCollect$cores) else registerDoSEQ()
  if (!hyper_fixed) {
    pareto_fronts_vec <- 1:pareto_fronts
    count_mod_out <- nrow(resultHypParam[resultHypParam$robynPareto %in% pareto_fronts_vec, ])
  } else {
    pareto_fronts_vec <- 1
    count_mod_out <- nrow(resultHypParam)
  }
  all_fronts <- unique(xDecompAgg$robynPareto)
  all_fronts <- sort(all_fronts[!is.na(all_fronts)])
  if (!all(pareto_fronts_vec %in% all_fronts)) pareto_fronts_vec <- all_fronts

  if (check_parallel_plot()) {
    if (!quiet) message(paste(">> Plotting", count_mod_out, "selected models on", OutputCollect$cores, "cores..."))
  } else {
    if (!quiet) message(paste(">> Plotting", count_mod_out, "selected models on 1 core (MacOS fallback)..."))
  }

  if (!quiet && count_mod_out > 0) {
    pbplot <- txtProgressBar(min = 0, max = count_mod_out, style = 3)
  }
  temp <- OutputCollect$allPareto$plotDataCollect
  all_plots <- list()
  cnt <- 0

  for (pf in pareto_fronts_vec) { # pf = pareto_fronts_vec[1]
    plotMediaShare <- filter(
      xDecompAgg, .data$robynPareto == pf,
      .data$rn %in% InputCollect$paid_media_spends
    )
    uniqueSol <- unique(plotMediaShare$solID)

    # parallelResult <- for (sid in uniqueSol) { # sid = uniqueSol[1]

    ######## [!!! Suggestion: Message of Convergence on plot ] ###########
    # Reason: practical suggestion for checking convergence
    # on the resulted plots, (not only logs)
    convergence <- list()
    OutputModels$convergence$conv_msg[1]

    if (str_detect(OutputModels$convergence$conv_msg[1], "not")) {
      convergence[1] <- "not converged"
    } else {
      convergence[1] <- "converged"
    }

    if (str_detect(OutputModels$convergence$conv_msg[2], "not")) {
      convergence[2] <- "not converged"
    } else {
      convergence[2] <- "converged"
    }

    parallelResult <- foreach(sid = uniqueSol) %dorng% { # sid = uniqueSol[1]
      plotMediaShareLoop <- plotMediaShare[plotMediaShare$solID == sid, ]
      rsq_train_plot <- round(plotMediaShareLoop$rsq_train[1], 4)
      nrmse_plot <- round(plotMediaShareLoop$nrmse[1], 4)
      decomp_rssd_plot <- round(plotMediaShareLoop$decomp.rssd[1], 4)
      mape_lift_plot <- ifelse(!is.null(InputCollect$calibration_input),
        round(plotMediaShareLoop$mape[1], 4), NA
      )
      errors <- paste0(
        "R2 train: ", rsq_train_plot,
        ", NRMSE = ", nrmse_plot, " (", convergence[2], ")",
        ", DECOMP.RSSD = ", decomp_rssd_plot, " (", convergence[1], ")",
        ifelse(!is.na(mape_lift_plot), paste0(", MAPE = ", mape_lift_plot), "")
      )

      errors <- paste0(
        "R2 train: ", rsq_train_plot,
        ", NRMSE = ", nrmse_plot, " (", convergence[2], ")",
        ", DECOMP.RSSD = ", decomp_rssd_plot, " (", convergence[1], ")",
        ifelse(!is.na(mape_lift_plot), paste0(", MAPE = ", mape_lift_plot), "")
      )
      ##################### [End of Suggestion.] ##################

      ## 1. Spend x effect share comparison
      plotMediaShareLoopBar <- temp[[sid]]$plot1data$plotMediaShareLoopBar
      plotMediaShareLoopLine <- temp[[sid]]$plot1data$plotMediaShareLoopLine
      ySecScale <- temp[[sid]]$plot1data$ySecScale
      plotMediaShareLoopBar$variable <- stringr::str_to_title(gsub("_", " ", plotMediaShareLoopBar$variable))
      type <- ifelse(InputCollect$dep_var_type == "conversion", "CPA", "ROI")
      plotMediaShareLoopLine$type_colour <- type_colour <- "#03396C"
      names(type_colour) <- "type_colour"
      p1 <- ggplot(plotMediaShareLoopBar, aes(x = .data$rn, y = .data$value, fill = .data$variable)) +
        geom_bar(stat = "identity", width = 0.5, position = "dodge") +
        geom_text(aes(y = 0, label = paste0(round(.data$value * 100, 1), "%")),
          hjust = -.1, position = position_dodge(width = 0.5), fontface = "bold"
        ) +
        geom_line(
          data = plotMediaShareLoopLine, aes(
            x = .data$rn, y = .data$value / ySecScale, group = 1
          ),
          color = type_colour, inherit.aes = FALSE
        ) +
        geom_point(
          data = plotMediaShareLoopLine, aes(
            x = .data$rn, y = .data$value / ySecScale, group = 1, color = type_colour
          ),
          inherit.aes = FALSE, size = 3.5
        ) +
        geom_text(
          data = plotMediaShareLoopLine, aes(
            label = round(.data$value, 2), x = .data$rn, y = .data$value / ySecScale, group = 1
          ),
          color = type_colour, fontface = "bold", inherit.aes = FALSE, hjust = -.4, size = 4
        ) +
        scale_y_percent() +
        coord_flip() +
        theme_lares(axis.text.x = element_blank(), legend = "top", grid = "Xx") +
        scale_fill_brewer(palette = 3) +
        scale_color_identity(guide = "legend", labels = type) +
        labs(
          title = paste0("Share of Spend VS Share of Effect with total ", type),
          y = "Total Share by Channel", x = NULL, fill = NULL, color = NULL
        )

      ## 2. Waterfall
      plotWaterfallLoop <- temp[[sid]]$plot2data$plotWaterfallLoop
      p2 <- suppressWarnings(
        ggplot(plotWaterfallLoop, aes(x = .data$id, fill = .data$sign)) +
          geom_rect(aes(
            x = .data$rn, xmin = .data$id - 0.45, xmax = .data$id + 0.45,
            ymin = .data$end, ymax = .data$start
          ), stat = "identity") +
          scale_x_discrete("", breaks = levels(plotWaterfallLoop$rn), labels = plotWaterfallLoop$rn) +
          scale_y_percent() +
          scale_fill_manual(values = c("Positive" = "#59B3D2", "Negative" = "#E5586E")) +
          theme_lares(legend = "top") +
          geom_text(mapping = aes(
            label = paste0(
              formatNum(.data$xDecompAgg, abbr = TRUE),
              "\n", round(.data$xDecompPerc * 100, 1), "%"
            ),
            y = rowSums(cbind(.data$end, .data$xDecompPerc / 2))
          ), fontface = "bold", lineheight = .7) +
          coord_flip() +
          labs(
            title = "Response Decomposition Waterfall by Predictor",
            x = NULL, y = NULL, fill = "Sign"
          )
      )

      ## 3. Adstock rate
      if (InputCollect$adstock == "geometric") {
        dt_geometric <- temp[[sid]]$plot3data$dt_geometric
        p3 <- ggplot(dt_geometric, aes(x = .data$channels, y = .data$thetas, fill = "coral")) +
          geom_bar(stat = "identity", width = 0.5) +
          theme_lares(legend = "none", grid = "Xx") +
          coord_flip() +
          geom_text(aes(label = formatNum(100 * .data$thetas, 1, pos = "%")),
            hjust = -.1, position = position_dodge(width = 0.5), fontface = "bold"
          ) +
          scale_y_percent(limit = c(0, 1)) +
          labs(
            title = "Geometric Adstock: Fixed Rate Over Time",
            y = sprintf("Thetas [by %s]", InputCollect$intervalType), x = NULL
          )
      }
      if (InputCollect$adstock %in% c("weibull_cdf", "weibull_pdf")) {
        weibullCollect <- temp[[sid]]$plot3data$weibullCollect
        wb_type <- temp[[sid]]$plot3data$wb_type
        p3 <- ggplot(weibullCollect, aes(x = .data$x, y = .data$decay_accumulated)) +
          geom_line(aes(color = .data$channel)) +
          facet_wrap(~ .data$channel) +
          geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
          geom_text(aes(x = max(.data$x), y = 0.5, vjust = -0.5, hjust = 1, label = "Halflife"), colour = "gray") +
          theme_lares(legend = "none", grid = "Xx") +
          labs(
            title = paste("Weibull", wb_type, "Adstock: Flexible Rate Over Time"),
            x = sprintf("Time unit [%ss]", InputCollect$intervalType), y = NULL
          )
      }

      ## 4. Response curves
      dt_scurvePlot <- temp[[sid]]$plot4data$dt_scurvePlot
      dt_scurvePlotMean <- temp[[sid]]$plot4data$dt_scurvePlotMean
      trim_rate <- 1.3 # maybe enable as a parameter
      if (trim_rate > 0) {
        dt_scurvePlot <- dt_scurvePlot %>%
          filter(
            .data$spend < max(dt_scurvePlotMean$mean_spend) * trim_rate,
            .data$response < max(dt_scurvePlotMean$mean_response) * trim_rate
          )
      }
      if (!"channel" %in% colnames(dt_scurvePlotMean)) {
        dt_scurvePlotMean$channel <- dt_scurvePlotMean$rn
      }
      p4 <- ggplot(
        dt_scurvePlot[dt_scurvePlot$channel %in% InputCollect$paid_media_spends, ],
        aes(x = .data$spend, y = .data$response, color = .data$channel)
      ) +
        geom_line() +
        geom_point(data = dt_scurvePlotMean, aes(
          x = .data$mean_spend, y = .data$mean_response, color = .data$channel
        )) +
        geom_text(
          data = dt_scurvePlotMean, aes(
            x = .data$mean_spend, y = .data$mean_response, color = .data$channel,
            label = formatNum(.data$mean_spend, 2, abbr = TRUE)
          ),
          show.legend = FALSE, hjust = -0.2
        ) +
        theme_lares(pal = 2) +
        theme(
          legend.position = c(0.9, 0.2),
          legend.background = element_rect(fill = alpha("grey98", 0.6), color = "grey90")
        ) +
        labs(
          title = "Response Curves and Mean Spends by Channel",
          x = "Spend", y = "Response", color = NULL
        ) +
        scale_y_abbr() +
        scale_x_abbr()

      ## 5. Fitted vs actual
      xDecompVecPlotMelted <- temp[[sid]]$plot5data$xDecompVecPlotMelted %>%
        mutate(
          linetype = ifelse(.data$variable == "predicted", "solid", "dotted"),
          variable = stringr::str_to_title(.data$variable),
          ds = as.Date(.data$ds, origin = "1970-01-01")
        )
      # rsq <- temp[[sid]]$plot5data$rsq
      p5 <- ggplot(
        xDecompVecPlotMelted,
        aes(x = .data$ds, y = .data$value, color = .data$variable)
      ) +
        geom_path(aes(linetype = .data$linetype), size = 0.6) +
        theme_lares(legend = "top", pal = 2) +
        scale_y_abbr() +
        guides(linetype = "none") +
        labs(
          title = "Actual vs. Predicted Response",
          # subtitle = paste("Train R2 =", round(rsq, 4)),
          x = "Date", y = "Response", color = NULL
        )

      ## 6. Diagnostic: fitted vs residual
      xDecompVecPlot <- temp[[sid]]$plot6data$xDecompVecPlot
      p6 <- qplot(x = .data$predicted, y = .data$actual - .data$predicted, data = xDecompVecPlot) +
        geom_hline(yintercept = 0) +
        geom_smooth(se = TRUE, method = "loess", formula = "y ~ x") +
        scale_x_abbr() + scale_y_abbr() +
        theme_lares() +
        labs(x = "Fitted", y = "Residual", title = "Fitted vs. Residual")

      ## 7. Immediate vs carryover
      df_imme_caov <- temp[[sid]]$plot7data
      p7 <- df_imme_caov %>%
        mutate(type = factor(.data$type, levels = c("Carryover", "Immediate"))) %>%
        ggplot(aes(
          x = .data$percentage, y = .data$channels, fill = reorder(.data$type, as.integer(.data$type)),
          label = paste0(round(.data$percentage * 100), "%")
        )) +
        geom_bar(stat = "identity", width = 0.5) +
        geom_text(position = position_stack(vjust = 0.5)) +
        scale_fill_manual(values = c("Immediate" = "#59B3D2", "Carryover" = "coral")) +
        scale_x_percent() +
        theme_lares(legend = "top", grid = "Xx") +
        labs(
          x = "% Response", y = NULL, fill = NULL,
          title = "Immediate vs. Carryover Response Percentage"
        )

      ## 8. Bootstrapped ROI/CPA with CIs
      if ("ci_low" %in% colnames(xDecompAgg)) {
        metric <- ifelse(InputCollect$dep_var_type == "conversion", "CPA", "ROI")
        p8 <- xDecompAgg %>%
          filter(!is.na(.data$ci_low), .data$solID == sid) %>%
          select(.data$rn, .data$solID, .data$boot_mean, .data$ci_low, .data$ci_up) %>%
          ggplot(aes(x = .data$rn, y = .data$boot_mean)) +
          geom_point(size = 3) +
          geom_text(aes(label = signif(.data$boot_mean, 2)), vjust = -0.7, size = 3.3) +
          geom_text(aes(y = .data$ci_low, label = signif(.data$ci_low, 2)), hjust = 1.1, size = 2.8) +
          geom_text(aes(y = .data$ci_up, label = signif(.data$ci_up, 2)), hjust = -0.1, size = 2.8) +
          geom_errorbar(aes(ymin = .data$ci_low, ymax = .data$ci_up), width = 0.25) +
          labs(title = paste("In-cluster bootstrapped", metric, "with 95% CI & mean"), x = NULL, y = NULL) +
          coord_flip() +
          theme_lares()
        if (metric == "ROI") {
          p8 <- p8 + geom_hline(yintercept = 1, alpha = 0.5, colour = "grey50", linetype = "dashed")
        }
      } else {
        p8 <- lares::noPlot("No bootstrap results")
      }

      ## Aggregate one-pager plots and export
      ver <- as.character(utils::packageVersion("Robyn"))
      rver <- utils::sessionInfo()$R.version
      onepagerTitle <- sprintf("One-pager for Model ID: %s", sid)
      onepagerCaption <- sprintf("Robyn v%s [R-%s.%s]", ver, rver$major, rver$minor)
      pg <- wrap_plots(p2, p5, p1, p8, p3, p7, p4, p6, ncol = 2) +
        plot_annotation(
          title = onepagerTitle, subtitle = errors,
          theme = theme_lares(background = "white"),
          caption = onepagerCaption
        )
      all_plots[[sid]] <- pg

      if (export) {
        ggsave(
          filename = paste0(OutputCollect$plot_folder, "/", sid, ".png"),
          plot = pg, limitsize = FALSE,
          dpi = 400, width = 17, height = 19
        )
      }
      if (check_parallel_plot() && !quiet && count_mod_out > 0) {
        cnt <- cnt + 1
        setTxtProgressBar(pbplot, cnt)
      }
      return(all_plots)
    }
    if (!quiet && count_mod_out > 0) {
      cnt <- cnt + length(uniqueSol)
      setTxtProgressBar(pbplot, cnt)
    }
  }
  if (!quiet && count_mod_out > 0) close(pbplot)
  # Stop cluster to avoid memory leaks
  if (check_parallel_plot()) stopImplicitCluster()
  return(invisible(parallelResult[[1]]))
}