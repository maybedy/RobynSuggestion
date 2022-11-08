# Copyright (c) Meta Platforms, Inc. and its affiliates.

# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

#############################################################################################
####################         Facebook MMM Open Source - Robyn 3.8.1    ######################
####################                    Quick guide                   #######################
#############################################################################################

################################################################
library(Robyn)
packageVersion("Robyn")
Sys.setenv(R_FUTURE_FORK_ENABLE = "true")
options(future.fork.enable = TRUE)
getwd()
setwd("C:/Users/YEEUN/DESKTOP/")
robyn_object <- "C:/Users/YEEUN/DESKTOP/MyRobyn.RDS"
################################################################
#### Step 1: Load data
## Check simulated dataset or load your own dataset
data("dt_simulated_weekly")
head(dt_simulated_weekly)
## Check holidays from Prophet
# 59 countries included. If your country is not included, please manually add it.
# Tipp: any events can be added into this table, school break, events etc.
data("dt_prophet_holidays")
head(dt_prophet_holidays)
# Directory where you want to export results to (will create new folders)
#robyn_object <- "~/Desktop"
### DEPRECATED: It must have extension .RDS. The object name can be different than Robyn:

#results = readRDS(robyn_object)

################################################################
#### Step 2a: For first time user: Model specification in 4 steps
InputCollect <- robyn_inputs(
  dt_input = dt_simulated_weekly,
  dt_holidays = dt_prophet_holidays,
  date_var = "DATE", # date format must be "2020-01-01"
  dep_var = "revenue", # there should be only one dependent variable
  dep_var_type = "revenue", # "revenue" (ROI) or "conversion" (CPA)
  prophet_vars = c("trend", "season", "holiday"), # "trend","season", "weekday" & "holiday"
  prophet_country = "DE", # input one country. dt_prophet_holidays includes 59 countries by default
  context_vars = c("competitor_sales_B", "events"), # e.g. competitors, discount, unemployment etc
  paid_media_spends = c("tv_S", "ooh_S", "print_S", "facebook_S", "search_S"), # mandatory input
  paid_media_vars = c("tv_S", "ooh_S", "print_S", "facebook_I", "search_clicks_P"), # mandatory.
  # paid_media_vars must have same order as paid_media_spends. Use media exposure metrics like
  # impressions, GRP etc. If not applicable, use spend instead.
  organic_vars = "newsletter", # marketing activity without media spend
  # factor_vars = c("events"), # force variables in context_vars or organic_vars to be categorical
  window_start = "2016-11-21",
  window_end = "2018-08-20",
  adstock = "weibull_cdf" # geometric, weibull_cdf or weibull_pdf.
)

#### 2a-2: Second, define and add hyperparameters
hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)

## 1. IMPORTANT: set plot = TRUE to see helper plots of hyperparameter's effect in transformation
plot_adstock(plot = FALSE)
plot_saturation(plot = FALSE)

# [Additional Suggestion: Interface of experiments] 
# Reason for listing hyperparameters: 
# actual usage pattern of multiple hyper-param sets of media for experiments
# Example hyperparameters ranges for Geometric adstock
hyperparameters <- list(
  facebook_S_alphas = c(0.5, 3),
  facebook_S_gammas = c(0.3, 1),
  facebook_S_thetas = c(0, 0.3),
  print_S_alphas = c(0.5, 3),
  print_S_gammas = c(0.3, 1),
  print_S_thetas = c(0.1, 0.4),
  tv_S_alphas = c(0.5, 3),
  tv_S_gammas = c(0.3, 1),
  tv_S_thetas = c(0.3, 0.8),
  search_S_alphas = c(0.5, 3),
  search_S_gammas = c(0.3, 1),
  search_S_thetas = c(0, 0.3),
  ooh_S_alphas = c(0.5, 3),
  ooh_S_gammas = c(0.3, 1),
  ooh_S_thetas = c(0.1, 0.4),
  newsletter_alphas = c(0.5, 3),
  newsletter_gammas = c(0.3, 1),
  newsletter_thetas = c(0.1, 0.4)
)

######################################################
######################################################
#start
#function for setting hyperparameters (for convenience)
put_hyppar <- function(InputCollect = NULL,
                       vars_lst,
                       hyper_lst){
  vars_temp = c()
  vars_all = InputCollect$all_media
  for (i in 1:length(vars_lst)){
    vars_temp = append(vars_temp, vars_lst[[i]])
  }
  if (length(vars_temp)==length(vars_all)  & setequal(vars_temp, vars_all)){
    print ("OK")
  } else{
    print (vars_temp)
    print (vars_all)
    print ("There are missing components.")
  }

  hyperparameters = list()
  for (i in 1:length(vars_lst)){
    for (j in vars_lst[[i]]){
      tmp_hyper = list()
      for (k in hyper_lst[[i]]){
        tmp_hyper = append(tmp_hyper, list(k))
      }
      hyper_name = paste(j, "_", names(hyper_lst[[i]]), sep="")
      names(tmp_hyper) = hyper_name
      hyperparameters = append(hyperparameters, tmp_hyper)
    }
  }
  return (hyperparameters)
}
input_vars = list()
input_vars$type1 = c("tv_S", "ooh_S", "facebook_S")
input_vars$type2 = c("print_S", "search_S")
input_vars$type3 = c("newsletter")
input_hyper = list()
input_hyper$type1 = list(alphas = c(0.5, 3),
                         gammas = c(0.3, 1),
                         shapes = c(0.0001, 5),
                         scales = c(0, 0.1))
input_hyper$type2 = list(alphas = c(0.5, 1),
                         gammas = c(0.3, 1),
                         shapes = c(0.0001, 10),
                         scales = c(0, 0.1))
input_hyper$type3 = list(alphas = c(0.5, 1),
                         gammas = c(0.3, 1),
                         shapes = c(0.0001, 1),
                         scales = c(0, 0.5))

hyperparameters = put_hyppar(InputCollect, input_vars, input_hyper)
hyperparameters
#end
######################################################
######################################################

InputCollect <- robyn_inputs(InputCollect = InputCollect, hyperparameters = hyperparameters)
print(InputCollect)
################################################################
#### Step 2b: For known model specification, setup in one single step
#### Check spend exposure fit if available
if (length(InputCollect$exposure_vars) > 0) {
  InputCollect$modNLS$plots$facebook_I
  InputCollect$modNLS$plots$search_clicks_P
}

##### Manually save and import InputCollect as JSON file
# robyn_write(InputCollect, dir = "~/Desktop")
# InputCollect <- robyn_inputs(
#   dt_input = dt_simulated_weekly,
#   dt_holidays = dt_prophet_holidays,
#   json_file = "~/Desktop/RobynModel-inputs.json")

################################################################
#### Step 3: Build initial model

## Run all trials and iterations. Use ?robyn_run to check parameter definition
OutputModels <- robyn_run(
  InputCollect = InputCollect, # feed in all model specification
  # cores = NULL, # default to max available
  # add_penalty_factor = FALSE, # Untested feature. Use with caution.
  # 2000 recommended for the dummy dataset with no calibration
  iterations = 2000,
  # 5 recommended for the dummy dataset
  trials = 1, 
  # outputs = FALSE disables direct model output - robyn_outputs()
  outputs = FALSE 
)
print(OutputModels)

## Check MOO (multi-objective optimization) convergence plots
OutputModels$convergence$moo_distrb_plot
OutputModels$convergence$moo_cloud_plot

######################################################
######################################################
## [Suggestion included in the function]
## Original version in plot.R
#start
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
    ##################################
    ##################################
    # [Suggestion: Message of Convergence on plot ]
    # Reason: practical suggestion for checking convergence on the resulted plots, (not only logs)
    #start
    convergence = list()
    OutputModels$convergence$conv_msg[1]
    if (str_detect(OutputModels$convergence$conv_msg[1], "not")){
      convergence[1] = "not converged"
    } else {
      convergence[1] = "converged"
    }
    if (str_detect(OutputModels$convergence$conv_msg[2], "not")){
      convergence[2] = "not converged"
    } else {
      convergence[2] = "converged"
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
      #end
      ########################################
      ########################################
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

#end
######################################################
######################################################


## Calculate Pareto optimality, cluster and export results and plots. See ?robyn_outputs
OutputCollect <- robyn_outputs(
  InputCollect, OutputModels,
  # pareto_fronts = "auto",
  # calibration_constraint = 0.1, # range c(0.01, 0.1) & default at 0.1
  csv_out = "pareto", # "pareto", "all", or NULL (for none)
  clusters = TRUE, # Set to TRUE to cluster similar models by ROAS. See ?robyn_clusters
  plot_pareto = TRUE, # Set to FALSE to deactivate plotting and saving model one-pagers
  plot_folder = robyn_object, # path for plots export
  export = TRUE # this will create files locally
)
print(OutputCollect)

## 4 csv files are exported into the folder for further usage. Check schema here:
## https://github.com/facebookexperimental/Robyn/blob/main/demo/schema.R
# pareto_hyperparameters.csv, hyperparameters per Pareto output model
# pareto_aggregated.csv, aggregated decomposition per independent variable of all Pareto output
# pareto_media_transform_matrix.csv, all media transformation vectors
# pareto_alldecomp_matrix.csv, all decomposition vectors of independent variables


################################################################
#### Step 4: Select and save the any model

## Compare all model one-pagers and select one that mostly reflects your business reality
print(OutputCollect)
select_model <- "1_216_5" # Pick one of the models from OutputCollect to proceed

#### Since 3.7.1: JSON export and import (faster and lighter than RDS files)
ExportedModel <- robyn_write(InputCollect, OutputCollect, select_model)
print(ExportedModel)

###########################################
###########################################
#robyn channel boundary
#need to modify the function into the general form of the Robyn Library
#start [Suggestion: Boundary]
# Input: Boundary (List of tuple) x expected spends (Int)
# Output: Boundary
# Comments: TO DO 
budget_boundary <- function(InputCollect,
                            cost_input_low,
                            cost_input_high,
                            total_spends){
  dt_mod = InputCollect$dt_modRollWind
  window_end = InputCollect$window_end
  paid_media_spends = InputCollect$paid_media_spends
  intervalType = InputCollect$intervalType

  boundary = list()
  initial_nonzero_cost = dt_mod %>%
    select(c("ds", paid_media_spends)) %>%
    summarise_all(~mean(.[.!=0], na.rm = TRUE)) %>%
    select(paid_media_spends)
  recent_nonzero_cost = dt_mod %>%
    select(c("ds", paid_media_spends)) %>%
    filter(ds %in% seq(window_end %m-% months(1), window_end, by = "days")) %>%
    summarise_all(~mean(.[.!=0], na.rm = TRUE)) %>%
    select(InputCollect$paid_media_spends)

  boundary$cost$low = cost_input_low / initial_nonzero_cost
  boundary$cost$high = cost_input_high / initial_nonzero_cost
  boundary$hist$low = rep(total_spends/sum(initial_nonzero_cost), length(initial_nonzero_cost))
  boundary$hist$high = rep(total_spends/sum(initial_nonzero_cost), length(initial_nonzero_cost))
  boundary$recent$low = total_spends * recent_nonzero_cost/sum(recent_nonzero_cost, na.rm=TRUE)/initial_nonzero_cost
  boundary$recent$high = total_spends * recent_nonzero_cost/sum(recent_nonzero_cost, na.rm=TRUE)/initial_nonzero_cost
  return (boundary)
}
InputCollect$paid_media_spends
budget_low = c(200000, 200000, 60000, 100000, 30000)
budget_high = c(400000, 400000, 100000, 200000, 100000)
exp_spends = 800000
budget_bd = budget_boundary(InputCollect, budget_low, budget_high, exp_spends)
#end
###########################################
###########################################

###########################################
###########################################
#start
# [Suggestion: Allocator Results] Generate analytical result of allocator
AllocatorCollect_opt <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  scenario = "max_response_expected_spend",
  channel_constr_low = unlist(budget_bd$cost$low, use.names = FALSE),
  channel_constr_up = unlist(budget_bd$cost$high, use.names = FALSE),
  expected_spend = exp_spends*2, # Total spend to be simulated
  expected_spend_days = 14, # Duration of expected_spend in days
  export = TRUE
)
AllocatorCollect_hist <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  scenario = "max_response_expected_spend",
  channel_constr_low = unlist(budget_bd$hist$low, use.names = FALSE),
  channel_constr_up = unlist(budget_bd$hist$high, use.names = FALSE),
  expected_spend = exp_spends*2, # Total spend to be simulated
  expected_spend_days = 14, # Duration of expected_spend in days
  export = TRUE
)
AllocatorCollect_recent <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  scenario = "max_response_expected_spend",
  channel_constr_low = unlist(budget_bd$recent$low, use.names = FALSE),
  channel_constr_up = unlist(budget_bd$recent$high, use.names = FALSE),
  expected_spend = exp_spends*2, # Total spend to be simulated
  expected_spend_days = 14, # Duration of expected_spend in days
  export = TRUE
)
Allocator_results <- function(AllocatorCollect_opt, AllocatorCollect_hist, AllocatorCollect_recent){
  temp <- data.frame(scenario = character(),
                     budget_init = numeric(),
                     budget_exp = numeric(),
                     budget_increase = numeric(),
                     response_init = numeric(),
                     response_exp = numeric(),
                     response_increase = numeric())
  temp_list = list(AllocatorCollect_opt,
                   AllocatorCollect_hist,
                   AllocatorCollect_recent)
  temp_name = c("optimal", "history", "recent")
  for (i in 1:3){
    budget_ini = round(temp_list[[i]]$dt_optimOut$initSpendUnitTotal[1],0)
    budget_exp = round(temp_list[[i]]$dt_optimOut$expSpendUnitTotal[1],0)
    budget_inc = round(100*temp_list[[i]]$dt_optimOut$optmSpendUnitTotalDelta[1], 2)
    resp_ini = round(temp_list[[i]]$dt_optimOut$initResponseUnitTotal[1],0)
    resp_exp = round(temp_list[[i]]$dt_optimOut$optmResponseUnitTotal[1],0)
    resp_inc = round(100*temp_list[[i]]$dt_optimOut$optmResponseUnitTotalLift[1],2)
    temp[i,] = c(temp_name[i],
                 budget_ini,
                 budget_exp,
                 budget_inc,
                 resp_ini,
                 resp_exp,
                 resp_inc
    )
  }
  return (temp)
}
Allocator_results(AllocatorCollect_opt, AllocatorCollect_hist, AllocatorCollect_recent)
#end
###########################################
###########################################

###########################################
###########################################
#start
# [Suggestion: Decompose KPI of media, non-media, etc] 
# 
#check accuracy with dependent
pre_period = c(as.Date("2018-07-20"), as.Date("2018-08-19"))
decomp_dependent <- function(InputCollect,
                             pre_period=NULL){
  paid_media_spends = InputCollect$paid_media_spends
  if (length(pre_period)==0){
    data_cut = InputCollect$dt_modRollWind
  } else {
    data_cut = InputCollect$dt_modRollWind %>%
      filter(ds %in% seq(pre_period[1], pre_period[2], by="day"))
  }
  total_media = sum(data_cut %>%
                      select(all_of(paid_media_spends)))/nrow(data_cut)
  total_whole = sum(data_cut %>%
                      select(dep_var))/nrow(data_cut)
  total_non_media = total_whole - total_media
  media_share = total_media / total_whole
  return (list("media" = total_media,
               "non_media" = total_non_media,
               "whole" = total_whole,
               "media_share" = media_share))
}
decomp_dependent(InputCollect)
decomp_dependent(InputCollect, pre_period = pre_period)
###########################################
###########################################

###########################################
###########################################
#start
# [Suggestion] 
saturation_hill_new <- function(x,
                                alpha,
                                gamma,
                                index_end = NULL,
                                x_marginal = NULL){
  if (length(index_end)==0){
    x_saturation = x #
  } else {
    x_saturation = x[1:index_end] #
  }
  inflextion <- c(range(x_saturation) %*% c(1-gamma, gamma)) # Scaling parts
  if (is.null(x_marginal)){
    x_scurve <- x**alpha / (x**alpha + inflextion**alpha) # 
  } else {
    x_scurve <- x_marginal ** alpha / (x_marginal**alpha + inflextion**alpha) #
  }
  return (x_scurve)
}
adstock_weibull_new <- function(x,
                                shape,
                                scale,
                                windlen = length(x),
                                type = "cdf",
                                index_end = NULL) {
  x_bin <- 1:windlen
  if (length(index_end)==0){
    scaleTrans <- round(quantile(1:windlen, scale), 0) #
  } else {
    scaleTrans <- round(quantile(1:index_end, scale),0) # 
  }
  if (shape == 0) {
    thetaVecCum <- thetaVec <- rep(0, windlen)
  } else {
    if ("cdf" %in% tolower(type)) {
      thetaVec <- c(1, 1 - pweibull(head(x_bin, -1), shape = shape, scale = scaleTrans)) # plot(thetaVec)
      thetaVecCum <- cumprod(thetaVec) # plot(thetaVecCum)
    } else if ("pdf" %in% tolower(type)) {
      thetaVecCum <- .normalize(dweibull(x_bin, shape = shape, scale = scaleTrans)) # plot(thetaVecCum)
    }
  }
  x_decayed <- mapply(function(x_val, x_pos) {
    x.vec <- c(rep(0, x_pos - 1), rep(x_val, windlen - x_pos + 1))
    thetaVecCumLag <- lag(thetaVecCum, x_pos - 1, default = 0)
    x.prod <- x.vec * thetaVecCumLag
    return(x.prod)
  }, x_val = x, x_pos = x_bin[1:length(x)])
  x_decayed <- rowSums(x_decayed)[1:length(x)]
  return(list(x = x, x_decayed = x_decayed, thetaVecCum = thetaVecCum))
}
#end
###########################################
###########################################

###########################################
###########################################
#start
# [Suggestion: Get Performance of Each Media ]
post_period = c(as.Date("2018-08-27"), as.Date("2019-08-19"))
result_media <- function(InputCollect,
                         OutputCollect,
                         media_metric,
                         select_model,
                         type = "mean",
                         pre_period = NULL){ # pre_period: 
  dt_hyppar = OutputCollect$resultHypParam
  dt_coef = OutputCollect$xDecompAgg
  date_var = InputCollect$date_var
  media_vec = InputCollect$dt_input[[media_metric]]
  media_range = InputCollect$dt_modRollWind[[media_metric]]
  adstock = InputCollect$adstock
  if (adstock=="geometric"){
    theta = dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_thetas")]]
    x_list = adstock_geometric(x=media_vec, theta = theta, type = "CDF")
  } else {
    shape <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_shapes")]]
    scale <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_scales")]]
    if (str_detect(tolower(adstock), "cdf")){
      x_list = adstock_weibull(x=media_vec, shape = shape, scale = scale, type = "CDF")
    } else if(str_detect(tolower(adstock), "pdf"))
      x_list = adstock_weibull(x=media_vec, shape = shape, scale = scale, type = "PDF")
  }
  m_adstocked = x_list$x_decayed
  #saturation
  m_adstockedRW <- m_adstocked[InputCollect$rollingWindowStartWhich:InputCollect$rollingWindowEndWhich]
  alpha <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_alphas")]]
  gamma <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_gammas")]]
  saturation <- saturation_hill_new(m_adstockedRW,
                                    alpha = alpha,
                                    gamma = gamma)
  mean_cost = mean(media_range[media_range>0])
  mean_cost
  saturation_response <- saturation_hill_new(m_adstockedRW,
                                             alpha = alpha,
                                             gamma = gamma,
                                             index_end = NULL,
                                             x_marginal = mean_cost)
  saturation_dependent <- saturation_hill_new(m_adstockedRW,
                                              alpha = alpha,
                                              gamma = gamma)
  #Decomp
  coeff <- dt_coef[dt_coef$solID == select_model &
                     dt_coef$rn == media_metric,][["coef"]]
  response_var <- as.numeric(saturation_response * coeff)
  dependent_var <- as.numeric(saturation_dependent * coeff)
  return (list("response" = response_var,
               "dependent" = sum(dependent_var, na.rm=TRUE)/length(dependent_var)))
}

result_media(InputCollect,OutputCollect,
             "print_S",
             select_model,
             type = "mean")

#form of the post_data is equal to the form of the InputCollect$dt_input
#just it has different date range
# [Sub function for upper suggestion: make result based on post period]
result_media_post <- function(InputCollect,
                              OutputCollect,
                              post_data,
                              post_period,
                              media_metric,
                              select_model,
                              type = "mean"){
  adstock = InputCollect$adstock
  dt_hyppar = OutputCollect$resultHypParam
  dt_coef = OutputCollect$xDecompAgg
  date_var = InputCollect$date_var
  media_vec = post_data %>% filter(DATE %in% seq(min(InputCollect$dt_input$DATE), post_period[2], by="day"))
  media_vec = media_vec[[media_metric]]
  media_range = post_data %>% filter(DATE %in% seq(post_period[1], post_period[2], by="day")) %>%
    select(media_metric)
  index_end = length(InputCollect$dt_input[[media_metric]])
  if (adstock=="geometric"){
    theta = dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_thetas")]]
    x_list = adstock_geometric(x=media_vec, theta = theta, type = "CDF")
  } else {
    shape <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_shapes")]]
    scale <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_scales")]]
    if (str_detect(tolower(adstock), "cdf")){
      x_list = adstock_weibull_new(x=media_vec, shape = shape, scale = scale, type = "CDF", index_end = index_end)
    } else if(str_detect(tolower(adstock), "pdf"))
      x_list = adstock_weibull_new(x=media_vec, shape = shape, scale = scale, type = "PDF", index_end = index_end)
  }
  m_adstocked = x_list$x_decayed
  #saturation
  m_adstockedRW <- m_adstocked[InputCollect$rollingWindowStartWhich:length(media_vec)]
  alpha <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_alphas")]]
  gamma <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_gammas")]]
  index_end = InputCollect$rollingWindowEndWhich - InputCollect$rollingWindowStartWhich + 1
  saturation <- saturation_hill_new(m_adstockedRW,
                                    alpha = alpha,
                                    gamma = gamma,
                                    index_end)
  mean_cost = mean(media_range[media_range>0])
  saturation_response <- saturation_hill_new(m_adstockedRW,
                                             alpha = alpha,
                                             gamma = gamma,
                                             ,
                                             x_marginal = mean_cost)
  saturation_dependent <- saturation_hill_new(m_adstockedRW,
                                              alpha = alpha,
                                              gamma = gamma)
  #Decomp
  coeff <- dt_coef[dt_coef$solID == select_model &
                     dt_coef$rn == media_metric,][["coef"]]
  response_var <- as.numeric(saturation_response * coeff)
  dependent_var <- as.numeric(saturation_dependent * coeff)
  return (list("response" = response_var,
               "dependent" = sum(dependent_var, na.rm=TRUE)/length(dependent_var)))
}

result_media(InputCollect,OutputCollect,
             "print_S",
             select_model,
             type = "mean")
result_media_post(InputCollect,OutputCollect,
                  dt_simulated_weekly,
                  c(InputCollect$window_start, InputCollect$window_end),
                  "print_S",
                  select_model,
                  type = "mean")

result_media_post(InputCollect,OutputCollect,
                  dt_simulated_weekly,
                  post_period,
                  "tv_S",
                  select_model,
                  type = "mean")

###########################################
###########################################
###########################################
###########################################
#start
# [ Suggestion: Get Total Performance from whole media]
#total_response
decomp_dependent(InputCollect)
InputCollect$paid_media_spends
result_total <- function(InputCollect,
                         OutputCollect,
                         select_model){
  paid_media_spends = InputCollect$paid_media_spends
  response = 0
  dependent = 0
  for (i in paid_media_spends){
    temp = result_media(InputCollect, OutputCollect, i, select_model, type="mean")
    response = response + temp$response
    dependent = dependent + temp$dependent
  }
  return (list("response" = response, "dependent" = dependent))
}
result_total(InputCollect, OutputCollect, select_model)

result_total_post <- function(InputCollect,
                         OutputCollect,
                         post_data,
                         post_period,
                         select_model){
  paid_media_spends = InputCollect$paid_media_spends
  response = 0
  dependent = 0
  for (i in paid_media_spends){
    temp = result_media_post(InputCollect, OutputCollect, post_data, post_period, i, select_model, type="mean")
    response = response + temp$response
    dependent = dependent + temp$dependent
  }
  return (list("response" = response, "dependent" = dependent))
}
result_total_post(InputCollect, OutputCollect, dt_simulated_weekly, post_period, select_model)

#end
###########################################
###########################################

###########################################
###########################################
#start
# [Main Suggestion: Validate the results by history and prediction ]
#compare history and prediction
#need to modify the output form
validation_test <- function(InputCollect, OutputCollect, post_data, post_period, select_model){
  history = result_total(InputCollect, OutputCollect, select_model)
  predict = result_total_post(InputCollect, OutputCollect, post_data, post_period, select_model)
  decompose = decomp_dependent(InputCollect)
  predict_media_response = decompose$non_media + decompose$media * predict$response/history$response
  predict_media_dependent = decompose$non_media + predict$dependent
  realized_value = post_data %>% filter(DATE %in% seq(post_period[1], post_period[2], by="day")) %>%
    select(revenue)
  total_dependent = sum(realized_value)/length(realized_value$revenue)
  return (total_dependent / predict_media_response)
}
realized_value
predict_media_response
predict_media_dependent
validation_test(InputCollect, OutputCollect, dt_simulated_weekly, post_period, select_model)
#end
###########################################
###########################################


#start
# [Suggestion: Able to analyze without non-zero spend days]
#focus on all days (w/ non-zero spend days)
compare_nonzero <- function(InputCollect,
                            AllocatorCollect,
                            pre_date) {
  dt_optimOut = AllocatorCollect$dt_optimOut
  rollingWindowLength = InputCollect$rollingWindowLength
  hist_spend = dt_optimOut$histSpendTotal[1] / rollingWindowLength
  exp_spend = dt_optimOut$expSpendUnitTotal[1]
  hist_response = sum(dt_optimOut$initResponseUnit * dt_optimOut$histSpend / dt_optimOut$initSpendUnit) / rollingWindowLength
  exp_response = dt_optimOut$optmResponseUnitTotal[1]
  return (list("init_spend" = hist_spend,
               "exp_spend" = exp_spend,
               "init_response" = hist_response,
               "exp_response" = exp_response))
}

compare_nonzero(InputCollect,
                AllocatorCollect_hist,
                c(InputCollect$window_start, InputCollect$window_end))


Allocator_results_new <- function(AllocatorCollect_opt, AllocatorCollect_hist, AllocatorCollect_recent){
  temp <- data.frame(scenario = character(),
                     budget_init = numeric(),
                     budget_exp = numeric(),
                     budget_increase = numeric(),
                     response_init = numeric(),
                     response_exp = numeric(),
                     response_increase = numeric())
  temp_list = list(AllocatorCollect_opt,
                   AllocatorCollect_hist,
                   AllocatorCollect_recent)
  temp_name = c("optimal", "history", "recent")
  for (i in 1:3){
    temp_result = compare_nonzero(InputCollect, temp_list[[i]])
    budget_ini = round(temp_result$init_spend,0)
    budget_exp = round(temp_result$exp_spend,0)
    budget_inc = round(100*(budget_exp/budget_ini-1), 2)
    resp_ini = round(temp_result$init_response,0)
    resp_exp = round(temp_result$exp_response,0)
    resp_inc = round(100*(resp_exp/resp_ini-1),2)
    temp[i,] = c(temp_name[i],
                 budget_ini,
                 budget_exp,
                 budget_inc,
                 resp_ini,
                 resp_exp,
                 resp_inc
    )
  }
  return (temp)
}
Allocator_results_new(AllocatorCollect_opt, AllocatorCollect_hist, AllocatorCollect_recent)
#end
###########################################
###########################################

##--------------- Belows are non-zero version -------------------------------------------##
###########################################
###########################################

#start
result_media_new <- function(InputCollect,
                         OutputCollect,
                         media_metric,
                         select_model,
                         type = "mean",
                         pre_period = NULL){
  dt_hyppar = OutputCollect$resultHypParam
  dt_coef = OutputCollect$xDecompAgg
  date_var = InputCollect$date_var
  media_vec = InputCollect$dt_input[[media_metric]]
  media_range = InputCollect$dt_modRollWind[[media_metric]]
  adstock = InputCollect$adstock
  if (adstock=="geometric"){
    theta = dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_thetas")]]
    x_list = adstock_geometric(x=media_vec, theta = theta, type = "CDF")
  } else {
    shape <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_shapes")]]
    scale <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_scales")]]
    if (str_detect(tolower(adstock), "cdf")){
      x_list = adstock_weibull(x=media_vec, shape = shape, scale = scale, type = "CDF")
    } else if(str_detect(tolower(adstock), "pdf"))
      x_list = adstock_weibull(x=media_vec, shape = shape, scale = scale, type = "PDF")
  }
  m_adstocked = x_list$x_decayed
  #saturation
  m_adstockedRW <- m_adstocked[InputCollect$rollingWindowStartWhich:InputCollect$rollingWindowEndWhich]
  alpha <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_alphas")]]
  gamma <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_gammas")]]
  saturation <- saturation_hill_new(m_adstockedRW,
                                    alpha = alpha,
                                    gamma = gamma)
  mean_cost = mean(media_range[media_range>0])
  mean_cost
  saturation_response <- saturation_hill_new(m_adstockedRW,
                                             alpha = alpha,
                                             gamma = gamma,
                                             index_end = NULL,
                                             x_marginal = mean_cost)
  saturation_dependent <- saturation_hill_new(m_adstockedRW,
                                              alpha = alpha,
                                              gamma = gamma)
  #Decomp
  coeff <- dt_coef[dt_coef$solID == select_model &
                     dt_coef$rn == media_metric,][["coef"]]
  response_var <- as.numeric(saturation_response * coeff)
  dependent_var <- as.numeric(saturation_dependent * coeff)
  return (list("response" = response_var,
               "dependent" = sum(dependent_var, na.rm=TRUE)/length(dependent_var)))
}

result_media(InputCollect,OutputCollect,
             "print_S",
             select_model,
             type = "mean")

#form of the post_data is equal to the form of the InputCollect$dt_input
#just it has different date range
result_media_post <- function(InputCollect,
                              OutputCollect,
                              post_data,
                              post_period,
                              media_metric,
                              select_model,
                              type = "mean"){
  adstock = InputCollect$adstock
  dt_hyppar = OutputCollect$resultHypParam
  dt_coef = OutputCollect$xDecompAgg
  date_var = InputCollect$date_var
  media_vec = post_data %>% filter(DATE %in% seq(min(InputCollect$dt_input$DATE), post_period[2], by="day"))
  media_vec = media_vec[[media_metric]]
  media_range = post_data %>% filter(DATE %in% seq(post_period[1], post_period[2], by="day")) %>%
    select(media_metric)
  index_end = length(InputCollect$dt_input[[media_metric]])
  if (adstock=="geometric"){
    theta = dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_thetas")]]
    x_list = adstock_geometric(x=media_vec, theta = theta, type = "CDF")
  } else {
    shape <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_shapes")]]
    scale <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_scales")]]
    if (str_detect(tolower(adstock), "cdf")){
      x_list = adstock_weibull_new(x=media_vec, shape = shape, scale = scale, type = "CDF", index_end = index_end)
    } else if(str_detect(tolower(adstock), "pdf"))
      x_list = adstock_weibull_new(x=media_vec, shape = shape, scale = scale, type = "PDF", index_end = index_end)
  }
  m_adstocked = x_list$x_decayed
  #saturation
  m_adstockedRW <- m_adstocked[InputCollect$rollingWindowStartWhich:length(media_vec)]
  alpha <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_alphas")]]
  gamma <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_gammas")]]
  index_end = InputCollect$rollingWindowEndWhich - InputCollect$rollingWindowStartWhich + 1
  saturation <- saturation_hill_new(m_adstockedRW,
                                    alpha = alpha,
                                    gamma = gamma,
                                    index_end)
  mean_cost = mean(media_range[media_range>0])
  saturation_response <- saturation_hill_new(m_adstockedRW,
                                             alpha = alpha,
                                             gamma = gamma,
                                             ,
                                             x_marginal = mean_cost)
  saturation_dependent <- saturation_hill_new(m_adstockedRW,
                                              alpha = alpha,
                                              gamma = gamma)
  #Decomp
  coeff <- dt_coef[dt_coef$solID == select_model &
                     dt_coef$rn == media_metric,][["coef"]]
  response_var <- as.numeric(saturation_response * coeff)
  dependent_var <- as.numeric(saturation_dependent * coeff)
  return (list("response" = response_var,
               "dependent" = sum(dependent_var, na.rm=TRUE)/length(dependent_var)))
}

result_media(InputCollect,OutputCollect,
             "print_S",
             select_model,
             type = "mean")
result_media_post(InputCollect,OutputCollect,
                  dt_simulated_weekly,
                  c(InputCollect$window_start, InputCollect$window_end),
                  "print_S",
                  select_model,
                  type = "mean")

result_media_post(InputCollect,OutputCollect,
                  dt_simulated_weekly,
                  post_period,
                  "tv_S",
                  select_model,
                  type = "mean")

###########################################
###########################################
###########################################
###########################################
#start
#total_response
decomp_dependent(InputCollect)
InputCollect$paid_media_spends
result_total <- function(InputCollect,
                         OutputCollect,
                         select_model){
  paid_media_spends = InputCollect$paid_media_spends
  response = 0
  dependent = 0
  for (i in paid_media_spends){
    temp = result_media(InputCollect, OutputCollect, i, select_model, type="mean")
    response = response + temp$response
    dependent = dependent + temp$dependent
  }
  return (list("response" = response, "dependent" = dependent))
}
result_total(InputCollect, OutputCollect, select_model)

result_total_post <- function(InputCollect,
                              OutputCollect,
                              post_data,
                              post_period,
                              select_model){
  paid_media_spends = InputCollect$paid_media_spends
  response = 0
  dependent = 0
  for (i in paid_media_spends){
    temp = result_media_post(InputCollect, OutputCollect, post_data, post_period, i, select_model, type="mean")
    response = response + temp$response
    dependent = dependent + temp$dependent
  }
  return (list("response" = response, "dependent" = dependent))
}
result_total_post(InputCollect, OutputCollect, dt_simulated_weekly, post_period, select_model)

#end
###########################################
###########################################

###########################################
###########################################
#start
#compare history and prediction
#need to modify the output form
validation_test <- function(InputCollect, OutputCollect, post_data, post_period, select_model){
  history = result_total(InputCollect, OutputCollect, select_model)
  predict = result_total_post(InputCollect, OutputCollect, post_data, post_period, select_model)
  decompose = decomp_dependent(InputCollect)
  predict_media_response = decompose$non_media + decompose$media * predict$response/history$response
  predict_media_dependent = decompose$non_media + predict$dependent
  realized_value = post_data %>% filter(DATE %in% seq(post_period[1], post_period[2], by="day")) %>%
    select(revenue)
  total_dependent = sum(realized_value)/length(realized_value$revenue)
  return (total_dependent / predict_media_response)
}
realized_value
predict_media_response
predict_media_dependent
#end
###########################################
###########################################


###########################################
###########################################
#start
#check accuracy with response
InputCollect$dt_modRollWind



alpha
saturation <- saturation_hill_new(m_adstockedRW,
                                  alpha = alpha,
                                  gamma = gamma,
                                  index_end)

result_media <- function(InputCollect,
                         OutputCollect,
                         media_metric,
                         select_model,
                         type = "mean",
                         pre_period = NULL,
                         post = FALSE,
                         post_data = NULL,
                         post_period = NULL){
  dt_hyppar = OutputCollect$resultHypParam
  dt_coef = OutputCollect$xDecompAgg
  date_var = InputCollect$date_var
  if (post == FALSE){
    media_vec = InputCollect[[media_metric]]
  } else {
    media_vec = post_data[[media_metric]]
  }
  if (adstock=="geometric"){
    theta = dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_thetas")]]
    x_list = adstock_geometric(x=media_vec, theta = theta, type = "CDF")
  } else {
    shape <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_shapes")]]
    scale <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_shapes")]]
    if (str_detect(tolower(adstock), "cdf")){
      x_list = adstock_weibull(x=media_vec, shape = shape, scale = scale, type = "CDF")
    } else if(str_detect(tolower(adstock), "pdf"))
      x_list = adstock_weibull(x=media_vec, shape = shape, scale = scale, type = "PDF")
  }
  m_adstocked <- x_list$x_decayed

  #saturation
  if (post == FALSE){
    m_adstockedRW <- m_adstocked[InputCollect$rollingWindowStartWhich:InputCollect$rollingWindowEndWhich]
  } else {
    m_adstockedRW <- m_adstocked[InputCollect$rollingWindowStartWhich:InputCollect$rollingWindowEndWhich]
  }
  alpha <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_alphas")]]
  gamma <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_gammas")]]

}

response_media <- function(InputCollect,
                          OutputCollect,
                          post_data = NULL,
                          post_period = NULL,
                          media_metric,
                          select_model,
                          type = "mean"){
  if (length(post_data)==0){
    post_data = InputCollect$dt_inputRollWind
    post_period = c(InputCollect$window_start, InputCollect$window_end)
  }
  x_lst = post_data %>%
    filter(DATE %in% seq(post_period[1], post_period[2], by="day")) %>%
    select(all_of(media_metric))
  print (mean(x_lst[[media_metric]]))
  cost_mean = mean(x_lst[[media_metric]])[[1]]
  if (type=="mean"){
    Response_mean <- robyn_response(
      InputCollect = InputCollect,
      OutputCollect = OutputCollect,
      select_model = select_model,
      media_metric = media_metric,
      metric_value = cost_mean
    )
    Response_mean = Response_mean$response[1]
  } else if (type == "day"){
    Response_avg = 0
    for (i in x_lst[[media_metric]]){
      if (i>0){
        Response_day <- robyn_response(
          InputCollect = InputCollect,
          OutputCollect = OutputCollect,
          select_model = select_model,
          media_metric = media_metric,
          metric_value = i
        )
        Response_avg = Response_avg + Response_day$response[1]
      }
    }
    Response_mean = Response_avg/length(x_lst[[media_metric]])
  }
  return (Response_mean)
}

response_total <- function(InputCollect,
                                OutputCollect,
                                post_data = NULL,
                                post_period = NULL,
                                select_model,
                                type = "mean"){
  total_mean = 0
  for (i in InputCollect$all_media){
    temp = response_post(InputCollect,
                         OutputCollect,
                         post_data,
                         post_period,
                         i,
                         select_model,
                         type)
    total_mean = total_mean + temp
  }
  return (total_mean)
}
response_total(InputCollect,
                    OutputCollect,
                    ,
                    ,
                    select_model,
                    "mean")
#end
###########################################
###########################################




decomp_response(InputCollect, pre_period)

accuracy_response <- function(InputCollect,
                                OutputCollect,
                              AllocatorCollect,
                                post_data,
                                post_period,
                                select_model,
                                type = "mean"){
  decomposition = decomp_dependent(InputCollect,
                              pre_period=NULL)
  pre_dependent = sum(InputCollect$dt_inputRollWind %>%
    select(revenue)) / nrow(InputCollect$dt_inputRollWind)
  pre_response = AllocatorCollect$dt_optimOut$initResponseUnitTotal[1]

  post_data = post_data %>% filter(DATE %in% seq(post_period[1], post_period[2], by = "day"))
  post_dependent = sum(post_data %>% select(revenue)) / nrow(post_data)

}
AllocatorCollect_hist$dt_optimOut$initResponseUnitTotal[1]
decomp_response(InputCollect)
#end
###########################################
###########################################

###########################################
###########################################
#start
#check accuracy with dependent
accuracy_dependent <- function(InputCollect,
                              OutputCollect,
                              post_data,
                              post_period,
                              select_model,
                              type = "mean"){
  return (1)
}
#end
###########################################
###########################################
#print total response and total dependent
#if we focus on the history data,
#we can calculate the total response and dependent for each select_model
#if we focus on the expected(but realized) data,
#there are two scenarios for calculating the new resposne / dependent.
response_dependent <- function(InputCollect,
                   OutputCollect,
                   media_metric,
                   select_model,
                   adstock,
                   post_data = NULL,
                   post_period = NULL,
                   type = "mean"){
  if (length(post_data)==0){
    dt_target = InputCollect$dt_inputRollWind
    period_target = c(InputCollect$window_start, InputCollect$window_end)
  } else {
    dt_target = post_data
    period_target = post_period
  }
  dt_hyppar <- OutputCollect$resultHypParam
  dt_coef <- OutputCollect$xDecompAgg
  date_var = InputCollect$date_var
  media_vec = rd_cost[[media_metric]]
  if (adstock=="geometric"){
    theta = dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_thetas")]]
    x_list = adstock_geometric(x=media_vec, theta = theta, type = "CDF")
  } else {
    shape <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_shapes")]]
    scale <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_shapes")]]
    if (str_detect(tolower(adstock), "cdf")){
      x_list = adstock_weibull(x=media_vec, shape = shape, scale = scale, type = "CDF")
    } else if(str_detect(tolower(adstock), "pdf"))
      x_list = adstock_weibull(x=media_vec, shape = shape, scale = scale, type = "PDF")
  }
  m_adstocked <- x_list$x_decayed

  #saturation
  m_adstockedRW <- m_adstocked[]
}


################################################################
#### Step 8: get marginal returns

## Example of how to get marginal ROI of next 1000$ from the 80k spend level for search channel

# Run ?robyn_response to check parameter definition

## -------------------------------- NOTE v3.6.0 CHANGE !!! ---------------------------------- ##
## The robyn_response() function can now output response for both spends and exposures (imps,
## GRP, newsletter sendings etc.) as well as plotting individual saturation curves. New
## argument names "media_metric" and "metric_value" instead of "paid_media_var" and "spend"
## are now used to accommodate this change. Also the returned output is a list now and
## contains also the plot.
## ------------------------------------------------------------------------------------------ ##

# Get response for 80k from result saved in robyn_object
Spend1 <- 60000
Response1 <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  media_metric = "search_S",
  metric_value = Spend1
)
Response1$response / Spend1 # ROI for search 80k
Response1$plot

#### Or you can call a JSON file directly (a bit slower)
# Response1 <- robyn_response(
#   json_file = json_file,
#   dt_input = dt_simulated_weekly,
#   dt_holidays = dt_prophet_holidays,
#   media_metric = "search_S",
#   metric_value = Spend1
# )

# Get response for +10%
Spend2 <- Spend1 * 1.1
Response2 <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  media_metric = "search_S",
  metric_value = Spend2
)
Response2$response / Spend2 # ROI for search 81k
Response2$plot

# Marginal ROI of next 1000$ from 80k spend level for search
(Response2$response - Response1$response) / (Spend2 - Spend1)

## Example of getting paid media exposure response curves
imps <- 50000000
response_imps <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  media_metric = "facebook_I",
  metric_value = imps
)
response_imps$response / imps * 1000
response_imps$plot

## Example of getting organic media exposure response curves
sendings <- 30000
response_sending <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  media_metric = "newsletter",
  metric_value = sendings
)
response_sending$response / sendings * 1000
response_sending$plot

################################################################
#### Optional: recreate old models and replicate results [v3.7.1]

# From an exported JSON file (which is created automatically when exporting a model)
# we can re-create a previously trained model and outputs. Note: we need to provide
# the main dataset and the holidays dataset, which are NOT stored in the JSON file.
# These JSON files will be automatically created in most cases.

############ WRITE ############
# Manually create JSON file with inputs data only
robyn_write(InputCollect, dir = "~/Desktop")

# Manually create JSON file with inputs and specific model results
robyn_write(InputCollect, OutputCollect, select_model)


############ READ ############
# Recreate `InputCollect` and `OutputCollect` objects
# Pick any exported model (initial or refreshed)
json_file <- "~/Desktop/Robyn_202208231837_init/RobynModel-1_100_6.json"

# Optional: Manually read and check data stored in file
json_data <- robyn_read(json_file)
print(json_data)

# Re-create InputCollect
InputCollectX <- robyn_inputs(
  dt_input = dt_simulated_weekly,
  dt_holidays = dt_prophet_holidays,
  json_file = json_file)

# Re-create OutputCollect
OutputCollectX <- robyn_run(
  InputCollect = InputCollectX,
  json_file = json_file,
  export = FALSE)

# Or re-create both by simply using robyn_recreate()
RobynRecreated <- robyn_recreate(
  json_file = json_file,
  dt_input = dt_simulated_weekly,
  dt_holidays = dt_prophet_holidays,
  quiet = FALSE)
InputCollectX <- RobynRecreated$InputCollect
OutputCollectX <- RobynRecreated$OutputCollect

# Re-export model and check summary (will get exported in your current working directory)
myModel <- robyn_write(InputCollectX, OutputCollectX, dir = "~/Desktop")
print(myModel)

# Re-create one-pager
myModelPlot <- robyn_onepagers(InputCollectX, OutputCollectX, export = FALSE)
# myModelPlot$`1_204_5`$patches$plots[[6]]

# Refresh any imported model
RobynRefresh <- robyn_refresh(
  json_file = json_file,
  dt_input = InputCollectX$dt_input,
  dt_holidays = InputCollectX$dt_holidays,
  refresh_steps = 6,
  refresh_mode = "manual",
  refresh_iters = 1000,
  refresh_trials = 1
)

# Recreate response curves
robyn_response(
  InputCollect = InputCollectX,
  OutputCollect = OutputCollectX,
  media_metric = "newsletter",
  metric_value = 50000
)
