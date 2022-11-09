# 1. put_hyppar: Put Hyperparam
#    -> demo.R
# 2. generate_budget_boundaries: Get Boundary
#    -> suggest/budget.R
# 3. Allocator_results: Get analytical result of allocator
#    -> suggest/allocator.r
# 4. decompose_dependent_vars: Decompose KPI of media, non-media, etc
#    -> suggest/media_analyzer.r
# 5. saturation_hill_revised, adstock_weibull_revised:
#    -> suggest/transformation.r
# 6. get_individual_result, predict_individual_result: get the results of each media
#    -> suggest/media_analyzer.r
# 7. get_response_sum_on_trainining, predict_response_sum_on_test: get the result of whole media
#    -> suggest/media_analyzer.r
# 8. validation_test: validate. Main Feature
#    -> suggest/validate.r
##### For non-zero analytics
# 9. compare_nonzero
#    -> suggest/non_zero/comparison.r
# 10. ...


####################################################################
#' [Suggestion] Allocator Results
#'
#' TODO:: Write description
#' Generate analytical result of allocato
#'
#' @param AllocatorCollect_opt TODO::Type. TODO::Description
#' @param AllocatorCollect_hist TODO::Type. TODO::Description
#' @param AllocatorCollect_recent TODO::Type. TODO::Description
#' @return List(). Contains list of hyperparameters.
#' @export
Allocator_results <- function(AllocatorCollect_opt,
                              AllocatorCollect_hist,
                              AllocatorCollect_recent) {
  temp <- data.frame(
    scenario = character(),
    budget_init = numeric(),
    budget_exp = numeric(),
    budget_increase = numeric(),
    response_init = numeric(),
    response_exp = numeric(),
    response_increase = numeric()
  )
  temp_list <- list(
    AllocatorCollect_opt,
    AllocatorCollect_hist,
    AllocatorCollect_recent
  )
  temp_name <- c("optimal", "history", "recent")
  for (i in 1:3) {
    budget_ini <- round(temp_list[[i]]$dt_optimOut$initSpendUnitTotal[1], 0)
    budget_exp <- round(temp_list[[i]]$dt_optimOut$expSpendUnitTotal[1], 0)
    budget_inc <- round(100 * temp_list[[i]]$dt_optimOut$optmSpendUnitTotalDelta[1], 2)
    resp_ini <- round(temp_list[[i]]$dt_optimOut$initResponseUnitTotal[1], 0)
    resp_exp <- round(temp_list[[i]]$dt_optimOut$optmResponseUnitTotal[1], 0)
    resp_inc <- round(100 * temp_list[[i]]$dt_optimOut$optmResponseUnitTotalLift[1], 2)
    temp[i, ] <- c(
      temp_name[i],
      budget_ini,
      budget_exp,
      budget_inc,
      resp_ini,
      resp_exp,
      resp_inc
    )
  }
  temp
}



####################################################################
#' [Suggestion-10] Validate the results by history and prediction
#'
#' TODO:: Write description
#' compare history and prediction
#' need to modify the output form
#' @param InputCollect TODO::Type. TODO::Description
#' @param OutputCollect TODO::Type. TODO::Description
#' @param post_data TODO::Type. TODO::Description
#' @param post_period TODO::Type. TODO::Description
#' @param select_model TODO::Type. TODO::Description
#' @return TODO::Type. TODO::Description
#' @export
validation_test <- function(InputCollect,
                            OutputCollect,
                            post_data,
                            post_period,
                            select_model) {
  history <- get_response_sum_on_trainining(InputCollect, OutputCollect, select_model)
  predict <- predict_response_sum_on_test(InputCollect, OutputCollect, post_data, post_period, select_model)
  decompose <- decompose_dependent_vars(InputCollect)
  predict_media_response <- decompose$non_media + decompose$media * predict$response / history$response
  predict_media_dependent <- decompose$non_media + predict$dependent
  realized_value <- post_data %>%
    filter(DATE %in% seq(post_period[1], post_period[2], by = "day")) %>%
    select(revenue)
  total_dependent <- sum(realized_value) / length(realized_value$revenue)

  temp <- data.frame(
    scenario = character(),
    predicted_value = numeric(),
    realized_value = numeric(),
    error = numeric()
  )
  temp[1, ] <- c("response", round(predict_media_response, 0), round(total_dependent, 0), round(100 * (predict_media_response / total_dependent - 1), 2))
  temp[2, ] <- c("dependent", round(predict_media_dependent, 0), round(total_dependent, 0), round(100 * (predict_media_dependent / total_dependent - 1), 2))

  temp
}
