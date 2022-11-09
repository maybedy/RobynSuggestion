####################################################################
#' ***Suggestion*** Validate the results by history and prediction
#'
#' Presents a table comparing the predicted values from each approach
#' with the actual values and calculating the errors
#' @param InputCollect
#' @param OutputCollect
#' @param post_data
#' @param post_period
#' @param select_model
#' @export
validate_predicts <- function(InputCollect,
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

  return(temp)
}
