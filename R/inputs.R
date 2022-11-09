####################################################################
#' [Suggestion] Set hyperparameters from Input
#'
#' TODO:: Write description
#'
#' @param InputCollect TODO::Type. TODO::Description
#' @param vars_lst TODO::Type. TODO::Description
#' @param hyper_lst TODO::Type. TODO::Description
#' @return TODO::Type. TODO::Description
#' @export
put_hyppar <- function(InputCollect = NULL,
                       vars_lst,
                       hyper_lst) {
  vars_temp <- c()
  vars_all <- InputCollect$all_media
  for (i in 1:length(vars_lst)) {
    vars_temp <- append(vars_temp, vars_lst[[i]])
  }

  Check_Vars_Equality <- length(vars_temp) == length(vars_all) && setequal(vars_temp, vars_all)
  if (Check_Vars_Equality) {
    print("OK")
  } else {
    sprintf(
      "[Error] There are missing components.\nvars_temp:%s\nvars_all:%s",
      vars_temp,
      vars_all
    )
  }

  hyperparameters <- list()
  for (i in 1:length(vars_lst)) {
    for (j in vars_lst[[i]]) {
      tmp_hyper <- list()
      for (k in hyper_lst[[i]]) {
        tmp_hyper <- append(tmp_hyper, list(k))
      }
      hyper_name <- paste(j, "_", names(hyper_lst[[i]]), sep = "")
      names(tmp_hyper) <- hyper_name
      hyperparameters <- append(hyperparameters, tmp_hyper)
    }
  }
  return(hyperparameters)
}