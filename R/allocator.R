####################################################################
#' ***Suggestion*** get_allocator_benchmarks
#'
#' Provides some critical benchmarks to help analysts
#' to evaluate the efficiency of the suggested optimal allocation.
#'
#' @param AllocatorCollect_opt
#' @param AllocatorCollect_hist
#' @param AllocatorCollect_recent
#' @export
get_allocator_benchmarks <- function(AllocatorCollect_opt,
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
    return(temp)
}