#' RHtests_main
#' 
#' @param df A data.frame with the columns at least of `site`, 'date', 'varname'
#' @param st_moveInfo
#' 
#' @importFrom dplyr arrange mutate select
#' @export
homogenize_monthly <- function(df, st_moveInfo, sites, varname, ..., .parallel = FALSE) {
  # 首先在月尺度检测突变点，月尺度已经够用，运行效率更高
  sites %<>% as.integer() %>% set_names(., .)
  vars_sel <- c("date", varname)
  
  `%dof%` <- get_dof(.parallel) # get do function
  res <- foreach(sitename = sites, i = icount()) %dof% {
    runningId(i, 10)
    tryCatch({
      l = get_Input(df, sitename, varname)
      
      prefix <- "./OUTPUT/example01"
      
      r_month <- RHtests_process(l$month, NULL, l$metadata, prefix, is_plot = FALSE, maxgap = 366)
      r_year <- RHtests_process(l$year, NULL, l$metadata, prefix, is_plot = FALSE, maxgap = 366)
      # TP <- r_month$TP
      # r_daily <- RHtests_stepsize(l$day, NULL, TP, prefix = prefix, is_plot = TRUE)
      list(year = r_year, month = r_month)
    }, error = function(e) {
      message(sprintf("%s", e$message))
    })
  }
  res
}

homogenize_daily <- function(df, lst_TP, varname = "Tavg", ..., 
  .parallel = FALSE, nsite = NULL) 
{
  sites <- names(lst_TP) %>% as.integer()
  n = length(sites)
  nsite <- ifelse(!is.null(nsite), pmin(n, nsite), n) 

  `%dof%` <- get_dof(.parallel) # get do function
  res_daily <- foreach(TP = lst_TP, sitename = sites, i = icount(nsite)) %dof% {
    sitename = sites[i]
    runningId(i, 10)
    # 1. meta支持的TP
    # 2. month和yearly同时检测出的TP
    TP = lst_TP[[as.character(sitename)]]
    TP %<>% TP_high_conf()
    # TP = TP[kind == 0 || abs(year(date) - year(date_year) <= 1), ]
    
    tryCatch({
      l = get_Input(df, sitename, varname)
      prefix = "./OUTPUT/example01"
      # r_month <- RHtests_process(l$month, NULL, metadata, prefix, is_plot = FALSE, maxgap = 366)
      # r_year  <- RHtests_process(l$year, NULL, metadata, prefix, is_plot = FALSE, maxgap = 366)
      ## need to input TP
      # TP <- r_month$TP
      r_daily <- RHtests_stepsize(l$day, NULL, TP, prefix = prefix, is_plot = FALSE)
    }, error = function(e) {
      message(sprintf("%s", e$message))
      NULL
    })
  }
  res_daily
}
