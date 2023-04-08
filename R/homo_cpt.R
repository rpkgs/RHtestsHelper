homo_cpt <- function(df_org, info_TP, ..., nyear = 5) {
  # sites_cpt <- info_TP[, site] %>% set_names(., .)
  sites_cpt <- info_TP[year(TP) >= 2000, site] %>% set_names(., .)
  
  lst_adj = foreach(sitename = sites_cpt, i = icount()) %do% {
    runningId(i, 100)
    
    date_TP = info_TP[site == sitename, TP]
    d = df_org[site == sitename, ]
    
    tryCatch({
      d_day = cpt_QM(d$value, d$date, date_TP, nyear = nyear, methods = c("ecdf", "norm"))
    }, error = function(e) {
      message(sprintf('%s', e$message))
    })
  } %>% rm_empty()
  df_adj = melt_list(lst_adj, "site")  
  df_adj
}

detect_TP_cpt <- function(df_mon, minseglen = 12*5, fun = cpt.meanvar, ...) {
  # l_org <- df_org %>% dt_day2year()
  # df_mon <- l_org$mon %>% interp_hisavg_month()
  lst <- dt_dlply(df_mon, .(site),
    ~ cpt_meanvar(.x$value, .x$date, fun, minseglen = minseglen),
    .progress = "text"
  ) %>% rm_empty()
  map_dbl(lst, nrow) %>% table() %>% print()
  # lst_homo = lst[sites_homo] %>% rm_empty() #%>% length()
  # print(length(lst_homo))
  # 55个站点存在均值突变
  info_TP = map(lst, function(d) {
    data.table(TP = d$date[1], 
      mean_a = d$mean[1], mean_b = d$mean[2], 
      var_a = d$variance[1], var_b = d$variance[2]
    )
  }) %>% melt_list("site")
  info_TP
}

cpt_meanvar <- function(y, date = NULL, fun = cpt.meanvar, ...){
  tryCatch({
    l = fun(y, ...)
    if (length(l@cpts) > 1) {
      as.data.table(l@param.est) %>%
        cbind(cpt = as.integer(l@cpts), .) %>%
        mutate(date = date[cpt], .before = "cpt")
    } else {
      NULL
    }
  }, error = function(e) {
    message(sprintf('%s', e$message))
  })
}

cpt_bcp <- function(y, date = NULL, fun = cpt.meanvar, ...){
  tryCatch({    
    l = bcp::bcp(y, ...)
    prob = l$posterior.prob
    inds = which(prob > 0.5)
    if (is_empty(inds)) return(NULL)

    data.table(date = date[inds], cpt = inds, prob = prob[inds])
  }, error = function(e) {
    message(sprintf('%s', e$message))
  })
}
