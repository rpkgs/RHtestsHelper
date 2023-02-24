format_RHinput <- function(d) {
  varnames <- setdiff(colnames(d), c("site", "date"))
  d[, .(year = year(date), month = month(date), day = day(date))] %>%
    cbind(d[, ..varnames])
}

#' RHtests_main
#' 
#' @param df A data.frame with the columns at least of `site`, 'date', 'varname'
#' @param st_moveInfo
#' 
#' @export
homogenize_monthly <- function(df, st_moveInfo, sites, varname, ..., .parallel = FALSE) {
  # 首先在月尺度检测突变点，月尺度已经够用，运行效率更高
  sites %<>% as.integer() %>% set_names(., .)
  vars_sel <- c("date", varname)
  
  `%dof%` <- get_dof(.parallel) # get do function
  res <- foreach(sitename = sites, i = icount()) %dof% {
    runningId(i)
    tryCatch({
      d <- df[site == sitename, .SD, .SDcols = c("date", varname)]
      date_begin <- first(d$date)
      date_end <- last(d$date)

      # 站点迁移信息
      metadata <- st_moveInfo[site == sitename, ] %>%
        .[period_date_begin > date_begin &
          period_date_end < date_end, ] %>%
        mutate(date = period_date_begin)

      if (nrow(d) == 0) {
        message("no data!")
        return()
      }

      l <- RHtests_input(d)
      prefix <- "./OUTPUT/example01"

      r_month <- RHtests_process(l$month, NULL, metadata, prefix, is_plot = FALSE, maxgap = 366)
      r_year <- RHtests_process(l$year, NULL, metadata, prefix, is_plot = FALSE, maxgap = 366)
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
  sites <- names(lst_TP) %>% as.integer() %>% set_names(., .)
  n = length(sites)
  nsite <- ifelse(!is.null(nsite), pmin(n, nsite), n) 

  `%dof%` <- get_dof(.parallel) # get do function
  res_daily <- foreach(sitename = sites, TP = lst_TP, i = icount(nsite)) %dof% {
    sitename = sites[i]
    runningId(i)
    TP <- lst_TP[[i]]

    tryCatch({
      d <- df[site == sitename, .SD, .SDcols = c("date", varname)]

      metadata <- missInfo::st_moveInfo[site == sitename, ] %>% 
        mutate(date = period_date_begin)

      if (nrow(d) == 0) {
        message("no data!")
        return()
      }

      l <- RHtests_input(d)
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

RHtests_rm_empty <- function(res) {
  I_left1 <- map(res, 1) %>% which.notnull()
  I_left2 <- map(res, 2) %>% which.notnull()
  I_left <- intersect(I_left1, I_left2) # %>% sort()
  res[I_left]
}

TP_merge_ym <- function(res2) {
  sites_TP <- names(res2) %>% as.integer() %>% set_names(., .)
  
  lst <- foreach(sitename = sites_TP, x = res2, i = icount()) %do% {
    runningId(i, 100)
    meta <- st_moveInfo[site == sitename, ] %>% 
      mutate(date = as.Date(period_date_begin))

    year <- x$year$TP
    month <- x$month$TP
    # year$date %<>% as.character() %>% as.Date("%Y%m%d")
    # month$date %<>% as.character() %>% as.Date("%Y%m%d")

    # for(j in 1:nrow(month)) {
    month2 <- foreach(j = 1:nrow(month)) %do% {
      date <- month$date[j]
      diff_year <- difftime(date, year$date, units = "days") %>% as.numeric()
      I_year <- which.min(abs(diff_year))

      diff_meta <- difftime(date, meta$date, units = "days") %>% as.numeric()
      I_meta <- which.min(abs(diff_meta))

      cbind(month[j, 1:9],
        date_year = year$date[I_year], day2_year = diff_year[I_year],
        date_meta = meta$date[I_meta], day2_meta = diff_meta[I_meta]
      ) %>%
        reorder_name(c(
          "kind", "Idc", "Ic", "date", "date_meta", "date_year",
          "day2_meta", "day2_year"
        ))
    } %>% do.call(rbind, .)
    cbind(site = sitename, month2)
    # listk(TP = month2, meta)
  }
  info <- do.call(rbind, lst)
  info
}
