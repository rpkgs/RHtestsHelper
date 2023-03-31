#' @export
TP_merge <- function(res) {
  ind <- map(res, "TP") %>% which.notnull()
  res2 <- res[ind]
  ## merge yearly and monthly TP
  info <- TP_mergeYM_sites(res2)
  info2 <- TP_high_conf(info)
  # info2 <- info[abs(year(date) - year(date_year)) <= 1, ][Idc != "No  ", ]
  sites_adj <- info2[, .N, .(site)][, site]
  # res_adj = res2[sites_adj]
  lst_TP <- split(info2, info2$site)
  lst_TP
}

# > 合并年和月的TPs
## 首先保存，具有元数据支持的TP
diffday <- function(x, y, ..., unit = "days") {
  difftime(x, y, units = "days") %>% as.integer()
}

#' @param l Object returned by [homogenize.wRef()], a list with the elements of
#' - year
#' - month
#' @rdname TP_mergeYM_sites
TP_mergeYM <- function(l) {
  TP_year <- l$year$TP
  TP_mon <- l$month$TP
  if (is.null(TP_year) || is.null(TP_mon)) return(NULL)

  # 年和月匹配的部分
  TP_matched = matchedTP_in_year(TP_mon, TP_year)

  ## 落选的部分，可能会有可取的部分
  # inds = match(unique(TP_matched$date_year), TP_year$date)
  # if (length(inds) < nrow(TP_year)) {
  #   # 挑选最近的month
  #   # TP_year2 = TP_year[-inds, ]
  #   # TP2 = matchedTP_in_mon(TP_year2, TP_mon)
  #   # TP_matched %<>% rbind(TP2)
  # }
  TP_matched
}

matchedTP_in_year <- function(TP_mon, TP_year) {
  foreach(j = 1:nrow(TP_mon), .combine = rbind.data.frame) %do% {
    d_mon = TP_mon[j, ]
    diffday_ym <- diffday(d_mon$date, TP_year$date)
    I0 <- which.min(abs(diffday_ym))

    d_mon %>%
      rename(date_mon = date) %>%
      mutate(date_year = TP_year$date[I0], .after = date_mon) %>%
      mutate(diffday_ym = diffday_ym[I0])
  }
}

matchedTP_in_mon <- function(TP_year, TP_mon) {
  foreach(j = 1:nrow(TP_year), .combine = rbind.data.frame) %do% {
    d_year = TP_year[j, ]
    diffday_ym <- diffday(d_year$date, TP_mon$date)
    I0 <- which.min(abs(diffday_ym))

    d_year %>%
      rename(date_year = date) %>%
      mutate(date_mon = TP_mon$date[I0], .before = date_year) %>%
      mutate(diffday_ym = diffday_ym[I0])
  }
}

#' TP_mergeYM_sites
#'
#' @param res2 object returned by [RHtests()]
#' @export
TP_mergeYM_sites <- function(res) {
  res2 = RHtests_rm_empty(res) # year & mon 必须同时含有
  sites <- names(res2) %>% as.integer() %>% set_names(., .)

  lst <- foreach(sitename = sites, x = res2, i = icount()) %do% {
    runningId(i, 100)
    TP_mon <- TP_mergeYM(x)
    cbind(site = sitename, TP_mon)
  }
  do.call(rbind, lst)
}


# merge_adjusted <- function(df, varname) {
#   infile = glue("OUTPUT/RHtests_{varname}_QMadjusted.RDS")
#   out <- readRDS(infile)
#   df_adj <- map(out, ~.$data[, .(date, base, QM_adjusted)]) %>% melt_list("site")
#   sites_adj <- sort(unique(df_adj$site))

#   varnames = c("site", "date", varname)
#   df_good = df[!(site %in% sites_adj), .SD, .SDcols = varnames] %>% cbind(QC = 1)
#   df_adj2 = df_adj[, .(site, date, QM_adjusted)] %>% set_colnames(varnames) %>% cbind(QC = 0)

#   rbind(df_good, df_adj2) %>%
#     set_colnames(c(varnames, paste0("QC_", varname)))
# }
