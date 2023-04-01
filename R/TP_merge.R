#' merge monthly and yearly TPs and mask bads out
#' @param d A data.frame returned by [TP_mergeYM_sites()]
#' @param nyear the monthly TP will be filter out, if the distance to the
#' 
#' nearest yearly TP is longer than `nyear`.
#' @export
TP_highConf <- function(d, nyear = 1) {
  if (is.null(d) || nrow(d) == 0) return(NULL)
  
  d[(kind == 1 & abs(year(date_mon) - year(date_year)) <= nyear) | 
    (kind == 0 & Idc != "No  "), ] %>% 
  .[Idc != "No  "]
}

TP_high_conf <- TP_highConf

filter_TPMetaSupport <- function(d) {
  d[(kind == 0 | Idc != "No"), ]
}

# > 合并年和月的TPs
## 首先保存，具有元数据支持的TP
diffday <- function(x, y, ..., unit = "days") {
  difftime(x, y, units = "days") %>% as.integer()
}

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

#' @param l Object returned by [homogenize.wRef()], a list with the elements of
#' - year
#' - month
#' @rdname TP_mergeYM_sites
TP_mergeYM <- function(l, site = NULL) {
  TP_year <- l$year$TP
  TP_mon <- l$month$TP
  if (is.null(TP_year) || is.null(TP_mon)) return(NULL)

  # 年和月匹配的部分
  TP_matched = matchedTP_in_year(TP_mon, TP_year)

  ## 落选的部分，可能会有可取的部分
  inds = match(unique(TP_matched$date_year), TP_year$date)
  if (length(inds) < nrow(TP_year)) {
    # 挑选最近的month
    TP_year2 = TP_year[-inds, ]
    TP2 = matchedTP_in_mon(TP_year2, TP_mon)
    TP_matched %<>% rbind(TP2)
  }
  cbind(site, TP_matched)
}

matchedTP_in_year <- function(TP_mon, TP_year) {
  foreach(j = 1:nrow(TP_mon), .combine = rbind.data.frame) %do% {
    d_mon = TP_mon[j, ]
    diffday_ym <- diffday(d_mon$date, TP_year$date)
    I0 <- which.min(abs(diffday_ym))

    d_mon %>%
      mutate(date_mon = date, date_year = TP_year$date[I0], .after = date) %>%
      mutate(diffday_ym = diffday_ym[I0], scale = "mon")
  }
}

matchedTP_in_mon <- function(TP_year, TP_mon) {
  foreach(j = 1:nrow(TP_year), .combine = rbind.data.frame) %do% {
    d_year = TP_year[j, ]
    diffday_ym <- diffday(d_year$date, TP_mon$date)
    I0 <- which.min(abs(diffday_ym))

    d_year %>%
      mutate(date_mon = TP_mon$date[I0], date_year = date, .after = date) %>%
      mutate(diffday_ym = diffday_ym[I0], scale = "year")
  }
}


#' TP_mergeYM_sites
#'
#' @param res object returned by [RHtests()]
#' @export
TP_mergeYM_sites <- function(res) {
  con_year <- map(res, "year") %>% sapply(is.null)
  con_mon <- map(res, "month") %>% sapply(is.null)

  # 仅有一方含有TP
  l_mon = which(con_year & !con_mon) %>% res[.]
  TP_mon = l_mon %>%
    map_df(~ .x$month$TP, .id = "site") %>%
    filter_TPMetaSupport() %>% # 有站点信息支持的部分
    mutate(date_mon = date, date_year = NA_Date_, .after = date) %>%
    mutate(diffday_ym = NA, scale = "mon")

  l_year = which(con_mon & !con_year) %>% res[.]
  TP_year = l_year %>%
    map_df(~ .x$year$TP, .id = "site") %>%
    filter_TPMetaSupport() %>% # 有站点信息支持的部分
    mutate(date_mon = NA_Date_, date_year = date, .after = date) %>%
    mutate(diffday_ym = NA, scale = "year")
  TP1 = rbind(TP_mon, TP_year) %>%
    arrange(site, date) %>%
    mutate(site = as.integer(site))

  # 二者同时含有TP
  l2 = which(!con_mon & !con_year) %>% res[.]
  TP2 <- foreach(x = l2, sitename = as.integer(names(l2)),
    .combine = rbind.data.frame, i = icount()) %do% {
      runningId(i, 100)
      TP_mergeYM(x, sitename) # merged TP of year and month
  }
  TP = rbind(TP1, TP2)
  TP
}
