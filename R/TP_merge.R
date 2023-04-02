#' merge monthly and yearly TPs and mask bads out
#' @param d A data.frame returned by [TP_mergeYM_sites()]
#' @param nyear the monthly TP will be filter out, if the distance to the
#'
#' nearest yearly TP is longer than `nyear`.
#' @export
TP_highConf <- function(d, nyear = 1) {
  if (is.null(d) || nrow(d) == 0) return(NULL)

  # 这里会出现BUG
  d[(kind == 1 & abs(year(date_mon) - year(date_year)) <= nyear) |
    (kind == 0 & Idc != "No  "), ] %>%
  .[Idc != "No  "]
}

TP_high_conf <- TP_highConf

filter_TPMetaSupport <- function(d) {
  d[(kind == 0 & Idc != "No"), ]
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
#' @importFrom lubridate NA_Date_
#' @export
TP_mergeYM <- function(l, site = NULL) {
  TP_year <- l$year$TP
  TP_mon <- l$month$TP

  if (is.null(TP_year) && is.null(TP_mon)) {
    return(NULL)
  } else if (is.null(TP_year)) {
    # 只有月的部分
    TP_matched = TP_mon %>%
      mutate(date_mon = date, date_year = NA_Date_, .after = date) %>%
      mutate(diffday_ym = NA, scale = "mon")
    # return(TP_matched)
  } else if (is.null(TP_mon)) {
    # 只有年的部分
    TP_matched = TP_year %>%
      mutate(date_mon = NA_Date_, date_year = date, .after = date) %>%
      mutate(diffday_ym = NA, scale = "year")
    # return(TP_matched)
  } else {
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
#' @importFrom purrr map_df
#' @importFrom data.table rbindlist
#' @export
TP_mergeYM_sites <- function(res) {
  ok("Merging TPs of yearly and monthly input ...")
  
  foreach(x = res, sitename = as.integer(names(res)), i = icount()) %do% {
    runningId(i, 200)
    TP_mergeYM(x, sitename) # merged TP of year and month
  } %>% rm_empty() %>% rbindlist()
}
