#' get the potential reference sites
#'
#' @param st A dataframe of the sites location and covering period information,
#' with the `nrow=nsite`.
#' @param dist A distance matrix with the dimension of `[nsite, nsite]`
#' @param mat_month The monthly input data matrix, with the dimension of `[ntime, nsite]`
#'
#' @export
st_refer <- function(st, mat_month, .parallel=FALSE, nsite = NULL, ..., sites_bad = NULL) {
  if (!isTRUE(all.equal(st$site %>% as.character(), colnames(mat_month)))) {
    stop("check site names order first!")
  }

  sites <- st$site %>% set_names(., .)
  coord <- st[, cbind(lon, lat)]
  dist <- rdist.earth(coord) # in km

  st_refs <- list()
  `%dof%` <- get_dof(.parallel) # get do function

  if (is.null(nsite)) nsite = length(sites)

  inds = set_names(seq_along(sites), sites)
  foreach(i = inds, icount(nsite)) %dof% {
  # for (i in seq_along(sites)) {
    runningId(i, 100)

    disti <- dist[, i]
    ind_near <- which(disti <= 350) %>% setdiff(i) # in the circle buffer of 350km
    alt_t <- st[i, alt]
    # |HR -HT| ≤200m if HT<2500 m; |HR -HT| ≤500m if HT≥2500 m
    alt_diffMax <- ifelse(alt_t < 2500, 200, 500)

    st_ref <- st[ind_near, ]
    st_ref$alt_t <- alt_t
    st_ref$dist_ref <- disti[ind_near]
    st_ref <- st_ref[abs(alt - alt_t) <= alt_diffMax]

    ## 计算重叠的时段
    period_x <- st[i, c(date_begin, date_end)]
    # y = st_ref[, .(date_begin, date_end)]
    st_ref$perc_cov <- period_coverage(period_x, st_ref[, .(date_begin, date_end)])
    st_ref$perc_cov_period <- period_coverage(period_x, st_ref[, .(period_date_begin, period_date_end)])

    ## 去除存在突变的站点
    if (is.null(sites_bad)) {
      st_ref = st_ref[!(site %in% sites_bad), ]
    }

    ## 计算一阶导的相关系数
    ind_near2 <- match(st_ref$site, sites) %>% c(i, .)
    if (length(ind_near2) == 1) {
      st_ref <- NULL
    } else {
      data <- mat_month[, ind_near2]
      data1 <- diff(data)
      st_ref$cor <- cor(data1, use = "pairwise.complete.obs")[1, -1]
    }
    st_ref
  }
}

#' the percentage of overlaped date period of x and y to the length of period x
#'
#' @param x A vector with `date_begin` and `date_end`.
#' @param y A data.frame with the columns of `date_begin` and `date_end`
#'
#' @export
period_coverage <- function(x, y) {
  date_begin <- pmax(x[1], y[[1]])
  date_end <- pmin(x[2], y[[2]])

  units <- "days"
  n_valid <- difftime(date_end, date_begin, units = units) %>% as.numeric()
  n_all <- difftime(x[2], x[1], units = units) %>% as.numeric()
  n_valid / n_all
  # n_miss_head = difftime(x[1], date_begin, units = "days")
  # n_miss_tail = -difftime(x[2], date_begin, units = "days")
  # as.numeric(n_miss_head + n_miss_tail)
}

filter_good_refer <- function(x, cor_min = 0.7, sites_worst = NULL) {
  if (!is.null(sites_worst)) {
    x <- x[!(site %in% sites_worst)]
  }
  x <- x[perc_cov > 0.8 & cor >= cor_min, ]

  if (nrow(x[cor >= 0.90]) >= 5) cor_min <- 0.90
  if (nrow(x[cor >= 0.85]) >= 5) cor_min <- 0.85
  if (nrow(x[cor >= 0.80]) >= 5) cor_min <- 0.80

  x[cor >= cor_min, ]
}

filter_good_refers <- function(st_refs, sites_worst = NULL) {
  map(rm_empty(st_refs), filter_good_refer) %>% rm_empty()
}

tidy_st_refer <- function(st_raw, siteHomoInfo, nmax = 5) {
  vars_st = c("site", "target", "moveTimes", "tag", "period_date_begin", "period_date_end",
            "date_begin", "date_end", "perc_cov", "perc_cov_period", "cor", "homo")
  st_raw = st_raw %>%
    merge(siteHomoInfo) %>%
    select(all_of(vars_st))
  st = st_raw[homo == "Yes", ] %>%
    filter_good_refer() %>%
    arrange(-perc_cov_period, -perc_cov)

  if (nrow(st) > nmax) st = st[1:nmax, ]
  st
}

#' get the optimal reference site for each site
#'
#' @param st_refs object returned by [st_refer()]
#' @param sites_worst sites where TP is confirmed by both of monthly and yearly data
#'
#' @rdname st_refer
#' @export
st_refer_opt <- function(st_refs, sites_worst = NULL) {
  # filter bad sites out
  st_refs2 <- filter_good_refers(st_refs, sites_worst)

  # 这里有改进空间
  out <- map(st_refs2, function(x) {
    x[which.max(perc_cov), ]
  })
  nmiss <- length(st_refs) - length(out) # which.isnull(out) %>% length()
  warn(sprintf("[w] %s sites have no reference site!\n", nmiss))
  out
}

#' select reference sites
#'
#' @param df2 a data.frame with columns of `site`, `date`, and `varname`
#' @param sites_worst sites with significant TPs
#'
#' @import glue
#' @importFrom lubridate ymd
#' @export
find_refer <- function(df2, st, varname = "Tavg", sites_worst) {
  file <- glue("OUTPUT/mete2481_{varname}_st_refer.rda")

  if (!file.exists(file)) {
    ## searching potential reference sites
    # check site names order first
    # 1. 站点长度至少大于30年（76个站点被移除）
    # coord <- st[, .(lon = deg2dec(lon), lat = deg2dec(lat))] %>% as.matrix()
    st_refs <- st_refer(st, mat_month)
    st_refs_opt <- st_refer_opt(st_refs, sites_worst)
    d_refs <- melt_list(st_refs_opt, "target")
    save(st_refs, st_refs_opt, d_refs, file = file)
  }
  load(file, envir = .GlobalEnv)
}
