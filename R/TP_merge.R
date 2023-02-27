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

#' merge monthly and yearly TPs and mask bads out
#'
#' @param d A data.frame returned by [TP_mergeYM_sites()]
#' @param nyear the monthly TP will be filter out, if the distance to the
#' nearest yearly TP is longer than `nyear`.
#'
#' @export
TP_high_conf <- function(d, nyear = 1) {
  if (is.null(d) || nrow(d) == 0) return(NULL)
  d[abs(year(date) - year(date_year)) <= nyear, ][Idc != "No  ", ]
}



#' @param x Object returned by [homogenize.wRef()], a list with the elements of
#' - year
#' - month
#' - day
#' @rdname TP_mergeYM_sites
TP_mergeYM <- function(x) {
  TP_year <- x$year$TP
  TP_month <- x$month$TP
  if (is.null(TP_year) || is.null(TP_month)) return(NULL)

  foreach(j = 1:nrow(TP_month)) %do% {
    date <- TP_month$date[j]
    diff_year <- difftime(date, TP_year$date, units = "days") %>% as.numeric()
    I_year <- which.min(abs(diff_year))

    # diff_meta <- difftime(date, meta$date, units = "days") %>% as.numeric()
    # I_meta <- which.min(abs(diff_meta))

    # c("kind", "Idc", "date", "Ic", "Nseg", "stepsize", "probL", "probU", "plev", "date_meta", "diff")
    cbind(TP_month[j, ],
      date_year = TP_year$date[I_year], day_year = diff_year[I_year]
      # date_meta2 = meta$date[I_meta], day2_meta = diff_meta[I_meta]
    ) %>%
      reorder_name(c(
        "kind", "Idc", "Ic", "date", "date_meta", "date_year",
        "day2_meta", "day2_year"))
  } %>% do.call(rbind, .)
}


#' TP_mergeYM_sites
#'
#' @param res2 object returned by [RHtests()]
#' @export
TP_mergeYM_sites <- function(res2) {
  sites <- names(res2) %>% as.integer() %>% set_names(., .)

  lst <- foreach(sitename = sites, x = res2, i = icount()) %do% {
    runningId(i, 100)
    TP_month <- TP_mergeYM(x)
    cbind(site = sitename, TP_month)
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
