#' @export
TP_merge <- function(res) {
  ind <- map(res, "TP") %>% which.notnull()
  res2 <- res[ind]
  ## merge yearly and monthly TP
  info <- TP_tidy_sites(res2)
  info2 <- TP_mask(info)

  info2 <- info[abs(year(date) - year(date_year)) <= 1, ][Idc != "No  ", ]
  sites_adj <- info2[, .N, .(site)][, site]
  # res_adj = res2[sites_adj]
  lst_TP <- split(info2, info2$site)
}

#' merge monthly and yearly TPs and mask bads out
#'
#' @param d A data.frame returned by [TP_tidy_sites()]
#' @param nyear the monthly TP will be filter out, if the distance to the
#' nearest yearly TP is longer than `nyear`.
#'
#' @export
TP_mask <- function(d, nyear = 1) {
  if (is.null(d) || nrow(d) == 0) {
    return(NULL)
  }
  d[abs(year(date) - year(date_year)) <= nyear, ][Idc != "No  ", ]
}

#' TP_tidy_sites
#'
#' @param res2 object returned by [RHtests()]
#' @export
TP_tidy_sites <- function(res2) {
  names <- names(res2)
  if (is.null(names)) names <- seq_along(res2)
  sites_TP <- set_names(names, names)

  lst <- foreach(sitename = sites_TP, x = res2, i = icount()) %do% {
    runningId(i, 100)
    month2 <- TP_tidy_site(x)
    cbind(site = sitename, month2)
  }
  # do.call(rbind, .)
  # browser()
  do.call(rbind.data.frame, lst)
}

#' @param x Object returned by [homo_ref()], a list with the elements of
#' - year
#' - month
#' - day
#' @rdname TP_tidy_sites
TP_tidy_site <- function(x) {
  year <- x$year$TP
  month <- x$month$TP
  if (is.null(month) || is.null(year)) {
    return(NULL)
  }

  month2 <- foreach(j = 1:nrow(month)) %do%
    {
      date <- month$date[j]
      diff_year <- difftime(date, year$date, units = "days") %>% as.numeric()
      I_year <- which.min(abs(diff_year))

      diff_meta <- difftime(date, month$date_meta, units = "days") %>% as.numeric()
      I_meta <- which.min(abs(diff_meta))
      if (length(I_meta) == 0) I_meta <- NA
      # browser()
      cbind(month[j, 1:9],
        stepsize_year = year$stepsize[I_year],
        date_year = year$date[I_year], day2_year = diff_year[I_year],
        date_meta = month$date_meta[I_meta], day2_meta = diff_meta[I_meta]
      )
    } %>%
    do.call(rbind, .) %>%
    reorder_name(c(
      "kind", "Idc", "Ic", "date", "date_year", "date_meta",
      "day2_meta", "day2_year"
    ))
}

# merge_adjusted <- function(df, varname) {
#     infile = glue("OUTPUT/RHtests_{varname}_QMadjusted.RDS")
#     out <- readRDS(infile)
#     df_adj <- map(out, ~.$data[, .(date, base, QM_adjusted)]) %>% melt_list("site")
#     sites_adj <- sort(unique(df_adj$site))

#     varnames = c("site", "date", varname)
#     df_good = df[!(site %in% sites_adj), .SD, .SDcols = varnames] %>% cbind(QC = 1)
#     df_adj2 = df_adj[, .(site, date, QM_adjusted)] %>% set_colnames(varnames) %>% cbind(QC = 0)
#     ans = rbind(df_good, df_adj2) %>% set_colnames(c(varnames, paste0("QC_", varname)))
#     ans
# }
