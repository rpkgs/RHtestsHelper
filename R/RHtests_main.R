# num2date <- function(x) {
#     as.character(x) %>% gsub("00$", "01", .) %>% as.Date("%Y%m%d")
# }

format_RHinput <- function(d) {
    varnames <- setdiff(colnames(d), c("site", "date"))
    d[, .(year = year(date), month = month(date), day = day(date))] %>%
        cbind(d[, ..varnames])
}

#' get the turning points of monthly and yearly data
#'
#' @param df A data.frame with the columns at least of `site`, 'date', 'varname'
#' @param st_moveInfo
#'
#' @export
RHtests_main <- function(df, st_moveInfo, sites, varname, .parallel = FALSE) {
    sites %<>% set_names(., .)
    res <- foreach(sitename = sites, i = icount()) %dopar% {
        # for(i in seq_along(sites_rural[1:30])) {
        runningId(i)
        # sitename = sites_rural[i]
        tryCatch({
            # d <- df[site == sitename, .(date, Tavg)] %>% format_RHinput()
            d <- df[site == sitename, .SD, .SDcols = c("date", varname)]
            date_begin = d$date[1]
            date_end = d$date[nrow(d)]
            metadata = st_moveInfo[site == sitename, ] %>%
                .[period_date_begin > date_begin &
                    period_date_end < date_end, ]
            metadata[, date := period_date_begin]

            if (nrow(d) == 0) { message("no data!"); return() }
            l <- RHtests_input(d)
            ## 以monthly为准
            prefix  = "./OUTPUT/example01"
            # browser()
            r_month <- RHtests_process(l$month, NULL, metadata, prefix, is_plot = FALSE, maxgap = 366)
            r_year  <- RHtests_process(l$year, NULL, metadata, prefix, is_plot = FALSE, maxgap = 366)
            # TP <- r_month$TP
            # r_daily <- RHtests_stepsize(l$day, NULL, TP, prefix = prefix, is_plot = TRUE)
            r       <- list(year = r_year, month = r_month)
            TP      <- tidy_TP_site(r)

            r$daily <- RHtests_stepsize(l$day, NULL, TP, prefix = prefix, is_plot = FALSE)
            r$TP    <- TP
            r
        }, error = function(e) {
            message(sprintf("[%d] %s", i, e$message))
        })
    }
    res
}




