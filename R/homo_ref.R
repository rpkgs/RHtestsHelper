#' Homogeneous time series data with reference series
#'
#' @param d A data.frame of the inputdata:
#' - if 3 columns, they are `date`, `varname` and `ref`, and homo with ref will 
#' be used.
#' - if 2 columns, they are `date`, `varname`, and homo without ref will
#' be used.
#' @param metedata A data.frame with column date indicating turning point
#'
#' @export
homo_ref <- function(d, metedata = NULL, prefix  = "./OUTPUT/example01") {
    if (nrow(d) == 0) { message("no data!"); return() }    
    I_base = c(1, 2, 3, 4)
    I_ref  = c(1, 2, 3, 5)

    has_ref = ncol(d) == 3
    # tryCatch({
        l <- RHtests_input(d_ref) # %>% str()
        # prefix <- "../../OUTPUT/example02/example02"
        # prefix <- "OUTPUT/example02/example02"
        if (has_ref) {
            ref_year  = l$year[, I_ref]
            ref_month = l$month[, I_ref]
            ref_day   = l$day[, I_ref]
        } else {
            ref_year = ref_month = ref_day = NULL
        }

        r_month <- RHtests_process(l$month[, I_base], ref_month, metadata, prefix, is_plot = FALSE, maxgap = 366)
        r_year <- RHtests_process(l$year[, I_base], ref_year, metadata, prefix, is_plot = FALSE, maxgap = 366)

        r <- list(year = r_year, month = r_month)
        r$TP <- tidy_TP_site(r)

        r$day <- if (nrow(r$TP) != 0) {
            RHtests_stepsize(l$day[, I_base], ref_day, r$TP, prefix = prefix, is_plot = FALSE)
        } else NULL
        r %>% .[c("year", "month", "day", "TP")]
    # }, error = function(e) {
    #     message(sprintf('%s', e$message))
    # })
}


#' @export
get_metadata <- function(d, sitename, st_moveInfo) {
    date_begin <- d$date[1]
    date_end <- d$date[nrow(d)]
    metadata <- st_moveInfo[site == sitename, ] %>%
        .[period_date_begin > date_begin & period_date_end < date_end, ] %>%
        .[, date := period_date_begin]
}


#' get the turning points of monthly and yearly data
#'
#' @param lst List of multiple sites data.frame
#' The data.frame of each site:
#' - if 3 columns, they are `date`, `varname` and `ref`, and homo with ref will
#' be used.
#' - if 2 columns, they are `date`, `varname`, and homo without ref will
#' be used.
#' @param st_moveInfo A data.frame with the site relocation moving information
#' 
#' @export
homo_ref.list <- function(lst, st_moveInfo, .parallel = FALSE) {
    sites <- names(lst) #%>% set_names(., .)
    sites %<>% set_names(., .)

    res <- foreach(d = lst, sitename = sites, i = icount()) %dopar% {
        runningId(i)
        tryCatch({
            metadata = get_metadata(d, sitename, st_moveInfo)
            # prefix <- "./OUTPUT/example01"
            r = homo_ref(d, metadata)
        }, error = function(e) {
            message(sprintf("[%d] %s", i, e$message))
        })
    }
    res
}

# lst = df[, .(site, date, Tavg)] %>% split(.[, -1], .$site)
