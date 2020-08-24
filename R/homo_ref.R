#' Homogeneous time series data with reference series
#'
#' @param d A data.frame with columns of date, varname, ref
#' @param metedata A data.frame with column date indicating turning point
#'
#' @export
homo_ref <- function(d, metedata = NULL) {
    if (nrow(d) == 0) { message("no data!"); return() }
    ## 以monthly为准
    prefix  = "./OUTPUT/example01"
    I_base = c(1, 2, 3, 4)
    I_ref  = c(1, 2, 3, 5)

    tryCatch({
        l <- RHtests_input(d) # %>% str()
        # prefix <- "../../OUTPUT/example02/example02"
        # prefix <- "OUTPUT/example02/example02"
        r_month <- RHtests_process(l$month[, I_base], l$month[, I_ref], metadata, prefix, is_plot = FALSE, maxgap = 366)
        r_year <- RHtests_process(l$year[, I_base], l$year[, I_ref], metadata, prefix, is_plot = FALSE, maxgap = 366)

        r <- list(year = r_year, month = r_month)
        r$TP <- tidy_TP_site(r)

        r$day <- if (nrow(r$TP) != 0) {
            RHtests_stepsize(l$day[, I_base], l$day[, I_ref], r$TP, prefix = prefix, is_plot = FALSE)
        } else NULL
        r %>% .[c("year", "month", "day", "TP")]
    }, error = function(e) {
        message(sprintf('%s', e$message))
    })
}
