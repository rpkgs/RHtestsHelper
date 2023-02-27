#' Homogeneous time series data with reference series
#'
#' Get the turning points of monthly and yearly data
#'
#' @param d A data.frame of the inputdata:
#' - if 3 columns, they are `date`, `varname` and `ref`, and homo with ref will
#' be used.
#' - if 2 columns, they are `date`, `varname`, and homo without ref will
#' be used.
#' @param metedata A data.frame with column date indicating turning point
#'
#' @export
homogenize.wRef <- function(d, metadata = NULL, prefix = "./OUTPUT/example01") {
  indir <- dirname(prefix)
  if (!dir.exists(indir)) dir.create(indir, recursive = TRUE)

  if (nrow(d) == 0) {
    message("no data!")
    return()
  }
  I_base <- c(1, 2, 3, 4)
  I_ref <- c(1, 2, 3, 5)

  has_ref <- ncol(d) == 3
  l <- RHtests_input(d) # %>% str()
  
  if (has_ref) {
    ref_year  <- l$year[, I_ref]
    ref_month <- l$month[, I_ref]
    ref_day   <- l$day[, I_ref]
  } else {
    ref_year <- ref_month <- ref_day <- NULL
  }

  r_month <- RHtests_process(l$month[, I_base], ref_month, metadata, prefix, is_plot = FALSE, maxgap = 366)
  r_year <- RHtests_process(l$year[, I_base], ref_year, metadata, prefix, is_plot = FALSE, maxgap = 366)

  r <- list(year = r_year, month = r_month)
  r$TP <- TP_mergeYM(r)
  TP <- TP_high_conf(r$TP, nyear = 1) # mask bad ones

  r$day <- if (!is.null(TP) && nrow(TP) != 0) {
    RHtests_stepsize(l$day[, I_base], ref_day, TP, prefix = prefix, is_plot = FALSE)
  } else {
    NULL
  }
  r$TP_high_conf <- TP

  names <- c("year", "month", "day", "TP", "TP_high_conf")
  r[names] %>% set_names(names)
}


#' @param lst List of multiple sites data.frame
#' The data.frame of each site:
#' - if 3 columns, they are `date`, `varname` and `ref`, and homo with ref will
#' be used.
#' - if 2 columns, they are `date`, `varname`, and homo without ref will
#' be used.
#' @param st_moveInfo A data.frame with the site relocation moving information
#'
#' @rdname homogenize.wRef
#' @export
homogenize.wRef.list <- function(lst, st_moveInfo, ..., 
  .parallel = FALSE, .debug = FALSE) 
{
  sites <- names(lst) %>% set_names(., .)
  
  res <- foreach(d = lst, sitename = sites, i = icount()) %dopar% {
    runningId(i)
    tryCatch({
      prefix <- "./OUTPUT/example01"
      metadata <- get_metadata(d, sitename)
      r <- homogenize.wRef(d, metadata)
    }, error = function(e) {
      message(sprintf("[%d] %s", i, e$message))
    })
  }
  res
}
