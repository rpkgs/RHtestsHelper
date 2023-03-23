range2 <- function(x, y, na.rm = TRUE) {
  c(min(x, y, na.rm = na.rm), max(x, y, na.rm = na.rm))
}

#' @export
num2date <- function(x) {
  if (is(x, "Date")) {
    return(x)
  }
  # else
  as.character(x) %>%
    gsub("-", "", .) %>%
    gsub("0000$", "0101", .) %>%
    gsub("00$", "01", .) %>%
    as.Date("%Y%m%d")
}

#' @export
date2num <- function(date) {
  as.character(date) %>%
    gsub("-", "", .) %>%
    as.numeric()
}

is.Date <- function(x) is(x, "Date")


width_str <- function(str, width = NULL) {
  if (!is.null(width) && width > 0) {
    pattern <- sprintf("%%%ds", width)
    sprintf(pattern, str)
  } else {
    sprintf("%s", str)
  }
}

last <- function(x) {
  x[length(x)]
}

#' @importFrom dplyr select
split_site <- function(d) {
  split(select(d, -site), d$site)
}

#' @importFrom Ipaper %dopar% %do%
get_dof <- function(.parallel = FALSE) {
  ifelse(.parallel, `%dopar%`, `%do%`)
}

#' @importFrom matrixStats colMeans2
#' @export
matrixStats::colMeans2

convert_day2mon <- function(
    df2, varname = "RH_avg", ...,
    fun = colMeans2, max.nmiss = 3) {
  ## daily转monthly
  date_max <- max(df2$date)
  date_min <- min(df2$date)
  date <- seq(ymd(date_min), ymd(date_max), by = "day")
  mat <- dcast(df2, date ~ site, value.var = varname)[, -1] %>% as.matrix()

  ## when aggregate daily to monthly scale, if more than 3 invalid values, monthly
  # value will be set to NA
  mat_month <- apply_col(mat, by = format(date, "%Y-%m-01"))
  mat_month_miss <- apply_col(is.na(mat), by = format(date, "%Y-%m-01"), fun)
  mat_month[mat_month_miss > max.nmiss] <- NA_real_
  mat_month
}
