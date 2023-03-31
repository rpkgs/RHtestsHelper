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

#' @export
rename_vars <- function(d, new, old) {
  pos = match2(old, colnames(d))
  colnames(d)[pos$I_y] = new[pos$I_x]
  d
}

#' dt_day2year
#' @param dat A data.table, at least with the columns of `c("site", "date", "value")`
#' @export 
dt_day2year <- function(dat, 
  nmiss_day_per_mon = 3, nmiss_MonPerYear = 0, nmin_year = 55, ...) 
{
  # dat %<>% fix_uncontinue()
  dat_mon <- dat[, .(
    value = mean(value, na.rm = TRUE),
    # nmiss =  days_in_month(date[1]) - sum(!is.na(value))
    n_valid = sum(!is.na(value)) 
    # nmiss = sum(is.na(value))
  ), .(site, date = date_ym(date))]
  dat_mon %<>% mutate(n_miss = days_in_month(date) - n_valid)
  
  dat_year <- dat_mon[n_miss <= nmiss_day_per_mon, .(
    value = mean(value, na.rm = TRUE),
    n_miss = 12 - .N
  ), .(site, year(date))]
  
  ans <- dat_year[n_miss <= nmiss_MonPerYear, .(site, year, value)] # %>% set_names(c("site", "date", varname))
  # 去除数据长度过短的站点
  # 最长的数据有62年，
  info <- ans[, .N, site]
  # info[N >= 55, ] # 至少有55年的数据
  merge(ans, info[N >= nmin_year, .(site)]) # yeraly data
}
