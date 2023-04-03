# format_RHinput <- function(d) {
#   varnames <- setdiff(colnames(d), c("site", "date"))
#   d[, .(year = year(date), month = month(date), day = day(date))] %>%
#     cbind(d[, ..varnames])
# }

#' @export
get_metadata <- function(d, sitename) {
  date_begin <- first(d$date)
  date_end <- last(d$date)

  # 站点迁移信息, fix error v20230224
  metadata <- tidymet::st_moveInfo[site == sitename, ] %>%
    .[period_date_begin > date_begin &
      period_date_end <= date_end, ] %>%
    mutate(date = period_date_begin) %>%
    select(date) %>%
    rbind(data.table(date = as.Date("2005-01-01"))) %>%
    arrange(date)
  metadata
}

get_Input <- function(df, sitename, varname = "RH_avg") {
  d <- df[site == sitename, .SD, .SDcols = c("date", varname)]
  if (nrow(d) == 0) {
    message("no data!")
    return(NULL)
  }
  l <- RHtests_input(d)
  l$metadata = get_metadata(d, sitename)
  set_class(l, "RHtests_In")
}

#' @export
print.RHtests_In <- function(x, ...) {
  l = map(x, as.data.table)
  print(l)
}


#' getInput_refer
#'
#' @param df_mat A data.table, with the daily data and the columns of `c(date, sites)`
#' @param st_refer
#' - `target`:
#' - `site`  : reference site
#' - `cor`   : correlation of target and reference site
#'
#' @importFrom matrixStats rowWeightedMeans
#' @export
getInput_refer <- function(df_mat, st_refer) {
  date = df_mat$date

  site <- st_refer$target[1] %>% as.character()
  sites_refer <- st_refer$site %>% as.character()

  d_ref <- df_mat[, ..sites_refer] #%>% as.matrix()
  # base <- df_mat[, ..site][, 1]
  raw = df_mat %>% select(all_of(c("date", site))) %>% set_names(c("date", "base"))
  ref = rowWeightedMeans(as.matrix(d_ref), st_refer$cor^2, na.rm = TRUE)

  data = cbind(raw, d_ref)
  anomaly = cbind(date, raw$base - d_ref)
  Ref = cbind(raw, ref)

  listk(data, anomaly, Ref)
}


get_yearly <- function(d) {
  d[, lapply(.SD, mean, na.rm = TRUE), .(year(date))]
}

plot_check_input <- function(l) {
  # my_theme <- function() {
  #   list(labs(x = NULL, y = NULL))
  # }
  dat = l$data %>% get_yearly() %>%
    melt(c("year", "base"), variable.name = "site")

  p1 = ggplot(dat, aes(year, value, color = site)) +
    geom_line() +
    geom_line(aes(y = base), color = "black") +
    labs(x = NULL, y = "Original")
  # write_fig(p, "a.pdf")

  ## 2. anomaly
  dat = l$anomaly %>% get_yearly() %>%
    melt(c("year"), variable.name = "site")
  dat_avg = l$Ref %>% get_yearly() %>%
    mutate(anomaly = base - ref)

  p2 = ggplot(dat, aes(year, value, color = site)) +
    geom_line() +
    geom_line(data = dat_avg, aes(y = anomaly, color = NULL), color = "black", linewidth = 1) +
    labs(x = NULL, y = "Anomaly")

  ## 3. averaged
  pdat = dat_avg[, .(year, base, ref)] %>% melt(c("year"))
  p3 = ggplot(pdat, aes(year, value, color = variable)) +
    geom_line() +
    scale_color_manual(values = c("black", "red")) +
    labs(x = NULL, y = "Homogenized")
  p = p1 / p2 / p3
}

plot_check_out <- function(l) {
  d_out = r$day$data[, .(date, base, ref = QM_adjusted)] %>% get_yearly()
  if (is.null(d_out)) return(NULL)

  pdat = d_out %>% melt(c("year"))
  p = ggplot(pdat, aes(year, value, color = variable)) +
    geom_line() +
    scale_color_manual(values = c("black", "red")) +
    labs(x = NULL, y = NULL)
  p
}


convert_df2day <- function(df, varname = "value", ...) {
  ## daily转monthly
  date_begin <- min(df$date)
  date_end <- max(df$date)

  # 整年
  if (format(date_begin, "%m-%d") != "01-01") {
    date_begin <- make_date(year(date_begin), 1, 1)
  }
  if (format(date_end, "%m-%d") != "12-31") {
    date_end <- make_date(year(date_end), 12, 31)
  }

  date <- seq(ymd(date_begin), ymd(date_end), by = "day")
  mat <- dcast(df, date ~ site, value.var = varname) # [, -1] %>% as.matrix()
  mat
}

convert_day2mon <- function(
    df, varname = "RH_avg", ...,
    fun = colMeans2, max.nmiss = 3) {

  ## daily转monthly
  date_max <- max(df$date)
  date_min <- min(df$date)
  date <- seq(ymd(date_min), ymd(date_max), by = "day")
  mat <- dcast(df, date ~ site, value.var = varname)[, -1] %>% as.matrix()

  ## when aggregate daily to monthly scale, if more than 3 invalid values, monthly
  # value will be set to NA
  mat_month <- apply_col(mat, by = format(date, "%Y-%m-01"))
  mat_month_miss <- apply_col(is.na(mat), by = format(date, "%Y-%m-01"), fun)
  mat_month[mat_month_miss > max.nmiss] <- NA_real_
  mat_month
}
