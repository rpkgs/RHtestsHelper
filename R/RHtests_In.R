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