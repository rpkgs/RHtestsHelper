#' fix un-continuous observations for the INPUT data
#'
#' fix un-continuous observations for the INPUT data and only keep the complete
#' year.
#'
#' @param df A data.frame with the columns of `site`, `date` and others
#' @param complete_year Boolean. Note that `complete_year` has to be true for RHtests.
#'
#' @export
fix_uncontinue <- function(df, complete_year = TRUE) {
    # df %>% group_by(site) %>% group_modify(fix_uncontinue_site)
    df[, fix_uncontinue_site(.SD), site] # about 2x faster
    # ddply(df, .(site), fix_uncontinue_site, .progress = "text")
}


fix_uncontinue_site <- function(d, ..., complete_year = TRUE) {
  n <- nrow(d)
  date_begin <- d$date[1]
  date_end <- d$date[n]
  
  if (complete_year) {
    if (month(date_begin) != 1) {
      date_begin <- make_date(year(date_begin) + 1, 1, 1)
    }
    if (month(date_end) != 12) {
      date_end <- make_date(year(date_end) - 1, 12, 31)
    }
  }
  temp <- data.table(date = seq.Date(date_begin, date_end, by = "day"))
  merge(d, temp, c("date"), all.y = TRUE) #%>% select(-site)
}
