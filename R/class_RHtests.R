RHtests_rm_empty <- function(res) {
  I_left1 <- map(res, "year") %>% which.notnull()
  I_left2 <- map(res, "month") %>% which.notnull()
  
  # TODO: 这里应该改成or，或者移除这个函数
  I_left <- intersect(I_left1, I_left2) # %>% sort()
  res[I_left]
}

#' getHomoData
#' 
#' @examples
#' getData_day(l)
#' getData_day()
getHomoData <- function(lst) {
  lst %>%
    map(~ .$data[, .(date, value = QM_adjusted)]) %>%
    rm_empty() %>% 
    melt_list("site") #%>%
    # rename({{ varname }} := QM_adjusted)
}

getTP_DoubleCheck <- function(lst) {
  lst2 <- RHtests_rm_empty(lst) # 任一存在NULL, 则剔除
  TP_info <- TP_mergeYM_sites(lst2)
  TP_info_highCoef <- TP_highConf(TP_info)

  listk(TP_info, TP_info_highCoef)
}

#' merge monthly and yearly TPs and mask bads out
#' @param d A data.frame returned by [TP_mergeYM_sites()]
#' @param nyear the monthly TP will be filter out, if the distance to the
#' 
#' nearest yearly TP is longer than `nyear`.
#' @export
TP_highConf <- function(d, nyear = 1) {
  if (is.null(d) || nrow(d) == 0) return(NULL)
  d[abs(year(date) - year(date_year)) <= nyear, ][Idc != "No  ", ]
}

TP_high_conf <- TP_highConf
