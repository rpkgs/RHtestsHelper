RHtests_rm_empty <- function(res) {
  con1 <- map(res, "year") %>% sapply(is.null)
  con2 <- map(res, "month") %>%  sapply(is.null)
  inds_bad = which(con1 & con2)
  # I_left <- intersect(I_left1, I_left2) # %>% sort()
  res[-inds_bad]
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
