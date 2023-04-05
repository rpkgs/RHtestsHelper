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

is_empty <- function (x) {
  is.null(x) || (is.data.frame(x) && nrow(x) == 0) || length(x) == 0
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


#' @export
rename_vars <- function(d, new, old) {
  pos = match2(old, colnames(d))
  colnames(d)[pos$I_y] = new[pos$I_x]
  d
}

`%||%` <- function (x, y) {
  if (is.null(x)) 
    y
  else x
}
