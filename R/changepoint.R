get_cdf <- function(z, dist) {
  fun = get(sprintf("p%s", dist$distname))
  param = dist$estimate %>% as.list()
  param = c(list(z), param)
  do.call(fun, param)
}

get_invz <- function(p, dist) {
  fun = get(sprintf("q%s", dist$distname)) # inv_cdf
  param = dist$estimate %>% as.list()
  param = c(list(p), param)
  do.call(fun, param)
}

## mean, sd 矫正: 采用MCP的方法，不要求分布的类型


# 以第二段为基准，矫正第一段的时间序列
# - 均值要如何处理
#' @import fitdistrplus
QM <- function(z1, z2, ..., 
  nyear = NULL, 
  scale = "day", 
  method = c("norm", "ecdf"), pp.type = 7L, verbose = FALSE) {
  # ?如果是采用前后5年的数据？应该不需要这么麻烦
  nyear = nyear %||% Inf # 默认采用全部数据
  
  nperyear = switch(scale, year = 1, month = 12, day = 365)
  nmax = nperyear * nyear
  ind1 = length(z1) %>% {seq(pmax(. - nmax + 1, 1), .)}
  ind2 = length(z2) %>% {seq(1, pmin(nmax, .))}
  zz1 = z1[ind1]
  zz2 = z2[ind2]
  
  # print2(ind1, ind2, z1, z2, zz1, zz2)
  # summary(z2)%>% print()
  # summary(zz2) %>% print()
  method = match.arg(method)
  if (method == 1) {
    ## 方案1
    # tau1 <- seq(0, 1, length = length(z1))
    # quan1 <- quantile(z1, tau1, type = pp.type, na.rm = TRUE)
    # p1 <- approx(quan1, tau1, xout = z1) # 获取这一时段的p
    # # 还需要另一时段的转化函数
    # tau2 <- seq(0, 1, length = length(z2_bench))
    # quan2 <- quantile(z2_bench, tau2, type = pp.type, na.rm = TRUE)
    # z1_adj <- approx(tau2, quan2, xout = p1)
  } else if (method == "ecdf") {
    ## 方案2
    p1 <- ecdf(zz1)(z1)
    z1_adj = quantile(zz2, p1, na.rm = TRUE) %>% set_names(NULL)
    c(z1_adj, z2)
  } else if (method == "norm") {
    ## 方案3
    dist1 <- fitdist(rm_empty(zz1), "norm")
    dist2 <- fitdist(rm_empty(zz2), "norm")

    if (verbose) {
      print(dist1)
      print(dist2)
    }
    q1 = get_cdf(z1, dist1)
    z1_adj = get_invz(q1, dist2)
    # print(str(z1_adj, z1, z2))
    c(z1_adj, z2)
  }
}

cpt_QM <- function(z, date, date_TP = NULL, methods = c("ecdf", "norm"), ...) {
  loc = which(date <= date_TP) %>% last()
  n = length(z)
  z1 = z[1:(loc-1)]
  z2 = z[loc:n]
  # print2(z1, z2)
  
  methods %<>% set_names(., .)
  dat = foreach(method = methods) %do% {
    z_adj = QM(z1, z2, method = method, ...)
  } %>% as.data.table()
  
  data.table(date, base = z) %>% cbind(dat)
}
