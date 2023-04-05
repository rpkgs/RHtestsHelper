library(Ipaper)
library(lubridate)
library(RHtests)
library(tidymet)
library(tidyfst)
# library(latticeGrob)
devtools::load_all()
devtools::load_all("../RHtests.R")

f_input <- "data-raw/INPUT/INPUT_met2474_Tmax&RHmax_for_HImax_1951-2022_V2.fst"
# export_fst(df2, f_input)
df = import_fst(f_input)

sites <- df[, .N, .(site)]$site

InitCluster(10)
varnames = c("RH_avg", "Tair_max", "Tair_avg")

varname <- "Tair_max"
version <- "v20230331"

# load("debug.rda")
# sites_bad <- c(50548, 54416, 54916, 58942, 59265) %>% as.character()
run_noRef <- function(varname) {
  f_month <- glue("OUTPUT/ChinaHI/RHtests_{version}_{varname}_noRef_monthly.RDS")
  f_day <- glue("OUTPUT/ChinaHI/RHtests_{version}_{varname}_noRef_daily.RDS")

  if (!file.exists(f_month)) {
    # sink("log.txt")
    res <- homogenize_monthly(df, st_moveInfo, sites, varname, .parallel = TRUE)
    # res2 <- RHtests_rm_empty(res)
    saveRDS(res, f_month)
    # sink(NULL)
  } else {
    res = readRDS(f_month)
  }
  
  # # merge yearly and monthly TP
  TP <- TP_mergeYM_sites(res)
  TP_high <- TP %>% TP_high_conf()
  sites_adj = TP_high[, .N, .(site)][, site]
  
  lst_TP <- TP_high %>% split_site()
  out <- homogenize_daily(df, lst_TP, varname, .parallel = TRUE)
  saveRDS(out, f_day)
}

foreach(varname = varnames, i = icount(1)) %do% {
  run_noRef(varname)
}
