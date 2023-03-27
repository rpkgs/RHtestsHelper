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
varnames = c("Tair_max", "Tair_avg", "RH_avg")

varname <- "Tair_max"
version <- "v20230327"

# sites_bad <- c(50548, 54416, 54916, 58942, 59265) %>% as.character()

run_noRef <- function(varname) {
  f_month <- glue("OUTPUT/ChinaHI/RHtests_{version}_{varname}_noRef_monthly.RDS")
  f_day <- glue("OUTPUT/ChinaHI/RHtests_{version}_{varname}_noRef_daily.RDS")

  if (!file.exists(f_month)) {
    # sink("log.txt")
    res <- homogenize_monthly(df, st_moveInfo, sites, varname, .parallel = TRUE)
    res2 <- RHtests_rm_empty(res)
    saveRDS(res2, f_month)
    # sink(NULL)
  } else {
    res2 = readRDS(f_month)
  }
  
  # merge yearly and monthly TP
  info <- TP_mergeYM_sites(res2)
  info2 <- info[abs(year(date) - year(date_year)) <= 1, ][Idc != "No  ", ]
  sites_adj = info2[, .N, .(site)][, site]

  # res_adj = res2[sites_adj]
  lst_TP <- split(info2, info2$site)
  out <- homogenize_daily(df, lst_TP, varname, .parallel = TRUE)
  saveRDS(out, f_day)
}

# update_noRef_daily <- function() {
#   version <- "v20230327"
#   f_month <- glue("OUTPUT/ChinaHI/RHtests_{version}_{varname}_noRef_monthly.RDS")

#   version <- "v20230228"
#   f_day <- glue("OUTPUT/ChinaHI/RHtests_{version}_{varname}_noRef_daily.RDS")

#   res_mon = readRDS(f_month)
#   info <- TP_mergeYM_sites(res_mon)
#   info2 <- info[abs(year(date) - year(date_year)) <= 1, ][Idc != "No  ", ]
#   sites_adj = info2[, .N, .(site)][, site]
  
#   # res_adj = res2[sites_adj]
# }
foreach(varname = varnames, i = icount()) %do% {
  run_noRef(varname)
}
