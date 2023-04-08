library(Ipaper)
library(lubridate)
library(RHtests)
library(tidymet)
library(tidyfst)
devtools::load_all()

# devtools::load_all("../RHtests.R")
InitCluster(30)
# ncpus <- Sys.getenv("LSB_MAX_NUM_PROCESSORS") %>% as.numeric()
# InitCluster(ncpus)

# print(ncpus)
f_input <- "data-raw/INPUT/INPUT_met2474_Tmax&RHmax_for_HImax_1951-2022_V2.fst"
df <- import(f_input)
# sites <- query_site(df)

varnames = c("RH_avg", "Tair_max", "Tair_avg")
varname <- varnames[1]
version <- "v20230408"


## 修改df，只做时间序列比较长的站点
main <- function(varname = "RH_avg") {
  ## 1. 读取数据
  fs = query_fileList(varname, version)
  df_org = get_DF_INPUTS(df, varname, fs)
  l_org = dt_day2year(df_org)
  sites_long <- query_site(l_org$year) # 2140

  ##
  if (!file.exists(fs$noRef_mon)) {
    res_noRefMon <- homogenize_monthly(df_org, st_moveInfo, sites_long, varname = "value", .parallel = TRUE)
    export(res_noRefMon, fs$noRef_mon)
  } else {
    res_noRefMon = import(fs$noRef_mon)
  }

  # # merge yearly and monthly TP
  TP <- TP_mergeYM_sites(res_noRefMon)
  TP_high <- TP %>% TP_high_conf()
  sites_adj = TP_high[, .N, .(site)][, site]
  lst_TP <- TP_high %>% split_site()

  siteHomoInfo <- query_siteHomoInfo(res_noRefMon, TP)
  siteHomoInfo = siteHomoInfo[site %in% sites_long, ]
  export(siteHomoInfo, fs$homoInfo)
  
  out <- homogenize_daily(df_org, lst_TP, varname = "value", .parallel = TRUE)
  export(out, fs$noRef_day)
}

main("RH_avg")
