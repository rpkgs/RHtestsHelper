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
df <- import_fst(f_input)
sites <- query_site(df)

varnames = c("RH_avg", "Tair_max", "Tair_avg")
varname <- varnames[1]
version <- "v20230408"


## 修改df，只做时间序列比较长的站点
main <- function(varname = "RH_avg") {
  fs = query_fileList(varname, version)
  
  ## 1. 读取数据
  df_org <- select(df, all_of(c("site", "date", varname))) %>% set_names(c("site", "date", "value"))
  if (varname == "RH_avg") {
    df_adj = import_fst(fs$cpt)

    sites_adj <- query_site(df_adj)
    df_org <- rbind(
      df_org[site %!in% sites_adj, ],
      df_adj[, .(site, date, value = ecdf)]
    ) # 采用ecdf的结果
  }
  l_org = dt_day2year(df_org)
  sites_long <- query_site(l_org$year) # 2140

  ##
  if (!file.exists(fs$noRef_mon)) {
    res_noRefMon <- homogenize_monthly(df_org, st_moveInfo, sites_long, varname = "value", .parallel = TRUE)
    saveRDS(res_noRefMon, fs$noRef_mon)
  } else {
    res_noRefMon = readRDS(fs$noRef_mon)
  }

  # # merge yearly and monthly TP
  TP <- TP_mergeYM_sites(res_noRefMon)
  TP_high <- TP %>% TP_high_conf()
  sites_adj = TP_high[, .N, .(site)][, site]
  lst_TP <- TP_high %>% split_site()

  siteHomoInfo <- query_siteHomoInfo(res_noRefMon, TP)
  siteHomoInfo = siteHomoInfo[site %in% sites_long, ]
  fwrite(siteHomoInfo, fs$homoInfo)
  
  out <- homogenize_daily(df_org, lst_TP, varname = "value", .parallel = TRUE)
  saveRDS(out, fs$noRef_day)
}

main("RH_avg")
