library(Ipaper)
library(lubridate)
library(RHtests)
library(missInfo)
# library(latticeGrob)

devtools::load_all()



## 2. 计算热浪特征指标 ---------------------------------------------------------
# file_HI <- "../ChinaHW_data/OUTPUT/OBS_China_HI_met2481-(1951-2018).RDS"
file_mete_monthly <- "OUTPUT/INPUT_HImete_st2138.rda"
load(file_mete_monthly)

df <- readRDS("OUTPUT/INPUT_HImete_daily_st2138.RDS")

## fill data of date uncontinues values
# st = st_met2370[, .(id, site, lon, lat, kind = is_city)]
sites_rural <- st_met2370[is_city == "Rural", site] %>% set_names(., .)
sites = df[, .N, .(site)]$site

# I_bad = c(15, 30, 166, 172, 190)
# I_bad = c(274, 327, 492, 502, 744, 796)

InitCluster(10, kill = FALSE)


varname <- "RHavg"
res  <- RHtests_main(df, st_moveInfo, sites, varname)
res2 <- RHtests_rm_empty(res)

saveRDS(res2, glue("OUTPUT/RHtests_{varname}_monthly_2138_V2.RDS"))

## merge yearly and monthly TP
info  <- TP_merge_ym(res2)
info2 <- info[abs(year(date) - year(date_year)) <= 1, ][Idc != "No  ", ]
sites_adj = info2[, .N, .(site)][, site]

res_adj = res[sites_adj]
lst_TP <- split(info2, info2$site)

out <- RHtests_adj_daily(df, lst_TP, varname)
saveRDS(out, glue("OUTPUT/RHtests_{varname}_QMadjusted_V2.RDS"))
