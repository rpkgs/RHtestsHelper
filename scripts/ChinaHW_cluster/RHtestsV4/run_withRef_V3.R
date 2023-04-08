library(Ipaper)
library(lubridate)
library(RHtests)
library(tidymet)
library(tidyfst)
devtools::load_all()
# devtools::load_all("../RHtests.R")
ErrorMSG = ""

get_CPTadjust_Inputs <- function(df_org, df_adj) {
  sites_adj <- query_site(df_adj)
  df_org <- rbind(
    df_org[site %!in% sites_adj, ],
    df_adj[, .(site, date, value = ecdf)]
  ) # 采用ecdf的结果
}

# ------------------------------------------------------------------------------
#' main_RHtests_met2481
#' @param other parameters to [homo_withRef_multi()]
#' @export
main_RHtests_met2481 <- function(
  df,
  varname = "RH_avg",
  version = "v20230331",
  version_ref = "v20230403", ...,
  nmax = 5)
{
  fs = query_fileList(varname, version, version_ref)
  ## 1. 读取数据
  df_org <- select(df, all_of(c("site", "date", varname))) %>% 
    set_names(c("site", "date", "value"))
  if (varname == "RH_avg") {
    df_adj = import_fst(fs$cpt)
    df_org = get_CPTadjust_Inputs(df_org, df_adj)
  }
  
  siteHomoInfo = fread(fs$homoInfo)
  
  ## 2.2. 带有参考站的（withRef）均一化检测
  if (!file.exists(fs$withRef_day)) {
    # 挑选参考站
    if (!file.exists(fs$stRef)) {
      ok("Finding Reference sites ...")
      sites_long = siteHomoInfo$site
      st = st_met2481[site %in% sites_long]
      mat_mon = convert_day2mon(df_org[site %in% sites_long])
      st_refs <- st_refer(st, mat_mon, nsite = NULL, .parallel = TRUE)
      saveRDS(st_refs, fs$stRef)
    } else {
      st_refs = readRDS(fs$stRef)
    }
    
    df_day <- convert_df2day(df_org)
    res_ref <- homo_withRef_multi(df_day, st_refs, siteHomoInfo, nmax = nmax, ..., debug = FALSE)
    saveRDS(res_ref, file = fs$withRef_day)
  }
}

## 3. 数据清洗
# df_final = merge_refer2(df, f_Ref_day, f_noRef_day, varname)
# fwrite(df_final, f_final)
InitCluster(30)
# ncpus <- Sys.getenv("LSB_MAX_NUM_PROCESSORS") %>% as.numeric()
# print(ncpus)
# InitCluster(ncpus)

f_input <- "data-raw/INPUT/INPUT_met2474_Tmax&RHmax_for_HImax_1951-2022_V2.fst"
df <- import_fst(f_input)

# date <- gsub("-", "", format(Sys.Date()))
# version <- glue("RHtests_v{date}")
# version <- "RHtests_v20230228"

version <- "v20230408"
version_ref <- "v20230408"

varnames <- c("RH_avg", "Tair_max", "Tair_avg")[1]
varname = varnames[1]

for(varname in varnames) {
  main_RHtests_met2481(df, varname, version, version_ref)
}

# main_RHtests_met2481("Tair_max", version, version_ref)
# main_RHtests_met2481("Tair_avg", version, version_ref)
