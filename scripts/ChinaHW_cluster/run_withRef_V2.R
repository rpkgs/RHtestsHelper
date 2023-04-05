library(Ipaper)
library(lubridate)
library(RHtests)
library(tidymet)
library(tidyfst)
devtools::load_all()
# devtools::load_all("../RHtests.R")
ErrorMSG = ""

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
  df_org <- df %>%
    select(all_of(c("site", "date", varname))) %>%
    set_names(c("site", "date", "value"))
  df_day <- convert_df2day(df_org)
  
  fs = query_fileList(varname, version, version_ref)

  if (!file.exists(fs$homoInfo)) {
    df_year_org <- dt_day2year(df_org)$year
    sites_long <- query_site(df_year_org)

    res_noRefMon = readRDS(fs$noRef_mon)
    TP = TP_mergeYM_sites(res_noRefMon)
    # TP_high = TP %>% TP_high_conf()
    # lst_TP = TP_high %>% split_site()
    # length(lst_TP)
    siteHomoInfo <- query_siteHomoInfo(res_noRefMon, TP)
    siteHomoInfo = siteHomoInfo[site %in% sites_long, ]
    fwrite(siteHomoInfo, fs$homoInfo)
  } else {
    siteHomoInfo = fread(fs$homoInfo)
  }

  load(fs$stRef)
  ## 2.2. 带有参考站的（withRef）均一化检测
  res_ref <- homo_withRef_multi(df_day, st_refs, siteHomoInfo, nmax = nmax, ..., debug = FALSE)
  saveRDS(res_ref, file = fs$withRef_day)
}

## 3. 数据清洗
# df_final = merge_refer2(df, f_Ref_day, f_noRef_day, varname)
# fwrite(df_final, f_final)
# InitCluster(64)

# parallel works
ncpus <- Sys.getenv("LSB_MAX_NUM_PROCESSORS") %>% as.numeric()
# print(ncpus)
InitCluster(ncpus)


f_input <- "data-raw/INPUT/INPUT_met2474_Tmax&RHmax_for_HImax_1951-2022_V2.fst"
df <- import_fst(f_input)
date <- gsub("-", "", format(Sys.Date()))
version <- glue("RHtests_v{date}")
# version <- "RHtests_v20230228"

version <- "v20230331"
version_ref <- "v20230403"

varnames <- c("RH_avg", "Tair_max", "Tair_avg")[1]
for(varname in varnames) {
  main_RHtests_met2481(df, varname, version, version_ref)
}

# main_RHtests_met2481("Tair_max", version, version_ref)
# main_RHtests_met2481("Tair_avg", version, version_ref)
