library(Ipaper)
library(lubridate)
library(RHtests)
library(tidymet)
# library(tidyfst)
ErrorMSG = ""

devtools::load_all()
# devtools::load_all("../RHtests.R")

# 有5个站点出现错误。
# # [data.table]:
# # A data frame: 6 × 14
#    site date       RH_avg RH_min Tair_avg Tair_max Tair_min Pa_avg   Pa_max Pa_min q_mean RH_min2
#   <int> <date>      <dbl>  <int>    <dbl>    <dbl>    <dbl>  <dbl>    <dbl>  <dbl>  <dbl>   <dbl>
# 1 50548 2022-09-14 41738      35   41683.   999998      8.6   98.5     98.8   98.4  -1.65  38006.
# 2 54416 2022-12-01 43518.     12   43471.   999998    -12.7 4446.  100000.   102.   -1.65  39786.
# 3 54916 2022-08-04 43549.     49   43509.   999998     29.5   99.9    100.    99.8  -1.65  39818.
# 4 58942 2022-10-06 41750.     76   41689.   999998     22    101.     101.   101.   -1.65  38018.
# 5 59265 2020-05-13 47699.     65   47644.   999998     23.2 4857.  100000.    99.5  -1.65  43966.
# 6 59265 2020-09-21 41752.     64   41693.   999998     24.9 4262.  100000.    99.4  -1.65  38020.
fix_badValues <- function(df) {
  inds_bad <- df[, which(RH_avg >= 200)]
  # df[inds_bad, ]
  df[inds_bad, `:=`(
    RH_avg = NA_real_,
    Tair_avg = NA_real_, Tair_max = NA_real_,
    Pa_avg = NA_real_, Pa_max = NA_real_,
    q_mean = NA_real_, RH_min2 = NA_real_,
    HI_max = NA_real_, HI_max_e = NA_real_
  )]
  invisible()
}

date <- gsub("-", "", format(Sys.Date()))
version <- glue("RHtests_v{date}")
# version <- "RHtests_v20230228"

## Input data
main_RHtests_met2481 <- function(
  varname = "RH_avg", 
  version = "v20230331") 
{
  sites <- df[, .N, .(site)]$site
  st = st_met2481[site %in% sites]
  
  # varname <- "RH_avg"
  lst = select(df, all_of(c("site", "date", varname))) %>% split_site()
  if (!isTRUE(all.equal(as.character(sites), names(lst)))) {
    stop("site order error")
  }

  # 这个是月尺度的结果
  f_stRef     <- glue("OUTPUT/ChinaHI/RHtests_{version}_{varname}_st_refer.rda")  
  f_noRef_mon <- glue("OUTPUT/ChinaHI/RHtests_{version}_{varname}_noRef_monthly.RDS")
  f_noRef_day <- glue("OUTPUT/ChinaHI/RHtests_{version}_{varname}_noRef_daily.RDS")
  f_Ref_day   <- glue("OUTPUT/ChinaHI/RHtests_{version}_{varname}_withRef_daily.RDS")
  f_final     <- glue("OUTPUT/ChinaHI/OUTPUT_mete2481_1961-2022_RHtests_{version}_{varname}.csv")
  
  if (!file.exists(f_noRef_mon)) {
    # sink("log.txt")
    res <- homogenize_monthly(df, st_moveInfo, sites, varname, .parallel = TRUE)
    res_noRefMon <- RHtests_rm_empty(res)
    saveRDS(res_noRefMon, f_noRef_mon)
    # sink(NULL)
  } else {
    res_noRefMon <- readRDS(f_noRef_mon)
  }

  # 获取需要调整的站点
  TP <- TP_mergeYM_sites(res_noRefMon)
  TP_high <- TP %>% TP_high_conf()
  sites_adj = TP_high[, .N, .(site)][, site]
  lst_TP <- TP_high %>% split_site()
  siteHomoInfo <- query_siteHomoInfo(res_noRefMon, TP)
  
  ## 2. 带有参考站的（withRef）均一化检测
  # ? 如果WithRef未检测到TP，withRef是否有可能检测到？
  if (!file.exists(f_Ref_day)) {
    ### 2.1. 挑选参考站
    # if (!file.exists(f_stRef)) {
      load(f_stRef)
      
      mat_mon = convert_day2mon(df, varname)
      if (!isTRUE(all.equal(colnames(mat_mon), as.character(st$site)))) {
        stop("check site names order first!")
      }
      ok("Finding Reference sites ...")
      st_refs <- st_refer(st, mat_mon, nsite = NULL, .parallel = TRUE)

      # 单纯更新`st_refer_opt`即可
      ## TODO: 参考站选取
      st_refs_opt <- st_refer_opt(st_refs, sites_adj)
      d_refs <- melt_list(st_refs_opt, "target")
      sites_miss <- setdiff(sites, d_refs$target) %>% as.character()
    #   save(st_refs, st_refs_opt, d_refs, sites_miss, file = f_stRef)
    # } else {
    #   load(f_stRef)
    # }
    inds <- d_refs$target %>% set_names(seq_along(.), .)
    m <- nrow(d_refs)
    ok("Homogenization withRef ...")

    ### 2.2. 带有参考站的（withRef）均一化检测
    res_ref <- foreach(i = inds, icount()) %dopar% {
      runningId(i)
      # if (i == 2) break()
      ## TODO: update this part
      site_target <- d_refs$target[i]
      site_refer <- d_refs$site[i]
      d_target <- lst[[as.character(site_target)]]
      d_refer <- lst[[as.character(site_refer)]]
      d <- merge(d_target, d_refer %>% set_names(c("date", "ref")), all.x = TRUE)
      # ----------------------------
      
      metadata <- get_metadata(d, site_target)

      tryCatch({
        r <- homogenize.wRef(d, metadata)
      }, error = function(e) {
        message(sprintf("[%d] %s", i, e$message))
      })
    }
    export(res_ref, file = f_Ref_day)
  } else {
    res_ref <- import(f_Ref_day)
  }
  
  ## 3. 数据清洗
  df_final = merge_refer2(df, f_Ref_day, f_noRef_day, varname)
  export(df_final, f_final)
  merge_final(fs)
}
