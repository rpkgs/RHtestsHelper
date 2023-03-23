library(Ipaper)
library(lubridate)
library(RHtests)
library(tidymet)
library(tidyfst)
ErrorMSG = ""

devtools::load_all()
devtools::load_all("../RHtests.R")

## Input data
main_RHtests_met2481 <- function(varname = "RH_avg") {
  sites <- df[, .N, .(site)]$site
  st = st_met2481[site %in% sites]
  
  # varname <- "RH_avg"
  lst = select(df, all_of(c("site", "date", varname))) %>% split_site()
  if (!isTRUE(all.equal(as.character(sites), names(lst)))) {
    stop("site order error")
  }

  # 这个是月尺度的结果
  date <- gsub("-", "", format(Sys.Date()))
  version <- glue("RHtests_v{date}")
  version <- "RHtests_v20230228"

  f_Ref       = glue("OUTPUT/ChinaHI/{version}_{varname}_st_refer.rda")
  f_noRef_mon = glue("OUTPUT/ChinaHI/{version}_{varname}_noRef_monthly.RDS")
  f_noRef     = glue("OUTPUT/ChinaHI/{version}_{varname}_noRef_daily.RDS")
  f_withRef   = glue("OUTPUT/ChinaHI/{version}_{varname}_withRef_daily.RDS")

  f_final     = glue("OUTPUT/ChinaHI/OUTPUT_mete2481_1961-2022_{version}_{varname}.csv")

  # fs = c(f_Ref, f_noRef_mon, f_noRef, f_withRef, f_final)
  # file.exists(fs)

  if (!file.exists(f_noRef_mon)) {
    # sink("log.txt")
    res <- homogenize_monthly(df, st_moveInfo, sites, varname, .parallel = TRUE)
    res_noRefMon <- RHtests_rm_empty(res)
    saveRDS(res_noRefMon, f_noRef_mon)
    # sink(NULL)
  } else {
    res_noRefMon <- readRDS(f_noRef_mon)
  }

  ### withRef
  ok("Merging TPs of yearly and monthly input ...")
  info  <- TP_mergeYM_sites(res_noRefMon)
  info2 <- info[abs(year(date) - year(date_year)) <= 1, ][Idc != "No  ", ]
  sites_adj = info2[, .N, .(site)][, site]

  ### 2.1. 挑选参考站
  if (!file.exists(f_Ref)) {
    mat_mon = convert_day2mon(df, varname)

    if (!isTRUE(all.equal(colnames(mat_mon), as.character(st$site)))) {
      stop("check site names order first!")
    }

    ok("Finding Reference sites ...")
    st_refs <- st_refer(st, mat_mon, nsite = NULL, .parallel = TRUE)
    st_refs_opt <- st_refer_opt(st_refs, sites_adj)
    d_refs <- melt_list(st_refs_opt, "target")

    sites_miss <- setdiff(sites, d_refs$target) %>% as.character()
    # length(sites_miss)
    save(st_refs, st_refs_opt, d_refs, sites_miss, file = f_Ref)  
  } else {
    load(f_Ref)
  }

  ### 2.2. 带有参考站的（withRef）均一化检测
  # ? 如果WithRef未检测到TP，withRef是否有可能检测到？
  if (!file.exists(f_withRef)) {
    inds <- d_refs$target %>% set_names(seq_along(.), .)
    m <- nrow(d_refs)

    ok("Homogenization withRef ...")

    res_ref <- foreach(i = inds, icount()) %dopar% {
      runningId(i)
      # if (i == 2) break()
      site_target <- d_refs$target[i]
      site_refer <- d_refs$site[i]
      i_t <- match(site_target, sites)
      i_r <- match(site_refer, sites)

      d_target <- lst[[i_t]]
      d_refer <- lst[[i_r]]
      d <- merge(d_target, d_refer %>% set_names(c("date", "ref")), all.x = TRUE)

      metadata <- get_metadata(d, site_target)

      tryCatch({
        r <- homogenize.wRef(d, metadata)
      }, error = function(e) {
        message(sprintf("[%d] %s", i, e$message))
      })
    }
    saveRDS(res_ref, file = f_withRef)
  } else {
    res_ref <- readRDS(f_withRef)
  }

  ## 3. 数据清洗
  ### 3.1. with refer, 含有TP的部分
  TPs <- map(res_ref, ~ .$day$TP)
  inds_fixed <- which.notnull(TPs)

  # > TPs 不为空的站点，采用`homogenize.wRef`修正；其余的采用no-ref进行修正
  d_ref <- map(res_ref[inds_fixed], ~ .$day$data[, .(date, QM_adjusted)]) %>%
    melt_list("site")

  ### 3.2. without refer, 含有TP的部分
  res_noRef = readRDS(f_noRef)

  d_noref <- res_noRef[sites_miss] %>%
    map(~ .$data[, .(date, QM_adjusted)]) %>%
    rm_empty() %>%
    melt_list("site")

  df_fixed <- rbind(d_ref, d_noref) %>% set_colnames(c("site", "date", varname))

  ## merge the unfixed and fixed
  sites_fixed <- df_fixed$site %>% unique()

  df_org = df[!(site %in% sites_fixed), ] %>% select(all_of(c("site", "date", varname)))

  df_final <- rbind(df_fixed, df_org)

  fwrite(df_final, f_final)
  df_final
}
