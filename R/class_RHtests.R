RHtests_rm_empty <- function(res) {
  con1 <- map(res, "year") %>% sapply(is.null)
  con2 <- map(res, "month") %>%  sapply(is.null)
  inds_bad = which(con1 & con2)
  # I_left <- intersect(I_left1, I_left2) # %>% sort()
  res[-inds_bad]
}

#' getHomoData
#'
#' @examples
#' getData_day(l)
#' getData_day()
getHomoData <- function(lst, extract.day = FALSE) {
  if (is.character(lst)) lst %<>% import()
  if (extract.day) lst = map(lst, ~ .x$day)

  inds <- map(lst, ~ .$TP) %>% which.notnull()
  fun = function(x) x$data %>% .[, .(date, base, value = QM_adjusted)]

  lst[inds] %>%
    map(fun) %>%
    rm_empty() %>%
    melt_list("site") #%>%
    # rename({{ varname }} := QM_adjusted)
}

getTP_DoubleCheck <- function(lst) {
  lst2 <- RHtests_rm_empty(lst) # 任一存在NULL, 则剔除
  TP_info <- TP_mergeYM_sites(lst2)
  TP_info_highCoef <- TP_highConf(TP_info)

  listk(TP_info, TP_info_highCoef)
}

getTP_daily <- function(lst) {
  map_df(lst, "TP", .id = "site")
}

query_site <- function(d) {
  unique_sort(d$site) %>% as.integer()
}

#' query_siteHomoInfo
#' @return
#' - `Yes`: has no TP
#' - `No` : high confident TP
#' - `?`  ：low confident TP
#' @export
query_siteHomoInfo <- function(res, TP = NULL) {
  res2 <- RHtests_rm_empty(res) # year & mon 必须同时含有
  if (is.null(TP)) TP = TP_mergeYM_sites(res2)
  TP_high = TP %>% TP_high_conf()

  sites_good <- setdiff(names(res), names(res2))
  sites_bad_high <- query_site(TP_high)
  sites_bad <- query_site(TP) %>% setdiff(sites_bad_high)

  info = data.table(site = sites_good, homo = "Yes") %>%
    rbind(data.table(site = sites_bad_high, homo = "No")) %>%
    rbind(data.table(site = sites_bad, homo = "?")) %>%
    mutate(site = as.integer(site))

  print(info[, .N, homo])
  info
}

# global variable: 
merge_final <- function(fs) {
  # fs <- query_fileList(varname)
  if (!file.exists(fs$out)) {
    df_org <- get_DF_INPUTS(df, varname, fs)

    l_noRef_day <- import(fs$noRef_day)
    l_Ref_day <- import(fs$withRef_day)

    df_final <- merge_refer2(df_org, l_Ref_day, l_noRef_day)
    export(df_final, fs$out)
  }
}

## merge noref and withref
#' merge_refer2
#' @param df with the column of `c("site", "date", "value")`
merge_refer2 <- function(df, l_Ref_day, l_noRef_day, ...) {
  if (is.character(l_Ref_day)) l_Ref_day %<>% readRDS()
  if (is.character(l_noRef_day)) l_noRef_day %<>% readRDS()

  sites <- query_site(df)
  ### 3.1. with refer, 含有TP的部分
  # > TPs 不为空的站点，采用`homogenize.wRef`修正；其余的采用no-ref进行修正
  df_ref <- l_Ref_day %>% getHomoData(extract.day = TRUE)

  sites_ref     <- as.integer(names(l_Ref_day))
  sites_ref_yes <- query_site(df_ref) # with TP
  sites_miss    <- setdiff(sites, sites_ref) # 无参考站的站点

  ### 3.2. without refer, 含有TP的部分
  df_noref <- l_noRef_day[as.character(sites_miss)] %>% getHomoData(extract.day = FALSE)
  sites_noref_yes <- query_site(df_noref)

  sites_org <- setdiff(sites, c(sites_ref_yes, sites_noref_yes)) %>% as.character()

  linfo_site <- list(Ref = sites_ref_yes, NoRef = sites_noref_yes, Original = sites_org) %>%
    map(as.integer)
  print(str(linfo_site))

  ## merge the unfixed and fixed
  df_org <- df[site %in% as.integer(sites_org), ] %>%
    # select(all_of(c("site", "date", varname))) %>%
    # set_names(c("site", "date", "value")) %>%
    mutate(base = value, .before = value)
  
  # TODO: error here
  df_final <- list("Original" = df_org, NoRef = df_noref, Ref = df_ref) %>%
    melt_list("type_homo")
  df_final
}

query_fileList <- function(
    varname = "RH_avg",
    version = "v20230408",
    version_ref = "v20230408") {
  
  f_HomoInfo  <- glue("OUTPUT/ChinaHI/RHtests_{version}_{varname}_siteHomoInfo.csv")
  f_stRef     <- glue("OUTPUT/ChinaHI/RHtests_{version}_{varname}_st_refer.RDS")
  f_cpt       <- glue("OUTPUT/ChinaHI/RHtests_{version}_{varname}_cpt.meanvar.fst")
  f_noRef_mon <- glue("OUTPUT/ChinaHI/RHtests_{version}_{varname}_noRef_monthly.RDS")
  f_noRef_day <- glue("OUTPUT/ChinaHI/RHtests_{version}_{varname}_noRef_daily.RDS")
  
  f_Ref_day      <- glue("OUTPUT/ChinaHI/RHtests_{version_ref}_{varname}_withRef_daily.RDS")
  f_Ref_day_TP   <- glue("OUTPUT/ChinaHI/RHtests_{version_ref}_{varname}_withRef_daily.RDS")
  f_Ref_day_data <- glue("OUTPUT/ChinaHI/RHtests_{version_ref}_{varname}_withRef_daily.fst")
  f_final        <- glue("OUTPUT/ChinaHI/OUTPUT_mete2481_1961-2022_RHtests_{version_ref}_{varname}.fst")

  list(
    homoInfo    = f_HomoInfo,
    cpt         = f_cpt, 
    stRef       = f_stRef,
    noRef_mon   = f_noRef_mon,
    noRef_day   = f_noRef_day,
    withRef_day      = f_Ref_day_TP,
    withRef_day_TP   = f_Ref_day_TP,
    withRef_day_data = f_Ref_day_data,
    out              = f_final
  )
}
