library(plyr)
library(glue)
library(foreach)
library(iterators)
library(lubridate)
library(missInfo)
library(purrr)
library(matrixStats)

# ------------------------------------------------------------------------------
dir_root <- "N:/DATA/China/2400climate data" %>% path.mnt()
varnames <- c("EVP", "GST", "PRE", "PRS", "RHU", "SSD", "TEM", "WIN")
# merge_mete2000_txts(dir_root, is_save = TRUE)
vars_common = c("site", "lat", "lon", "alt", "year", "month", "day")

# 2019-2020年数据异常，观测数据不是完整的一整月的数据，
# df = lst$TEM
file_met = "OUTPUT/mete2481_Tavg_daily (195101-202003).rda"
if (!file.exists(file_met)) {
    files = dir(dir_root, "*.csv", full.names = TRUE) %>% set_names(varnames)
    I_sel = 1:13
    read_data <- function(file) fread(file, select = I_sel) %>% set_colnames(vars_common[I_sel])
    # lst <- map(files[7], read_data)
    df <- read_data(files[7])
    obs_types <- c("avg", "max", "min")
    prefix    <- "T"
    varnames  <- c(paste0(prefix, obs_types), paste0("QC_", prefix, obs_types))
    colnames(df)[8:13] <- varnames
    df[df == 32766] = NA_integer_
    df[, date := make_date(year, month, day)]

    st_full      = df[, .(site, lat, lon, alt, date = make_date(year, month, day))]
    st_1961_2018 = st_full[date >= "1960-01-01" & date <= "2018-12-31"]

    # load_all("../missInfo/")
    # 1961_2018, 132 unchanged: lon^2 + lat^2 + alt
    st_moveInfo_raw  = get_moveInfo(st_full)
    st_moveInfo = revise_locaiton_multi(st_moveInfo_raw, dist_max = 50)
    # info_full       = get_moveInfo(st_full)
    # info2_1961_2018 = revise_locaiton_multi(info_1961_2018, dist_max = 50)
    # 337 unchanged: lon^2 + lat^2

    st = ddply(st_moveInfo, .(site), . %>% .[which.max(n_period), ])
    coord = st[, .(lon = deg2dec(lon), lat = deg2dec(lat))] %>% as.matrix()
    dist = rdist.earth(coord)

    df2 = df[, .(site, date, Tavg)] %>% fix_uncontinue(complete_year = TRUE)
    save(df2, st_moveInfo, st, dist, file = file_met)
} else {
    load(file_met)
}

## 仅为代码测试，只选择100个站点

## 修复了26/51个站点
# use_data(st_moveInfo, overwrite = TRUE)
# nrow(df)/nrow(df2)
# st_refer_process(df2, "Tavg")
varname = "Tavg"
sites = st$site
nsite = length(sites)
# file_RHtests_monthly = glue("OUTPUT/RHtests_mete{nsite}_{varname}_monthly.RDS")
file_RHtests_daily   = glue("OUTPUT/RHtests_mete{nsite}_{varname}_QMadjusted.RDS")

InitCluster(14)
## 1. PMF test -----------------------------------------------------------------
lst = df2[, .(site, date, Tavg)] %>% {split(.[, -1], .$site)}
system.time(res <- homo_ref.list(lst, st_moveInfo))
saveRDS(res, file_RHtests_daily)

res <- readRDS(file_RHtests_daily)
TP_info = TP_merge(res)
sites_worst = names(TP_info)

find_refer(df2, varname = "Tavg", sites_worst)
# monthly和yearly一致的，884 TPs
# temp <- RHtests_main(df2, st_moveInfo = info2_1961_2018, sitename, varname)
sites_miss = setdiff(sites, d_refs$target) %>% as.character()

# 剩余的153个站点中，64个存在显著的突变点，21个不存在突变点，其余monthly和yearly数据的突变点不一致
#  81 not ref-sites
# 156 not ref-sites, sites_worst
# 308 not ref-sites, sites_worse
# setkeyv(df2, c("site", "date"))

for (i in 1:nrow(d_refs)) {
    if (i == 2) break()
    site_target = d_refs$target[i]
    site_refer  = d_refs$site[i]
    i_t = match(site_target, sites)
    i_r = match(site_refer, sites)

    d_target = lst[[i_t]]
    d_refer  = lst[[i_r]]
    d = merge(d_target, d_refer %>% set_names(c("date", "ref")), all.x = TRUE)
    metadata = get_metadata(d, site_target, st_moveInfo)

    r = homo_ref(d, metadata)
}
