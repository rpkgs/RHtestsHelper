# 计算观测的热浪特征指标
# 剔除了缺失大于30d的site-year
# 2020-06-11
devtools::load_all()
source("test/main_pkgs.R")

library(ncdf4)
library(JuliaCall)
julia <- julia_setup()
julia_source("inst/julia/heat_index.jl")

## 1. 整理输入数据 -------------------------------------------------------------
file_HI = "../ChinaHW_data/OUTPUT/OBS_China_HI_met2481-(1951-2018).RDS"
if (!file.exists(file_HI)) {
    vars_common = c("site", "lat", "lon", "alt", "year", "month", "day")
    vars        = c("avg", "max", "min", "avg", "max", "min")
    vars_temp   = paste0(rep(c("", "QC_"), each = 3), "T", vars)
    vars_rh     = c("RHavg", "RHmin") %>% paste0(rep(c("", "QC_"), each = length(.)), .)

    df_temp = fread("N:/DATA/China/2400climate data/SURF_CLI_CHN_MUL_DAY_TEM (1951-2018).csv")
    df_Rh   = fread("N:/DATA/China/2400climate data/SURF_CLI_CHN_MUL_DAY_RHU (1951-2018).csv")

    df = cbind(df_temp, df_Rh[, -(1:7)]) %>% set_names(c(vars_common, vars_temp, vars_rh))
    df[df == 32766] = NA_integer_
    df[year <= 2014 & RHavg < 0, .N, .(site)]

    df_input = df[, .(site, lat, lon, alt, year, month, day, Tavg = Tavg/10, RHavg = RHavg*1.0)]
    # rm(Favg, RH)
    df_input$Favg = celsius.to.fahrenheit(df_input$Tavg) #%>% as.matrix()
    # RH   = as.matrix(df_input$RHavg)
    I_na <- df_input[, which(is.na(Tavg + RHavg))]
    # can hold NA values
    df_input$HI   <- with(df_input, julia_call("heat_index", Favg, RHavg)) %>% fahrenheit.to.celsius()
    saveRDS(df_input, file_HI)
}

## 2. 计算热浪特征指标 ---------------------------------------------------------
file_HW <- "../ChinaHW_data/OUTPUT/OBS_China_HW_characteristics_met2481-(1951-2018).RDS"
if (!file.exists(file_HW)) {
    df = readRDS(file_HI)
    df[, date := make_date(year, month, day)]
    sites = st_met2370$site
    # df = df[site %in% sites]
    # missinfo: rm site-year with missing values greater than 30 days
    info = df[, .(n_miss = sum(is.na(HI))), .(site, year)]
    df_mat = dcast(df, date~site, value.var = "HI")

    r <- HW_index(df_mat, probs = probs)
    r = info[n_miss < 30, 1:2] %>% merge(r, by = c("site", "year"))
    saveRDS(r, file_HW)
} else {
    r = readRDS(file_HW)
}

## 2. convert to site HW characteristics observations into 5deg ----------------
{
    HW_sps = readRDS("../ChinaHW_data/OUTPUT/OBS_China_HW_characteristics_met2481-(1951-2018).RDS")
    
    # 在绘制图2.年变化曲线时，不需要计算anomaly
    # 只分析同时存在农村站点和城市站点的网格
    st <- st_all[, .(site, type_region = is_east, type_data = is_city, gridId = gridId_rural)] 
    
    r = merge(HW_sps, st[!is.na(gridId), ], by = c("site")) %>% 
        melt(measure.vars = indices, variable.name = "index")
    df_HW <- HW_point2grid(r, gridInfo_5deg)
    df_HW.obs <- HW_grid_RegionMean(df_HW)

    save(df_HW.obs, "data-raw/data_Figure2_df_HW.obs.RDS")
    # df_HW.obs = readRDS("data-raw/data_Figure2_df_HW.obs.RDS")
}

## check the annual variation of site HWs
ggplot(df_HW.obs[type_region == "China" & probs == 0.99], aes(year, value, color = type_data)) + 
    geom_line() + 
    facet_wrap(~index, scales = "free_y") 


## check the performance of T and RH -------------------------------------------
# 比较年anomaly即可
variables = c("Tavg", "RHavg", "HI")
df[, date := make_date(year, month, day)]
info = df[, .(n_valid = sum(!is.na(Tavg + RHavg))), .(site, year)]
df2  = df[, .(site, year, date, Tavg, Favg, RHavg, HI)] %>% 
    merge(info[n_valid >= 335, ])
dn_obs <- df2[, .N, .(site, year)][, .N, .(year)][order(year)]
df_yearly <- df2[, lapply(.SD, mean, na.rm = TRUE), .(site, year), .SDcols = variables]


df_yearly.anorm <- df_yearly[, lapply(.SD, get_anomaly_year, year = year), .(site), .SDcols = variables] %>% 
    cbind(df_yearly[, .(year)], .)  %>% 
    merge(st[!is.na(gridId), ])

d_grid <- df_yearly.anorm[, lapply(.SD, mean, na.rm = TRUE), .(year, id = gridId), .SDcols = variables] %>% 
    merge(gridInfo_5deg@data[, c("id", "w")])
# get regional mean
r <- d_grid[, lapply(.SD, weightedMean2, w = w), .(year), .SDcols = variables] %>% .[order(year), ]
d = melt(r, "year")
ggplot(d[year >= 1961, ], aes(year, value)) + geom_line() + facet_wrap(~variable) + 
    geom_smooth(method = "lm")

## previous version ------------------------------------------------------------
# dates_nc = seq(as.Date("2008-01-01"), as.Date("2008-12-31"), by = "day") %>% format("-%m-%d") %>%
#     paste(rep(1951:2014, each = 366), ., sep = "")
# dates = seq(as.Date("1951-01-01"), as.Date("2014-12-31"), by = "day") %>% format()
# info = match2(dates, dates_nc)
# ind = info$I_y

# file = "N:/DATA/China/surface_met2474_daily.nc"
# fid <- nc_open(file)
# mat <- ncvar_get(fid, "RHUM") %>% set_dim(c(2474, 64*12*31))
# mat <- mat[, ind]

# sites_bad = c(50527, 50557)
# sitename = sites_bad[1]
# id = st_2474[site == sitename]$I
# value = mat[id, ]
# d <- df[site == sitename & year <= 2014, ]
# d <- data.table(date = dates, value)
