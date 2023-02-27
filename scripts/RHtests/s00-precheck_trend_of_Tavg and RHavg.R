library(lubridate)
load_all()
## 2. 计算热浪特征指标 ---------------------------------------------------------
file_HI <- "../ChinaHW_data/OUTPUT/OBS_China_HI_met2481-(1951-2018).RDS"
sites_rural <- st_met2370[is_city == "Rural", site] %>% set_names(., .)
sites_miss  <- c("56079", "56147", "56167", "56185", "56202", "56227", "56273", "57173")
# missing percentage >= 1%

file_mete_monthly = "OUTPUT/INPUT_HImete_monthly_st2138.rda"
if (!file.exists(file_mete_monthly)) {
    varnames = c("Tavg", "RHavg", "HI")
    df = readRDS(file_HI)
    df[, date := make_date(year, month, day)]
    info_miss = df[, missInfo(HI, date), .(site)][order(site)]

    ## 删除缺失过多的站点
    # 1. 数据观测少于30年的站点
    # 2. 删除1961-1990期间少于15年的站点
    # 3. 缺测比例大于1%的站点
    # 4. daily->monthly过程中，缺少3个值则判为缺测，
    st = st_met2481
    st[, n := get_intersectDays(date_begin, date_end)]
    st = st[(n >= 365*15 & n_all >= 365*30 & info_miss$perc_miss * 100 < 1) | 
            site %in% sites_rural, ]
    # st
    sites  = st$site
    df_sel = df[site %in% sites, ]
    setkeyv(df_sel, c("site", "date"))

    ## 过滤掉缺失过多的年份和月份
    # - monthly
    #   1. 月缺测数大于3设置为NA

    # - yearly
    #   1. 需有12个月的观测
    #   2. 年总缺测少于30
    #   3. 年最多缺测一个月
    {
        miss_month = df_sel[, .(n_miss = sum(is.na(HI))), .(site, year, month)]
        
        miss_year  = miss_month[, .(n_month = sum(.N), 
                                    n_miss = sum(n_miss), 
                                    n_missTimes = sum(n_miss > 0)), .(site, year)]
        # miss_year  = miss_year[n_month == 12 & !(n_missTimes >= 2 | n_miss > 30)]
        # miss_month = miss_month[n_miss <= 3, ]
    }

    df_month <- df_sel[, map(.SD, mean, na.rm = TRUE), .(site, year, month), .SDcols = varnames]
    df_year  <- df_sel[, map(.SD, mean, na.rm = TRUE), .(site, year), .SDcols = varnames]

    df_month2 = merge(df_month, miss_month)
    df_month2 = df_month2[n_miss <= 3, 1:6] %>% mutate(date = make_date(year, month, 1))
    df_year2  = merge(df_year, miss_year)
    df_year2 <- df_year2[n_month == 12 & n_miss < 30 & n_missTimes <= 3, 1:5] %>%
        mutate(date = make_date(year, 1, 1))

    trend_year.lm = df_year2[, as.list(map(.SD, ~slope_p(.x, year)) %>% unlist()), site, .SDcols=varnames]
    trend_year.mk = df_year2[, as.list(map(.SD, ~slope_mk(.x, year)) %>% unlist()), site, .SDcols=varnames]

    ## 1. anomaly
    df_year2.anorm <- df_year2[, map(.SD, ~ get_anomaly_year(., year)), .(site), .SDcols = varnames] %>%
        cbind(df_year2[, .(year)])
    ## 2. fill missing obs in df_month2 and df_year2
    df_month = ddply(df_month2, .(site), function(d) {
        d_full = d[, .(date = seq(min(date), max(date), by = "month")), .(site)]
        merge(d, d_full, by =c("site", "date"), all.y = TRUE)
    }, .progress = "text")
    df_month[, `:=`(year = year(date), month = month(date))]
    
    
    save(df_year2, df_year2.anorm, df_month, trend_year.lm, trend_year.mk, st, file = file_mete_monthly)
    saveRDS(df_sel, "OUTPUT/INPUT_HImete_daily_st2138.RDS")
} else {
    load(file_mete_monthly)
}


## urban and rural trend
{
    # plot urban grids
    dem <- rgdal::readGDAL("E:/08_heatwave/ArcGIS/grid/chinadem_d010" %>% path.mnt(), silent = TRUE)
    cellsize <- 5
    
    # range_CH <- c(16, 55, 71.5, 140)
    range_CH <- c(70, 140, 15, 55)
    grid <- get_grid(range_CH, cellsize = 5)
    grid@grid@cellcentre.offset
    grid_poly <- as(grid, "SpatialPolygonsDataFrame")
    
    years <- c(1980, 1990, 1995, 2000, 2005, 2010, 2015) %>% set_names(., .)
    # lst_st_urban = readRDS("data-raw/V02/st_dynamic_urban.RDS")
    # HW_sps = readRDS("../ChinaHW_data/OUTPUT/OBS_China_HW_characteristics_met2481-(1951-2018).RDS")
    st_met2370$kind = st_met2370$is_city
}

df <- sp2grid_urbanCont(trend_year.lm, st_met2370, grid)
gridId <- df[type_data %in% c("Rural", "Urban"), .N, .(id)][N == 2, id] %>% 
    as.character() %>% as.numeric()
grid_clip <- grid[gridId, ]

d_urban <- df[id %in% gridId & type_data == "Urban"]
d_rural <- df[id %in% gridId & type_data == "Rural"]
d_cont  <- d_urban[, -(1:2)] - d_rural[, -(1:2)]

grid_clip@data <- d_cont[, c(1, 3, 5)]
{
    brks   <- { c(0.2, 0.5, 1, 2, 5, 10, Inf) / 100 } %>% c(-rev(.), 0, .)
    ncolor <- length(brks) - 1
    cols   <- rcolors::get_color("NCV_blue_red", ncolor)
    p <- spplot_grid(grid_clip,
        colors = cols, brks = brks,
        legend.num2factor = TRUE,
        panel = function(x, y, z, subscripts, ..., sp.layout) {
            sppanel(list(sp.layout), panel.number(), first = TRUE)
            # z = rep(1, length(x))
            panel.levelplot(x, y, z, subscripts,
                # at = c(0, 1, 2),
                # col.regions = "grey90" %>% rep(3),
                ...,
                col = "black", lty = 3,
                border = "black",
                lwd = 0.5
            )
            # I_bad <- match(ids_bad, z)
            # panel.points(x[subscripts][I_bad], y[subscripts][I_bad], col = "red", pch = 4, cex = 2)
            panel.text(x[subscripts], y[subscripts], gridId)
            sppanel(list(sp.layout), panel.number(), first = FALSE)
        },
        # as.table = TRUE,
        sp.layout = list("sp.lines", CH_arc),
        layout = c(2, 2),
        strip = TRUE,
        aspect = 0.7, 
        par.settings2 = list(axis.line = list(col = "black"))) + 
        theme_lattice(plot.margin = c(0.5, 6, 0.5, 0.5))
    write_fig(p, "Urban contribution to HI, Tavg and RH.pdf", 8, 6)
}



## 测试华北平原的异常点
library(sp2)

# -6.859294e-03
st = st_all[, .(id, site, lon, lat, kind = is_city, gridId = gridId_all)]
st2 = st[gridId == 35, .(site, kind, lat, lon)]
sp2 <- df2sp(st2)

d = merge(trend_year.lm, st2[, .(site, kind)])
d[, as.list(summary(Tavg.slope)), .(kind)]
