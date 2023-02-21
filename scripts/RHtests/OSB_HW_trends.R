devtools::load_all()
source("test/main_pkgs.R")
source("test/00_basement.R")

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
    # file_HW <- "../ChinaHW_data/OUTPUT/OBS_China_HW_characteristics_met2481-(1951-2018).RDS"
    file_HW <- "../ChinaHW_data/OUTPUT/OBS_China_HW_characteristics_met2481-(1951-2018)-(QM-adjusted).RDS"
    r = readRDS(file_HW)
    r[duration == 0, `:=`(intensity = 0, volume = 0)]
    HW_sps <- r
    
    # st_met2370$kind   = st_met2370$is_city
    st_met2370$gridId = raster::extract(raster(grid), df2sp(st_met2370))
    st_ren = st_met2370[, .(site, lon, lat, kind = is_city, gridId)]
}

strategy = c("Liao", "Ren", "Liao_Urban + Ren_Rural")[1]
type = "dynamic"
urban_cont_5grid(grid, "Liao", type)
urban_cont_5grid(grid, "Ren", type)
urban_cont_5grid(grid, "Liao_Urban + Ren_Rural", type)

type <- "static"
urban_cont_5grid(grid, "Liao", type)
# urban_cont_5grid(grid, "Ren", type)
urban_cont_5grid(grid, "Liao_Urban + Ren_Rural", type)

### 7*7 (3+1+3)buffer
# static urban 331 sites, 35 grids
## index      Rural      Urban       Cu
# 1:  duration 0.06580317 0.08545900 23.00030
# 2: frequency 0.02260061 0.02816431 19.75441
# 3: intensity 0.01326124 0.01933464 31.41201
# 4:    volume 0.09906967 0.14235004 30.40418
# 5:        PR 0.01801647 0.02339430 22.98778

## 1.1 Liao static urban
#        index      Rural      Urban       Cu
# 1:  duration 0.07099894 0.09001262 21.12336
# 2: frequency 0.02532126 0.02980181 15.03447
# 3: intensity 0.01536280 0.01971841 22.08904
# 4:    volume 0.11427996 0.14894299 23.27268
# 5:        PR 0.01943702 0.02464121 21.11986
## 1.2  Liao dynamic
#        index      Rural      Urban       Cu
# 1:  duration 0.07154847 0.08970851 20.24339
# 2: frequency 0.02505813 0.03021278 17.06115
# 3: intensity 0.01420302 0.01921819 26.09597
# 4:    volume 0.11226051 0.15079825 25.55582
# 5:        PR 0.01958732 0.02455790 20.24027

## 检查 gridId == 53的情况


# saveRDS(res, "data-raw/OBS_China_HW_characteristics_met2481_dynamic_urban.RDS")
# res[probs == 0.99, .(slope = lm(value~year)$coefficients[2]), .(type_data, probs, index)]

# ggplot(res[probs == 0.99 & index == "frequency"], aes(year, value, color = type_data)) + 
#     geom_line() + geom_smooth(method = "lm")
# st = lst_st_urban$`1980`
# 在绘制图2.年变化曲线时，不需要计算anomaly
# 只分析同时存在农村站点和城市站点的网格
# st <- st_all[, .(site, type_region = is_east, type_data = is_city, gridId = gridId_rural)] 
# 
# r = merge(HW_sps, st[!is.na(gridId), ], by = c("site")) %>% 
#     melt(measure.vars = indices, variable.name = "index")
# df_HW <- HW_point2grid(r, gridInfo_5deg)
# df_HW.obs <- HW_grid_RegionMean(df_HW)
