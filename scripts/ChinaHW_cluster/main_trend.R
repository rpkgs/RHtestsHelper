fun_aggregate <- function(x) {
  list(
    value = mean(x, na.rm = TRUE),
    nmiss = sum(is.na(x))
  )
}

# save(lst_mon, lst_year, l_trend, df, file = fout)
# aggregate_year <- function(df, varnames, fout = NULL, ...) {
#   # 1. 合并到月是，每个月缺测值不能超过三个
#   # 2. 合并到年时，月数据不能有缺测
# }

cal_trend <- function(lst_year, year_max = 2022, ...) {
  l_trend <- foreach(dat = lst_year, i = icount()) %do% {
    runningId(i)
    dat = dat[year <= year_max, ]

    info <- dat[, .(
      year_begin = min(year), year_end = max(year),
      nvalid = .N, nmiss = max(year) + 1 - min(year) - .N
    ), .(site)]

    d_trend <- list(
      mk = dat[, slope_mk(value, year) %>% as.list(), .(site)],
      lm = dat[, slope_p(value, year)[1:2] %>% as.list(), .(site)]
    ) %>% melt_list("type")
    merge(info, d_trend)
  }
  df <- melt_list(l_trend, "varname") %>% merge(st_met2481[, .(site, lon, lat)], .)
  df
}

plot_trend_tair <- function(pdat, ..., 
  theme = NULL, 
  fontsize = 12, lgd_title = lab_trend_Tair)
{
  p = ggplot(pdat, aes(lon, lat, size = slp_shape)) +
    add_china() + 
    geom_sf(data = shp_prov, inherit.aes = FALSE, 
      color = "black", fill = "transparent", linewidth = 0.15) + 
    geom_point(
      data = pdat[pvalue <= 0.05],
      aes(color = slp_lev), na.rm = FALSE, shape = 19, 
      stroke = 0.2,
      alpha = 0.7
    ) +
    # non-significant
    geom_point(
      data = pdat[pvalue > 0.05],
      aes(color = slp_lev), na.rm = FALSE, shape = 21, stroke = 0.3,
      color = "black", 
      fill = "transparent", 
      alpha = 0.3
    ) + 
    layer_PosNeg_sign(aes(z = slope, mask = pvalue <= 0.05), 
      hjust = 0, vjust = 1,
      x = 0.3, y = 0.95, height.factor = 2) + 
    layer_barchart(aes(z = slope * 10), brks = brks, cols = cols, 
      x = 0.005, y = 0.005,
      width = unit(0.35, "npc"), height = unit(0.3, "npc"),
      fontsize = 7) + 
    # scale_size_binned(range = c(0.2, 1)) +
    # scale_size_area(max_size = 1) +
    scale_color_manual(values = cols) + 
    scale_size_manual(values = c(0.2, 0.6, 0.8, 1)) + 
    # theme_bw() + 
    # theme(
    #   axis.ticks.length = unit(-0.1, "cm"),
    #   axis.title = element_blank(),
    #   axis.text = element_blank()
    # ) + 
    theme(
      panel.background = element_rect(fill = "white"),
      axis.ticks = element_blank(), 
      strip.text.y.left = element_textbox(size = fontsize, face = "bold", 
          # angle = 90,
        orientation = "left-rotated", 
        margin = margin(l = 2, r = 2)),
      strip.text.x = element_textbox(size = fontsize, face = "bold", margin = margin(t = 2, b = 0)),
      legend.position = "none"
    ) + 
    facet_grid(varname ~ type_homo, labeller = label_mk, switch = "y")
  if (!is.null(theme)) p <- p + theme
  p  + 
    gg.layers::make_colorbar(
      at = brks, col = cols, width = 1.5,
      title = lgd_title, 
      legend.title = element_text(family = "Times"),
      tck = 0
    )
}
