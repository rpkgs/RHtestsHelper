## FUNCTIONS for ggplot spatial map --------------------------------------------
shp = "//kong-nas/CMIP6/GitHub/shapefiles/China/bou1_4l_south_sml.shp" %>% sf::read_sf()
shp_prov <- read_sf("//kong-nas/CMIP6/GitHub/cug-hydro/CUG-HydroMeteorology.py/data/shp/bou2_4p_ChinaProvince_sml.shp")

add_china <- function(size = 0.5) {
  list(
    geom_sf(data = shp, aes(x = NULL, y = NULL, z = NULL, color = NULL), fill = NA, size = size),
    coord_sf(expand = T, xlim = c(75, 134), ylim = c(16, 53)),
    labs(x = "", y = ""),
    theme(
      # plot.margin = margin(7, 7, 7, 7, unit = "points"),
      # legend.margin = margin(),
      axis.ticks.length = unit(-0.1, "cm"),
      axis.title = element_blank(),
      axis.text = element_blank()
      # axis.ticks = element_line(size = 0.2),
      # axis.text.y = element_blank(),
      # legend.position = "none"
      # legend.key.width = unit(0.7, "cm")
    )
  )
}

lab_trend_Tair = expression("℃ yr"^"-1")
lab_trend_RH = expression("% yr"^"-1")

get_color2 <- function(col, brks) {
  n <- length(brks) - 1
  n_neg <- sum(brks < 0)
  n_pos <- n - n_neg

  brks2 <- brks[brks > 0] %>% c(-rev(.), 0, .)
  nbrk <- length(brks2) - 1
  cols <- get_color(col, nbrk)

  levs <- cut(0, brks2) %>% levels()
  info <- data.table(cols, levs, ymin = brks2[-nbrk], xmax = brks2[-1])
  info[(n_pos - n_neg + 1):(n_pos * 2), ]
}


#' layer_barchart
#'
#' @inheritParams ggplot2::geom_bar
#' @inheritParams grid::viewport
#' @inheritParams make_colorbar
#'
#' @export
layer_legend <- function(
    mapping = NULL, data = NULL,
    # brks, cols,
    lgd = NULL,
    x = 0, y = 0, width = unit(0.5, "npc"), height = unit(0.5, "npc"), just = c(0, 0),
    ...) {
  fun <- function(data, coords) {
    # p <- add_barchart(data$z, brks, cols, fontsize = fontsize, theme = theme, ...)

    g <- grid::grobTree(as.grob(lgd),
      vp = grid::viewport(
        x = x, y = y, just = just,
        width = width, height = height
      )
    )
    # need to return a grob object
    g
  }
  grid_panel(fun, mapping, data)
}
