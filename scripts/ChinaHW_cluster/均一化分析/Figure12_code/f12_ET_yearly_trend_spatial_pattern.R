library(data.table)
library(magrittr)
library(missInfo)
library(ggplot2)
library(dplyr)
library(tidyr)
devtools::load_all("E:/github_repos/hydroET/")

sf_china <- sf::read_sf("shp/WGS84-1.shp")
sf_basins <-
  sf::read_sf("H:/data for cal_PET/OUTPUT/Final_output/water_balance/shp_Bou/All_Basins.shp")

dt_plot <-
  lapply(
    dir("inst/data/paper_output/PML_ET_forcing/", "_8day", full.names = T) %>% set_names(., .),
    function(dt) {
      fread(dt) %>%
        ET_day8_to_yearly() %>%
        .[ET != 0]
    }
  ) %>%
  rbindlist(idcol = "product") %>%
  dplyr::mutate(product = stringr::str_extract(product, "(?<=dt_ET_).*?(?=_8day_)")) %>%
  dplyr::mutate(
    product = fcase(
      product == "CFSV2", "CFSV2",
      product == "MERRA2", "MERRA2",
      product == "JRA55", "JRA55",
      product == "GLDASV21", "GLDASV21",
      product == "ERA5L", "ERA5L",
      product == "score", "OPTIMAL"
    ),
    product = forcats::fct_relevel(product, c("OPTIMAL", "ERA5L", "CFSV2", "GLDASV21", "JRA55", "MERRA2"))
  ) %>%
  .[, .(dt = .(.SD)), .(product, site)] %>%
  mutate(
    slp = purrr::map_dbl(dt, ~ rtrend::mkTrend(.x$ET)[5]),
    pval = purrr::map_dbl(dt, ~ rtrend::mkTrend(.x$ET)[4]),
    dt = NULL
  ) %>%
  `[`(st_met2481[, .(site, lon, lat)], ., on = "site") %>%
  mutate(
    size_sig = fcase(
      pval < 0.01, "p<0.01",
      pval < 0.05, "p<0.05",
      default = "Not significant"
    ),
    size_sig = forcats::fct_relevel(size_sig, c("p<0.01", "p<0.05", "Not significant"))
  )


brks <- c(-Inf, seq(-10, 10, 2), Inf)
col_name <- "amwg256"
cols <- rcolors::get_color(col = col_name, length(brks) - 1)
# cols[6:7] = 'whitesmoke'

dt_lab <-
  dt_plot[, .(product)] %>%
  unique() %>%
  .[order(product)] %>%
  dplyr::mutate(lab = glue::glue("({letters[1:6]})"))


# add Positive/Negative
lab <-
  dt_plot %>%
  mutate(PorN = ifelse(slp >= 0, "P", "N")) %>%
  group_by(product) %>%
  summarise(
    P = (length(PorN[which(PorN == "P")]) / dplyr::n() * 100) %>% round(1) %>% paste0(., "%"),
    N = (length(PorN[which(PorN == "N")]) / dplyr::n() * 100) %>% round(1) %>% paste0(., "%"),
    P0.05 = (length(PorN[which(PorN == "P" & pval < 0.05)]) /
      dplyr::n() * 100) %>% round(1) %>% paste0(., "%"),
    N0.05 = (length(PorN[which(PorN == "N" & pval < 0.05)]) /
      dplyr::n() * 100) %>% round(1) %>% paste0(., "%"),
    Plab = paste0("P: ", P, " (", P0.05, ")"),
    Nlab = paste0("N: ", N, " (", N0.05, ")")
  ) %>%
  as.data.table()


dt_plot %>%
  dplyr::mutate(slp_cut = cut(slp, brks)) %>%
  tidyr::drop_na() %>%
  ggplot() +
  theme_bw() +
  ggpp::geom_text_npc(
    data = dt_lab, aes(npcx = 0.05, npcy = 0.95, label = lab),
    family = "sans", fontface = "bold",
  ) +
  ggpp::geom_text_npc(
    data = lab, aes(npcx = 0.31, npcy = 0.95, label = Plab),
    family = "sans", color = rcolors::get_color("amwg256", 8)[7],
    hjust = 0
  ) +
  ggpp::geom_text_npc(
    data = lab, aes(npcx = 0.31, npcy = 0.88, label = Nlab),
    family = "sans", color = rcolors::get_color("amwg256", 8)[2],
    hjust = 0
  ) +
  geom_point(aes(lon, lat, color = slp_cut, size = size_sig, shape = size_sig)) +
  geom_sf(data = sf_china, color = "black", size = 1) +
  geom_sf(data = sf_basins, color = "black", fill = NA, size = 0.4) +
  scale_size_manual(values = c(1, 0.5, 0.1)) +
  scale_shape_manual(values = c(19, 19, 3)) +
  coord_sf(ylim = c(18.01558, 53.55793)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values = cols) +
  facet_wrap(~product, nrow = 2) +
  theme(
    plot.margin = unit(rep(0.4, 4), "cm"),
    panel.border = element_rect(size = 1),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.justification = "top",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, -10, -10, -10),
    legend.background = element_blank(),
    legend.key = element_blank(),
    axis.text = element_text(color = "black"),
    text = element_text(family = "sans")
  ) +
  guides(color = "none") +
  labs(x = NULL, y = NULL, shape = NULL, size = NULL) +
  facet_subgraphs(
    data = dt_plot %>%
      dplyr::mutate(slp_cut = cut(slp, brks)) %>%
      tidyr::drop_na(),
    group_colnames = "product",
    plot_func = plot_func,
    xmin = 73.7, xmax = 97, ymin = 18.3, ymax = 27
  ) +
  gg.layers::make_colorbar(
    at = brks, col = cols, width = 1.5,
    family = "sans", title = expression("mm yr"^"-1"), tck = 0
  ) -> p


ggsave(
  filename = "figures/paper_Figures/Figure12_ET_yearly_MKtrend_spatial_dist.jpg",
  plot = p, dpi = 600, height = 7, width = 14
)


plot_func <- function(dt) {
  dt_pt <-
    dt$slp_cut %>%
    table() %>%
    cbind(I = 1:nrow(.)) %>%
    data.table(fct = rownames(.)) %>%
    .[, .(fct, Freq = ., I)] %>%
    mutate(
      perc = Freq / sum(Freq) * 100,
      col = cols,
      I = as.character(I),
      I = forcats::fct_relevel(I, as.character(1:12))
    ) %>%
    drop_na(perc)

  bar_cols <- dt_pt$col

  dt_pt %>%
    ggplot(aes(I, perc)) +
    theme_classic() +
    geom_bar(aes(fill = I), color = "black", stat = "identity", show.legend = F) +
    scale_fill_manual(values = bar_cols) +
    scale_y_continuous(expand = rep(0, 2)) +
    scale_x_discrete(expand = rep(0, 2)) +
    theme(
      panel.background = element_blank(),
      plot.background = element_blank(),
      text = element_text(family = "sans"),
      axis.text.x = element_blank(),
      axis.text = element_text(color = "black", size = 6),
      axis.ticks.x = element_blank(),
      plot.margin = unit(rep(0, 4), "mm"),
      axis.title.y = element_text(size = 7, vjust = 0.1)
    ) +
    labs(x = NULL, y = expression("Fraction (%)"))
}

# ggplot add annotation to each facet

