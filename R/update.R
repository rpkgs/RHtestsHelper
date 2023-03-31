plot_output <- function(data, outfile = NULL, ..., show = FALSE) {
  data %<>% mutate(anomaly_adjusted = QM_adjusted - base + base_anomaly)

  vars_bot = c("base_anomaly", "fit_of_base_anomaly", "fit_of_deseason_base", "anomaly_adjusted")
  vars_top = c("base_trend+meanShift", "base_mean_adj", "QM_adjusted")
  # vars_del = c("base")

  dates = data$date
  nday = difftime(dates[2], dates[1], units = "days") %>% 
    as.integer() %>% abs()
  scale = ifelse(nday >= 100, "year", "month")
  if (scale == "month") vars_top %<>% c("annualC+meanShifts")

  d_obs <- data[, .(date, base)]
  pdat <- melt(data, c("id", "date"))
  data_bot = pdat[variable %in% vars_bot, ]
  data_top = pdat[variable %in% vars_top, ]
  
  func <- function(data, show.obs = TRUE) {
    p = ggplot(data, aes(date, value)) +
      geom_line(aes(color = variable)) +
      labs(color = NULL, x = NULL) +
      theme(legend.position = "right")
    
    if (show.obs) 
      p = p + geom_line(data = d_obs, aes(y = base), color = alpha("black", 0.6))
    p
  }

  p1 = func(data_top)
  p2 = func(data_bot, show.obs = FALSE)
  p = patchwork::wrap_plots(p1, p2, nrow = 2)
  # geom_point(aes(color = variable, shape = variable))
  if (!is.null(outfile)) {
    write_fig(p, outfile, 10, 6, show = show)
  } else {
    p
  }
}
