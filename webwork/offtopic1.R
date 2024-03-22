library(ggplot2)
library(zoo)

my_acf = function(h){
  (16/11)*((1/2)^h) - (5/11)*((1/5)^h)
}
set.seed(443)

sigma_2 = 1
sim = arima.sim(n = 1000, model = list(ar = c(7/10, -1/10)), sd = sigma_2)

plotDF = data.frame(
  lag = 0:15,
  theoretical = sapply(0:15, my_acf),
  observed = acf(sim, plot = FALSE, lag.max = 15)$acf
)

p <- ggplot(plotDF) + theme_bw() +
  geom_segment(
    aes(
      x = lag,
      xend = lag,
      y = 0,
      yend = observed
    ),
    color = "#ff0a54",
    alpha = 0.5
  )  +
  geom_segment(aes(
    x = lag,
    xend = lag,
    y = 0,
    yend = theoretical
  ),
  color = "#adc2cc")  +
  geom_point(aes(x = lag, y = theoretical, col = "t"),
             shape = 21,
             fill = "#adc2cc") +
  geom_point(aes(x = lag, y = observed, col = "o"),
             shape = 21,
             fill = "#fac3d4") +
  scale_color_manual(
    values = c("t" = "#08415c", "o" = "#720026"),
    labels = c("Theoretical", "Observed"),
    name = "ACF"
  ) +
  theme(
    panel.grid.minor = element_line(
      color = "grey90",
      linetype = "dashed",
      linewidth = 0.5
    ),
    legend.position = "top"
  ) +
  labs(
    x = "Lag",
    y = "Autocorrelation",
    title = "Correlogram of Second-Order AR Process",
    subtitle = "Simulated Values (n = 1000) vs. Theoretical"
  )
print(p)
