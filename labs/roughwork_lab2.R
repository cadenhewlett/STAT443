#read.csv("labs//LakeLevels.csv")
df1 = read.csv("labs//dataTempPG.csv")

summer_data = df1$Summer

# the data are yearly starting in 1919
summer_series = ts(data = summer_data,
                   start = c(1919),
                   frequency = 1)

# prepare for plotting
library(zoo)
library(ggplot2)
p1data = fortify.zoo(summer_series)
p1 <- ggplot(p1data, aes(x = Index, y = summer_series)) +
  geom_line(color = "#E63946", linewidth = 0.65) +
  labs(
    title = "Summer Temperatures at Prince George, BC",
    subtitle = "Measured in homogenized daily minimum temperatures (C)",
    x = "Mean Temperatures (Celsius)",
    y = "Year"
  ) + theme_bw() +
  theme(panel.grid.minor = element_line(
    color = "grey90",
    linetype = "dashed",
    linewidth = 0.5
  ))
print(p1)

# get autocorrelation

p2data = data.frame(
  h = 0:19,
  rh = acf(summer_series, plot = FALSE)$acf
)

p2 <- ggplot(p2data, aes(x = h, y = rh)) +
  geom_segment(aes(xend = h, yend = 0),
               color = "#eb5e28",
               size = 1) +
  geom_point(
    col = "#f6bd60",
    fill = "#f7ede2",
    stroke = 1,
    size = 3,
    shape = 21
  ) +
  geom_hline(yintercept = 0.2, linetype = "dashed", col = "#ffbd00")+
  geom_hline(yintercept = -0.2, linetype = "dashed", col = "#ffbd00")+
  ylim(-0.2, 1)+
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "darkgray") +
  labs(x = "Lag", y = "Autocorrelation", 
       title = "Correlogram of Summer Temperature Data") +
  theme_bw()
print(p2)
acf(summer_series, plot = TRUE)
