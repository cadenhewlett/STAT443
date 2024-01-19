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