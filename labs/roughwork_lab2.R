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


subset = window(summer_series, start = 1968, end = 2008)
p3data <- fortify.zoo(subset)

p3 <- ggplot(p3data, aes(x = Index, y = subset)) +
  geom_line(color = "#cc7a00", linewidth = 0.65) +
  labs(
    title = "Recent Summer Temperatures at Prince George, BC",
    subtitle = "Measured in homogenized daily minimum temperatures (C)",
    x = "Mean Temperatures (Celsius)",
    y = "Year"
  ) + theme_bw() 
  theme(panel.grid.minor = element_line(
    color = "grey90",
    linetype = "dashed",
    linewidth = 0.5
  ))
print(p3)
df_roll_mean = data.frame(RollingMean = rollmean(subset, k = 5, fill = NA),
                          Time = p3data$Index)
p3 = p3 + geom_line(
  data = df_roll_mean,
  aes(x = Time, y = RollingMean),
  color = "#665200",
  na.rm = TRUE
)
print(p3)

p4df = data.frame(
  subset = p3data$subset,
  rollmea = rollmean(subset, k = 5, fill = NA),
  Index = p3data$Index
)
p4df_long <- melt(p4df, id.vars = "Index", variable.name = "LineType", value.name = "Value")

p4 <- ggplot(p4df_long, aes(x = Index, y = Value, color = LineType)) +
  geom_line(linewidth = 0.65) +
  labs(
    title = "Recent Summer Temperatures at Prince George, BC",
    subtitle = "Measured in homogenized daily minimum temperatures (C)",
    x = "Year",
    y = "Mean Temperatures (Celsius)"
  ) + 
  theme_bw() +
  scale_color_manual(
    values = c("subset" = "#cc7a00", "rollmea" = "#665200"),
    labels = c("Temperature", "5-Year Rolling Mean")
  ) +
  theme(panel.grid.minor = element_line(
    color = "grey90",
    linetype = "dashed",
    linewidth = 0.5
  ))

print(p4)


########### PART 2 ##########

df2 = read.csv("labs//LakeLevels.csv")

nrow(df2)

lakeseries = ts(df2$LakeLevel, 
                start = c(2007, 1),
                frequency = 365)
acf(lakeseries, lag.max = nrow(df2))

plot(lakeseries)

p5data = data.frame(
  h = 0:(nrow(df2)-1),
  rh = acf(lakeseries, lag.max = nrow(df2))$acf
)

p5 <- ggplot(p5data, aes(x = h, y = rh)) +
  geom_segment(aes(xend = h, yend = 0),
               color = "grey60",
               size = 1) +
  geom_hline(yintercept = 0.2, linetype = "dashed", col = "grey20")+
  geom_hline(yintercept = -0.2, linetype = "dashed", col = "grey20")+
  ylim(-0.75, 1)+
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "darkgray") +
  labs(x = "Lag", y = "Autocorrelation", 
       title = "Correlogram of Summer Temperature Data") +
  theme_bw()
print(p5)
