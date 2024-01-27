dat = read.csv("assignments/NY_Temperature_Data.csv")

library(zoo)
library(gridExtra)
library(grid)
x = zoo(dat$TMAX, zoo::as.Date(dat$Date))

monthly_max = aggregate(x, as.yearmon, FUN=max)

# # 34 years and the start of one quarter
# length(aggregate(x, as.yearqtr, FUN=max))/4
# # reflected here
# min(dat$Date), max(dat$Date)

# library(ggfortify)
# p2df <- fortify.zoo(monthly_max)
# ggplot(p2df, aes(x = Index, y = monthly_max)) +
#   geom_line(color = "#f48c06") + theme_bw() +
#   labs(
#     x = "Year",
#     y =  expression(paste("Monthly Maximum (", degree, "C)")),
#     title = expression(paste(
#       "Monthly Maximum Temperature (", degree, "C) in New York"
#     )),
#     subtitle = "Measured From 1990-2024, Sourced from NOAA"
#   ) +
#   coord_fixed(ratio = 0.275)

month_ts <- ts(zooreg(monthly_max), 
               start = c(1990, 1),
               end = c(2024, 1),
               frequency = 12)

month_decomp <- decompose(month_ts, type = "additive")
mseasonal = month_decomp$seasonal
mtrend = month_decomp$trend
mnoise = month_decomp$random

sp2c <- ggplot(data = fortify.zoo(mseasonal), aes(x = Index, y = mseasonal)) +
  geom_line(color = "#9d0208", linewidth = 0.5) +
  theme_bw() +
  labs(
    title = "Seasonal Component of Additive Model",
    y = "st",
    x = NULL
  ) +  xlim(1990, 2024)

mp2c <- ggplot(data = fortify.zoo(mtrend), aes(x = Index, y = mtrend)) +
  geom_line(color = "#dc2f02", linewidth = 0.5) +
  theme_bw() +
  labs(
    title = "Trend Component of Additive Model",
    y = "mt",
    x = NULL
  )  +  xlim(1990, 2024)

zp2c <- ggplot(data = fortify.zoo(mnoise), aes(x = Index, y = mnoise)) +
  geom_line(color = "#e85d04", linewidth = 0.5) +
  theme_bw() +
  labs(
    title = "Random Component of Additive Model",
    y = "Zt",
    x = "Year"
  )  +  xlim(1990, 2024)

grid.arrange(sp2c, mp2c, zp2c)

##### part d

deseasonalized = month_ts - mtrend
acf(deseasonalized, na.action = na.pass)
?acf
#time(month_ts)
