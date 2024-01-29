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

deseasonalized = month_ts - mseasonal
time(deseasonalized)
# lag on 1 year : evidence of perhaps un-accounted for serial dependence dependent
# on the month of the year. cyclical seasonal dependence
acf(deseasonalized, na.action = na.pass, lag.max = 12)

# lag on many years: this pattern repeats on a consistent cycle. 
# although, declines over time
acf(deseasonalized, na.action = na.pass, lag.max = 408)

# calculating what's one year in the TS
delta = time(deseasonalized)[2] - time(deseasonalized)[1]
# calculating what's the entire TS
(2024-1990)/delta

d_acf <- acf(deseasonalized, 
             na.action = na.pass, 
             lag.max = 408, plot = FALSE)


p2data = data.frame(
  h = 0:19,
  rh = acf(summer_series, plot = FALSE)$acf
)

p2 <- ggplot(p2data, aes(x = h, y = rh)) +
  geom_segment(aes(xend = h, yend = 0),
               color = "#eb5e28",
               linewidth = 1) +
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