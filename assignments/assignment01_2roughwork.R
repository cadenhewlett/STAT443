dat = read.csv("assignments/NY_Temperature_Data.csv")

library(zoo)

x = zoo(dat$TMAX, zoo::as.Date(dat$Date))

monthly_max = aggregate(x, as.yearmon, FUN=max)

# # 34 years and the start of one quarter
# length(aggregate(x, as.yearqtr, FUN=max))/4
# # reflected here
# min(dat$Date), max(dat$Date)

plot(monthly_max)
