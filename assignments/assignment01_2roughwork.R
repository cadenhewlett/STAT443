dat = read.csv("assignments/NY_Temperature_Data.csv")

library(zoo)

x = zoo(dat$TMAX, zoo::as.Date(dat$Date))

monthly_max = aggregate(x, as.yearmon, FUN=max)

# # 34 years and the start of one quarter
# length(aggregate(x, as.yearqtr, FUN=max))/4
# # reflected here
# min(dat$Date), max(dat$Date)

library(ggfortify)
p2df <- fortify.zoo(monthly_max)
ggplot(p2df, aes(x = Index, y = monthly_max)) +
  geom_line(color = "#f48c06") + theme_bw() +
  labs(
    x = "Year",
    y =  expression(paste("Monthly Maximum (", degree, "C)")),
    title = expression(paste(
      "Monthly Maximum Temperature (", degree, "C) in New York"
    )),
    subtitle = "Measured From 1990-2024, Sourced from NOAA"
  ) +
  coord_fixed(ratio = 0.275)

