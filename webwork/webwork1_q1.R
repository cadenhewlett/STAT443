## Questions 1 and 2
library(zoo)
library(tseries)
df = read.csv("webwork/file1dc4079c75a.csv")

series = ts(df$Seattle, 
            start = c(2004, 3),
            frequency = 4)

length(series) == nrow(df)

plot(series, ylab = "Seattle Home Prices")

smoothed = rollmean(series, k = 4,  align=  'center')
as.numeric(smoothed)[3]

## estimating seasonal effect
diff_ts = series - smoothed

period = 4
S_j =  sapply(1:period, function(p){
          mean(diff_ts[ cycle(difference_ts) == p ])})

epsilon = sum(S_j) / period
# these get the adjusted S_j values
adjusted = S_j - epsilon
# this is the adjusted seasonal effect
print(adjusted)

####### QUESTION 3 ######
df2 = read.csv("webwork/file2146e0e8396.csv")
# series2 = ts(df2$Seattle, 
#            start = c(2008, 3),
#            frequency = 4
#           )

comb = rbind(df, df2)
series_big = ts(comb$Seattle,
                start = c(2004, 3), 
                frequency = 4)
plot(series_big)


### FOR FUNSIES ####
library(forecast)

fit <- HoltWinters(series, 
                   seasonal = "additive", 
                   beta = FALSE, 
                   gamma = TRUE)

forecasted_values <- forecast(fit, h=12)

# collect actual data
original_df <- data.frame(
  Time = as.numeric(time(series_big)),
  Value = as.numeric(series_big)
)
# format into data frame
forecast_df <- data.frame(
  Time = time(forecasted_values$mean),
  Forecast = as.numeric(forecasted_values$mean),
  Lower80 = as.numeric(forecasted_values$lower[, "80%"]),
  Upper80 = as.numeric(forecasted_values$upper[, "80%"]),
  Lower95 = as.numeric(forecasted_values$lower[, "95%"]),
  Upper95 = as.numeric(forecasted_values$upper[, "95%"])
)

# original_df$Type <- "Actual"
# forecast_df$Type <- "Forecast"

original_df <- original_df %>% mutate(Type = "Actual")
forecast_df <- forecast_df %>% mutate(Type = "Forecast", 
                                      Value = Forecast) 


library(dplyr)
combined_df <- bind_rows(
  forecast_df,
  original_df
)


g<-ggplot(data = combined_df, aes(x = Time)) +
  geom_line(aes(y = Value, color = Type), size = 1, na.rm = TRUE) +
  geom_ribbon(
    data = subset(combined_df, Type == "Forecast"),
    aes(ymin = Lower95, ymax = Upper95),
    fill = "#b7e4c7",
    alpha = 0.4,
    na.rm = TRUE
  ) +
  geom_ribbon(
    data = subset(combined_df, Type == "Forecast"),
    aes(ymin = Lower80, ymax = Upper80),
    fill = "#52b788",
    alpha = 0.2,
    na.rm = TRUE
  ) +
  geom_vline(
    xintercept = 2008.5,
    color = "#95d5b2",
    linetype = "dashed",
    size = 1
  ) +
  scale_color_manual(
    values = c("Actual" = "#2d6a4f", 
               "Forecast" = "#52b788")) +
  xlim(2005, 2010) +
  labs(
    title = "Time Series and Forecast of Home Sales in Seattle, 2005-2010",
    subtitle = "Holt-Withers Forecast Includes 80% and 95% Error Bounds",
    x = "Year",
    y = "Average Number of Homes Sold"
  ) +
  annotate("text", x = 2009.10, color = "#52b788",
           y = 1660, label = "July, 2008", 
           hjust = 1, vjust = 0) +
  theme_bw() +
  theme(panel.grid.minor =  element_line(color = "grey90", 
                                         size = 0.35,
                                         linetype = "dashed"))
print(g)

