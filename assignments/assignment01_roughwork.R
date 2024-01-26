library(zoo)
library(tseries)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(forecast)


#######   QUESTION 1 #######
df <- read.csv("assignments/usual_hours_worked_ca.csv")

jobseries = ts(data = df$Hours,
               start = c(1987, 1),
               frequency = 12)

# contains monthly average values of the usual hours
# worked across all industries in Canada for the period 
# from January 1987 until December 2023
p1data = fortify.zoo(jobseries)
p1 <- ggplot(p1data, aes(x = Index, y = jobseries)) +
  geom_line(color = "#52b69a", linewidth = 0.65) +
  labs(
    title = "Monthly Average of Usual Hours Worked in Canada",
    subtitle = "Across all Industries from January 1987 to December 2023",
    x = "Monthly Mean Working Time (Hours)",
    y = "Year"
  ) + theme_bw() +
  theme(panel.grid.minor = element_line(
    color = "grey90",
    linetype = "dashed",
    linewidth = 0.5
  ))
print(p1)


### test
set.seed(09042002)
st = 2*seq(from = 0, to = 8*pi, length.out = 1000)
mt = seq(from = 10, to = 0, length.out = 1000)
par(mfrow = c(1,2))
plot( mt + sin( st ), type = 'l',
      ylab = "Value of Xt", xlab = "Time (t)",
      main = "Additive Seasonal Effect Model")
plot( mt*sin(st), type = 'l',
      ylab = "Value of Xt", xlab = "Time (t)",
      main = "Multiplicative Seasonal Effect Model")


### q1b

train <- window(jobseries, 
           # starting at the beginning of 1987
           start = 1987, 
           # ending at the end of 2021
           end = c(2021, 12), 
           # monthly
           frequency = 12)

# get test data
test <- window(jobseries,
               start = c(2022, 1),
               end = c(2023, 12),
               frequency = 12)
length(test)
# verify we've done things correctly
all.equal(length(test) + length(train),
          length(jobseries),
          nrow(df))

to_additive = log(train)
loess = (stl(to_additive, s.window = "periodic"))
seasonal = loess$time.series[, 1]
trend = loess$time.series[, 2]
noise = loess$time.series[, 3]
# zt back to multiplicative
exp(mean(noise))

sp2 <- ggplot(data = fortify.zoo(seasonal), aes(x = Index, y = seasonal)) +
  geom_line(color = "#5e548e", linewidth = 0.5) +
  theme_bw() + 
  labs(
    title = "Seasonal Component of Multiplicative Model",
    y = "log(st)",
    x = NULL
  ) + 
  ylim(-0.02, 0.03)+ #xlim(1990, 2000)+
  theme(panel.grid.minor = element_line(
    color = "grey90",
    linetype = "dashed",
    linewidth = 0.5
  ))


mp2 <- ggplot(data = fortify.zoo(trend), aes(x = Index, y = trend)) +
  geom_line(color = "#9f86c0", linewidth = 0.5) +
  theme_bw() + 
  labs(
    title = "Trend Component of Multiplicative Model",
    y = "log(mt)",
    x = NULL
  ) + 
  theme(panel.grid.minor = element_line(
    color = "grey90",
    linetype = "dashed",
    linewidth = 0.5
  ))

zp2 <- ggplot(data = fortify.zoo(noise), aes(x = Index, y = noise)) +
  geom_line(color = "#be95c4", linewidth = 0.5) +
  theme_bw() + 
  labs(
    title = "Random Component of Multiplicative Model",
    y = "log(Zt)",
    x = "Year"
  ) + 
  theme(panel.grid.minor = element_line(
    color = "grey90",
    linetype = "dashed",
    linewidth = 0.5
  ))

grid.arrange(sp2, mp2, zp2, nrow = 3)


### now, specifically, the trend component

trend_df <- fortify.zoo(trend)

colnames(trend_df) = c("Time", "log_hours")

trend_mod <- lm(log_hours~Time, data = trend_df)

trend_summary = summary(trend_mod)

se = trend_summary$coefficients[2, "Std. Error"]
est = trend_summary$coefficients[2, "Estimate"]

tobs = (est - 0)/se

n = nrow(trend_df); k = 1

2 * min( pt( tobs, df = n - (k + 1), lower.tail = TRUE ),
         pt( tobs, df = n - (k + 1), lower.tail = TRUE ))


period = unique(seasonal)

########## part 3

beta_0 = trend_summary$coefficients[1, "Estimate"]
beta_1 = trend_summary$coefficients[2, "Estimate"]

# use our (poor) linear model with time variable
mt_pred = beta_1*time(test) + beta_0
# we're predicting two periods into the future
st_pred = (rep(period, 2))
# then the predicted value is undoing the log transform
preds = exp(mt_pred + st_pred)
# rough plot for now
par(mfrow = c(1,1))
plot(test, ylim = c(34, 37))
lines(preds, col = 'red')
test

p3df <- data.frame(
          Time = as.numeric(time(test)),
          Actual = as.numeric(test), 
          Predicted = preds)
# create custom-spaced month/date strings
date_strings <- unlist(lapply(2022:2023, function(year) {
  sapply(1:12, function(month) {
    if (month %% 6 == 0) paste(month, "/", year, sep = "")
    else ""
  })
}))
# specify additional parameters
date_strings[1] = "1/2022"
low_ylim = 34.5; up_ylim = 36.5
# create plot
p3<-ggplot(p3df) +
  geom_line(aes(x = Time, y = Actual, color = "Actual")) +
  geom_line(aes(x = Time, y = Predicted, color = "Forecast")) +
  scale_color_manual(values = c("Actual" = "#373d20", "Forecast" = "#a98467"),
                     name = "Type", labels = c("Actual", "Forecast")) +
  labs(
    title = "Monthly Average of Usual Hours Worked in Canada, with Forecast",
    subtitle = "Across all Industries from January 2022 to December 2023",
    y = "Monthly Mean Working Time (Hours)",
    x = "Year"
  ) + theme_bw() + 
  scale_x_continuous(breaks = p3df$Time, labels = date_strings) +
  scale_y_continuous(limits = c(low_ylim, up_ylim),
                     breaks = seq(low_ylim, up_ylim, by = 0.5),
                     labels = seq(low_ylim, up_ylim, by = 0.5)) +
  theme(panel.grid.major = element_line(
    color = "grey95",
    linetype = "solid",
    linewidth = 0.5
  ), 
  panel.grid.minor.y = element_line(
    color = "grey95",
    linetype = "solid",
    linewidth = 0.5
  ))
print(p3)


