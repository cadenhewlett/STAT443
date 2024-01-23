library(zoo)
library(tseries)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(forecast)

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

trend_df

2022 + (0:23)/12

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



library(splines)

# cubic spline model
n_knots <- 5
knots <- quantile(trend_df$Time, 
                  probs = seq(0, 1, length.out = n_knots + 2)[-c(1, n_knots + 2)])
model <- lm(log_hours ~ bs(Time, knots=knots, degree=3), data = trend_df)
mt_pred_fancy <- predict(model, data.frame(Time = time(test)))
preds_f = exp(mt_pred_fancy + st_pred)
pr_df = data.frame(p = as.numeric(preds_f), t = time(test))

# poly model
modelp <- lm(log_hours ~ poly(Time, degree = 3), data = trend_df)
mt_pred_poly <- predict(modelp, data.frame(Time = time(test)))
preds_p = exp(mt_pred_poly + st_pred)
pp_df = data.frame(p = as.numeric(preds_p), t = time(test))

# GAM (Generalized Additive Model)
# \mathbb{E}{Y \mid X=x} = \beta_0 + f_1(x_{1})+\cdots+f_p(x_{p}).
# produced via Generalized Cross Validation (GCV) 

library(mgcv)
gam_vers <- gam(log_hours ~ s(Time), data = trend_df)
mt_pred_g <- predict(gam_vers, data.frame(Time = time(test)))
preds_g = exp(mt_pred_g + st_pred)
pg_df = data.frame(p = as.numeric(preds_g), t = time(test))

# rough plot for now
par(mfrow = c(1, 1))
plot(test, ylim = c(33, 38))
lines(x = as.numeric(pr_df$t), y  = pr_df$p, col = 'blue')
lines(preds, col = 'red')
# lines(x = as.numeric(pp_df$t), y  = pp_df$p, col = 'green')
lines(x = as.numeric(pg_df$t), y = pg_df$p, col = "purple")


####### resids


