library(zoo)
library(tseries)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(forecast)
library(ggfortify)
library(grid)
library(gt)
library(gtExtras)

test = arima.sim(n = 1000, list(ar = c(0, -0.5), ma = c(0.5)),
          sd = 80)
acf(test)
