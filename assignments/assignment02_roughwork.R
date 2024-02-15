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
par(mfrow = c(1, 1))
sigma = 0.11
test = arima.sim(n = 1000, list(ar = c(0,  -0.5), ma = c(0.5)),
          sd = 0.11)
acf(test)
  ?arima.sim
# https://webspace.maths.qmul.ac.uk/b.bogacka/TimeSeries/TS_Chapter4_6.pdf
psi <- function(j, delta = 0.5){
  # \{\psi_j\} = (-1)^{\,j}(-\delta)^{\lceil j / 2\rceil} B^{\,j}
  (-1)^(j) * (-1*delta)^(ceiling(j/2))
  # (-1*delta)^ceiling(j / 2)
}
pi <- function(j, delta = 0.5){
 # \big(\mathbbm{1}[j \geq 2]\delta(-\delta)^{j - 2}\big) + (- \delta)^j
 (j >= 2)*delta*((-1*delta)^j) + (-1*delta)^j
}

(4/3)*(-1/4)
acf_vlas[3]
# 1 - sum( plot(sapply(0:10, pi)) )
# sum( sapply(0:10000, psi) )
# 
# for_fun = sapply(0:20, function(j){
#   c(psi(j), pi(j))
# })
# 
# plot(for_fun[1, ], for_fun[2, ], type ='l')
# p = 10


par(mfrow = c(1, 2))

test = arima.sim(n = 10000000, list(ar = c(0,  -0.5), ma = c(0.5)),
          sd = 1)

36/3
p = 20

acf_vals = acf(test, lag.max = p, plot= F)$acf

myacf = function(h){
  if((h %% 2) == 0){
    (-0.5)^(h/2)
  }
  else{
    # why not 1/4?
   (1/5)*((-0.5)^((h-1)/2))
  }
}

cols=sapply(0:p, function(h){
  ifelse((h %% 2) == 0, 'red', 'blue')
})
plot(x = sapply(0:p, myacf),
     y = acf_vals, 
     col = cols)
abline(a = 0, b = 1, col = "black", lty='dotted')

plot((acf_vals -sapply(0:p, myacf) )^2)
sum((acf_vals -sapply(0:p, myacf) )^2)

(data.frame(
  x = round(sapply(0:p, myacf), 4),
  y = round(acf_vals, 4)
))
# odds = (0:10000)[0:10000 %% 2 == 1]
# evens = (0:10000)[0:10000 %% 2 == 0]
# sum((-0.5)^odds) + sum(  (-0.5)^evens )
# sum( sapply(0:10000, function(x){(0.5)^(2*x)}) )

jason_acf = function(h){
  if((h %% 2) == 0){
    (-0.5)^(h/2)
  }
  else{
    -(2/5)*((-0.5)^((h+1)/2))
  }
}

cols=sapply(0:p, function(h){
  ifelse((h %% 2) == 0, 'red', 'blue')
})
plot(x = sapply(0:p, jason_acf),
     y = acf_vals, 
     col = cols)
abline(a = 0, b = 1, col = "black", lty='dotted')

plot((acf_vals -sapply(0:p, myacf) )^2)
sum((acf_vals -sapply(0:p, myacf) )^2)

data.frame(
  sapply(0:p, jason_acf),
  sapply(0:p, myacf)
)



theo = sapply(0:15, my_acf)
# R code block for simulation and plotting
set.seed(443)
# Further simulation and plotting code goes here

# lines(sapply(0:15, my_acf), col = 'red')
## resids



library(ggplot2)
set.seed(443)
# add my acf
my_acf = function(h){
  (16/11)*((1/2)^h) - (5/11)*((1/5)^h)
}

# simulate using arima.sim
sigma_2 = 1
sim = arima.sim(n = 1000, model = list(ar = c(7/10, -1/10)), sd = sigma_2)
sim2 = arima.sim(n = 10000, model = list(ar = c(7/10, -1/10)), sd = sigma_2)

lagmax = 15
plotDF = data.frame(
  lag = 0:lagmax,
  theoretical = sapply(0:lagmax, my_acf),
  observed = acf(sim, plot = F, lag.max = lagmax)$acf,
  observed_large = acf(sim2, plot = F, lag.max = lagmax)$acf
)

ggplot(plotDF) + 
  geom_point(aes(x = lag, y = theoretical)) + 
  geom_point(aes(x = lag, y = observed), col = 'red') + 
 # geom_point(aes(x = lag, y = observed_large), col = 'blue')  +
  theme_bw()

plotDF$resids = plotDF$observed - plotDF$theoretical 
# hypothesis testing
mean(plotDF$resids) / ( sd(plotDF$resids) / sqrt(16) )
# resids = sapply(seq(from = 500, to = 11000, by = 100), 
#                 function(n)
#                 {
#                   sim = arima.sim(n = n, model = list(ar = c(7/10, -1/10)), sd = sigma_2)
#                   sample_acf = acf(sim, lag.max = 15, plot = FALSE)
#                   rss = sum((sample_acf$acf - theo)^2)
#                   rss
#                 })
# 
# plot(y = resids, x = seq(from = 500, to = 11000, by = 100), ylab = "RSS", xlab = "Sample Size")
# points(y = resids[6], x = 1000, col = 'red', pch = 12)