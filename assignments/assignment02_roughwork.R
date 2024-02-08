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
# acf_vlas = acf(test, lag.max = p, plot= F)$acf
# 
# plot(x = psi(0:p),
#      y = acf_vlas,
#      col = 'red')
# abline(a = 0, b = 1, col = "black", lty='dotted')
# 
# cbind(acf_vlas, psi(0:p))
# lot( acf(test, lag.max = 100, plot= F)$acf, type = 'l')
# 
# 
# dumb = function(i){
#   (-1*0.5)^(2*i)
# }
# sum(dumb(0:1000))
# 4/3

psi_o = function(i){
  (-0.5)^(2*i - 1)
}

psi_o(2)
(1/2)^3
psi_o(0)

test_acf = function(h){
   
}