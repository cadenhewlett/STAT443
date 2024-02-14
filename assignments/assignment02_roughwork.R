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
  sapply(0:p, my_acf)
)
