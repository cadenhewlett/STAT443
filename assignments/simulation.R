library(zoo)
library(tseries)


########## THE FIX: ODD SHOULD BE (i + 1)/2 NOT (i - 1)/2
par(mfrow = c(1, 2))

test = arima.sim(n = 1000000, list(ar = c(0,  -0.5), ma = c(0.5)),
                 sd = 1)


p = 100

acf_vals = acf(test, lag.max = p, plot= F)$acf

myacf = function(h){
  if((h %% 2) == 0){
    (-0.5)^(h/2)
  }
  else{
    # why not 1/4 
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
sum( (acf_vals -sapply(0:p, myacf) )^2)
