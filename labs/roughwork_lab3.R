library(zoo)
library(tseries)

?arima.sim

coefs = c(1, -1.3, 0.4)
?arima.sim
# simulate the MA(2) process
simulated_data <- arima.sim(n = 1000, 
                            model = list(ma = coefs))

plot(simulated_data)

acf(simulated_data)

# arma ACF
?ARMAacf

#  theoretical ACF for an MA(2) process
acf_values <- ARMAacf(ma = c(1,-1.3, 0.4), lag.max = 10)
acf_values
# plotted theoretical ACF
plot(acf_values, type = "h", 
     main = "Theoretical ACF of MA(2) Process", 
     xlab = "Lag", ylab = "ACF", ylim = c(-0.8, 1))
abline(h = 0, lty = 'dashed')
abline(h = -0.2, lty = 'dashed', col = 'red')
abline(h = 0.2, lty = 'dashed', col = 'red')

0.3 + -1.3*(-0.8) + 0.4*(1)
-1.3*(-0.8) 
