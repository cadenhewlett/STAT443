# par(mfrow = c(2, 3))
# test = arima.sim(n = 100, list(ar = c(-0.65)),  sd = 1)
# acf(test, lag.max = 10, main = "AR(p = 1) with alpha < 0")
# test2 = arima.sim(n = 100, list(ar = c(0.9)),  sd = 1)
# acf(test2, lag.max = 10, main = "AR(p = 1) with alpha > 0")
# test5 = arima.sim(n = 100, model = list(), sd = 1)
# acf(test5, lag.max = 10, main = "White Noise")
# test3 = arima.sim(n = 100, list(ma = c(-0.5)),  sd = 1)
# acf(test3, lag.max = 10, main = "MA(q = 1) with beta < 0")
# test4 = arima.sim(n = 100, list(ma = c(0.5)),  sd = 1)
# acf(test4, lag.max = 10, main = "MA(q = 1) with beta > 0")
# test6 = arima.sim(n = 100, model = list(order = c(0, 1, 0)), sd = 1)
# acf(test6, lag.max = 10, main = "White Noise with d = 1")
# (1 / (1- -0.5))
beta = 0.6
n = 100000
sigma = 2
test = arima.sim(n = n, list(ma = c(0.64)),  sd = sigma)
c0 = sum( ( test - mean(test) )^2) / n
c0/(1 + beta^2)

