library(zoo)
library(tseries)

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

# ```{r}
# set.seed(443)
# theo = sapply(0:15, my_acf)
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
# ```

# odds = (0:10000)[0:10000 %% 2 == 1]
# evens = (0:10000)[0:10000 %% 2 == 0]
# sum((-0.5)^odds) + sum(  (-0.5)^evens )
# sum( sapply(0:10000, function(x){(0.5)^(2*x)}) )
# acf_vlas = acf(test, lag.max = p, plot= F)$acf
# 
# plot(x = psi(0:p),
#      y = acf_vlas, 
#      col = 'red')
# abline(a = 0, b = 1, col = "black", lty='dotted')
# 
# cbind(acf_vlas, psi(0:p))
# 
# # plot( acf(test, lag.max = 100, plot= F)$acf, type = 'l')
# https://webspace.maths.qmul.ac.uk/b.bogacka/TimeSeries/TS_Chapter4_6.pdf
# psi <- function(j, delta = 0.5){
#   # \{\psi_j\} = (-1)^{\,j}(-\delta)^{\lceil j / 2\rceil} B^{\,j}
#   (-1)^(j) * (-1*delta)^(ceiling(j/2))
#   # (-1*delta)^ceiling(j / 2)
# }
# pi <- function(j, delta = 0.5){
#   # \big(\mathbbm{1}[j \geq 2]\delta(-\delta)^{j - 2}\big) + (- \delta)^j
#   (j >= 2)*delta*((-1*delta)^j) + (-1*delta)^j
# }
# q1test = function(h){
#   # \frac{16}{11}\Big(\frac{1}{2}\Big)^{|h|}-\frac{6}{11}\Big(\frac{1}{5}\Big)^{|h|} 
#   (16/11)*((1/2)^h) - (5/11)*((1/5)^h)
# }
# q1test(0)
# q1test(1)
# 7/11
# q1test(2)
# 19/55
my_acf = function(h){
  (16/11)*((1/2)^h) - (5/11)*((1/5)^h)
}
set.seed(443)

sigma_2 = 1
sim = arima.sim(n = 1000, model = list(ar = c(7/10, -1/10)), sd = sigma_2)

error_line = 2 / sqrt(2000)
p <- ggplot(plotDF) + theme_bw() +
  geom_segment(aes(x = lag, xend = lag, y = 0, yend = observed), color = "#ff0a54", alpha = 0.25) +
  geom_segment(aes(x = lag, xend = lag, y = 0, yend = theoretical), color = "#adc2cc") +
  geom_point(aes(x = lag, y = theoretical, color = "Theoretical", shape = "Theoretical"), size = 2.0) +
  geom_point(aes(x = lag, y = observed, color = "Observed", shape = "Observed"), size = 2.0) +
  scale_color_manual(values = c("Theoretical" = "#08415c", "Observed" = "#720026"), name = "ACF Type") +
  scale_shape_manual(values = c("Theoretical" = 4, "Observed" = 3), name = "ACF Type") +
  theme(panel.grid.minor = element_line(color = "grey90", linetype = "dashed", linewidth = 0.5), legend.position = "top") +
  labs(x = "Lag", y = "Autocorrelation", title = "Correlogram of Second-Order AR Process", 
       subtitle = "Simulated Values (n = 1000) vs. Theoretical")


print(p)

sim2 = arima.sim(n = 10000, model = list(ar = c(7/10, -1/10)), sd = sigma_2)

plotDF = data.frame(
  lag = 0:15,
  theoretical = sapply(0:15, my_acf),
  observed = acf(sim, plot = F, lag.max = 15)$acf,
  observed_long = acf(sim2, plot = F, lag.max = 15)$acf
)

plotDF$resids = plotDF$observed - plotDF$theoretical

p2 <- ggplot(plotDF )+ theme_bw() +
      geom_point(aes(x = lag, y = resids)) +
      geom_segment(aes(x = lag, xend = lag, y = 0, yend = resids) )

# wilcox.test((plotDF$observed - plotDF$theoretical), mu = 0, paired = FALSE, correct = F)
# 
# wilcox.test((plotDF$observed_long - plotDF$theoretical), mu = 0, paired = FALSE, correct = F)
wilcox.test(plotDF$observed,
            plotDF$theoretical+1e-8, correct = F)
?wilcox.test
