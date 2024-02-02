df = read.csv("labs/lab4data.csv")
plot(df$x)
lab4ts = ts(df$x)
plot(lab4ts, ylab  = 'Lab 4 Data', main = "Plot of Lab 4 Time Series (Time Scale Unknown)")

# lag	:an integer indicating which lag to use.
# differences	: an integer indicating the order of the difference.
d = 1
l4ts_d1 = diff(lab4ts, lag = 1, differences = d)

s = 5; D = 1
l4ts_Ds = diff(l4ts_d1, lag = s, differences = D)
plot(l4ts_Ds, ylab = "y_t - y_{t -s}",
     main = paste(
       "Seasonal Differenced Series, D =", D, 
       "and s =", s))
acf(l4ts_Ds)

