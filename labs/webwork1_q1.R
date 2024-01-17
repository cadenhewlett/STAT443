## Questions 1 and 2

df = read.csv("webwork/file1dc4079c75a.csv")

series = ts(df$Seattle, 
            start = c(2004, 3),
            frequency = 4)

length(series) == nrow(df)

plot(series, ylab = "Seattle Home Prices")

smoothed = rollmean(series, k = 4,  align=  'center')
as.numeric(smoothed)[3]

## estimating seasonal effect
diff_ts = series - smoothed

period = 4
S_j =  sapply(1:period, function(p){
          mean(diff_ts[ cycle(difference_ts) == p ])})

epsilon = sum(S_j) / period
# these get the adjusted S_j values
adjusted = S_j - epsilon
# this is the adjusted seasonal effect
print(adjusted)


