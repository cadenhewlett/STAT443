df = read.csv("webwork/file1dc4079c75a.csv")

series = ts(df$Seattle, 
            start = c(2004, 3),
            frequency = 4)

length(series) == nrow(df)

plot(series, ylab = "Seattle Home Prices")

?rollmean

rollmean(series, k = 4)
