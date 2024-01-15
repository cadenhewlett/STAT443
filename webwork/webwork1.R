library(zoo)
library(tseries)
df <- read.csv("webwork/file1d174cf8d84.csv")

?acf
tail(df)
series <- ts(df$Value, 
        start = c(1972, 1), 
        end = c(2014, 9),
        frequency = 12)
length(series)
nrow(df)
plot(series)
