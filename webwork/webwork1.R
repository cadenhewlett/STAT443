library(zoo)
library(tseries)
df <- read.csv("webwork/file1d174cf8d84.csv")

# get df info so that we can make the tseries
tail(df)
series <- ts(df$Value, 
        start = c(1972, 1), 
        #  end = c(2014, 9),
        frequency = 12)
length(series)
nrow(df)
plot(series)

###### QUESTION 2.1 #########

acf_result = acf(series)
# extract autocorrelation coefficient 1
r1 = acf_result$acf[2]
r1

