library(zoo)
library(tseries)
library(ggplot2)
df <- read.csv("webwork/file1d174cf8d84.csv")


# get df info so that we can make the tseries
series <- ts(df$Value, 
        start = c(1972, 1), 
        #  end = c(2014, 9),
        frequency = 12)
length(series)
nrow(df)
plot(series)

###### QUESTION 2.1 #########
par(mfrow = c(1, 3))
# Part (a)
# Use the acf command to compute the sample autocorrelation 
# function for the time series. Give the value of 
# r1 to three decimal places.
length(acf_result$acf)
acf_result = acf(series, plot = TRUE)

# extract autocorrelation coefficient 1
r1 = acf_result$acf[2]


###### QUESTION 2.2 #########
d1_series = diff(series)

# take consecutive differences
acf_d1 = acf(d1_series, plot = TRUE)
# find r1 for these
r1_d1 = acf_d1$acf[2]
r1_d1

###### QUESTION 2.3 #########
d2_series = diff(d1_series)

# take consecutive differences
acf_d2 = acf(d2_series, plot = TRUE)
# find r1 for these
r1_d2 = acf_d2$acf[2]
r1_d2
# output all r1 values
print(round(c(r1, r1_d1, r1_d2), 3))
