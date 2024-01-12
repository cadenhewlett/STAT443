library(knitr)
library(tseries)
library(ggplot2)
dat <- read.csv("labs\\LakeLevels.csv")

ggplot(dat, aes(x = 1:nrow(dat), y = LakeLevel)) +
  geom_point(shape = 1, col = "darkblue") +  
  xlab("Index") +  
  ylab("Lake Level (Depth, metres)") +
  ggtitle("Plot of \"LakeLevel\" Variable") + 
  theme_bw()

tail(dat)
x <- ts(dat$LakeLevel, 
        start = c(2007, 1), 
        end = c(2012, 1), # note: (2011, 12) omitted december
        frequency = 365)
plot(x)
length(x) == nrow(dat)
