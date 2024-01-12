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
