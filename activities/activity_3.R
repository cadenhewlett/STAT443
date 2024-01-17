library(ggplot2)

# input, vector of all observations (vec) and lag (h)
my_acf <- function(vec, h){
  # get the mean
  xbar = mean(vec)
  # get the x_t - x_{t + h} covariance
  difh <- sum ( (vec[1:(n - h)] - xbar)*(vec[(h+1):n] - xbar) )
  # calculate sum of squares
  sumsq <- sum( ( vec - xbar ) ^ 2 )
  # rh via approximation method
  rh <- difh/sumsq
  # return
  return(rh)
}


q <- c(-9.1, 0.2, 9.2, 0.3, 
       -9.6, 0.7, 8.3, 0.3, 
       -7.8, -1.5, 8.2, -2.3,
        -7.3, -0.4, 8.0, 0.3)

plotDF <- data.frame(
  h = 0:15,
  # gets the acf as a function of lag
  rh = sapply(0:15, my_acf, vec = q)
)

ggplot(plotDF, aes(x = h, y = rh)) +
  geom_segment(aes(xend = h, yend = 0), color = "#84a59d", size = 1) +  
  geom_point(col = "#f6bd60", fill = "#f7ede2", 
             stroke = 1, size = 3, shape = 21) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") + 
  labs(x = "Lag", y = "Autocorrelation", title = "Correlogram of Activity 3 Data") +
  theme_bw()
