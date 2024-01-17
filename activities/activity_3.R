my_acf <- function(vec, h){
  
  xbar = mean(vec)
  difh <- sum ( (vec[1:(n - h)] - xbar)*(vec[(h+1):n] - xbar) )
  sumsq <- sum( ( vec - xbar ) ^ 2 )
  
  rh <- difh/sumsq
  return(rh)
}


q <- c(-9.1, 0.2, 9.2, 0.3, 
       -9.6, 0.7, 8.3, 0.3, 
       -7.8, -1.5, 8.2, -2.3,
        -7.3, -0.4, 8.0, 0.3)

plotDF <- data.frame(
  h = 1:15,
  rh = sapply(1:15, my_acf, vec = q)
)

