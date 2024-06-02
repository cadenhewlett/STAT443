
library(zoo)

par(mfrow = c(1,1))
set.seed(1)
mu_Y = -2
n = 750 
reps_per_n = 100

sims = sapply(2:n, function(n){
  print(paste("Performing Simulation at sample size ",
              n, "...", sep =""))
  biases_at_n = 
    sapply(1:reps_per_n, function(i){
      
            X <- rnorm(n, mean = 3, sd = 1)
            Y <- rnorm(n, mean = mu_Y, sd = 1)
  
            df = data.frame(X,Y)
  
            observed_responses = rbinom(n, 1, 3/4)
            # print(sum(observed_responses == 0))
            
            df$impute <- df$Y
            # small sample size non-response adjustment
            # ensures at least one value will be imputed
            if (sum(observed_responses == 0) == 0){
              observed_responses[1] = 0
            }
            else if (sum(observed_responses == 0) == n){
              observed_responses[n] = 1
            }
            
            df$impute[observed_responses == 0] = NA
  
            x_vals_m <- df$X[is.na(df$impute)]
            x_vals_r <- df$X[!is.na(df$impute)]
                             
            imputation = sapply(x_vals_m, function(x){
              index = which.min( abs(x - x_vals_r) )
              c(index, df$Y[index])
            })
            imputed_y_vals = imputation[2]
            # gets the d_i counts
            # table(imputation[1, ])
            y_r <- df$Y[!is.na(df$impute)]
            y_bar_nni <- mean(c(y_r, imputed_y_vals))
            # compute bias
            bias = y_bar_nni - mu_Y
            abs(bias)
            })
    mean(biases_at_n)
})
plot(sims, type = 'l', main = "bounding of bias(y bar NNI) with 1/sqrt(n)")

lines(1/sqrt(4:n), col = 'red')

smoothed_data <- rollmean(sims, 50, fill = NA)
lines(smoothed_data, col = 'blue',  lwd = 0.2)

# plot of d-i values
# plot(density(table(imputation[1, ])))
