sigma = 2.66
beta = 0.75
w = 0.286


# X(t) = Z(t) + 0.75 Z(t-1), at \omega = 0.286
q1 =(sigma^2 / pi)*(1 + beta^2 + 2*beta*cos(w))
round(q1, 3)


## 
f = function(w){
  gamma_0 = 1 + beta_1^2 + beta_2^2
  gamma_1 = (beta_1 + beta_1*beta_2)
  gamma_2 = beta_2
  (sigma^2 / pi)*(gamma_0 + 2*gamma_1*cos(w) + 2*gamma_2*cos(2*w))
}


## q2
beta_1 = 1
beta_2 = 0.41
print( round( f(w = 0.262), 3) )


## q3
beta_1 = 0.67
beta_2 = -0.32
print(round( f(w = 0.314), 3) ) 



##
0.64 - qnorm(0.9)*0.01
0.64 + qnorm(0.9)*0.01
