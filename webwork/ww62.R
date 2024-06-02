sigma = 1
  
gamma_0 = sigma^2*(1 + (alpha + beta)^2 /(1 - alpha^2))
round(gamma_0, 3)

gamma_ <- function(h){
  t1 = (alpha + beta)*( alpha^(h - 1) )
  t2 = ( (alpha + beta)^2*(alpha^h)  )/ (1 - alpha^2)
  sigma^2 * (t1 + t2)
}

round(gamma_(1), 3)
round(gamma_(2), 3)

round(pi^2 / 2, 3)

w = 0.357
test = function(h){
  (1 / h^2) - cos(pi * h) / h^2
}

test(8)
round(test(9), 2)
test(9)

# spec dens
alpha = 0.7 
beta = 0.5

f <- function(w){
  top =  1 + 2*beta*cos(w) + beta^2
  bot = pi * (1 + alpha^2 - 2*alpha*cos(w))
  top/bot
}
round(f(0.524), 3)
