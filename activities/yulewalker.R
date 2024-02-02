rho_1 = 1.322/1.34
rho_2 = 1.2*rho_1 - 0.22

d = Re(polyroot(c(0.1, 0.22, -1.3, 1)))


D = matrix(
  data = c(1, 1, 1, 
           d[1], d[2], d[3], 
           d[1]^2, d[3]^2, d[3]^2),
  nrow = 3, byrow = T
)
D
rhos = c(1, rho_1, rho_2)
A = solve(D, rhos)

-4^2
(-4)^2
acf_gen <- function(h){
  return(
    sum(sapply(1:3, function(i){
      A[i]*(d[i]^h)
    }))
  )
}
acf_gen(0)
acf_gen(1)
acf_gen(2)
acf_gen(3)

plot(sapply(1:100, acf_gen))
