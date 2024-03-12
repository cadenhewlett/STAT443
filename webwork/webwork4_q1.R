alpha = 0.674
beta = 0.411
x_t = -3.886
xhat_t = -6.445
sigma_hat = 3.148
Z = qnorm(0.975)
Z
# q1 
zhat_t = (x_t - xhat_t)
xhat_t1 = alpha*x_t + beta*zhat_t
xhat_t1

# q2
psi_0 = 1
psi_1 = alpha+beta
alpha+beta
en_1_var = psi_0^2 * sigma_hat^2

round( 
  xhat_t1 - Z*sqrt(en_1_var),
  3)

# q3

xhat_t2 = alpha*xhat_t1
round( xhat_t2 , 3)

# q4
en_2_var = sigma_hat^2 * (psi_0^2 + psi_1^2)
xhat_t2 + Z*sqrt(en_2_var)

# q5

xhat_t3 = alpha*xhat_t2
psi_2 = beta*alpha + alpha^2


en_3_var = sigma_hat^2 *( psi_0^2 + psi_1 ^2 + psi_2 ^2 )

xhat_t3 - Z*sqrt(en_3_var)
#xhat_t2 - Z*sqrt(en_2_var)
