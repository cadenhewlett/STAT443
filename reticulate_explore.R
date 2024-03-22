library(reticulate)
# py_eval("1+1")
# reticulate::py_available()
# reticulate::py_config()
# reticulate::install_python()
sympy <- import("sympy")
# a <- sympy$symbols("a")
# left_side_coefficient = 1
# right_side = (3 - 2*a) / (6*a)
# rho_1 = sympy$simplify(right_side / left_side_coefficient)
# rho_1
A = sympy$symbols('a')
B = -A
C = -1*0.5
d = sympy$symbols('d')

roots = sympy$solve(A*d**2 + B*d + C, d)
for( r in roots ){
  print( sympy$simplify(r) )
}
# roots_simplified = [simplify(root) for root in roots]
# roots_simplified
# 
# roots_simplified