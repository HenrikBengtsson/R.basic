n <- 4; k <- 2;
print( nChooseK(n,k) )   # 6

n <- 6; k <- 2;
print( nChooseK(n,k) )   # 15

n <- 6; k <- 3;
print( nChooseK(n,k) )   # 20

# (a+b)^2 = 1*a^2 + 2*a*b + 1*b^2
n <- 2; k <- 0:2;
print( nChooseK(n,k) )   # 1 2 1

# (a+b)^3 = 1*a^3 + 3*a^2*b + 3*a*b^2 + 1*b^3
n <- 3; k <- 0:3;
print( nChooseK(n,k) )   # 1 3 3 1


