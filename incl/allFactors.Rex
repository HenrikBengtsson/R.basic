x <- 2*2*2*3
print( primeFactors(x) )
print( allFactors(x) )
# 2 2 2 3
# 2 3 4 6 8 12 24

x <- 1
print( primeFactors(x) )
print( allFactors(x) )
# NULL
# NULL

x <- 132319
print( primeFactors(x) )
print( allFactors(x) )
# 11 23 523
# 11 23 253 523 5753 12029

x <- 3128392
print( primeFactors(x) )
print( allFactors(x) )
# 2 2 2 391049
# 2 4 8 391049 782098 1564196

x <- 1:10
str( primeFactors(x) )
# Gives a list of 10 vectors of (non-unique) prime factors
#  List of 10
#   $ 1 : NULL
#   $ 2 : num 2
#   $ 3 : num 3
#   $ 4 : num [1:2] 2 2
#   $ 5 : num 5
#   $ 6 : num [1:2] 2 3
#   $ 7 : num 7
#   $ 8 : num [1:3] 2 2 2
#   $ 9 : num [1:2] 3 3
#   $ 10: num [1:2] 2 5

str( allFactors(x) ) 
# Gives a list of 10 vectors of (unique) integer factors.
# Note that obvious factors are excluded, meaning that factorizing
# primes will result in an empty vector.
#  List of 10
#   $ 1 : NULL
#   $ 2 : NULL
#   $ 3 : NULL
#   $ 4 : num 2
#   $ 5 : NULL
#   $ 6 : num [1:2] 2 3
#   $ 7 : NULL
#   $ 8 : num [1:2] 2 4
#   $ 9 : num 3
#   $ 10: num [1:2] 2 5


