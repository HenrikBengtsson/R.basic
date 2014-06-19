# Example of Euclide's algorithm:
#
# Let a = 2322 and b = 654. Calculate gcd(a,b)!
# 
# 2322 = 654*3 + 360   gcd(2322, 654) = gcd(654, 360) 
# 654  = 360*1 + 294   gcd(654, 360)  = gcd(360, 294) 
# 360  = 294*1 + 66    gcd(360, 294)  = gcd(294, 66) 
# 294  = 66*4 + 30     gcd(294, 66)   = gcd(66, 30) 
# 66   = 30*2 + 6      gcd(66, 30)    = gcd(30, 6) 
# 30   = 6*5           gcd(30, 6)     = 6 
# 
# In other words, gcd(2322,654) = 6.

print( gcd(2322, 654) )     # 6
