x <- c("a", "b", "c", "d")

p1 <- NULL
for (k in 1:factorial(length(x))-1) 
  p1 <- rbind(p1, lexicographicPermutation(x, k))
print(p1)

p2 <- lexicographicPermutations(x)
print(p2)

if (!identical(p1, p2))
  stop("Permutations 'p1' and 'p2' should be identical!")


