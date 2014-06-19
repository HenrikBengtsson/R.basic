x <- c("a", "b", "c", "d")

p1 <- permutations(x)
print(p1)

p2 <- lexicographicPermutations(x)
print(p2)

if (!identical(p1, p2))
  stop("Permutations 'p1' and 'p2' should be identical!")


