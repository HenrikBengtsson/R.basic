# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Creating a ListVector
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
theta <- ListVector(a=1:5, b=2, c=100:103, bb=22:24)
print(theta)

# If arguments contains lists these are unwrapped into
# named vectors. 
theta2 <- ListVector(list(list(a=1:5), b=2), c=100:103, bb=22:24)
stopifnot(identical(theta, theta2))

# Unnamed elements in a list get the same name as the list
theta3 <- ListVector(list(a=list(1:5), b=2), c=100:103, bb=22:24)
stopifnot(identical(theta, theta3))

# Creating a ListVector out of another ListVector work too.
theta5 <- ListVector(theta)
stopifnot(identical(theta, theta5))

# Creating a ListVector out of another vector as well.
theta4 <- ListVector(as.vector(theta))
stopifnot(identical(theta, theta4))

# Overriding element names
xi <- ListVector(c=list(a=2, b=3))
print(xi)  # Note that 'a' and 'b' are ignored since 'c' is given.



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Subsetting a ListVector with "$" and "[[" like a list, but with
# partial name matching.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

cat("--------------------------------------------------\n")
cat("List-style subsetting\n")
cat("--------------------------------------------------\n")
cat("Partial name matching:\n")
print(theta$a)
print(theta$b)
print(theta$c)

print(theta[["a"]])
print(theta[["b"]])
print(theta[["c"]])

cat("Recursive name matching:\n")
print(theta[[c("a", "c")]])

cat("Non-partial name matching:\n")
print(theta[["a", .partial=FALSE]])
print(theta[["b", .partial=FALSE]])
print(theta[["c", .partial=FALSE]])

cat("Non-existing elements:\n")
print(theta[["na"]])

cat("as.list():\n")
print(as.list(theta))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Subsetting a ListVector with "[" like a vector, but with
# partial name matching.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cat("--------------------------------------------------\n")
cat("Vector-style subsetting\n")
cat("--------------------------------------------------\n")
cat("Partial name matching:\n")
print(theta["a"])
print(theta["b"])
print(theta["c"])

cat("Non-partial name matching:\n")
print(theta["a", .partial=FALSE])
print(theta["b", .partial=FALSE])
print(theta["c", .partial=FALSE])

cat("as.vector():\n")
print(as.vector(theta))


