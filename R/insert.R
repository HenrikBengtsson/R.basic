#########################################################################/**
# @RdocDefault insert
#
# @title "Insert values to a vector at certain positions"
#
# \description{
#  Insert values to a vector at certain positions.
# }
#
# @synopsis
#
# \arguments{
#   \item{x}{The @vector of data values.}
#   \item{index}{The indices where the values should be inserted.}
#   \item{value}{The values to be inserted. The values will be looped over
#     if this vector is shorter than the index vector.}
#   \item{...}{Not used.}
# }
#
# @author
#
# \examples{
#   vec     <- c(1:10,13:15,19:20)
#   missing <- setdiff(1:20, vec)
#   vec2 <- insert(vec, missing)
#   print(vec2)
#   # [1]  1  2  3  4  5  6  7  8  9 10 NA NA 13 14 15 NA NA NA 19 20
# }
#
# @keyword "manip"
#*/#########################################################################t
setMethodS3("insert", "default", function(x, index, value=NA, ...) {
  nbrOfInserts <- length(index);
  value <- rep(value, length.out=nbrOfInserts);
  use <- rep(1, length.out=length(x) + nbrOfInserts);
  use[index] <- 0;
  idx <- cumsum(use);
  idx[index] <- value;
  x[idx];
})


############################################################################
# HISTORY:
# 2005-02-20
# o Now using setMethodS3() and added '...' to please R CMD check.
# 2001-11-24
# * Created.
############################################################################
