#########################################################################/**
# @RdocDefault factorToNumeric
#
# @title "Converts factors to numeric"
#
# \description{
#   Converts factors to numeric.
# }
#
# @synopsis
#
# \arguments{
#   \item{f}{A @factor object.}
#   \item{...}{Not used.}
# }
#
# @author
#
# \examples{
#   x <- factor(20:30)
#   print(x)
#   print(factorToNumeric(x))
# }
#
# \seealso{
#   @see "base::factor".
# }
#
# @keyword "manip"
#*/#########################################################################
setMethodS3("factorToNumeric", "default", function(f, ...) {
  as.numeric(levels(f))[as.integer(f)];
})



############################################################################
# HISTORY:
# 2005-02-20
# o Now using setMethodS3() and added '...' to please R CMD check.
# 2002-10-14
# * Created from R FAQ "7.12 How do I convert factors to numeric?".
############################################################################
