#########################################################################/**
# @set class=ListVector
# @RdocFunction ListVector
#
# @title "Vector that can be subsetted as a list"
#
# \description{
#   @get "title".
#   This class has special subsetting methods, that is, \code{"["},
#   \code{"[["}, and \code{"$"} that allows the object to be treated as
#   as a @vector or as a @list.
# }
#
# @synopsis
#
# \arguments{
#  \item{...}{Named arguments to be put in the vector. All elements from
#    the same argument gets the same name in the vector. If one of the
#    arguments is a @list it is recursively unwrapped into a named @vector.}
#  \item{.overrideNames}{If @TRUE, the names of named elements in a @list
#    element are overridden by the name of the list element itself.}
# }
#
# \value{
#  Return a named @vector of class \code{ListVector}.
#  If no arguments were given, @NULL is returned.
# }
#
# @author
#
# @examples "../incl/ListVector.Rex"
#
# \seealso{
#   @see "base::vector".
#   @see "base::list".
# }
#*/#########################################################################
ListVector <- function(..., .overrideNames=TRUE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 0. Local functions
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  listToVector <- function(l) {
    listNames <- names(l);
    vector <- NULL;
    names <- c();
    for (kk in seq(along=l)) {
      # The key of the current element
      key <- listNames[kk];
      if (length(key) == 0 || nchar(key) == 0)
        key <- NULL;

      # The value of the current element
  	values <- l[[kk]];

      # If value is a list, unwrap list.
      if (is.list(values))
        values <- listToVector(values);

      # Name the values
      if (.overrideNames && !is.null(key)) {
    	keys <- rep(key, length=length(values));
      } else {
        keys <- names(values);
      }

      # Validate keys
      if (any(nchar(keys) == 0))
        stop("Some arguments were not named (neither recursively).");

      names <- c(names, keys);
      vector <- c(vector, values);
    } # for (kk in ...)

    if (!is.null(vector))
      names(vector) <- names;

    vector;
  } # listToVector()

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 1. Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  args <- list(...);

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 2. Create object
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  object <- listToVector(args);
  if (!is.null(object)) {
    class(object) <- c("ListVector", "vector", class(object));
  }

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 3. Return object
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  object;
}



###########################################################################/**
# @RdocMethod "["
#
# @title "Vector-style subseting of object"
#
# @synopsis
#
# \description{
#  @get "title" similar to subsetting of a @vector.
# }
#
# \arguments{
#  \item{name}{A @vector of partial element names to be retrieved.}
#  \item{.partial}{If @TRUE, partial name matching is applied, otherwise
#     exact names are required.}
# }
#
# \value{
#   Returns a named @vector of length zero or more consisting of
#   matched elements.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#
# \keyword{programming}
# \keyword{methods}
#*/###########################################################################
setMethodS3("[", "ListVector", function(object, name, .partial=TRUE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 1. Extract parameters
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (is.numeric(name)) {
  } else if (is.character(name)) {
  } else {
    stop(paste("Subsetting a ListVector with \"[\" with unknown type:",
                                                              mode(name)));
  }

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 2. Partial matching
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (is.numeric(name)) {
    value <- unclass(object)[name];
  } else {
  	names <- names(object);
    if (.partial) {
    	patterns <- paste("^", name, sep="");
    } else {
    	patterns <- paste("^", name, "$", sep="");
    }
  	idx <- NULL;
  	for (pattern in patterns)
  		idx <- c(idx, grep(pattern, names));
  	value <- unclass(object)[idx];
  	if (length(idx) == 0)
  		names(value) <- NULL;
  }

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 3. Return elements
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  value;
}, createGeneric=FALSE)




###########################################################################/**
# @RdocMethod "[["
#
# @title "List-style subseting of object"
#
# @synopsis
#
# \description{
#  @get "title" similar to subsetting of a @list.
# }
#
# \arguments{
#  \item{name}{A @vector of (partial) element names to be retrieved.}
#  \item{.partial}{If @TRUE, partial name matching is applied, otherwise
#     exact names are required.}
#  \item{.dropUnique}{If @TRUE and only one matching element exists, the element
#     is returned by itself without being wrapped up in a @list.}
# }
#
# \value{
#   Returns either a @list, a @vector of length one or more, or as @NULL.
#   If more than one element were matched, the elements are returned
#   as a @list.
#   If \code{.dropUnique==TRUE} and only one element was matched, the element
#   is returned "as is".
#   If no matching element exists, @NULL is returned.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#
# \keyword{programming}
# \keyword{methods}
#*/###########################################################################
setMethodS3("[[", "ListVector", function(object, name, .partial=TRUE, .dropUnique=TRUE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 1. Extract parameters
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (is.numeric(name)) {
  } else if (is.character(name)) {
  } else {
    stop(paste("Subsetting a ListVector with \"[[\" with unknown type:",
                                                              mode(name)));
  }

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 2. Partial matching
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (is.numeric(name)) {
    list <- as.list(object)[name];
  } else {
  	# Unclass object
  	object <- unclass(object);

  	names <- names(object);
    if (.partial) {
    	patterns <- paste("^", name, sep="");
    } else {
    	patterns <- paste("^", name, "$", sep="");
    }
  	list <- list();
  	for (kk in seq(along=patterns)) {
  		key <- name[kk];
  		pattern <- patterns[kk];
  		idx <- grep(pattern, names);
  		if (length(idx) > 0) {
  			value <- object[idx];
  			names(value) <- NULL;
  			list[[key]] <- value;
  		}
  	}
  }

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 3. Return elements
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (length(list) == 0) {
    list <- NULL;
  } else if (.dropUnique) {
   if (length(list) == 1) {
     list <- list[[1]];
   }
  }
  list;
}, createGeneric=FALSE)




###########################################################################/**
# @RdocMethod "$"
#
# @title "List-style subseting of object"
#
# @synopsis
#
# \description{
#  @get "title" similar to subsetting of a @list.
# }
#
# \value{
#   Returns the matching set of named elements either as a single element or
#   as a @vector.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#
# \keyword{programming}
# \keyword{methods}
#*/###########################################################################
setMethodS3("$", "ListVector", function(object, name) {
  object[[name, .partial=TRUE, .dropUnique=TRUE]];
}, createGeneric=FALSE)






###########################################################################/**
# @RdocMethod as.list
#
# @title "Gets a list representation of the ListVector"
#
# @synopsis
#
# \description{
#  @get "title".
# }
#
# \value{
#   Returns a @list.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#
# \keyword{programming}
# \keyword{methods}
#*/###########################################################################
setMethodS3("as.list", "ListVector", function(x, ...) {
  # To please R CMD check...
  object <- x;

  names <- names(object);
  keys <- unique(names);

  object <- unclass(object);
  list <- list();
  for (key in keys) {
    idx <- (names == key);
    value <- object[idx];
    names(value) <- NULL;
    list[[key]] <- value;
  }
  list;
})




###########################################################################/**
# @RdocMethod as.vector
#
# @title "Gets a vector representation of the ListVector"
#
# @synopsis
#
# \description{
#  @get "title".
# }
#
# \arguments{
#  \item{...}{Other arguments passed to the default @see "base::as.vector"
#      function.}
# }
#
# \value{
#   Returns a @vector.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#
# \keyword{programming}
# \keyword{methods}
#*/###########################################################################
setMethodS3("as.vector", "ListVector", function(x, ...) {
  # To please R CMD check...
  object <- x;
  unclass(object);
}, createGeneric=FALSE)



###########################################################################
# HISTORY:
# 2013-02-07
# o ListVector no longer defines a Class constructor, but just a plain
#   function. This because it may return NULL, which is invalid R.oo code.
# 2005-02-20
# o Added '...' argument to please R CMD check.
# 2004-06-27
# o Now ListVector() (without arguments) returns NULL instead of giving an
#   error.
# o as.vector() must accept at least two arguments like the built-in.
# 2004-05-24
# o Added an extensive Rdoc example. Class seems to be really useful.
#   I wonder why this hasn't been done before or maybe it has?!?
# o Created. To be used in optim() etc.
###########################################################################
