########################################################################/**
# @RdocDefault gridOptim
#
# @title "Global optimization by partial gridding"
#
# \description{
#   @get "title" where the actual optimization is done transparently
#   by calls to @see "stats::optim".
# }
#
# @synopsis
#
# \arguments{
#  \item{theta}{A @vector of parameters for which the grid search should
#    be performed.
#    Thes values are also used as the initial center of the grid.}
#  \item{par}{A @vector of "free" parameters for which the objective 
#    function is optimized over, given fix \code{theta} parameters.
#    Passed to the optimizer function.}
#  \item{min,max}{Two @vectors of bounds for the grid search. No 
#    \code{theta} parameters will be search for outside these bounds. 
#    If @NA, there are no bounds.}
#  \item{width}{A @vector of initial widths for each of the \code{theta} 
#    parameters.}
#  \item{steps}{A @vector of the number of steps each \code{theta} 
#    parameter should search over. The total number of \code{theta}
#    parameter sets to be optimized over is \code{prod(steps)}.}
#  \item{depth}{The depth, or the number of "generations", to grid search
#    over. When all \code{theta} parameters sets have been searched, the 
#    grid can be narrowed around the optimal \code{theta} and recursively
#    search around that point. If \code{depth==1}, only the current 
#    generation is searched.}
#  \item{shrink}{A @vector of shrink factors in (0,1) specifying how much
#    the grid widths should be shrunk at each generation.}
#  \item{...}{Other arguments passed to the optimizer function.}
#
#  \item{optimizer}{An optimizer @function called at each grid point.
#    For details see below.
#    By default the optimizer is @see "stats::optim".}
#
#  \item{maximize}{If @TRUE, maximization is performed, 
#    otherwise minimization. Note that for instance the \code{control}
#    argument used by @see "stats::optim" is \emph{not} interpreted here.}
#
#  \item{.checkArgs}{If @TRUE, all arguments are checked and
#    modified/extended if necessary. If @FALSE, all arguments are assumed
#    to be of correct length, type etc. This argument is used by internal
#    recursive calls.}
#
#  \item{progressHandler}{A @function that is called at certain steps of 
#    optimization.}
#
#  \item{verbose}{If @TRUE, detailed information while grid searching is 
#    printed.}
# }
#
# \value{
#  Return a @list with information about the optimal settings:
#   \item{theta}{The grid point \code{theta} for which the objective
#    function was optimized.}
#   \item{optim}{The result returned by @see "stats::optim" at the optimum.}
#   \item{gridCount}{Total number of grid points tested.}
#   \item{callCount}{Total number of calls to the objective (and the
#    gradient) function.}
# }
#
# \section{Requirements for the optimizer function}{
#   The optimizer function specified by the \code{optimizer} argument must
#   accept the argument \code{par} as the first argument, cf. 
#   @see "stats::optim", followed by \code{...}, followed by the named
#   argument \code{theta} at any position.
#
#   The optimizer function must return a @list with the element 
#   \code{value} containing the optimal numerical value given the
#   current gridpoint (passed to the function by an argument named
#   \code{theta}).
#   Optionally, it may return the number of internal optimization steps 
#   using the \code{count} element (a @numeric scalar or @vector), which
#   is then summed up together over the whole grid (including recursive
#   grid) by this function. 
#   For instance, the @see "stats::optim" function returns the number of
#   calls to the objective and the gradient functions.
# }
#
# \section{Details for progress handler function}{
#  The \code{progressHandler} function must accept the following arguments:
#  \itemize{
#   \item{event}{A @character string specifying what type of event has occured.}
#   \item{args}{A @list of arguments used to call this function.}
#   \item{optimum}{A @list specifying the current optimum. Due to the
#     recursive nature of this function, this does not have to be the
#     optimum found by all optimizers, but only in the child optimizers.}
#  }
# }
#
# @author
#
# @examples "../incl/gridOptim.Rex"
#
# \seealso{
#   @see "stats::optim".
# }
#
# @keyword optimize
#*/########################################################################
setMethodS3("gridOptim", "default", function(theta, par, min=NA, max=NA, width=max-min, steps=1, depth=1, shrink=1/2, optimizer=stats::optim, ..., maximize=TRUE, .checkArgs=TRUE, progressHandler=NULL, verbose=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 0. Local function definitions
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  pasteVector <- function(x, collapse=",", sep="") {
    paste("(", paste(x, collapse=collapse), ")", sep=sep);
  }

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 1. Verify the arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Check arguments by default, but to speed up recursive calls allow
  # for skipping this. If so, all arguments must be complete and of
  # correct lengths.
  if (.checkArgs) {
    nTheta <- length(theta);
    
    # Argument: 'min'
    if (!is.na(min) && !is.numeric(min))
      stop(paste("Argument 'min' must be numeric:", mode(min)));
    if (length(min) > nTheta)
      stop(paste("Too many 'min' arguments:", length(min)));
    min <- rep(min, length.out=nTheta);
    
    # Argument: 'max'
    if (!is.na(max) && !is.numeric(max))
      stop(paste("Argument 'max' must be numeric:", mode(max)));
    if (length(max) > nTheta)
      stop(paste("Too many 'max' arguments:", length(max)));
    max <- rep(max, length.out=nTheta);
    if (any(max < min, na.rm=TRUE))
      stop(paste("Argument 'max' is greater than 'min':",
           pasteVector(max), ">", pasteVector(min)));
  
    # Argument: 'theta'
    if (!is.numeric(theta))
      stop(paste("Argument 'theta' must be numeric:", mode(theta)));
    if (any(theta < min, na.rm=TRUE))
      stop(paste("Argument 'theta' is out of range ('min'):",
           pasteVector(theta), "<", pasteVector(min)));
    if (any(theta > max, na.rm=TRUE))
      stop(paste("Argument 'theta' is out of range ('max'):",
           pasteVector(theta), ">", pasteVector(max)));
    
    # Argument: 'width'
    if (!is.numeric(width))
      stop(paste("Argument 'width' must be numeric:", mode(width)));
    if (length(width) > nTheta)
      stop(paste("Too many 'width' arguments:", length(width)));
    width <- rep(width, length.out=nTheta);
    if (any(!is.finite(width) | width <= 0))
      stop(paste("Argument 'width' must be greater than zero:",
           pasteVector(width)));
  
    # Argument: 'steps'
    if (!is.numeric(steps))
      stop(paste("Argument 'steps' must be numeric:", mode(steps)));
    if (length(steps) > nTheta)
      stop(paste("Too many 'steps' arguments:", length(steps)));
    steps <- rep(steps, length.out=nTheta);
  
    # Argument: 'depth'
    if (!is.numeric(depth))
      stop(paste("Argument 'depth' must be numeric:", mode(depth)));
    if (length(depth) != 1)
      stop(paste("Argument 'depth' must be of length one:", 
           length(depth)));
    if (depth < 1)
      stop(paste("Argument 'depth' must be a postive integer:", depth));
    
    # Argument: 'shrink'
    if (!is.numeric(shrink))
      stop(paste("Argument 'shrink' must be numeric:", mode(shrink)));
    if (length(shrink) > nTheta)
      stop(paste("Too many 'shrink' arguments:", length(shrink)));
    if (!all(is.finite(shrink) & 0 < shrink & shrink < 1))
      stop(paste("Argument 'shrink' must be in (0,1):", 
           pasteVector(shrink)));
    shrink <- rep(shrink, length.out=nTheta);
  
    # Argument: 'maximize'
    if (length(maximize) != 1)
      stop(paste("Argument 'maximize' must be of length one:", 
                                                  length(maximize)));
    if (!is.logical(maximize))
      stop(paste("Argument 'maximize' must be logical:", mode(maximize)));

    if (is.function(progressHandler)) {
    } else if (!is.null(progressHandler)) {
      stop("Unknown value of argument 'progressHandler'");
    }
  } # if (.checkArgs)
  
  optimSign <- if (maximize) -1 else +1;

  callArgs <- list(theta=theta, 
                   min=min, max=max, width=width, steps=steps, 
                   depth=depth, shrink=shrink, 
                   optimizer=optimizer, par=par, ..., 
                   maximize=maximize,
                   progressHandler=progressHandler,
                   .checkArgs=.checkArgs,
                   verbose=verbose);

  # Progress handler?
  if (!is.null(progressHandler)) {
    progressHandler(event="onEnter", args=callArgs);
  }
  
  # Verbose?
  if (verbose) {  
    str(callArgs);
  }

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 2. Get next free dimension of the grid
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Grid parameters for which 'steps' > 1.
  toGrid <- which(steps > 1);

  if (length(toGrid) == 0) {
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # 2a) No more "free" dimensions, that is, all are now fixed.
    #      i) Run the optimizer() for this single grid point.
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # Progress handler?
    if (!is.null(progressHandler)) {
      progressHandler(event="onEnterOptimizer", args=callArgs);
    }
  
#    if (verbose)
#      cat("Running optimizer() with theta=", pasteVector(theta),
#                                                             ": ", sep="");
    
    # Run optimizer() on the current 'theta' vector with 'par' free.
    optim <- optimizer(par=par, ..., theta=theta, verbose=verbose);

#    if (verbose)
#      cat(optim$value, "\n");
    
    # Return optimum
    optimum <- list(
       theta=theta,
       optim=optim,
       callCount=optim$count,
       gridCount=1
    );
    # If theta is returned by the optimizer() function, use that theta
    # is assumed to be the optimal theta. This allows the optimizer()
    # function to for instance optimize also theta locally.
    if (!is.null(optim$theta)) {
      optimum$theta <- optim$theta;
    }
    # Progress handler?
    if (!is.null(progressHandler)) {
      progressHandler(event="onExitOptimizer", optimum=optimum, args=callArgs);
    }
  } else {
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # 2a) There are more "free" dimensions. 
    #      i) Fix the next, and 
    #     ii) grid search the remaining.
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # Next "free" dimension 'jj'.
    jj <- toGrid[1];

    # Fix it...
    currSteps <- steps;
    currSteps[jj] <- 1;

    # ...and create a 1D-grid along it.
    from <- max(min[jj], theta[jj]-width[jj]/2, na.rm=TRUE);
    to   <- min(max[jj], theta[jj]+width[jj]/2, na.rm=TRUE);
    thetajjs <- seq(from=from, to=to, length=steps[jj]);
#    if (verbose)
#      cat("Gridding parameter ", jj, ": ", pasteVector(thetajjs), 
#                                                             "\n", sep="");

    # The optimimum for this subgrid and below.
    optimum <- list(
       theta=theta,
       optim=list(value=NA),
       gridCount=0,
       callCount=0
    );
  
    # For each point of the current grid dimension, optimize the
    # optimizer() function for the remaining dimensions.
    for (thetajj in thetajjs) {
      theta[jj] <- thetajj;

      # depth=1: Do not allow narrowing of grid in recursive gridding.
      opt <- gridOptim(theta=theta, 
                       min=min, max=max, width=width, steps=currSteps,
                       depth=1, shrink=shrink, 
                       optimizer=optimizer, par=par, ..., 
                       maximize=maximize, 
                       .checkArgs=FALSE, 
                       progressHandler=progressHandler, 
                       verbose=verbose);
        
      # Update counts
      optimum$callCount <- optimum$callCount + opt$callCount;
      optimum$gridCount <- optimum$gridCount + opt$gridCount;

      # optimSign == +1 (-1) for minimization (maximization).
      diffValue <- optimSign*(opt$optim$value - optimum$optim$value);
      if (is.na(diffValue) || diffValue < 0) {
        optimum$theta <- opt$theta;
        optimum$optim <- opt$optim;

        # Progress handler?
        if (!is.null(progressHandler)) {
          progressHandler(event="onNewOptimum", optimum=optimum, args=callArgs);
        }
      }
    } # for (thetajj ...)

    # Progress handler?
    if (!is.null(progressHandler)) {
      progressHandler(event="onExitGrid", optimum=optimum, args=callArgs);
    }

    if (verbose) {
      print(c(theta=optimum$theta, value=optimum$optim$value));
    }


    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # 2b. Narrow grid search
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if (depth > 1) {
      # Progress handler?
      if (!is.null(progressHandler)) {
        progressHandler(event="onEnterSubgrid", optimum=optimum, args=callArgs);
      }

      #   i) Center search around found optimum
      thetaN <- optimum$theta;
      
      #  ii) Shrink grid
      widthN <- shrink * width;
      
      # iii) Decrease depth
      depthN <- depth - 1;
  
      #  iv) Initial 'par' parameter values.
      #      What's the strategy here? TODO /HB 2004-05-18
      parN <- optimum$optim$par;
      if (is.null(parN))
        parN <- par;
  
      if (verbose) {
        cat("\n");
        cat("Narrowing grid search around theta:\n");
        cat("   from: ", pasteVector(thetaN-widthN/2), "\n", sep="");
        cat(" center: ", pasteVector(thetaN), "\n", sep="");
        cat("     to: ", pasteVector(thetaN+widthN/2), "\n", sep="");
        cat("\n");
      }
      
      oldOptimum <- optimum;

      # Apply grid search on narrowed grid.
      # Note: This will indeed calculate the objective function at the
      # current optimal theta again.
      # TODO: Add functionality to avoid this. /HB 2004-05-18
      optimum <- gridOptim(theta=thetaN, 
                           min=min, max=max, width=widthN, steps=steps,
                           depth=depthN, shrink=shrink, 
                           optimizer=optimizer, par=parN, ...,
                           maximize=maximize, 
                           .checkArgs=FALSE, 
                           progressHandler=progressHandler, 
                           verbose=verbose);

      # Update counts
      optimum$callCount <- optimum$callCount + oldOptimum$callCount;
      optimum$gridCount <- optimum$gridCount + oldOptimum$gridCount;

      # Progress handler?
      if (!is.null(progressHandler)) {
        progressHandler(event="onExitSubgrid", optimum=optimum, args=callArgs);
      }
    }
  } # if  (length(toGrid) == 0)

  # Progress handler?
  if (!is.null(progressHandler)) {
    progressHandler(event="onExit", optimum=optimum, args=callArgs);
  }

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 3. Return the optimum settings
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  optimum;
}) # gridOptim()


###########################################################################
# HISTORY:
# 2010-11-28
# o DOCUMENTATION: Fixed some Rd syntax errors.
# 2004-05-24
# o Extended progressHandler calls.
# 2004-05-23
# o Added a progressHandler.
# o BUG FIX: Way too many recursive calls. 
# 2004-05-22
# o Tried to reconstructor after HDD crash.
# 2004-05-19
# o Cleaned up code and made it nicer.
# 2004-05-18
# o Created.
###########################################################################
