objective <- function(par, ..., theta=theta, verbose=FALSE) {
  if (verbose) {
    args <- list(
      theta=theta,
      par=par,
      ...
    );
    str(args);
  }
  
  sum(theta);
}

theta <- c(1,1)
par <- 0
min <- theta - 2
max <- theta + 2
depth <- 2
steps <- 3

opt <- gridOptim(theta=theta, par=par, min=min, max=max, steps=steps,
                 depth=depth, fn=objective, maximize=FALSE)
print(opt)

