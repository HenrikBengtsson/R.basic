smooth.spline.prepare <- function(x, w=NULL, df=5, spar=NULL, cv=FALSE, all.knots=FALSE, df.offset=0, penalty=1, control.spar=list()) {
  sknotl <- function(x) {
    n.kn <- function(n) {
      if (n < 50)
        n
      else trunc({
        a1 <- log(50, 2)
        a2 <- log(100, 2)
        a3 <- log(140, 2)
        a4 <- log(200, 2)
        if (n < 200) 2^(a1 + (a2 - a1) * (n - 50)/150) else if (n <
          800) 2^(a2 + (a3 - a2) * (n - 200)/600) else if (n <
          3200) 2^(a3 + (a4 - a3) * (n - 800)/2400) else 200 +
          (n - 3200)^0.2
      })
    }
    nk <- n.kn(n <- length(x))
    c(rep(x[1], 3), x[seq(1, n, len = nk)], rep(x[n], 3))
  }
  contr.sp <- list(low=-1.5, high=1.5, tol=1e-04, eps=2e-08, maxit=500,
                   trace=getOption("verbose"))
  contr.sp[(namc <- names(control.spar))] <- control.spar
  if (!all(sapply(contr.sp[1:4], is.double)) || contr.sp$tol < 0 ||
           contr.sp$eps <= 0 || contr.sp$maxit <= 0)
    stop("invalid `control.spar'")
  # ------------ Differences from smooth.spline BEGIN -----------
  n <- length(x)
  w <- if (is.null(w))
    rep(1, n)
  else {
    if (n != length(w))
      stop("lengths of x and w must match")
    if (any(w < 0))
      stop("all weights should be non-negative")
    if (all(w == 0))
      stop("some weights should be positive")
    (w * sum(w > 0))/sum(w)
  }
  x <- signif(x, 6)
  ux <- unique(sort(x))
  nx <- length(ux)
  if (nx <= 3)
      stop("need at least for unique `x' values")
  if (cv && nx < n)
      warning("crossvalidation with non-unique `x' seems doubtful")
  # ------------ Differences from smooth.spline BEGIN -----------
  if (nx == n)
    ox <- 1:n
  else
    ox <- match(x, ux)
  # ------------ Differences from smooth.spline END -----------
  r.ux <- ux[nx] - ux[1]
  xbar <- (ux - ux[1])/r.ux
  if (all.knots) {
    knot <- c(rep(xbar[1], 3), xbar, rep(xbar[nx], 3))
    nk <- nx + 2
  } else {
    knot <- sknotl(xbar)
    nk <- length(knot) - 4
  }
  ispar <- if (is.null(spar) || missing(spar)) {
    if (contr.sp$trace) -1 else 0
  } else
    1
  spar <- if (ispar == 1) as.double(spar) else double(1)
  icrit <- if (cv) 2 else 1
  dofoff <- df.offset
  if (!missing(df)) {
    if (df > 1 && df <= nx) {
      icrit <- 3
      dofoff <- df
    } else
      warning(paste("you must supply 1 < df <= n,  n = #{unique x} =", nx))
  }
  iparms <- as.integer(c(icrit, ispar, contr.sp$maxit))
  names(iparms) <- c("icrit", "ispar", "iter")

  object <- list(penalty=penalty, dofoff=dofoff, xbar=as.double(xbar), nx=nx, knot=knot, nk=nk, iparms=iparms, spar=spar, contr.sp=contr.sp, ox=ox, n=n, df.offset=df.offset, w=w, ux=ux, r.ux=r.ux);
  class(object) <- "smooth.spline.prepare";
  object;
} # smooth.spline.prepare


smooth.spline.fit <- function(prep, y=NULL) {
  nx <- prep$nx
  n <- prep$n
  if (nx == n) {
    # Don't call tapply if not necessary. / HB 2002-03-02
    wbar <- prep$w
    ybar <- y
    yssw <- rep(0, n)
  } else {
    w <- prep$w
    ox <- prep$ox
    # The tapply is expensive! / HB 2002-03-02
    tmp <- matrix(unlist(tapply(seq(along=y), ox, function(i, y, w) {
         c(sum(w[i]), sum(w[i]*y[i]), sum(w[i]*y[i]^2))
       }, y=y, w=w)), ncol=3, byrow = TRUE)
    rm(w); rm(y);
    wbar <- tmp[, 1]
    ybar <- tmp[, 2]/ifelse(wbar > 0, wbar, 1)
    yssw <- sum(tmp[, 3] - wbar * ybar^2)
  }

  nk <- prep$nk

  # From R v1.9.0 internal qsbart is found in stats and not modreg;
  if (isPackageInstalled("graphics")) {
    qsbartPkg <- "stats";
  } else {
    qsbartPkg <- "modreg";
  }

  fit <- .Fortran("qsbart", as.double(prep$penalty), as.double(prep$dofoff),
    x=as.double(prep$xbar), y=as.double(ybar), w=as.double(wbar),
    ssw=as.double(yssw), as.integer(nx), as.double(prep$knot),
    as.integer(prep$nk), coef=double(nk), ty=double(nx), lev=double(nx),
    crit=double(1), iparms=prep$iparms, spar=prep$spar, parms=unlist(prep$contr.sp[1:4]),
    isetup=as.integer(0), scrtch=double((17 + nk) * nk), ld4=as.integer(4),
    ldnk=as.integer(1), ier=integer(1), DUP=FALSE,
    PACKAGE=qsbartPkg)[c("coef", "ty", "lev", "spar", "parms", "crit", "iparms", "ier")]

  fit$wbar <- wbar;
  fit$ybar <- ybar;
  fit
} # smooth.spline.fit


smooth.spline0 <- function(x, y=NULL, w=NULL, df=5, spar=NULL, cv=FALSE, all.knots=FALSE, df.offset=0, penalty=1, control.spar=list()) {
  if (inherits(x, "smooth.spline.prepare")) {
    prep <- x;
  } else {
    xy <- xy.coords(x,y);
    y <- xy$y;
    prep <- smooth.spline.prepare(x=xy$x, w=w, df=df, spar=spar, cv=cv, all.knots=all.knots, df.offset=df.offset, penalty=penalty, control.spar=control.spar);
  }

  fit <- smooth.spline.fit(prep, y=y);

  lev <- fit$lev
  df <- sum(lev)
  if (is.na(df))
    stop("NA lev[]; probably smoothing parameter `spar' way too large!")
  if (fit$ier > 0) {
    sml <- fit$spar < 0.5
    wtxt <- paste("smoothing parameter value too", if (sml) "small" else "large")
    if (sml) {
      stop(wtxt)
    } else {
      nx <- length(x)
      fit$ty <- rep(mean(y), nx)
      df <- 1
      warning(paste(wtxt, "setting df = 1  __use with care!__", sep="\n"))
    }
  }

  ox <- prep$ox;
  wbar <- fit$wbar;
  n <- prep$n;

  cv.crit <- if (cv) {
    ww <- wbar
    ww[!(ww > 0)] <- 1
    weighted.mean(((y - fit$ty[ox])/(1 - (lev[ox] * w)/ww[ox]))^2, w)
  } else
    weighted.mean((y - fit$ty[ox])^2, w)/(1 - (df.offset + penalty * df)/n)^2

  ybar <- fit$ybar;

  pen.crit <- sum(wbar * (ybar - fit$ty) * ybar)

  knot <- prep$knot;
  nk <- prep$nk;
  ux <- prep$ux;
  r.ux <- prep$r.ux;

  fit.object <- list(knot=knot, nk=nk, min=ux[1], range=r.ux, coef=fit$coef)
  class(fit.object) <- "smooth.spline.fit"
  object <- list(x=ux, y=fit$ty, w=wbar, yin=ybar, lev=lev, cv.crit=cv.crit,
             pen.crit=pen.crit, crit=fit$crit, df=df, spar=fit$spar,
             lambda=unname(fit$parms["low"]), iparms=fit$iparms, fit=fit.object,
             call=match.call())
  class(object) <- "smooth.spline"
  object
}


######################################################################
# 2010-11-28
# o BUG FIX: nx <- length(x) in smooth.spline0().
# 2002-04-21
# o Updated due to modreg is moved to/merged into stats from R v1.9.0.
# 2002-03-02
# o Splitted smooth.spline() into the two functions
#   smooth.spline.prepare() and smooth.spline.fit(). The purpose of
#   this is to speed up robust.spline(), especially when there are
#   duplicate x values!
# o Created!
######################################################################
