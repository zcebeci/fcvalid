allindexes <- function(x, u, v, m, t=NULL, eta, tidx="f"){
  if(missing(x))
    stop("Missing input argument. A ppclust object or a numeric data set is required")
  tidx <- match.arg(tidx, c("e","f","g"))
  if(inherits(x, "ppclust")){
    X <- as.matrix(x$x)
    if(!is.null(x$u)){
      U <- as.matrix(x$u)
      m <- x$m
    }
    else if(!is.null(x$t)){
      U <- as.matrix(x$t)
      m <- x$eta
    }
    else{
      stop("Argument 'x' does not have the fuzzy membership or typicality matrix")
    }
    V <- as.matrix(x$v)
    if(tidx == "e" || tidx == "g"){
      if(!is.null(x$t)){
        T <- x$t
        eta <- x$eta
      }
      else
        stop("Argument 'x' does not have the typicality matrix")
    }
  }
  else{
    dcheck <- TRUE
    if(!missing(x))
      if(is.matrix(x) || is.data.frame(x) || is.vector(x))
        X <- as.matrix(x)
      else
         stop("Argument 'x' must be a valid instance of the 'ppclust', a numeric vector, data frame or matrix")
    else
      stop("Missing argument 'x'")
    if(!missing(u))
      if(is.matrix(u) || is.data.frame(u))
        U <- as.matrix(u)
      else
        stop("Argument 'u' must be a numeric data frame or matrix")
    else
      stop("Missing argument 'u'")
    if(!missing(v))
      if(is.matrix(v) || is.data.frame(v))
        V <- as.matrix(v)
      else
        stop("Argument 'v' must be a numeric data frame or matrix")
    else
      stop("Missing argument 'v'")
    if(tidx != "f")
      if(!is.null(t))
        if(is.matrix(t) || is.data.frame(t))
          T <- as.matrix(t)
        else
          stop("Argument 't' must be a numeric data frame or matrix")
      else
        stop("Argument 't' is null")
    if(tidx != "f"){
      if(missing(eta))
        eta <- 2 
      if(!is.numeric(eta))
        stop("Argument 'eta' must be number") 
      if(eta < 1)
        stop("Argument 'eta' should be a positive number equals to or greater than 1") 
    }
    if(missing(m))
      m <- 2 
    if(!is.numeric(m))
      stop("Argument 'm' must be number") 
    if(m < 1)
      stop("Argument 'm' should be a positive number equals to or greater than 1") 
  }
  if(nrow(X) != nrow(U))
    stop("The number of rows of data set is not equal to the number of rows of the membership matrix")
  if(ncol(X) != ncol(V))
    stop("The number of columns of the data set matrix is not equal to the number of columns of prototypes matrix")
  if(ncol(U) != nrow(V))
    stop("The number of columns of the membership matrix is not equal to the number of rows of prototypes matrix")
  n <- nrow(U)
  k <- ncol(U)
  idx <- rep(0, 17)
  idx[1] <- pc(u=U, m=m, t=T, eta=eta, tidx=tidx)
  idx[2] <- mpc(u=U, m=m, t=T, eta=eta, tidx=tidx)
  idx[3] <- pe(u=U, m=m, t=T, eta=eta, tidx=tidx)
  idx[4] <- xb(x=X, u=U, v=V, m=m, t=T, eta=eta, tidx=tidx)
  idx[5] <- kwon(x=X, u=U, v=V, m=m, t=T, eta=eta, tidx=tidx)
  idx[6] <- tss(x=X, u=U, v=V, m=m, t=T, eta=eta, tidx=tidx)
  idx[7] <- fs(x=X, u=U, v=V, m=m, t=T, eta=eta, tidx=tidx)
  idx[8] <- pbm(x=X, u=U, v=V, m=m, t=T, eta=eta, tidx=tidx)
  idx[9] <- kpbm(x=X, u=U, v=V, m=m, t=T, eta=eta, tidx=tidx)
  idx[10] <- awcd(x=X, u=U, v=V, m=m, t=T, eta=eta, tidx=tidx)
  idx[11] <- cl(u=U, m=m, t=T, eta=eta, tidx=tidx)
  idx[12] <- fhv(x=X, u=U, v=V, m=m, t=T, eta=eta, tidx=tidx)
  idx[13] <- apd(x=X, u=U, v=V, m=m, t=T, eta=eta, tidx=tidx)
  idx[14] <- sc(x=X, u=U, v=V, m=m, t=T, eta=eta, tidx=tidx)
  idx[15] <- si(x=X, u=U, v=V, m=m, t=T, eta=eta, tidx=tidx)$sif
  idx[16] <- cwb(x=X, u=U, v=V, m=m, t=T, eta=eta, tidx=tidx)
  idx[17] <- cs(x=X, u=U, v=V, m=m, t=T, eta=eta, tidx=tidx)
  cvi <- list()
  for(i in 1:17)
    cvi <- c(cvi, idx[i])
  if(tidx == "f")
    names(cvi) <- c("pc","mpc","pe", "xb", "kwon", "tss", "fs", "pbm", "kpbm", "awcd", "cl", "fhv", "apd", "sc", "si", "cwb", "cs")
  else if(tidx == "e")
    names(cvi) <- c("pc.e","mpc.e","pe.e", "xb.e", "kwon.e", "tss.e", "fs.e", "pbm.e", "kpbm.e", "awcd.e", "cl.e", "fhv.e", "apd.e", "sc.e", "si.e", "cwb.e", "cs.e")
  else if(tidx == "g")
    names(cvi) <- c("pc.g","mpc.g","pe.g", "xb.g", "kwon.g", "tss.g", "fs.g", "pbm.g", "kpbm.g", "awcd.g", "cl.g", "fhv.g", "apd.g", "sc.g", "si.g", "cwb.g", "cs.g")
  return(cvi)
}
