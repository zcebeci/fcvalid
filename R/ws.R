ws <- function(x, u, v, m, t=NULL, eta, tidx="f", w){
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
    stop("The number of columns of the membership matrix is not equal to the number of rows of prototypes matrix")
  if(ncol(X) != ncol(V))
    stop("The number of columns of the data set matrix is not equal to the number of columns of prototypes matrix")
  if(ncol(U) != nrow(V))
    stop("The number of columns of the membership matrix is not equal to the number of rows of prototypes matrix")
  n <- nrow(X)
  k <- nrow(V)
  if(missing(w))
    w <- rep(1/k, k)
  else
    if(sum(w) != 1)
      stop("Sum of the weights of indexes must be equal to 1")
  idx <- rep(0, k); ws <- 0
  idx[1] <- 1/pc(u=U, m=m, t=T, eta=eta, tidx=tidx)
  idx[2] <- 1/mpc(u=U, m=m, t=T, eta=eta, tidx=tidx)
  idx[3] <- 1/pbm(x=X, u=U, v=V, m=m, t=T, eta=eta, tidx=tidx)
  idx[4] <- 1/kpbm(x=X, u=U, v=V, m=m, t=T, eta=eta, tidx=tidx)
  idx[5] <- 1/cl(u=U, m=m, t=T, eta=eta, tidx=tidx)
  idx[6] <- 1/si(x=X, u=U, v=V, m=m, t=T, eta=eta, tidx=tidx)$sif
  idx[7] <- pe(u=U, m=m, t=T, eta=eta, tidx=tidx)
  idx[8] <- xb(x=X, u=U, v=V, m=m, t=T, eta=eta, tidx=tidx)
  idx[9] <- fs(x=X, u=U, v=V, m=m, t=T, eta=eta, tidx=tidx)
  idx[10] <- tss(x=X, u=U, v=V, m=m, t=T, eta=eta, tidx=tidx)
  idx[11] <- kwon(x=X, u=U, v=V, m=m, t=T, eta=eta, tidx=tidx)
  idx[12] <- fhv(x=X, u=U, v=V, m=m, t=T, eta=eta, tidx=tidx)
  idx[13] <- cwb(x=X, u=U, v=V, m=m, t=T, eta=eta, tidx=tidx)
  idx[14] <- 1/apd(x=X, u=U, v=V, m=m, t=T, eta=eta, tidx=tidx)
  idx <- (idx-min(idx))/(max(idx)-min(idx))
  names(idx) <- c("pc", "mpc", "pbm", "kpbm", "cl", "si", "pe",
                  "xb", "fs", "tss", "kwon", "fhv", "cwb", "apd")
  for(i in 1:k)
    if(w[i] != 0)
      ws <- ws + w[i] * idx[i]
  if(tidx == "f")
    names(ws) <- c("ws") 
  if(tidx == "e")
    names(ws) <- c("ws.e") 
  if(tidx == "g")
    names(ws) <- c("ws.g") 
  results <- list()
   results$ws <- ws 
   results$idx <- idx
  return(results)
}
