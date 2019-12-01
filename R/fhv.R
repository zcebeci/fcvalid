fhv <- function(x, u, v, m, t=NULL, eta, tidx ="f"){
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
    dcheck <- FALSE
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
  if(tidx == "g"){
    if(!is.null(T))
      U <- T/rowSums(T)
    else
      stop("Typicality matrix is required to compute the generalized FHV")
  }
  n <- nrow(U)
  p <- ncol(V)
  k <- nrow(V)
  fhv <- vector(length=k)
  f <- array(0, c(p, p, k))
  for(j in 1:k){
    f[,,j] <- as.matrix(nrow=p, ncol=p, 0)
    totnom <- f[,,j]
    for(i in 1:n){
      if(tidx == "e")
        totnom <- totnom + (U[i,j] + T[i,j])*((X[i,]-V[j,]) %*% t((X[i,]-V[j,])))
      else
        totnom <- totnom + (U[i,j])*((X[i,]-V[j,]) %*% t((X[i,]-V[j,])))
    }
    if(tidx == "e")
      totdenom <- sum(U[,j] + T[,j])
    else
      totdenom <- sum(U[,j])
    f[,,j] <- totnom / totdenom
    fhv[j] <- sqrt(det(f[,,j]))
  }
  idx <- sum(fhv)
  if(tidx == "f")
    names(idx) <- "fhv"
  else if(tidx == "e")
    names(idx) <- "fhv.e"
  else if(tidx == "g")
    names(idx) <- "fhv.g"
  return(idx)
}
