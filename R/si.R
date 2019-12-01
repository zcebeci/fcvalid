si <- function(x, u, v, m, t=NULL, eta, av=1, tidx="f"){
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
      stop("Typicality matrix is required to compute the generalized SI index")
  }
  n <- nrow(U)
  k <- ncol(U)
  vm <- vector(length(n), mode = "numeric")
  for(i in 1:n)
    vm[i] <- which.max(U[i,])
  counts <- c()
  for(j in 1:k)
    counts[j] <- length(which(vm == j))
  D <- matrix(nrow = n, ncol = n, 0)
  for(i1 in 1:(n-1))
    for(i2 in (i1+1):n)
      D[i2,i1] <- D[i1,i2] <- sum((X[i1,]-X[i2,])^2)
  a <- b <- si.obj <- rep(0, n)
  Z <- matrix(nrow = n, ncol = k, 0)
  for(i in 1:n)
    for(j in 1:k)
      for(i2 in 1:n)
        if(vm[i2] == j) 
          Z[i,j] = Z[i,j] + D[i,i2]
  for(i in 1:n){
    for(j in 1:k){
      if(vm[i] == j){
        if(counts[j] != 1){
          Z[i,j] = Z[i,j]/(counts[j] - 1)
          a[i] = Z[i,j]
          Z[i,j] = max(Z[i,]) + 1
        }
      }
      else{
        Z[i,j] = Z[i,j]/counts[j]
      }
    }
    if(counts[vm[i]] != 1){
      b[i] = min(Z[i,])
      si.obj[i] = (b[i]-a[i])/max(a[i], b[i])
    }
    else 
      si.obj[i] = 0 
  }
  idx1 <- mean(si.obj)
  idx2 <- vector(length = n, mode = "numeric")
  Y <- rep(0, n)
  for(i in 1:n) 
    Y[i] <- (max(U[i,]) - max(U[i,][-(which.max(U[i,]))]))^av
  idx2 <- sum(Y * si.obj)/sum(Y)
  result = list()
    result$si.obj <- si.obj
    result$sih <- idx1
    if(tidx == "f")
      result$sif <- idx2
    else if(tidx == "e")
      result$sif.e <- idx2
    else if(tidx == "g")
      result$sif.g <- idx2
  return(result)
}
