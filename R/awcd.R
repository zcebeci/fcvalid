awcd <- function(x, u, v, m, t=NULL, eta, tidx="f"){
  if(missing(x))
    stop("Missing input arguments: x, u, v...")
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
        stop("Input argument is ppclust object but does not have the typicality matrix")
    }
  }
  else{
    if(!missing(x))
      if(is.matrix(x) || is.data.frame(x))
        X <- as.matrix(x)
      else
        stop("Argument 'x' must be a numeric data frame or matrix")
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
    if(tidx == "e"){
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
  if(tidx == "g"){
    if(!is.null(T))
      U <- T/rowSums(T)
    else
      stop("The typicality matrix is required to compute the generalized pe index")
  }
  n <- nrow(U)
  k <- ncol(U)
  total <- 0
  for(j in 1:k){
    total1 <- 0; total2 <- 0
    for(i in 1:n){
      if(tidx == "e"){
        total1 <- total1 + (U[i,j]^m + T[i,j]^eta) * sum((X[i,]-V[j,])^2)
        total2 <- total2 + (U[i,j]^m + T[i,j]^eta)
      }else{
        total1 <- total1 + (U[i,j]^m) * sum((X[i,]-V[j,])^2)
        total2 <- total2 + U[i,j]^m
      }
    }
    total <- total + total1 / total2
  }
  if(tidx == "e")
    idx <- total / (2*k)
  else
    idx <- total / k
  if(tidx == "f")
    names(idx) <- "awcd"
  else if(tidx == "e")
    names(idx) <- "awcd.e"
  else if(tidx == "g")
    names(idx) <- "awcd.g"
  return(idx)
}
