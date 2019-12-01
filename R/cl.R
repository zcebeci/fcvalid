cl <- function(u, m, t=NULL, eta, tidx="f"){
  if(missing(u))
    stop("Missing input membership matrix or ppclust object")
  tidx <- match.arg(tidx, c("e","f","g"))
  if(inherits(u, "ppclust")){
    if(!is.null(u$u)){
      U <- as.matrix(u$u)
      m <- u$m
    }
    else if(!is.null(u$t)){
      U <- as.matrix(u$t)
      m <- u$eta
    }
    else{
      stop("Missing fuzzy membership or typicality matrix")
    }
    if(tidx == "e" || tidx == "g"){
      if(!is.null(u$t)){
        T <- u$t
        eta <- u$eta
      }
      else
        stop("Input argument is a ppclust object but does not have the typicality matrix")
    }
  }
  else{
    if(!missing(u))
      if(is.matrix(u) || is.data.frame(u))
        U <- as.matrix(u)
      else
        if(!is.null(t))
          U <- as.matrix(t)
        else
          stop("Argument 'u' must be a numeric data frame or matrix")
    else{
      stop("Missing argument 'u'")
    }
    if(missing(m))
      m <- 2 
    if(!is.numeric(m))
      stop("Argument 'm' must be number") 
    if(m < 1)
      stop("Argument 'm' should be a positive number equals to or greater than 1") 
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
  }
  if(tidx == "g"){
    if(!is.null(T)){
      U <- T/rowSums(T)
      m <- eta
    }
    else
      stop("The typicality matrix is required to compute the generalized pe index")
  }
  n <- nrow(U)
  k <- ncol(U)
  totmax <- 0
  totmin <- 0
  for(i in 1:n){
    if(tidx == "e")
      totmax <- totmax + max(U[i,]^m + T[i,]^eta)
    else
      totmax <- totmax + max(U[i,]^m)
    for(j in 1:(k-1)){
      for(l in (j+1):k){
        if(tidx == "e")
          totmin <- totmin + min((U[i,j]^m + T[i,j]^eta), (U[i,l]^m + T[i,l]^eta))
        else
          totmin <- totmin + min(U[i,j]^m, U[i,l]^m)
      }
    }
  }
  K <- 0
  for(j in k-1)
    K <- K+j
  if(tidx == "e")
    idx <- (totmax/n/2)-((totmin/n/2)/ K)
  else
    idx <- (totmax/n)-((totmin/n)/K) 
  if(tidx == "f")
    names(idx) <- "cl"
  else if(tidx == "e")
    names(idx) <- "cl.e"
  else if(tidx == "g")
    names(idx) <- "cl.g"
  return(idx)
}
