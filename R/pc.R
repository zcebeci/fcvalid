pc <- function(u, m, t=NULL, eta, tidx="f"){
  if(missing(u))
    stop("Missing membership matrix or ppclust object")
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
        stop("Input argument is ppclust object but does not have the typicality matrix")
    }
  }
  else{
    if(!missing(u))
      if(is.matrix(u) || is.data.frame(u))
        U <- as.matrix(u)
      else
        stop("Argument 'u' must be a numeric data frame or matrix")
    else
      stop("Missing argument 'u'")
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
  if(tidx == "g"){
    if(!is.null(T))
      U <- T/rowSums(T)
    else
      stop("The typicality matrix is required to compute the generalized pc index")
  }
  if(tidx == "e")
    idx <- sum(((U)^m)+((T)^eta)) / (2*nrow(U))
  else
    idx <- sum(U^m) / nrow(U)
  if(tidx == "f")
    names(idx) <- "pc"
  else if(tidx == "e")
    names(idx) <- "pc.e"
  else if(tidx == "g")
    names(idx) <- "pc.g"
  return(idx)
}
