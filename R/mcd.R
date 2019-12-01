mcd <- function(x){
  if(missing(x))
    stop("Missing prototypes matrix or ppclust object")
  if(inherits(x, "ppclust"))
    V <- as.matrix(x$v)
  else{
    if(!missing(x))
      if(is.matrix(x) || is.data.frame(x))
        V <- as.matrix(x)
      else
        stop("Argument 'x' must be a numeric data frame or matrix containing the cluster prototypes matrix")
    else
      stop("Missing prototype matrix")
   }
   minv <- Inf
   k <- nrow(V)
   for(j in 1:(k-1)){
     for(l in (j+1):k){
       if(sum((V[j,]-V[l,])^2) < minv) 
          minv <- sum((V[j,]-V[l,])^2)
     }
   }
  idx <- minv
  names(idx) <- "mcd"
  return(idx)
}
