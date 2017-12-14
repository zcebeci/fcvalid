\name{mcd}
\alias{mcd}
\title{Minimum Centroid Distance}
\usage{
mcd(x)
}
\arguments{
  \item{x}{an object of class \sQuote{ppclust} containing the clustering results from a fuzzy and/or possibilistic clustering algorithm in the package \pkg{ppclust}. Alternatively, it is a numeric data frame or matrix containing the cluster prototypes.}
}
\description{
Computes the Minimum Centroid Distance (MCD) to test the result of a fuzzy clustering analysis (Schwaemmle & Jensen, 2010).
}
\details{
MCD values for different numbers of clusters (\code{k}) and different values of fuzzy exponent (\code{m}) can be used to compare the results of fuzzy cluster analysis in order find the optimal result. The formula of MCD  is:

\deqn{I_{MCD}=\min\limits_{j \neq l}(||\vec{v_j}-\vec{v_l}||^2)}{I_{MCD}=\min\limits_{j \neq l}(||\vec{v_j}-\vec{v_l}||^2)}

The optimal clustering is found at the maximum value of MCD.
}

\value{
 \item{mcd}{MCD value.}
}
\references{
Schwaemmle, V. & Jensen, O.N. (2010). A simple and fast method to determine the parameters for fuzzy c-means cluster validation. <arXiv:\url{http://arxiv.org/abs/1004.1307v1}>
}

\author{Zeynel Cebeci}

\seealso{
 \code{\link{allindexes}},
 \code{\link{apd}},
 \code{\link{cl}},
 \code{\link{cs}},
 \code{\link{cwb}},
 \code{\link{fhv}},
 \code{\link{fs}},
 \code{\link{kpbm}},
 \code{\link{kwon}},
 \code{\link{mpc}},
 \code{\link{pbm}},
 \code{\link{pc}},
 \code{\link{pe}},
 \code{\link{sc}},
 \code{\link{si}},
 \code{\link{tss}},
 \code{\link{ws}},
 \code{\link{xb}}
}

\examples{
# Load the dataset iris and use the first four feature columns 
data(iris)
x <- iris[,1:4]

# Run FCM algorithm in the package ppclust 
res.fcm <- ppclust::fcm(x, centers=3)

# Compute the MCD using res.fcm, which is a ppclust object
idx <- mcd(res.fcm)
print(idx)
 
# Compute the MCD index using V matrix
idx <- mcd(res.fcm$v)
print(idx)
}

\concept{internal validity indexes}
\concept{fuzzy validity indexes}
\concept{validation of clustering}
\concept{cluster validity}
\concept{cluster analysis}

\keyword{cluster}
\keyword{multivariate}