\name{awcd}
\alias{awcd}
\title{Average Within-Cluster Distance}
\usage{
awcd(x, u, v, m, t=NULL, eta, tidx="f")
}
\arguments{
  \item{x}{an object of class \sQuote{ppclust} containing the clustering results from a fuzzy clustering algorithm in the package \pkg{ppclust}. Alternatively, a numeric data frame or matrix containing the data set.}
  \item{u}{a numeric data frame or matrix containing the fuzzy membership values. It should be specified if \code{x} is not an object of \sQuote{ppclust}.}
  \item{v}{a numeric data frame or matrix containing the cluster prototypes. It should be specified if \code{x} is not an object of \sQuote{ppclust}.}
  \item{t}{a numeric data frame or matrix containing the cluster prototypes. It should be specified if \code{x} is not an object of \sQuote{ppclust} and the option \option{e} or \option{g} is assigned to \code{tidx}.}
  \item{m}{a number specifying the fuzzy exponent. It should be specified if \code{x} is not an object of \sQuote{ppclust}.}
  \item{eta}{a number specifying the typicality exponent. It should be specified if \code{x} is not an object of \sQuote{ppclust} and \code{tidx} is either \option{e} or \option{g}.}
  \item{tidx}{a character specifying the type of index. The default is \sQuote{f} for fuzzy index. The other options are \sQuote{e} for extended and \sQuote{g} for generalized index.}
}
\description{
Computes the Average Within-Cluster Distance (Krishnapuram & Freg, 1992) to validate the result of a fuzzy and/or possibilistic clustering analysis.
}
\details{
Average Within-Cluster Distance (AWCD) is defined as:

\deqn{I_{AWCD}=\frac{1}{n \; k} \frac{\sum\limits_{i=1}^n u_{ij}^m \; d_{ij}^2}{\sum\limits_{i=1}^n u_{ij}^m}}{I_{AWCD}=\frac{1}{n \; k} \frac{\sum\limits_{i=1}^n u_{ij}^m \; d_{ij}^2}{\sum\limits_{i=1}^n u_{ij}^m}}

As the \eqn{I_{AWCD}}{I_{AWCD}} monotonically decreases with the number of clusters. A knee in the curve of \eqn{I_{AWCD}}{I_{AWCD}} as a function of \eqn{k}{k} indicates a good fuzzy partition.
}

\value{
 \item{awcd}{AWCD index value if \code{tidx} is \sQuote{f}}
 \item{awcd.e}{extended AWCD index value if \code{tidx} is \sQuote{e}}
 \item{awcd.g}{generalized AWCD index value if \code{tidx} is \sQuote{g}}
}
\references{
Krishnapuram, R. & Freg, C. P. (1992). Fitting an unknown number of lines and planes to image data through compatible cluster merging. \emph{Pattern Recognition}, 25(4):385-400. <doi:10.1016/0031-3203(92)90087-Y> 

}

\author{Zeynel Cebeci}

\seealso{
 \code{\link{allindexes}},
 \code{\link{apd}},
 \code{\link{cs}},
 \code{\link{cwb}},
 \code{\link{fhv}},
 \code{\link{fs}},
 \code{\link{kpbm}},
 \code{\link{kwon}},
 \code{\link{mcd}},
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
# Load the dataset iris
data(iris)
x <- iris[,1:4]

# Run FCM algorithm in the package ppclust 
res.fcm <- ppclust::fcm(x, centers=3)

# Compute the AWCD index using res.fcm, which is a ppclust object
idx <- awcd(res.fcm)
print(idx)
 
# Compute the AWCD index using X, U and V matrices
idx <- awcd(res.fcm$x, res.fcm$u, res.fcm$v)
print(idx)

# Run UPFCM algorithm in the package ppclust 
res.upfc <- ppclust::upfc(x, centers=3)
# Compute the generalized AWCD index using res.upfc, which is a ppclust object
idx <- awcd(res.upfc, tidx="g")
print(idx)
}

\concept{internal validity indexes}
\concept{fuzzy validity indexes}
\concept{validation of clustering}
\concept{cluster validity}
\concept{cluster analysis}

\keyword{cluster}
\keyword{multivariate}
