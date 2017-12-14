\name{allindexes}
\alias{allindexes}
\title{Compute all of the internal indexes}
\usage{
allindexes(x, u, v, m, t=NULL, eta, tidx="f")
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
Computes all of the internal indexes included in the package \pkg{fcvalid}.
}

\value{
 \item{idx}{fuzzy index values if \code{tidx} is \sQuote{f}}
 \item{idx.e}{extended index values if \code{tidx} is \sQuote{e}}
 \item{idx.g}{generalized index values if \code{tidx} is \sQuote{g}}
}

\author{Zeynel Cebeci}

\seealso{
 \code{\link{apd}},
 \code{\link{cl}},
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
# Load the dataset iris and use the first four feature columns 
data(iris)
x <- iris[,1:4]

# Run FCM algorithm in the package ppclust 
res.fcm <- ppclust::fcm(x, centers=3)

# Compute the indexes using res.fcm, which is a ppclust object
idx <- allindexes(res.fcm)
print(idx)
 
# Run UPFC algorithm in the package ppclust 
res.upfc <- ppclust::upfc(x, centers=3)
# Compute the generalized indexes using res.upfc, which is a ppclust object
idx <- allindexes(res.upfc, tidx="g")
print(idx)
}

\concept{internal validity indexes}
\concept{fuzzy validity indexes}
\concept{validation of clustering}
\concept{cluster validity}
\concept{cluster analysis}

\keyword{cluster}
\keyword{multivariate}