\name{kwon}
\alias{kwon}
\title{Kwon Index}
\usage{
kwon(x, u, v, m, t=NULL, eta, tidx="f")
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
Computes the Kwon Index (Kwon, 1998) in order to validate the result of a fuzzy and/or possibilistic clustering analysis.
}
\details{
Kwon Index (\eqn{I_{Kwon}}{I_{Kwon}}) is defined as follows:

\deqn{I_{Kwon}=\frac{\sum\limits_{i=1}^n \sum\limits_{j=1}^k u_{ij}^m \; d_{ij}^2 + \frac{1}{k} \; \sum\limits_{j=1}^k ||\vec{v_i}-\bar{v}||^2}{\min\limits_{j\neq l}^k (||\vec{v_j} - \vec{v}_l||^2)}}{I_{Kwon}=\frac{\sum\limits_{i=1}^n \sum\limits_{j=1}^k u_{ij}^m \; d_{ij}^2 + \frac{1}{k} \; \sum\limits_{j=1}^k ||\vec{v_i}-\bar{v}||^2}{\min\limits_{j\neq l}^k (||\vec{v_j} - \vec{v}_l||^2)}}

The optimal clustering is obtained at the minimum value of \eqn{I_{Kwon}}{I_{Kwon}}.
}

\value{
 \item{kwon}{Kwon index value if \code{tidx} is \sQuote{f}}
 \item{kwon.e}{extended Kwon index value if \code{tidx} is \sQuote{e}}
 \item{kwon.g}{generalized Kwon index value if \code{tidx} is \sQuote{g}}
}
\references{
Kwon, S. H. (1998). Cluster validity index for fuzzy clustering. \emph{Electronics Letters}, 34(22):2176-2177.<10.1049/el:19981523>
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

# Compute the Kwon index using res.fcm, which is a ppclust object
idx <- kwon(res.fcm)
print(idx)
 
# Compute the Kwon index using X, U and V matrices
idx <- kwon(res.fcm$x, res.fcm$u, res.fcm$v)
print(idx)

# Run UPFCM algorithm in the package ppclust 
res.upfc <- ppclust::upfc(x, centers=3)
# Compute the generalized Kwon index using res.upfc, which is a ppclust object
idx <- kwon(res.upfc, tidx="g")
print(idx)
}

\concept{internal validity indexes}
\concept{fuzzy validity indexes}
\concept{validation of clustering}
\concept{cluster validity}
\concept{cluster analysis}

\keyword{cluster}
\keyword{multivariate}