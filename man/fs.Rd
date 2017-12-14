\name{fs}
\alias{fs}
\title{Fukuyama-Sugeno Index}
\usage{
fs(x, u, v, m, t=NULL, eta, tidx="f")
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
Computes the Fukuyama-Sugeno (Fukuyama & Sugeno, 1989) index for validating the result of a fuzzy and/or possibilistic cluster analysis.
}
\details{
In the Fukuyama-Sugeno index by (Fukuyama & Sugeno, 1989), the sum of inter- and intra-cluster differences are used to define the index formula as follows:

\deqn{I_{FS}=\sum\limits_{i=1}^n \sum\limits_{j=1}^k u_{ij}^m \; (d_{ij}^2 - ||\vec{v_j}-\bar{v}||^2)}{I_{FS}=\sum\limits_{i=1}^n \sum\limits_{j=1}^k u_{ij}^m \; (d_{ij}^2 - ||\vec{v_j}-\bar{v}||^2)}

The first term in the above equation measures the compactness of the clusters while the second term measures the distances of the clusters representatives (Halkidi et al, 2002b). The minimum \eqn{I_{FS}}{I_{FS}} indicates the optimal clustering.
}

\value{
 \item{fs}{FS index if \code{tidx} is \sQuote{f}}
 \item{fs.e}{extended FS index if \code{tidx} is \sQuote{e}}
 \item{fs.g}{generalized FS index if \code{tidx} is \sQuote{g}}
}

\references{
Fukuyama, Y. & Sugeno, M. (1989). A new method of choosing the number of cluster for the fuzzy c-means method, \emph{Proc. of the 5th Fuzzy Systems Symp.}, 247-250.

Halkidi, M., Batistakis, Y., & Vazirgiannis, M. (2002b). Clustering validity checking methods: part II. \emph{ACM Sigmod Record}, 31(3):19-27. <doi:10.1145/601858.601862>
}

\author{Zeynel Cebeci}

\seealso{
 \code{\link{allindexes}},
 \code{\link{apd}},
 \code{\link{cl}},
 \code{\link{cs}},
 \code{\link{cwb}},
 \code{\link{fhv}},
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
 \code{\link{xb}}}

\examples{
# Load the dataset iris and use the first four feature columns 
data(iris)
x <- iris[,1:4]

# For 3 clusters, run FCM algorithm in the package ppclust 
res.fcm <- ppclust::fcm(x, centers=3)

# Compute the FS index using ppclust object
idx <- fs(res.fcm)
print(idx)
 
# Compute the FS index using X,U and V matrices
idx <- fs(res.fcm$x, res.fcm$u, res.fcm$v, m=2)
print(idx)
}

\concept{internal validity indexes}
\concept{fuzzy validity indexes}
\concept{validation of clustering}
\concept{clustering validity}
\concept{cluster analysis}

\keyword{cluster}
\keyword{multivariate}