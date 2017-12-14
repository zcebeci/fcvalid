\name{xb}
\alias{xb}
\title{Xie-Beni Index}
\usage{
xb(x, u, v, m, t=NULL, eta, tidx="f")
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
Computes the Xie-Beni (Xie & Beni, 1991) index to validate the result of a fuzzy and/or possibilistic clustering analysis.
}
\details{
Xie-Beni index (XB) is a fuzzy validity criterion based on a validity function which identifies compact and separate fuzzy c-partitions without assumptions as to the number of substructures inherent in the data (Xie & Beni, 1991). Therefore it is also called \dfn{the compactness and separation validity function}. The index is first representative of its category with its successors. The formula of XB index is:

\deqn{I_{XB}=\frac{\sum\limits_{i=1}^n \sum\limits_{j=1}^k u_{ij}^m \; d_{ij}^2}{n \; \Big(\min\limits_{j\neq l}(||\vec{v_j} - \vec{v}_l||^2)\Big)}}{I_{XB}=\frac{\sum\limits_{i=1}^n \sum\limits_{j=1}^k u_{ij}^m \; d_{ij}^2}{n \; \Big(\min\limits_{j\neq l}(||\vec{v_j} - \vec{v}_l||^2)\Big)}}

Since the smaller values of the index indicate the compact and well-separated clusters, the optimal clustering is obtained at the minimum value of \eqn{I_{XB}}{I_{XB}}.
}

\value{
 \item{xb}{XB index value if \code{tidx} is \sQuote{f}}
 \item{xb.e}{extended XB index value if \code{tidx} is \sQuote{e}}
 \item{xb.g}{generalized XB index value if \code{tidx} is \sQuote{g}}
}
\references{
Xie, X.L. & Beni, G. (1991). A validity measure for fuzzy clustering, \emph{IEEE Transactions on Pattern Analysis and Machine Intelligence}, 13(8):841-847. <doi:10.1109/34.85677>
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
 \code{\link{mcd}},
 \code{\link{mpc}},
 \code{\link{pbm}},
 \code{\link{pc}},
 \code{\link{pe}},
 \code{\link{sc}},
 \code{\link{si}},
 \code{\link{tss}},
 \code{\link{ws}}
}

\examples{
# Load the dataset iris and use the first four feature columns 
data(iris)
x <- iris[,1:4]

# Run FCM algorithm in the package ppclust 
res.fcm <- ppclust::fcm(x, centers=3)

# Compute the XB index using res.fcm, which is a ppclust object
idx <- xb(res.fcm)
print(idx)
 
# Compute the XB index using X, U and V matrices
idx <- xb(res.fcm$x, res.fcm$u, res.fcm$v)
print(idx)

# Run UPFCM algorithm in the package ppclust 
res.upfc <- ppclust::upfc(x, centers=3)
# Compute the generalized XB index using res.upfc, which is a ppclust object
idx <- xb(res.upfc, tidx="g")
print(idx)
}

\concept{internal validity indexes}
\concept{fuzzy validity indexes}
\concept{validation of clustering}
\concept{cluster validity}
\concept{cluster analysis}

\keyword{cluster}
\keyword{multivariate}