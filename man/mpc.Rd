\name{mpc}
\alias{mpc}
\title{Modified Partition Coefficient}
\usage{
mpc(u, m, t=NULL, eta, tidx="f")
}
\arguments{
  \item{u}{an object of class \sQuote{ppclust} containing the clustering results from a fuzzy clustering algorithm in the package \pkg{ppclust}. Alternatively, a numeric data frame or matrix containing the membership degrees.}
  \item{t}{a numeric data frame or matrix containing the cluster prototypes. It should be specified if \code{x} is not an object of \sQuote{ppclust} and the option \option{e} or \option{g} is assigned to \code{tidx}.}
  \item{m}{a number specifying the fuzzy exponent. It should be specified if \code{x} is not an object of \sQuote{ppclust}.}
  \item{eta}{a number specifying the typicality exponent. It should be specified if \code{x} is not an object of \sQuote{ppclust} and \code{tidx} is either \option{e} or \option{g}.}
  \item{tidx}{a character specifying the type of index. The default is \sQuote{f} for fuzzy index. The other options are \sQuote{e} for extended and \sQuote{g} for generalized index.}
}
\description{
Computes the Modified Partition Coefficient (Dave, 1996) in order to validate the result of a fuzzy and/or possibilistic clustering analysis.
}
\details{
Dave's Modified Partition Coefficient (MPC) reduces the monotonic tendency of Partition Coefficient (PC). The formula of MPC is:

\deqn{I_{MPC}=1-\frac{k}{k-1} \Big(1-\sum\limits_{i=1}^n \sum\limits_{j=1}^k u_{ij}^m \Big)}{I_{MPC}=1-\frac{k}{k-1} \Big(1-\sum\limits_{i=1}^n \sum\limits_{j=1}^k u_{ij}^m \Big)}

The optimal clustering is found at the maximum value of \eqn{I_{MPC}}{I_{MPC}}.
}

\value{
 \item{mpc}{MPC index value if \code{tidx} is \sQuote{f}}
 \item{mpc.e}{extended MPC index value if \code{tidx} is \sQuote{e}}
 \item{mpc.g}{generalized MPC index value if \code{tidx} is \sQuote{g}}
}
\references{
Dave, R.N. (1996). Validating fuzzy partition obtained through c-shells clustering. \emph{Pattern Recognition Letters}, 17 (6):613-623. <doi:10.1016/0167-8655(96)00026-8>
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

# Compute the MPC index using res.fcm, which is a ppclust object
idx <- mpc(res.fcm)
print(idx)
 
# Compute the MPC index using the fuzzy membership matrix
idx <- mpc(res.fcm$u)
print(idx)

# Run UPFC algorithm in the package ppclust 
res.upfc <- ppclust::upfc(x, centers=3)
# Compute the generalized MPC index using res.upfc, which is a ppclust object
idx <- mpc(res.upfc, tidx="g")
print(idx)
}

\concept{internal validity indexes}
\concept{fuzzy validity indexes}
\concept{validation of clustering}
\concept{cluster validity}
\concept{cluster analysis}

\keyword{cluster}
\keyword{multivariate}