\name{cl}
\alias{cl}
\title{Chen-Linkens Index}
\usage{
cl(u, m, t=NULL, eta, tidx="f")
}
\arguments{
  \item{u}{an object of class \sQuote{ppclust} containing the clustering results from a fuzzy clustering algorithm in the package \pkg{ppclust}. Alternatively, a numeric data frame or matrix containing the data set.}
  \item{t}{a numeric data frame or matrix containing the cluster prototypes. It should be specified if \code{u} is not an object of \sQuote{ppclust} and the option \option{e} or \option{g} is assigned to \code{tidx}.}
  \item{m}{a number specifying the fuzzy exponent. It should be specified if \code{x} is not an object of \sQuote{ppclust}.}
  \item{eta}{a number specifying the typicality exponent. It should be specified if \code{x} is not an object of \sQuote{ppclust} and \code{tidx} is either \option{e} or \option{g}.}
  \item{tidx}{a character specifying the type of index. The default is \sQuote{f} for fuzzy index. The other options are \sQuote{e} for extended and \sQuote{g} for generalized index.}
}
\description{
Computes the Chen-Linkens index (Chen & Linkens, 2004) in order to validate the result of a fuzzy and/or possibilistic clustering analysis.
}
\details{
Chen-Linkens (CL) index consists of two terms. The first term reflects the compactness within a cluster. The second one indicates the separation between clusters (Chen & Linkens, 2004). The formula of CL index is:

\deqn{I_{CL}=\frac{1}{n} \sum\limits_{i=1}^n \max\limits_j (u_{ij}) - \frac{1}{K} \sum\limits_{j=1}^{k-1} \sum\limits_{l=j+1}^k \Big[ \frac{1}{n} \sum\limits_{i=1}^n \min(u_{ij}, u_{il})\Big]}{I_{CL}=\frac{1}{n} \sum\limits_{i=1}^n \max\limits_j (u_{ij}) - \frac{1}{K} \sum\limits_{j=1}^{k-1} \sum\limits_{l=j+1}^k \Big[ \frac{1}{n} \sum\limits_{i=1}^n \min(u_{ij}, u_{il})\Big]}

In the above equation \eqn{K}{K} is a summation as follows:

\eqn{K=\sum\limits_{j=1}^{k-1} j}{K=\sum\limits_{j=1}^{k-1} j}

The optimal clustering is obtained at the maximum value of \eqn{I_{CL}}{I_{CL}}.
}

\value{
 \item{cl}{CL index value if \code{tidx} is \sQuote{f}}
 \item{cl.e}{extended CL index value if \code{tidx} is \sQuote{e}}
 \item{cl.g}{generalized CL index value if \code{tidx} is \sQuote{g}}
}
\references{
Chen, M. Y. & Linkens, D. A. (2004). Rule-base self-generation and simplification for data-driven fuzzy models. \emph{Fuzzy Sets and Systems}, 142(2):243-265. <doi:10.1016/S0165-0114(03)00160-X>
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
 \code{\link{xb}}}

\examples{
# Load the dataset iris and use the first four feature columns 
data(iris)
x <- iris[,1:4]

# Run FCM algorithm in the package ppclust 
res.fcm <- ppclust::fcm(x, centers=3)

# Compute the CL index using res.fcm, which is a ppclust object
idx <- cl(res.fcm)
print(idx)
 
# Compute the XB index using X and U matrices
idx <- cl(u=res.fcm$u, m=2)
print(idx)

# Run UPFC algorithm in the package ppclust 
res.upfc <- ppclust::upfc(x, centers=3)
# Compute the generalized CL index using res.upfc, which is a ppclust object
idx <- cl(res.upfc, tidx="g")
print(idx)
}

\concept{internal validity indexes}
\concept{fuzzy validity indexes}
\concept{validation of clustering}
\concept{cluster validity}
\concept{cluster analysis}

\keyword{cluster}
\keyword{multivariate}