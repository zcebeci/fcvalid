\name{tss}
\alias{tss}
\title{Tang, Sun & Sun Index}
\usage{
tss(x, u, v, m, t=NULL, eta, tidx="f")
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
Computes the Tang, Sun & Sun (Tang et al, 2005) index in order to validate the result of a fuzzy and/or possibilistic clustering analysis.
}
\details{
Tang, Sun & Sun Index (TSS) index for fuzzy clustering has been proposed to eliminate the monotonically decreasing tendency as the number of clusters approaches to the number of data points and avoid the numerical instability of validation index when fuzzy weighting exponent increases (Tang et al, 2005).The index is defined as follows:

\deqn{I_{TSS}=\frac{\sum\limits_{i=1}^n \sum\limits_{j=1}^k u_{ij}^m \; d_{ij}^2 + \frac{1}{k (k-1)} \; \sum\limits_{j=1}^k \sum\limits_{l,j\neq l}^k  ||\vec{v_j}-\vec{v_l}||^2}{\min\limits_{j\neq l} (||\vec{v_j} - \vec{v_l}||^2) + \frac{1}{k}}}{I_{TSS}=\frac{\sum\limits_{i=1}^n \sum\limits_{j=1}^k u_{ij}^m \; d_{ij}^2 + \frac{1}{k (k-1)} \; \sum\limits_{j=1}^k \sum\limits_{l,j\neq l}^k  ||\vec{v_j}-\vec{v_l}||^2}{\min\limits_{j\neq l} (||\vec{v_j} - \vec{v_l}||^2) +\frac{1}{k}}}

The optimal clustering is obtained at the minimum value of \eqn{I_{TSS}}{I_{TSS}}.
}

\value{
 \item{tss}{TSS index value if \code{tidx} is \sQuote{f}}
 \item{tss.e}{extended TSS index value if \code{tidx} is \sQuote{e}}
 \item{tss.g}{generalized TSS index  value if \code{tidx} is \sQuote{g}}
}
\references{
Tang, Y., Sun, F. & Sun, Z. (2005). Improved validation index for fuzzy clustering. In \emph{Proc. of the American Control Conference, 2005}. pp. 1120-1125. IEEE. <doi:10.1109/ACC.2005.1470111>
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
 \code{\link{ws}},
 \code{\link{xb}}
}

\examples{
# Load the dataset iris
data(iris)
x <- iris[,1:4]

# Run FCM algorithm in the package ppclust 
res.fcm <- ppclust::fcm(x, centers=3)

# Compute the TSS index using res.fcm, which is a ppclust object
idx <- tss(res.fcm)
print(idx)
 
# Compute the TSS index using X, U and V matrices
idx <- tss(res.fcm$x, res.fcm$u, res.fcm$v)
print(idx)

# Run UPFC algorithm in the package ppclust 
res.upfc <- ppclust::upfc(x, centers=3)
# Compute the generalized TSS index using res.upfc, which is a ppclust object
idx <- tss(res.upfc, tidx="g")
print(idx)
}

\concept{internal validity indexes}
\concept{fuzzy validity indexes}
\concept{validation of clustering}
\concept{cluster validity}
\concept{cluster analysis}

\keyword{cluster}
\keyword{multivariate}