\name{sc}
\alias{sc}
\title{Separation/Compactness Ratio}
\usage{
sc(x, u, v, m, t=NULL, eta, tidx="f")
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
Computes the Separation-Compactness (Zahid et al, 1999) ratio index in order to validate the result of a fuzzy and/or possibilistic clustering analysis.
}
\details{

The formula of Separation-Compactness (SC) index is:

\deqn{I_{SC}= SC_1 - SC_2}{I_{SC}= SC_1 - SC_2}

In the above equation:

\eqn{SC_1}{SC_1} is a measure of fuzzy separation and is calculated as follows:

\deqn{SC_1 = \frac{\sum\limits_{j=1}^k ||\vec{v}_j - \bar{v}||^2/k}{\sum\limits_{j=1}^k \Big(\sum\limits_{i=1}^n (u_{ij})^m ||\vec{x}_i - \vec{v}_j||^2 / \sum\limits_{i=1}^n u_{ij} \Big)}}{SC_1 = \frac{\sum\limits_{j=1}^k ||\vec{v}_j - \bar{v}||^2/k}{\sum\limits_{j=1}^k \Big(\sum\limits_{i=1}^n (u_{ij})^m ||\vec{x}_i - \vec{v}_j||^2 / \sum\limits_{i=1}^n u_{ij} \Big)}}

\eqn{SC_2}{SC_2} is a measure of fuzzy compactness and is calculated as follows:

\deqn{SC_2 = \frac{\sum\limits_{j=1}^{k-1} \sum\limits_{l=1}^{k-j} \Big( \sum\limits_{i=1}^n (min(u_{ij}, u_{il})^2 / n_{jz} \Big)}{\sum\limits_{i=1}^n \max\limits_{1 \leq j \leq k}(u_{ij})^2 / \sum\limits_{i=1}^n \max\limits_{1 \leq j \leq k} u_{ij}}}{SC_2 = \frac{\sum\limits_{j=1}^{k-1} \sum\limits_{l=1}^{k-j} \Big( \sum\limits_{i=1}^n (min(u_{ij}, u_{il})^2 / n_{jz} \Big)}{\sum\limits_{i=1}^n \max\limits_{1 \leq j \leq k} (u_{ij})^2 / \sum\limits_{i=1}^n \max\limits_{1 \leq j \leq k} u_{ij}}}

Where:
\eqn{z=j+l}{z=j+l}.

Larger values of \eqn{SC_1}{SC_1} indicate that the fuzzy partition is considered well-separated and compact while smaller values of \eqn{SC_2}{SC_2} indicate compact and well-separated fuzzy clusters correspond to the partioning. The maximum of \eqn{I_{SC}}{I_{SC}} is expected to detect well-defined fuzzy partition (Zahid et al, 1999).
}

\value{
 \item{sc}{SC index value if \code{tidx} is \sQuote{f}}
 \item{sc.e}{extended SC index value if \code{tidx} is \sQuote{e}}
 \item{sc.g}{generalized SC index value if \code{tidx} is \sQuote{g}}
}
\references{
Zahid, N., Limouri, M. & Essaid, A. (1999). A new cluster-validity for fuzzy clustering. \emph{Pattern Recognition}, 32(7): 1089-1097. <doi:10.1016/S0031-3203(98)00157-5>
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

# Compute the SC index using res.fcm, which is a ppclust object
idx <- sc(res.fcm)
print(idx)
 
# Compute the SC index using X, U and V matrices
idx <- sc(res.fcm$x, res.fcm$u, res.fcm$v)
print(idx)

# Run UPFCM algorithm in the package ppclust 
res.upfc <- ppclust::upfc(x, centers=3)
# Compute the generalized XB index using res.upfc, which is a ppclust object
idx <- sc(res.upfc, tidx="g")
print(idx)
}

\concept{internal validity indexes}
\concept{fuzzy validity indexes}
\concept{validation of clustering}
\concept{cluster validity}
\concept{cluster analysis}

\keyword{cluster}
\keyword{multivariate}