\name{fhv}
\alias{fhv}
\title{Fuzzy Hyper Volume}
\usage{
fhv(x, u, v, m, t=NULL, eta, tidx="f")
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
Computes the Fuzzy Hyper Volume (Gath & Geva, 1989) value for validating the result of a fuzzy and/or possibilistic cluster analysis.
}
\details{
Fuzzy Hyper Volume proposed by (Gath & Geva, 1989) is calculated by summation of the determinants of the fuzzy covariance matrices of the clusters as follows:

\deqn{I_{FHV}=\sum\limits_{j=1}^k F_j}{I_{FHV}=\sum\limits_{j=1}^k F_j}
Where:
\eqn{F_j}{F_j} is the square root of the determinant of fuzzy covariance matrix of j-\emph{th} cluster:

\deqn{F_j=\sqrt{|\mathbf{S_j}|}}{F_j=\sqrt{|\mathbf{S_j}|}}

The fuzzy covariance matrix \eqn{\mathbf{S_j}}{\mathbf{S_j}} in the equation above is:

\deqn{\mathbf{S_j} = \frac{\sum\limits_{i=1}^n u_{ij}^m \; (\vec{x_i}-\vec{v_j})^T(\vec{x_i}-\vec{v_j})}{\sum\limits_{i=1}^n u_{ij}^m}}{\mathbf{S_j} = \frac{\sum\limits_{i=1}^n u_{ij}^m (\vec{x_i}-\vec{v_j})^T(\vec{x_i}-\vec{v_j})}{\sum\limits_{i=1}^n u_{ij}^m}}

Small values of \eqn{I_{FHV}}{I_{FHV}} indicate the existence of compact clusters (Halkidi et al, 2002b). Thus the minimum \eqn{I_{FHV}}{I_{FHV}} gives the optimal clustering.
}

\value{
 \item{fhv}{FHV value if \code{tidx} is \sQuote{f}}
 \item{fhv.e}{extended FHV value if \code{tidx} is \sQuote{e}}
 \item{fhv.g}{generalized FHV value if \code{tidx} is \sQuote{g}}
}

\references{
Gath, I. & Geva, A. B. (1989). Unsupervised optimal fuzzy clustering. \emph{IEEE Transactions on Pattern Analysis and Machine Intelligence}, 11(7):773-780. <doi:10.1109/34.192473>

Halkidi, M., Batistakis, Y. & Vazirgiannis, M. (2002b). Clustering validity checking methods: part II. \emph{ACM Sigmod Record}, 31(3):19-27. <doi:10.1145/601858.601862>
}

\author{Zeynel Cebeci}

\seealso{
 \code{\link{allindexes}},
 \code{\link{apd}},
 \code{\link{cl}},
 \code{\link{cs}},
 \code{\link{cwb}},
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

# For 3 clusters, run FCM algorithm in the package ppclust 
res.fcm <- ppclust::fcm(x, centers=3)

# Compute the FHV using ppclust object
idx <- fhv(res.fcm)
print(idx)
 
# Compute the FHV using X,U and V matrices
idx <- fhv(res.fcm$x, res.fcm$u, res.fcm$v, m=2)
print(idx)
}

\concept{internal validity indexes}
\concept{fuzzy validity indexes}
\concept{validation of clustering}
\concept{clustering validity}
\concept{cluster analysis}
\concept{fuzzy hypervolume}

\keyword{cluster}
\keyword{multivariate}