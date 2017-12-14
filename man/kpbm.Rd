\name{kpbm}
\alias{kpbm}
\title{Modified Kernel Form of Pakhira-Bandyopadhyay-Maulik Index}
\usage{
kpbm(x, u, v, m, t=NULL, eta, tidx="f")
}
\arguments{
  \item{x}{an object of class \sQuote{ppclust} containing the clustering results from a fuzzy clustering algorithm in the package \pkg{ppclust}. Alternatively, a numeric data frame or matrix containing the data set.}
  \item{u}{a numeric data frame or matrix containing the fuzzy membership values. It should be specified if \code{x} is not an object of \sQuote{ppclust}.}
  \item{v}{a numeric data frame or matrix containing the cluster prototypes. It should be specified if \code{x} is not an object of \sQuote{ppclust}.}
  \item{t}{a numeric data frame or matrix containing the cluster prototypes. It should be specified if \code{x} is not an object of \sQuote{ppclust}.}
  \item{m}{a number specifying the fuzzy exponent. It should be specified if \code{x} is not an object of \sQuote{ppclust}.}
  \item{eta}{a number specifying the typicality exponent. It should be specified if \code{x} is not an object of \sQuote{ppclust}.}
  \item{tidx}{a character specifying the type of index. The default is \sQuote{f} for fuzzy index. The other options are \sQuote{e} for extended and \sQuote{g} for generalized index.}
}
\description{
Computes the Modified Kernel Form of Pakhira-Bandyopadhyay-Maulik (KPBM) index for validating the result of a fuzzy clustering analysis.
}
\details{
The modified Kernel form of Pakhira-Bandyopadhyay-Maulik (KPBM) index avoids to use the numerator term \eqn{E_0}{E_0} of the original PBM (Pakhira et al, 2005) index. In this way, the computation time required by PBM is reduced without affecting its characteristic. For the modified form so-called KPBM, the prefix \eqn{K}{K} means \emph{kernel} or \emph{core}. The equation for calculating the KPBM index is:

\deqn{I_{KPBM}=\frac{1}{k} \;\frac{1}{E_k} \; D_k}{I_{KPBM}=\frac{1}{k} \;\frac{1}{E_k} \; D_k}, 
where:
\eqn{E_k = \sum\limits_{j=1}^k E_j}{E_k = \sum\limits_{j=1}^k E_j},  
\eqn{E_j = \sum\limits_{j=1}^k u_{ij} \; d_{ij}^2}{E_j = \sum\limits_{j=1}^k u_{ij} \; d_{ij}^2}, 
\eqn{D_k = \max\limits_{j,l=1}^k ||\vec{v}_j - \vec{v}_l||^2}{D_k = \max\limits_{j,l=1}^k ||\vec{v}_j - \vec{v}_l||^2} 

The optimal clustering is obtained with the maximum value of \eqn{I_{KPBM}}{I_{KPBM}}.
}

\value{
 \item{kpbm}{KPBM index value, if \code{tidx} is \sQuote{f}}
 \item{kpbm.e}{extended KPBM index, if \code{tidx} is \sQuote{e}}
 \item{kpbm.g}{generalized KPBM index value, if \code{tidx} is \sQuote{g}}
}

\references{
PatternWiz - A software package for pattern recognition. \url{http://shodhganga.inflibnet.ac.in/bitstream/10603/93443/16/16_chapter\%208.pdf}
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

# Compute the KPBM index using ppclust object
idx <- kpbm(res.fcm)
print(idx)
 
# Compute the KPBM index using X,U,V matrices
idx <- kpbm(res.fcm$x, res.fcm$u, res.fcm$v, m=2)
print(idx)
}

\concept{internal validity indexes}
\concept{fuzzy validity indexes}
\concept{validation of clustering}
\concept{cluster validity}
\concept{cluster analysis}

\keyword{cluster}
\keyword{multivariate}