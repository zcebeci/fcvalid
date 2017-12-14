\name{pbm}
\alias{pbm}
\title{Pakhira-Bandyopadhyay-Maulik Index}
\usage{
pbm(x, u, v, m, t=NULL, eta, tidx="f")
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
Computes the Pakhira-Bandyopadhyay-Maulik (Pakhira et al, 2005) index for validating the result of a fuzzy and/or possibilistic clustering analysis.
}
\details{
The Pakhira-Bandyopadhyay-Maulik (PBM) index proposed by Pakhira et al (2005) has two versions which can be used for validating both hard and soft clustering results. The formula in the following equation is the fuzzy version PBM index so-called PBMF in some of the literature:

\deqn{I_{PBM}=\frac{1}{k} \;\frac{E_0}{E_k} \; D_k}{I_{PBM}=\frac{1}{k} \;\frac{E_0}{E_k} \; D_k} 

Where:
\eqn{E_0 = \sum\limits_{i=1}^n ||x_i-\bar{v}||^2}{E_0 = \sum\limits_{i=1}^n ||x_i-\bar{v}||^2}, 
\eqn{E_k = \sum\limits_{j=1}^k E_j}{E_k = \sum\limits_{j=1}^k E_j},  
\eqn{E_j = \sum\limits_{j=1}^k u_{ij} \; d_{ij}^2}{E_j = \sum\limits_{j=1}^k u_{ij} \; d_{ij}^2}, 
\eqn{D_k = \max\limits_{j,l=1}^k ||\vec{v}_j-\vec{v}_l||^2}{D_k = \max\limits_{j,l=1}^k ||\vec{v}_j-\vec{v}_l||^2} 

The optimal clustering is obtained with the maximum value of \eqn{I_{PBM}}{I_{PBM}}.
}

\value{
 \item{pbm}{PBM index value, if \code{tidx} is \sQuote{f}}
 \item{pbm.e}{extended PBM index value, if \code{tidx} is \sQuote{e}}
 \item{pbm.g}{generalized PBM index value, if \code{tidx} is \sQuote{g}}
}
\references{
Pakhira, M.K., Bandyopadhyay, S. & Maulik, U. (2005). Validity index for crisp and fuzzy clusters, \emph{Pattern Recognition}, 37(3):487-501. <doi:10.1016/j.patcog.2003.06.005>
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

# For 3 clusters, run FCM algorithm in the package ppclust 
res.fcm <- ppclust::fcm(x, centers=3)

# Compute the PBM index using ppclust object
idx <- pbm(res.fcm)
print(idx)
 
# Compute the PBM index using X,U,V matrices
idx <- pbm(res.fcm$x, res.fcm$u, res.fcm$v, m=2)
print(idx)
}

\concept{internal validity indexes}
\concept{fuzzy validity indexes}
\concept{validation of clustering}
\concept{cluster validity}
\concept{cluster analysis}

\keyword{cluster}
\keyword{multivariate}