\name{apd}
\alias{apd}
\title{Average Partition Density}
\usage{
apd(x, u, v, m, t=NULL, eta, tidx="f")
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
Computes the Average Partition Density (Gath & Geva, 1989) value for validating the result of a fuzzy cluster analysis.
}
\details{
Average Partition Density proposed by (Gath & Geva, 1989) is calculated as follows:

\deqn{I_{APD}=\frac{1}{k}\sum\limits_{j=1}^k \frac{R_j}{\sqrt{|\mathbf{F_j}|}}}{I_{APD}=\frac{1}{k}\sum\limits_{j=1}^k \frac{R_j}{\sqrt{|\mathbf{F_j}|}}}

Where:
\eqn{\mathbf{F_j}}{\mathbf{F_j}} is the fuzzy covariance matrix of j-\emph{th} cluster:

\deqn{\mathbf{F_j} = \frac{\sum\limits_{i=1}^n u_{ij} \; (\vec{x_i}-\vec{v_j})^T(\vec{x_i}-\vec{v_j})}{\sum\limits_{i=1}^n u_{ij}}}{\mathbf{F_j} = \frac{\sum\limits_{i=1}^n u_{ij} (\vec{x_i}-\vec{v_j})^T(\vec{x_i}-\vec{v_j})}{\sum\limits_{i=1}^n u_{ij}}}

\eqn{R_j}{R_j} is the sum of central members:

\deqn{R_j=\sum\limits_j u_{ij}}{R_j=\sum\limits_j u_{ij}}

Since compact fuzzy clusters provide small values of \eqn{\sqrt{|\mathbf{F_j}|}}{\sqrt{|\mathbf{F_j}|}} and large values of \eqn{R_j}{R_j}, good fuzzy partitions are obtained by large values of \eqn{I_{APD}}{I_{APD}}.
}

\value{
 \item{apd}{APD value if \code{tidx} is \sQuote{f}}
 \item{apd.e}{extended APD value if \code{tidx} is \sQuote{e}}
 \item{apd.g}{generalized APD value if \code{tidx} is \sQuote{g}}
}

\references{
Gath, I. & Geva, A. B. (1989). Unsupervised optimal fuzzy clustering. \emph{IEEE Transactions on Pattern Analysis and Machine Intelligence}, 11(7):773-780. <doi:10.1109/34.192473>
}

\author{Zeynel Cebeci}

\seealso{
 \code{\link{allindexes}},
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

# Compute the APD using ppclust object
idx <- apd(res.fcm)
print(idx)
 
# Compute the APD using X,U and V matrices
idx <- apd(res.fcm$x, res.fcm$u, res.fcm$v, m=2)
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