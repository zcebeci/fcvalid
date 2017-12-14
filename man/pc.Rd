\name{pc}
\alias{pc}
\title{Partition Coefficient}
\usage{
pc(u, m, t=NULL, eta, tidx="f")
}
\arguments{
  \item{u}{an object of class \sQuote{ppclust} containing the clustering results from a fuzzy clustering algorithm in the package \pkg{ppclust}. Alternatively, a numeric data frame or matrix containing the fuzzy membership degrees.}
  \item{t}{a numeric data frame or matrix containing the cluster prototypes. It should be specified if \code{x} is not an object of \sQuote{ppclust} and the option \option{e} or \option{g} is assigned to \code{tidx}.}
  \item{m}{a number specifying the fuzzy exponent. It should be specified if \code{x} is not an object of \sQuote{ppclust}.}
  \item{eta}{a number specifying the typicality exponent. It should be specified if \code{x} is not an object of \sQuote{ppclust} and \code{tidx} is either \option{e} or \option{g}.}
  \item{tidx}{a character specifying the type of index. The default is \sQuote{f} for fuzzy index. The other options are \sQuote{e} for extended and \sQuote{g} for generalized index.}
}
\description{
Computes the Partition Coefficient (Bezdek, 1974) in order to validate the result of a fuzzy and/or possibilistic clustering analysis.
}
\details{
Partition Coefficient (Bezdek, 1974) is one of the earliest proposed fuzzy validity indexes. It uses only the membership degrees matrix associated with fuzzy c-partitions of a data set. The formula of PC is:

\deqn{I_{PC}=\frac{1}{n} \sum\limits_{i=1}^n \sum\limits_{j=1}^k u_{ij}^m}{I_{PC}=\frac{1}{n} \sum\limits_{i=1}^n \sum\limits_{j=1}^k u_{ij}^m}

The value of \eqn{I_{PC}}{I_{PC}} varies between \eqn{1/k}{1/k} and \eqn{1}{1} (\eqn{1/k \leq I_{PC} \leq 1}{1/k \leq I_{PC} \leq 1}) (Halkidi et al, 2002b). While it is close to \eqn{1/k}{1/k} the clustering is complelety fuzzy, and in this case, each object belongs to all clusters with the same degree of membership. This means that there is no clustering tendency in the considered dataset or the used clustering algorithm failed to reveal the clustering. On the other hand, if it equals to \eqn{1}{1} the clustering becomes fully crisp. In this case, the membership values are either 0 or 1. The optimal clustering is found at the maximum value of \eqn{I_{PC}}{I_{PC}}.
}

\value{
 \item{pc}{PC index if \code{tidx} is \sQuote{f}}
 \item{pc.e}{extended PC index if \code{tidx} is \sQuote{e}}
 \item{pc.g}{generalized PC index if \code{tidx} is \sQuote{g}}
}
\references{
Bezdek, J.C. (1974). Cluster validity with fuzzy sets. \emph{J Cybernetics}, 3(3):58-72. <doi:10.1080/01969727308546047>

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
 \code{\link{fs}},
 \code{\link{kpbm}},
 \code{\link{kwon}},
 \code{\link{mcd}},
 \code{\link{mpc}},
 \code{\link{pbm}},
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

# Compute the PC index using res.fcm, which is a ppclust object
idx <- pc(res.fcm)
print(idx)
 
# Compute the PC index using U matrix
idx <- pc(res.fcm$u)
print(idx)

# Run UPFC algorithm in the package ppclust 
res.upfc <- ppclust::upfc(x, centers=3)
# Compute the generalized PC index using res.upfc, which is a ppclust object
idx <- pc(res.upfc, tidx="g")
print(idx)
}

\concept{internal validity indexes}
\concept{fuzzy validity indexes}
\concept{validation of clustering}
\concept{cluster validity}
\concept{cluster analysis}

\keyword{cluster}
\keyword{multivariate}