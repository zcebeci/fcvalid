\name{pe}
\alias{pe}
\title{Partition Entropy}
\usage{
pe(u, m, t=NULL, eta, tidx="f")
}
\arguments{
  \item{u}{an object of class \sQuote{ppclust} containing the clustering results from a fuzzy clustering algorithm in the package \pkg{ppclust}. Alternatively, a numeric data frame or matrix containing the membership matrix.}
  \item{t}{a numeric data frame or matrix containing the cluster prototypes. It should be specified if \code{x} is not an object of \sQuote{ppclust} and the option \option{e} or \option{g} is assigned to \code{tidx}.}
  \item{m}{a number specifying the fuzzy exponent. It should be specified if \code{x} is not an object of \sQuote{ppclust}.}
  \item{eta}{a number specifying the typicality exponent. It should be specified if \code{x} is not an object of \sQuote{ppclust} and \code{tidx} is either \option{e} or \option{g}.}
  \item{tidx}{a character specifying the type of index. The default is \sQuote{f} for fuzzy index. The other options are \sQuote{e} for extended and \sQuote{g} for generalized index.}
}
\description{
Computes the Partition Entropy (Bezdek, 1974) in order to validate the result of a fuzzy and/or possibilistic clustering analysis.
}
\details{
Partition Entropy (PE) (Classification Entropy in some of the literature) (Bezdek, 1974) is one of the earliest fuzzy validity indexes. The formula of PE is very simple because it only requires the membership degrees matrix associated with fuzzy c-partitions of a data set as follows:

\deqn{I_{PE}=-\frac{1}{n} \sum\limits_{i=1}^n \sum\limits_{j=1}^k u_{ij} \; \log_\alpha(u_{ij})}{I_{PE}=-\frac{1}{n} \sum\limits_{i=1}^n \sum\limits_{j=1}^k u_{ij} \; \log_\alpha (u_{ij})}, 
where logarithmic base \eqn{\alpha \in (1, \infty)}{\alpha \in (1, \infty)} and \eqn{u_{ij} \log_\alpha u_{ij} /n = 0}{u_{ij} \log_\alpha u_{ij} /n = 0}.

The value of \eqn{I_{PE}}{I_{PE}} varies between \eqn{0}{0} and \eqn{\log_\alpha k}{\log_\alpha k} (\eqn{0 \leq I_{PE} \leq \log_\alpha k}{0 \leq I_{PE} \leq \log_\alpha k}). While it is 0 the clustering becomes entirely crisp, if it is \eqn{\log_\alpha k}{\log_\alpha k} the clustering reaches to the maximum fuzziness (Halkidi et al, 2002b). This means that there is no clustering structure in the dataset or the algorithm is unsuccessful to reveal the clustering. The optimal clustering is found at the minimum value of \eqn{I_{PE}}{I_{PE}}.
}

\value{
 \item{pe}{PE index value if \code{tidx} is \sQuote{f}}
 \item{pe.e}{extended PE index value if \code{tidx} is \sQuote{e}}
 \item{pe.g}{generalized PE index value if \code{tidx} is \sQuote{g}}
}
\references{
Bezdek, J.C. (1974). Cluster validity with fuzzy sets. \emph{J Cybernetics},3(3):58-72. <doi:10.1080/01969727308546047>

Halkidi, M., Batistakis, Y. & Vazirgiannis, M. (2002b). Clustering validity checking methods: part II. \emph{ACM Sigmod Record}, 31(3):19-27. <doi:10.1145/601858.601862>
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

# Compute the PE index using res.fcm, which is a ppclust object
idx <- pe(res.fcm)
print(idx)
 
# Compute the PE index U matrix
idx <- pe(res.fcm$u)
print(idx)

# Run UPFC algorithm in the package ppclust 
res.upfc <- ppclust::upfc(x, centers=3)
# Compute the generalized PE index using res.upfc, which is a ppclust object
idx <- pe(res.upfc, tidx="g")
print(idx)
}

\concept{internal validity indexes}
\concept{fuzzy validity indexes}
\concept{validation of clustering}
\concept{cluster validity}
\concept{cluster analysis}

\keyword{cluster}
\keyword{multivariate}