\name{ws}
\alias{ws}
\title{Weighted Summation Index}
\usage{
ws(x, u, v, m, t=NULL, eta, tidx="f", w)
}
\arguments{
  \item{x}{an object of class \sQuote{ppclust} containing the clustering results from a fuzzy clustering algorithm in the package \pkg{ppclust}. Alternatively, a numeric data frame or matrix containing the data set.}
  \item{u}{a numeric data frame or matrix containing the fuzzy membership values. It should be specified if \code{x} is not an object of \sQuote{ppclust}.}
  \item{v}{a numeric data frame or matrix containing the cluster prototypes. It should be specified if \code{x} is not an object of \sQuote{ppclust}.}
  \item{t}{a numeric data frame or matrix containing the cluster prototypes. It should be specified if \code{x} is not an object of \sQuote{ppclust} and the option \option{e} or \option{g} is assigned to \code{tidx}.}
  \item{m}{a number specifying the fuzzy exponent. It should be specified if \code{x} is not an object of \sQuote{ppclust}.}
  \item{eta}{a number specifying the typicality exponent. It should be specified if \code{x} is not an object of \sQuote{ppclust} and \code{tidx} is either \option{e} or \option{g}.}
  \item{tidx}{a character specifying the type of index. The default is \sQuote{f} for fuzzy index. The other options are \sQuote{e} for extended and \sQuote{g} for generalized index.}
  \item{w}{a numeric vector specifying the weights of individiual indexes included in summation. As default the value of \code{w} is \code{1/k}. Zero is assigned to the position of the index in \code{w} is not desired to include into WS. The order of the indexes in the \code{w} is \eqn{1:pc, 2:mpc, 3:pbm, 4:kpbm, 5:cl, 6:si, 7:pe, 8:xb, 9:fs, 10:ts, 11:kwon, 12:fhv, 13:cwb, 14:apd}{1:pc, 2:mpc, 3:pbm, 4:kpbm, 5:cl, 6:si, 7:pe, 8:xb, 9:fs, 10:ts, 11:kwon, 12:fhv, 13:cwb, 14:apd}. The sum of the weights in the \code{w} must be 1. For example, in case of \eqn{w=c(0.025, 0.025, 0.025, 0.1, 0.1, 0.025, 0.1, 0.2, 0.2, 0.0, 0.0, 0.1, 0.05, 0.05)}{w=c(0.025, 0.025, 0.025, 0.1, 0.1, 0.025, 0.1, 0.2, 0.2, 0.0, 0.0, 0.1, 0.05, 0.05)} the weights of \code{fhvs} and \code{cwb} are 0 that mean these indexes are not included while \code{xb} and \code{fs} more important than the others.}
}
\description{
Computes the Weighted Summation (Zhou et al, 2014) index in order to validate the result of a fuzzy and/or possibilistic cluster analysis.
}
\details{
In order to take advantage of each fuzzy cluster validity indexes and weaken their limitations, (Zhou et al, 2014) proposed a weighted summation type of indexes, which is the weighted sum of the fuzzy indexes. They argued that the proposed Weighted Summation (WS) is more effective in finding the optimal cluster numbers than single index when the weights of each index included in the calculation of WS. The proposed index has been defined as follows:

\deqn{I_{WS}=\sum\limits_{i=1}^N \omega_i I_i}{I_{WS}=\sum\limits_{i=1}^N \omega_i I_i}
Where:
\eqn{\omega_i}{\omega_i} is the weight representing the relative importance of the i\emph{th} index, and satisfies:

\eqn{0 \leq \omega_i \leq 1\; , \sum\limits_{i=1}^N \omega_i = 1}{0 \leq \omega_i \leq 1\; , \sum\limits_{i=1}^N \omega_i = 1}

Where, \eqn{I_i}{I_i} is one of the indexes of the package \sQuote{\pkg{fcvalid}} for fuzzy and/or possibilistic clustering. \eqn{N} stands for the number indexes used in summation formula of WS. Among the indexes, PC, MPC and PBM are maximum type indexes, i.e., the optimal cluster number \eqn{k^*}{k^*} is obtained at the maximum value of these indexes, while other indexes are minimum type. 
In order to obtain \eqn{k^*}{k^*} when WS is minimum, so the three maximum type indexes are converted into their corresponding reciprocal type indexes, which are presented as \eqn{PC_r = 1/PC}{PC_r = 1/PC}, \eqn{MPC_r = 1/MPC}{MPC_r = 1/MPC}, \eqn{PBM_r = 1/PBM}{PBM_r = 1/PBM}. The values of different indexes change in different range. To overcome the dominate influence of indexes in large values, all the indexes are normalized so that all of their values range in [0, 1]. The corresponding cluster number is optimal cluster number \eqn{k^*}{k^*} when the values of the normalized indexes equal to 0, and the minimum value of \eqn{I_{WS}}{I_{WS}} is achieved (Zhou et al, 2014).
}

\value{
 \item{ws}{WS index value if \code{tidx} is \sQuote{f}}
 \item{ws.e}{extended WS index value if \code{tidx} is \sQuote{e}}
 \item{ws.g}{generalized WS index value if \code{tidx} is \sQuote{g}}
}
\references{
Zhou, K.L., Ding, S., Fu, C. & Yang, S.L. (2014). Comparison and weighted summation type of fuzzy cluster validity indices. \emph{Int. J. Comput. Commun.}, 9(3):370-378. <\url{http://univagora.ro/jour/index.php/ijccc/article/viewFile/237/pdf_126}>
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
 \code{\link{tss}},
 \code{\link{xb}}
}

\examples{
# Load the dataset iris and use the first four feature columns 
data(iris)
x <- iris[,1:4]

# Run FCM algorithm in the package ppclust 
res.fcm <- ppclust::fcm(x, centers=3)

# Compute the WS index using res.fcm, which is a ppclust object
idx <- ws(res.fcm)
print(idx)
 
# Run UPFC algorithm in the package ppclust 
res.upfc <- ppclust::upfc(x, centers=3)
# Compute the generalized WS index using res.upfc, which is a ppclust object
idx <- ws(res.upfc, tidx="g")
print(idx)
}

\concept{internal validity indexes}
\concept{fuzzy validity indexes}
\concept{validation of clustering}
\concept{cluster validity}
\concept{cluster analysis}

\keyword{cluster}
\keyword{multivariate}