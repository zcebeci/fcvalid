\name{si}
\alias{si}
\title{Silhouette Index}
\usage{
si(x, u, v, m, t = NULL, eta, av = 1, tidx = "f")
}
\arguments{
  \item{x}{an object of class \sQuote{ppclust} containing the clustering results from a fuzzy clustering algorithm in the package \pkg{ppclust}. Alternatively, a numeric data frame or matrix containing the data set.}
  \item{u}{a numeric data frame or matrix containing the fuzzy membership values. It should be specified if \code{x} is not an object of \sQuote{ppclust}.}
  \item{v}{a numeric data frame or matrix containing the cluster prototypes. It should be specified if \code{x} is not an object of \sQuote{ppclust}.}
  \item{t}{a numeric data frame or matrix containing the cluster prototypes. It should be specified if \code{x} is not an object of \sQuote{ppclust} and the option \option{e} or \option{g} is assigned to \code{tidx}.}
  \item{m}{a number specifying the fuzzy exponent. It should be specified if \code{x} is not an object of \sQuote{ppclust}.}
  \item{eta}{a number specifying the typicality exponent. It should be specified if \code{x} is not an object of \sQuote{ppclust} and \code{tidx} is either \option{e} or \option{g}.}
  \item{av}{a number specifying the exponent \eqn{\alpha}{\alpha} which is a user-defined value. The default is 1.}
  \item{tidx}{a character specifying the type of index. The default is \sQuote{f} for fuzzy index. The other options are \sQuote{e} for extended and \sQuote{g} for generalized index.}
}
\description{
Computes the hard and fuzzy Silhouette Index (Rousseeuw, 1987; Campello & Hruschka, 2006) in order to validate the result of a cluster analysis.
}
\details{
The Silhouette Index (SI) values are the estimates of average silhouette widths. Silhouette width for each object is calculated as follows:

\deqn{s_i = (b_i-a_i)/max(b_i, a_i)}{s_i = (b_i-a_i)/max(b_i, a_i)}

\eqn{a_i}{a_i} is the average distance between the object \eqn{i}{i} and the other objects of the cluster of the object \eqn{i}{i}. \eqn{d(i, C_j)}{d(i, C_j)} is the average distance of the object \eqn{i}{i} to the objects locate in other clusters and \eqn{b_i}{b_i} is the smallest of all of these distances. 

Silhouette width values lie between -1 and 1. The well clustered objects which are closer to the center of the clusters have the higher \eqn{s_i}{s_i} values. Contrarily, the objects with smaller \eqn{s_i}{s_i} locate between the clusters. Negative \eqn{s_i}{s_i} means that the object locates in the wrong cluster.

The average of the silhouette widths of any cluster is called the average cluster silhouette width and obtained as follows: 
 
\deqn{\bar{s_j} = \frac{1}{n_j} \sum\limits_{i=1}^{n_j} s_i}{\bar{s_j} = \frac{1}{n_j} \sum\limits_{i=1}^{n_j} s_i}

After calculation of average silhouette widths of the clusters, the total average of these is calculated as follows and used as the Silhouette index.

\deqn{I_{SI} = \frac{1}{k} \sum\limits_{j=1}^k \bar{s_j}}{I_{SI} = \frac{1}{k} \sum\limits_{j=1}^k \bar{s_j}}

For fuzzy version version of the silhouette index is calculated as follows:

\deqn{I_{SI} = \frac{\sum\limits_{i=1}^n (u_{ij}-u_{lj})^\alpha \; s_i}{\sum\limits_{i=1}^n (u_{ij}-u_{lj})^\alpha}}{I_{SI} = \frac{\sum\limits_{i=1}^n (u_{ij}-u_{lj})^\alpha \; s_i}{\sum\limits_{i=1}^n (u_{ij}-u_{lj})^\alpha}}

where \eqn{s_i}{s_i} is the silhouette of object \eqn{i}{i}, \eqn{u_{ij}}{u_{ij}} and \eqn{u_{lj}}{u_{lj}} are the first and second largest elements of the \eqn{j}{j}-th column of the fuzzy membership matrix, and \eqn{\alpha \geq 0}{\alpha \geq 0} is a weighting exponent. When it approaches zero, the fuzzy measure of \eqn{I_{SI}}{I_{SI}} approaches to the hard measures of it (Campello & Hruschka, 2006). For extended and generalized values of the index, the function \code{si} is a modified and combined version the SIL and SIL.F of the package \sQuote{fclust} (Ferraro & Giordani, 2015).
}

\value{
 \item{si.obj}{silhouette widths of the objects}
 \item{sih}{hard SI value}
 \item{sif}{fuzzy SI value}
}
\references{
Rousseeuw, P. (1987). Silhouettes: A graphical aid to the interpretation and validation of
cluster analysis. \emph{J Computational and Applied Mathematics}, 20, 53:65. <doi:10.1016/0377-0427(87)90125-7>

Campello R.J.G.B. & Hruschka E.R. (2006). A fuzzy extension of the silhouette width criterion for cluster analysis. \emph{Fuzzy Sets and Systems}, 157 (21):2858-2875. <doi:10.1016/j.fss.2006.07.006>

Ferraro, M.B. & Giordani, P. (2015) A toolbox for fuzzy clustering using the R programming language, \emph{Fuzzy Sets and Systems}, 279:1-16.  <doi:10.1016/j.fss.2015.05.001>
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
 \code{\link{tss}},
 \code{\link{ws}},
 \code{\link{xb}}
}

\examples{
# Load the dataset iris
data(iris)
x <- iris[,1:4]

# Run FCM algorithm in the package ppclust 
res.fcm <- ppclust::fcm(x, centers=3)

# Compute the SI using res.fcm, which is a ppclust object
idx <- si(res.fcm)
print(idx)
 
# Compute the SI using X, U and V matrices
idx <- si(res.fcm$x, res.fcm$u, res.fcm$v)
print(idx)
}

\concept{internal validity indexes}
\concept{fuzzy validity indexes}
\concept{validation of clustering}
\concept{cluster validity}
\concept{cluster analysis}

\keyword{cluster}
\keyword{multivariate}