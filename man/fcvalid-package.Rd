\name{fcvalid-package}
\alias{fcvalid-package}
\docType{package}
\title{Internal Validity Indexes for Fuzzy and Possibilistic Clustering}

\description{
In data mining and knowledge discovery, partitioning cluster analysis is an important unsupervised explatory task for finding the meaningful patterns in numeric data sets. In cluster analysis, the quality of clustering or the performances of clustering algorithms are mostly evaluated by using several internal validity indexes (Halkidi et al, 2001, 2002a, 2002b; Zhou et al, 2014; Li et al, 2016). This package contains a compilation of the widely used internal indexes which have been proposed to validate the results of fuzzy clustering analyses. In addition to fuzzy index values, the options to compute the generalized and extended versions of the fuzzy internal indexes are also included in the package.
}

\details{
In fuzzy clustering analyses, the performances of clustering algorithms are mostly compared using several internal fuzzy validity indexes. The majority of the well-known fuzzy indices have originally been proposed for working with membership degrees produced by the basic Fuzzy C-Means Clustering (FCM) algorithm and its modifications (Rezaee et al, 1998; Halkidi et al, 2001, 2002a, 2002b). Therefore, the fuzzy internal indexes cannot be directly applied to validate the results from possibilistic algorithms which  produce typicality matrices instead of fuzzy membership matrices. Morevover, various variants of FCM and PCM such as Possibilistic Fuzzy C-means (PFCM), Fuzzy Possibilistic C-means (FPCM) and Unsupervised Possibilistic Fuzzy Clustering (UPFC) simultaneously result with probabilistic and possibilistic membership degrees. Thus, some kind of validity indices are needed for working with both of these results. For this purpose, the extended and generalized validity indices have been proposed in recent years. In this package, the implementations of these indices were included for validating the clustering results from FCM, PCM, FPCM, PFCM, UPFC and the other fuzzy/possibilistic  clustering algorithms. 

The extended index values is based on the combined use of the fuzzy membership values and possibilistic membership degrees (typicalities). For evaluation of the results from fuzzy and possibilistic algorithms, this option may be helpful to interprete the clustering results. In this package, the extended index value with any algorithm is calculated the summation of fuzzy and possibilistic membership degrees and is labeled as with the \code{.e} postfix in the validation results.

In fact, the generalized versions of the fuzzy validity indices based on normalization of typicality degrees (Yang & Wu, 2006) as follows:

\deqn{u_{ij}^g = \frac{t_{ij}}{\sum\limits_{l=1}^k t_{il}}}{u_{ij}^g = \frac{t_{ij}}{\sum\limits_{l=1}^k t_{il}}}

Where \eqn{u_{ij}^g}{u_{ij}^g} is the generalized typicality value of \eqn{t_{ij}}{t_{ij}} that satisfies the constraints for the fuzzy membership degrees because it lies between \eqn{0}{0} and \eqn{1}{1}. Cebeci et al (2017) revealed that the normalized values of typicality degrees can be successfuly used with the internal validity indexes which have originally been developed for validation of the partitions obtained with FCM. Such use of the normalized values of typicality degrees is called as the generalized index values in this package. An index value based on the normalized typicality degrees is labeled as with the \code{.g} postfix in the validation results for each algorithm. For instance, \code{xb.g} indicates the generalized index value computed with Xie-Beni (\code{xb}) index.
}

\references{
Rezaee, M. R., Lelieveldt, B. P. & Reiber, J. H. (1998). A new cluster validity index for the fuzzy c-mean. \emph{Pattern Recognition Letters}, 19(3):237-246.<doi:10.1016/S0167-8655(97)00168-2>

Halkidi, M., Batistakis, Y. & Vazirgiannis, M. (2001). On clustering validation techniques. \emph{J  Intelligent Information Systems}, 17(2):107-145.<doi:10.1023/A:1012801612483>

Halkidi, M., Batistakis, Y., & Vazirgiannis, M. (2002a). Cluster validity methods: part I. \emph{ACM Sigmod Record}, 31(2):40-45. <doi:10.1145/565117.565124>

Halkidi, M., Batistakis, Y., & Vazirgiannis, M. (2002b). Clustering validity checking methods: part II. \emph{ACM Sigmod Record}, 31(3):19-27. <doi:10.1145/601858.601862>

Zhou, K.L., Ding, S., Fu, C. & Yang, S.L. (2014). Comparison and weighted summation type of fuzzy cluster validity indices. \emph{Int. J. Comput. Commun.}, 9(3):370-378. \url{http://univagora.ro/jour/index.php/ijccc/article/viewFile/237/pdf_126}

Yang, M. S. & Wu, K. L. (2006). Unsupervised possibilistic clustering. \emph{Pattern Recognition}, 39(1): 5-21. <doi:10.1016/j.patcog.2005.07.005>

Li, H., Zhang, S., Ding, X., Zhang, C., & Dale, P. (2016). Performance evaluation of cluster validity indices (CVIs) on multi/hyperspectral remote sensing datasets. \emph{Remote Sensing}, 8(4), 295. <doi:10.3390/rs8040295>

Cebeci, Z., Kavlak, A. T. & Yildiz, F. (2017). Validation of fuzzy and possibilistic clustering results. In \emph{International Artificial Intelligence and Data Processing Symposium (IDAP 2017)}, IEEE. (pp. 1-7). <doi:10.1109/IDAP.2017.8090183>
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
 \code{\link{ws}},
 \code{\link{xb}}
}

\concept{internal validity indexes}
\concept{fuzzy validity indexes}
\concept{possibilistic validity indexes}
\concept{validation of clustering}
\concept{cluster validation}
\concept{clustering validity}
\concept{cluster validity index}
\concept{cluster analysis}
\concept{pattern discovery}
\concept{unsupervised learning}

\keyword{cluster}
\keyword{multivariate}