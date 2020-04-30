# fcvalid
## *An R Package for Validation of Results from Fuzzy and Possibilistic Clustering Algorithms*

`fcvalid` is an R package that contains the popular internal validity indexes which are used to assess the clustering results from any fuzzy and possibilistic clustering algorithm with special reference to [`ppclust`](https://cran.r-project.org/web/packages/ppclust/index.html) package in CRAN.

## Introduction
In data mining and knowledge discovery, partitioning cluster analysis is an important unsupervised explatory task for finding the meaningful patterns in numeric data sets. In cluster analysis, the quality of clustering or the performances of clustering algorithms are mostly evaluated by using several internal validity indexes (Halkidi et al, 2001, 2002a, 2002b; Zhou et al, 2014; Li et al, 2016). This package contains a compilation of the widely used internal indexes which have been proposed to validate the results of fuzzy clustering analyses. In addition to fuzzy index values, the options to compute the generalized and extended versions of the fuzzy internal indexes are also included in the package.

In fuzzy clustering analyses, the performances of clustering algorithms are mostly compared using several internal fuzzy validity indexes. The majority of the well-known fuzzy indices have originally been proposed for working with membership degrees produced by the basic Fuzzy C-Means Clustering (FCM) algorithm and its modifications (Rezaee et al, 1998; Halkidi et al, 2001, 2002a, 2002b). Therefore, the fuzzy internal indexes cannot be directly applied to validate the results from possibilistic algorithms which produce typicality matrices instead of fuzzy membership matrices. Morevover, various variants of FCM and PCM such as Possibilistic Fuzzy C-means (PFCM), Fuzzy Possibilistic C-means (FPCM) and Unsupervised Possibilistic Fuzzy Clustering (UPFC) simultaneously result with probabilistic and possibilistic membership degrees. Thus, some kind of validity indices are needed for working with both of these results. For this purpose, the extended and generalized validity indices have been proposed in recent years. In this package, the implementations of these indices were included for validating the clustering results from FCM, PCM, FPCM, PFCM, UPFC and the other fuzzy/possibilistic clustering algorithms.

The extended index values is based on the combined use of the fuzzy membership values and possibilistic membership degrees (typicalities). For evaluation of the results from fuzzy and possibilistic algorithms, this option may be helpful to interprete the clustering results. In this package, the extended index value with any algorithm is calculated the summation of fuzzy and possibilistic membership degrees and is labeled as with the .e postfix in the validation results. 

## Install the package 'fcvalid'
In order to install the package `fcvalid` from the GitHub repository you should first install the `devtools` package from CRAN into your local system. Then you can install the package  `fcvalid` using `install_github` of `devtools` package as shown with the R code chunks below:

```{r}
if(!require(devtools)) {install.packages('devtools'); library(devtools)}
install_github("zcebeci/fcvalid")
```
If you would like to have a compiled version of the vignettes of the package try to install the package `fcvalid` using `install_github` with `build_vignettes` argument set to TRUE as shown below:

```{r}
if(!require(devtools)) {install.packages('devtools'); library(devtools)}
 devtools::install_github("zcebeci/fcvalid", build_vignettes=TRUE)
```
If you have not already installed `rmarkdown` and `prettydoc` in your local system, before running the above install commands firstly install these packages as following:

 ```{r}
install.packages('prettydoc')
```

## Load the package into R working space
After you installed the package `fcvalid`, run the following command in order to load it to R working space.

```{r}
library(fcvalid)
```

## Help for the package
To get help about the use of functions included in the package `fcvalid`, run `help` in R as seen in the following code chunk.

```{r}
help(package="fcvalid")
```
For reaching the detailed vignette about the usage of package with examples, run the following command and then click *HTML* link on the accessed page in your web browser. Read the vignette and apply the examples.

```{r}
browseVignettes("fcvalid")
```

## Cite the package
To cite the package please use one of the following items fits to your references list:

Cebeci, Z. (2020). fcvalid: An R Package for Internal Validation of Probabilistic and Possibilistic Clustering. Sakarya University Journal of Computer and Information Sciences, 3 (1), 11-27. DOI: 10.35377/saucis.03.01.664560 

or in BibTeX format:
```
@article {saucis664560, 
journal = {Sakarya University Journal of Computer and Information Sciences}, 
eissn = {2636-8129}, 
publisher = {Sakarya University}, 
year = {2020}, 
volume = {3}, 
number = 1,
pages = {11 - 27}, 
title = {fcvalid: An R Package for Internal Validation of Probabilistic and Possibilistic Clustering}, 
author = {Cebeci, Zeynel},
doi = {10.35377/saucis.03.01.664560}, 
key = {cite}, 
}
 ```
 
