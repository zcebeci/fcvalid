# fcvalid: An R Package for Validation of Results from Fuzzy and Probabilistic Clustering ALgorithms

`fcvalid` is an R package that contains the popular internal validity indexes which are used to assess the clustering results from any fuzzy and possibilistic clustering algorithm with special reference to 'ppclust' package in CRAN.

## Install the package 'fcvalid'
In order to install the package `fcvalid` from the GitHub repository you should first install the `devtools` package from CRAN into your local system. Then you can install the package  `fcvalid` using `install_github` of `devtools` package as shown with the R code chunks below:

```{r}
if(!require(devtools) {install.packages('devtools'); library(devtools)}
install_github("zcebeci/fcvalid")
```

## Load the package into R working space
After you installed the package 'fcvalid' run the command below in the R working space.

```{r}
library(fcvalid)
```

## Help about the package
To get help about the use of package fcvalid run help in R as follows:

```{r}
help(package="fcvalid")
```
