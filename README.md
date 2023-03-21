# bifurcatoR

[![DOI](https://zenodo.org/badge/493282641.svg)](https://doi.org/10.5281/zenodo.7757221)
[![Build Status](https://travis-ci.org/VanAndelInstitute/bifurcatoR.png?branch=master)](https://travis-ci.org/VanAndelInstitute/bifurcatoR)

A library of functions to simulate and test for both structured and unstructured variance inflation due to genetics or other heritable factors.

# For users 

## Installation and an example dataset

The pre-release version of the package can be pulled from GitHub using the [BiocManager](https://cran.r-project.org/package=BiocManager") package:

```
    # install.packages("remotes")
    # install.packages("BiocManager")
    BiocManager::install("VanAndelInstitute/bifurcatoR", dependencies=c("Depends", "Imports", "LinkingTo", "Suggests"))
```

An example dataset (with, if you've installed the packages listed in Suggests, an interactive [iSEE](https://bioconductor.org/packages/iSEE) browser): 

```
    library(bifurcatoR)
    help(dxsmall) # try firing up iSEE per the example!
```

More information can be found in [the package wiki](https://github.com/VanAndelInstitute/bifurcatoR/wiki/Testing-for-variance-effects-when-the-causal-genotype-is-unclear) and in the (evolving) package documentation (which needs more working examples like `dxsmall`):

```
help(package="bifurcatoR") # it's a work in progress, outside of ?dxsmall
```

# For developers

The repository includes a Makefile for common tasks. The Makefile will need to be deleted for `R CMD check --as-cran .` to succeed. 

## Updating documentation

`$ make doc`. Requires the [roxygen2](https://github.com/klutometis/roxygen) package.

## Running tests

`$ make test`. Requires the [testthat](https://github.com/hadley/testthat) package. You can also specify a specific test file or files to run by adding a "file=" argument, like `$ make test file=logging`. `test_package` will do a regular-expression pattern match within the file names. See its documentation in the `testthat` package.

## Travis

You can go to [Travis](https://travis-ci.org/profile/VanAndelInstitute) and turn on continuous integration for your new package. You may need to click the "Sync account" button to get your new package to show up in the list.
