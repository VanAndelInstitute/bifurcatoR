# bifurcatoR

[![Build Status](https://travis-ci.org/VanAndelInstitute/bifurcatoR.png?branch=master)](https://travis-ci.org/VanAndelInstitute/bifurcatoR) 

### Travis

You can go to [Travis](https://travis-ci.org/profile/VanAndelInstitute) and turn on continuous integration for your new package. You may need to click the "Sync account" button to get your new package to show up in the list.

The pre-release version of the package can be pulled from GitHub using the [BiocManager](https://cran.r-project.org/package=BiocManager") package:

```
    # install.packages("remotes")
    # install.packages("BiocManager")
    BiocManager::install("VanAndelInstitute/bifurcatoR", dependencies=TRUE)

    library(bifurcatoR)
    help(bifurcatoR::dxsmall) # try firing up iSEE per the example!
```

## For developers

The repository includes a Makefile to facilitate some common tasks.

### Updating documentation

`$ make doc`. Requires the [roxygen2](https://github.com/klutometis/roxygen) package.

### Running tests

`$ make test`. Requires the [testthat](https://github.com/hadley/testthat) package. You can also specify a specific test file or files to run by adding a "file=" argument, like `$ make test file=logging`. `test_package` will do a regular-expression pattern match within the file names. See its documentation in the `testthat` package.
