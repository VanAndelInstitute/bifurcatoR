#' dxsmall
#'
#' A SummarizedExperiment with covariates and mRNA counts from 501 AML cases.
#'
#' @importFrom utils data
#' 
#' @usage data(dxsmall)
#' @format A \code{SummarizedExperiment} object
#' @source This data was created using inst/extdata/mRNA/dxsmall.reassemble.R
#'
#' @examples
#'
#'   data(dxsmall)
#'   if(requireNamespace("iSEE")) {
#'     library(iSEE)
#'     iSEE(dxsmall)
#'   }
#' 
"dxsmall"
