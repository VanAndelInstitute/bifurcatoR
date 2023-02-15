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
#'  data(dxsmall)
#'  show(dxsmall)
#' 
#'  # if `iSEE` is installed:
#'  if (require("iSEE")) {
#'    rownames(dxsmall)
#'    names(colData(dxsmall))
#'    table(dxsmall$FusionGroup)
#'    table(dxsmall$AgeGroup)
#'    iSEEapp(dxsmall)
#'  }
#'
#'  # if `mclust` is installed:
#'  if (require("mclust")) {
#'
#'    fit1 <- Mclust(logcounts(dxsmall)["MECOM", dxsmall$FusionGroup == "MLL"])
#'    plot(fit1, what="density", xlab="log(MECOM read counts + 1) in KMT2Ar")
#' 
#'    fit2 <- Mclust(t(logcounts(dxsmall)[ c("MECOM", "PRDM16"), ]), G=1:3)
#'    plot(fit2, what="classification")
#'    plot(fit2, what="uncertainty")
#'
#'  }
#' 
#' 
"dxsmall"
