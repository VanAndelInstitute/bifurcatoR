#' MLL
#'
#' Data from MLL-rearranged pediatric AML patients in COG clinical trials.
#' AgeGroup, Sex, FAB, BlastPercent, fusion, OS, OSI, and Protocol are clinical.
#' MECOM, PRDM16, CD33, CD34, NCAM1, and KDM5D are log-normalized mRNA CPMs 
#' (counts per million reads) for six genes, some of which are informative about
#' clinical outcomes (see vignette for more details). 
#'
#' @importFrom utils data
#' 
#' @usage data(MLL)
#' @format A \code{data.frame}
#'
#' @examples
#'
#'  data(MLL)
#'  library(mixR) 
#'  MLLfit <- mixfit(MLL$MECOM, ncomp=2)
#'  plot(MLLfit, xlab="log(MECOM transcripts)")
#' 
"MLL"


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


#' Ziru
#'
#' A tibble with weight measurements from C57BL/6 male mice over 10 weeks.
#'
#' @importFrom utils data
#' 
#' @usage data(Ziru)
#' @format A \code{tibble}
#' @source This data was created using inst/extdata/processZiru.R
#'
#' @examples
#'
#'  data(Ziru)
#'  show(Ziru)
#' 
#'  # if `ggplot2` is installed:
#'  if (require("ggplot2") & require("reshape2")) {
#'
#'    Ziru$mouse <- seq_len(nrow(Ziru))
#'    melted <- melt(Ziru, 
#'                   id.vars="mouse",
#'                   variable.name="week",
#'                   value.name="weight")
#'    melted$week <- as.ordered(melted$week)
#'
#'    ggplot(melted, aes(group=mouse, x=week, y=weight, color=mouse)) + 
#'      geom_point() + 
#'      geom_line() + 
#'      theme_minimal()
#'
#'  }
#'
#' 
"Ziru"
