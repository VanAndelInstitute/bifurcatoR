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
#'  dxsmall$MLLrearranged <- dxsmall$FusionGroup == "MLL"
#'  if(requireNamespace("iSEE")) {
#' 
#'    library(SingleCellExperiment)
#'    se <- as(dxsmall, "SingleCellExperiment") 
#'
#'    library(uwot) # for the reduced dimension plot
#'    reducedDim(se, "UMAP") <- umap(t(logcounts(se)), metric="cosine")
#'
#'    library(iSEE)
#'    app <- iSEE(se,
#'                initial=list(
#'                  UMAP=new("ReducedDimensionPlot",
#'                           ColorByColumnData = "MLLrearranged",
#'                           ColorBy = "Column data", 
#'                           Type = "UMAP"),
#'                  Gene=new("RowDataTable"),
#'                  Expression=new("FeatureAssayPlot", 
#'                                 Assay = "logcounts", 
#'                                 XAxis = "Column data", 
#'                                 XAxisColumnData = "MLLrearranged", 
#'                                 YAxisFeatureSource = "Gene", 
#'                                 YAxisFeatureDynamicSource = TRUE, 
#'                                 ColorByColumnData = "MLLrearranged", 
#'                                 ColorBy = "Column data") 
#'                )
#'    )
#'
#'    # finally, run the damn thing:
#'    shiny::runApp(app, port=12345)
#'
#'  }
#' 
"dxsmall"
