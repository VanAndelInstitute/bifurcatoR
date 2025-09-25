#' iSEEapp
#'
#' A simple wrapper function to launch an iSEE web application for dxsmall.
#' iSEE is a Bioconductor package that you must install to run this function.
#'
#' @param     sce   A SingleCellExperiment.
#'
#' @seealso   SingleCellExperiment
#' @seealso   dxsmall
#' @seealso   iSEE
#'
#' @examples 
#' if (require("iSEE")) {
#'   data(dxsmall)
#'   iSEEapp(dxsmall)
#' } 
#'
#' @importFrom methods new
#'
#' @export
iSEEapp <- function(sce) { 

  if (!require("iSEE")) {
    message("Please install necessary prerequisites before running iSEEapp:")
    message("install.packages('BiocManager')")
    message("BiocManager::install('iSEE')")
    stop("Cannot launch without `iSEE`.")
  } else { 
    iSEE(sce,
         initial=list(UMAP=new("ReducedDimensionPlot",
                               ColorByColumnData = "FusionGroup",
                               ColorBy = "Column data", 
                               Type = "UMAP"),
                      Gene=new("RowDataTable"),
                      Expression=new("FeatureAssayPlot", 
                                     Assay = "logcounts", 
                                     XAxis = "Column data", 
                                     XAxisColumnData = "FusionGroup", 
                                     YAxisFeatureSource = "Gene", 
                                     YAxisFeatureDynamicSource = TRUE, 
                                     ColorByColumnData = "FusionGroup", 
                                     ColorBy = "Column data") 
                      )
          )
  }

}
