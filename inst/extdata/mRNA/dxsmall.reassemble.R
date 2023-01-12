colDat <- read.csv("dxsmall.colData.csv", row=1)
rowDat <- read.csv("dxsmall.rowData.csv", row=1)
readcounts <- read.csv("dxsmall.readcounts.csv", row=1)
lognormcounts <- read.csv("dxsmall.lognormcounts.csv", row=1)

library(SummarizedExperiment) 
dxsmall <- SummarizedExperiment(rowData=rowDat, 
                                colData=colDat, 
                                assays=list(counts=readcounts, 
                                            logcounts=lognormcounts))
saveRDS(dxsmall, file="dxsmall.rds")

