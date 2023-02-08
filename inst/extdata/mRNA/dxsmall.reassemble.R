colDat <- read.csv("dxsmall.colData.csv", row=1)
rowDat <- read.csv("dxsmall.rowData.csv", row=1)
readcounts <- read.csv("dxsmall.readcounts.csv", row=1)
lognormcounts <- read.csv("dxsmall.lognormcounts.csv", row=1)

colDat <- colDat[colDat$FusionGroup != 'KDM5A',]
rowDat <- rowDat[c('MECOM','PRDM16','CD34','KDM5D'),]
readcounts <- readcounts[c('MECOM','PRDM16','CD34','KDM5D'), colDat$USI]
lognormcounts <-lognormcounts[c('MECOM','PRDM16','CD34','KDM5D'), colDat$USI]

library(SummarizedExperiment) 
dxsmall <- SummarizedExperiment(rowData=rowDat, 
                                colData=colDat, 
                                assays=list(counts=readcounts, 
                                            logcounts=lognormcounts))
save(dxsmall, file="../../../data/dxsmall.rda", compress="xz")