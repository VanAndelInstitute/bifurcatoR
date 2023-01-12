
dxsmall <- readRDS("dxsmall.rds")
genescovs <- cbind(as.data.frame(colData(dxsmall)),
                   t(assays(dxsmall)$logcounts))
is.na(genescovs$FAB) <- genescovs$FAB == "pending" 

library(reshape2)
melted <- melt(genescovs)

library(ggbeeswarm) 
p <- ggplot()
