# This might as well be a vignette tbh


# sample covariates for ~500 kids with high-risk gene fusions
coldf <- system.file("extdata", "mRNA", "dxsmall.colData.csv",
                     package="bifurcatoR", mustWork=TRUE)
colDat <- read.csv(coldf, row.names=1, stringsAsFactors=TRUE)


# feature covariates for mRNA expression of several genes
rowdf <- system.file("extdata", "mRNA", "dxsmall.rowData.csv",
                     package="bifurcatoR", mustWork=TRUE)
rowDat <- read.csv(rowdf, row.names=1)


# read counts per gene by sample, for the above genes
rcmat <- system.file("extdata", "mRNA", "dxsmall.readcounts.csv",
                     package="bifurcatoR", mustWork=TRUE)
readcounts <- data.matrix(read.csv(rcmat, row.names=1))


# log-normalized (by total read) counts per gene per sample, as above
lnmat <- system.file("extdata", "mRNA", "dxsmall.lognormcounts.csv",
                     package="bifurcatoR", mustWork=TRUE)
lognormcounts <- data.matrix(read.csv(lnmat, row.names=1))


# bolt them all together:
stopifnot(require("SummarizedExperiment"))
dxsmall <- SummarizedExperiment(rowData=rowDat, 
                                colData=colDat, 
                                assays=list(counts=readcounts, 
                                            logcounts=lognormcounts))

# what do we have now?
show(dxsmall)
#
# class: SummarizedExperiment 
# dim: 12 501 
# metadata(0):
# assays(2): counts logcounts
# rownames(12): MECOM PRDM16 ... PRAME C3orf80
# rowData names(4): gene_id medianLogCounts rangeLogCounts madLogCounts
# colnames(501): PAEAKL PAECCE ... PAYFYN PAYIET
# colData names(6): AgeGroup Sex ... fusion FusionGroup
#
# See https://doi.org/10.1038/nmeth.3252 for more on the above data structure. 


# What high-risk fusions are present?
sort(table(dxsmall$FusionGroup))
#
# KDM5A  NSD1   MLL
#    31   103   367


# drop KDM5A fusions and keep only a few genes for the time being
genesToKeep <- c("MECOM", "PRDM16", "CD33", "CD34", "NCAM1", "KDM5D")
dxsmall <- dxsmall[genesToKeep, ]

# convert to a SingleCellExperiment for iSEE 
stopifnot(require("SingleCellExperiment"))
dxsmall <- as(dxsmall, "SingleCellExperiment") 
mainExpName(dxsmall) <- "mRNA"

# add reduced dimensions
stopifnot(require("uwot")) 

# don't include KDM5D in the UMAP, though
g <- c("MECOM", "PRDM16", "CD33", "CD34", "NCAM1") 
reducedDim(dxsmall, "UMAP") <- umap(t(logcounts(dxsmall)[g, ]), metric="cosine")

# drop KDM5A fusions for now
samplesToKeep <- which(dxsmall$fusion != "NUP98-KDM5A")
dxsmall <- dxsmall[, samplesToKeep]
dxsmall$FusionGroup <- factor(dxsmall$FusionGroup)

sort(table(dxsmall$FusionGroup))
#
# NSD1   MLL
#  103   367

# test that it works with iSEE 
iSEEapp(dxsmall)

# resave?
if (FALSE) { 
  save(dxsmall, file="../../../data/dxsmall.rda", compress="xz")
}
