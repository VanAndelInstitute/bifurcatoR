# TARGET pAML data extract 

This folder contains an extract of a dozen genes tabulated via featureCounts
against an ENSEMBL transcriptome and then collapsed by symbol. The logcounts 
assay is computed against the total counts of all the symbols before subsetting.

# Files and descriptions

README.md                   this file

dxsmall.colData.csv         the column data (subject covariates) for dxsmall
dxsmall.rowData.csv         the row data (gene symbol covariates) for dxsmall 
dxsmall.readcounts.csv      read count for each gene in each subject specimen
dxsmall.lognormcounts.csv   log2(1 + (read count for gene / total read count)) 

dxsmall.rds                 a 12x583 SummarizedExperiment object from R
dxsmall.reassemble.R        reassemble the dxsmall object from above CSV files 
dxsmall.plots.R             template for plotting distributions

# Sanity checking 

If one runs dxsmall.reassemble.R and also reads in dxsmall.rds with 

```dxsmall0 <- readRDS("dxsmall.rds")```

then 

```identical(dxsmall, dxsmall0)```

should return TRUE. 


# Columns (subjects)

This data is, more or less, a collection of chromatin modifier fusion leukemia.
The subjects in the columns of the assays are pediatric acute myeloid leukemia
patients with an MLL (KMT2A) fusion or NUP98 fusion. These two genes are the
most promiscuous among fusion gene partners, and also some of the most lethal.
Within defined subgroups (KMT2A-X, NUP98-NSD1, NUP98-KDM5A) of fusions, there
remains substantial immunological, treatment response, and salvage heterogeneity
that is not explained by genetics, cytogenetics, age group, sex, race, or FAB 
(morphological disease subtype). This heterogeneity is prognostic and predicts 
whether a child is likely to survive their disease with existing therapies.

In our experience, it makes sense to treat the two NUP98 fusion classes as 
separate types of disease, and to treat the majority of KMT2A fusions as if 
they are one type of disease. The reason for this is relatively simple: KMT2A
fusions have a coherent underlying transcriptional signature (HOXA9, MEIS1, ...)
which is sufficient to recapitulate disease in the absence of the fusion. NUP98
fusions do not have such a signature and the mechanism by which each generates 
leukemia is not yet clear. Evidence suggests that aberrations on chromosome 13 
may explain some of the bimodality seen in KDM5A fusion leukemia, with stem-like
abnormal-13q cases having substantially better responses to therapy. The extent
of this relationship, and its biological mechanism, is an open research topic.

To a first approximation, then, we have two groups (MLL and KDM5A fusions) with
substantial unexplained bimodality in expression, and one (NUP98-NSD1) without. 

# Rows (genes)

The genes chosen for this dataset include five with bimodal correlated mRNA 
expression, one housekeeping gene (GAPDH), two rarely expressed genes (CFTR
and NCAM1/CD56), one highly expressed gene (IL3RA/CD123), and three (KDM5D,
PRAME, and C3orf80) with uninformative variation across the subjects in this
dataset. MECOM and PRDM16 are particularly noteworthy for their bimodality. 

See dxsmall.plots.R for some skeletal tire-kicking and plot-making code. 
