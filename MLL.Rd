% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{MLL}
\alias{MLL}
\title{MLL}
\format{
A \code{data.frame}
}
\usage{
data(MLL)
}
\description{
Data from MLL-rearranged pediatric AML patients in COG clinical trials.
AgeGroup, Sex, FAB, BlastPercent, fusion, OS, OSI, and Protocol are clinical.
MECOM, PRDM16, CD33, CD34, NCAM1, and KDM5D are log-normalized mRNA CPMs
(counts per million reads) for six genes, some of which are informative about
clinical outcomes (see vignette for more details).
}
\examples{

 data(MLL)
 library(mixR) 
 MLLfit <- mixfit(MLL$MECOM, ncomp=2)
 plot(MLLfit, xlab="log(MECOM transcripts)")

}
\keyword{datasets}
