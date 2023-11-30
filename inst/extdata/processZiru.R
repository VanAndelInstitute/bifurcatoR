library(readxl) 

ZiruPath <- system.file("extdata", "Ziru", package="bifurcatoR", mustWork=TRUE)
list.files(ZiruPath)
# [1] "ZiruLab_B6DietChallengeData_Normalized_041123_fixed.xlsx"

ZiruXL <- file.path(ZiruPath, 
                    "ZiruLab_B6DietChallengeData_Normalized_041123_fixed.xlsx") 
Ziru <- read_excel(ZiruXL, na = "", sheet = "Data", range = "C5:L53")
colnames(Ziru) <- seq_len(ncol(Ziru))
save(Ziru, file="../../data/Ziru.rda", compress="xz")
