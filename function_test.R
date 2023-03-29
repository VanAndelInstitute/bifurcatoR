install.packages("remotes")
install.packages("BiocManager")
BiocManager::install("VanAndelInstitute/bifurcatoR", dependencies=c("Depends", "Imports", "LinkingTo", "Suggests"))

library(remotes)
library(BiocManager)
library(bifurcatoR)
library(pwr)
library(dplyr)
library(olsrr)

help(dxsmall) # try firing up iSEE per the example!




getwd()
setwd("/home/ma.ding/Nadeau/bifurcatorR_Mao's fork/bifurcatoR_Mao's fork/R")

runApp("/home/ma.ding/Nadeau/bifurcatorR_Mao's fork/bifurcatoR_Mao's fork/R")
install.packages("iSEE")
library(iSEE)

# Install Shiny app in R Studio:
devtools::install_github("carmkhoo/experimentalRepo")

#Run Shiny app in R Studio:
shinyBifurcatoR::run_app()

################################
######## calc_power ########
################################
calc_power(n = 20, p1 = 0.25, p2 = 0.5, sel= 'Bodyweight', shift = 15, CI = 0)
calc_power(n = 550, p1 = 0.2, p2 = 0.9, sel= 'Fat Trim', shift = 30, CI = 1)
calc_power(n = 11050, p1 = 0.9, p2 = 0.6, sel= 'Fat NNAT', shift = 120, CI = 1)

################################
######## est_pow ########
################################
params_1 = list (p = 0.5, mu1 = 0.2, sd1 = 0.5, mu2 = 3, sd2 = 0.4, s1 = 1.2, s2 = 2.3, sp1 = 1.2, sp2 = 1.7, sc1 = 2, sc2 = 0.5)
params_2 = list (p = 0.2, mu1 = 0.1, sd1 = 0.6, mu2 = 5.3, sd2 = 15, s1 = 0.3, s2 = 6.7, sp1 = 5.1, sp2 = 9, sc1 = 12, sc2 = 3)
params_3 = list (p = 0.5, mu1 = 0.1, sd1 = 0.2, mu2 = 3.6, sd2 = 1.8, s1 = 9.2, s2 = 2.6, sp1 = 2.7, sp2 = 1.5, sc1 = 0.2, sc2 = 0.7)

est_pow(n = 20, alpha = 0.05, nsim = 20, dist = "norm", params = params_1,  tests = "Hartigans' dip test")  # input multiple random parameters, returns NULL
est_pow(n = 20, alpha = 0.05, nsim = 20, dist = "beta", params = params_2,  tests = "mclust") 
est_pow(n = 20, alpha = 0.02, nsim = 20, dist = "weib", params = params_3,  tests = "Mouse Trap") # input multiple random parameters, returns NULL


#####################################
######## est_pow_2samp ########
####################################

params_4 = list (p_1 = 0.5, p_2 = 0.7,
                 mean = 5, v_scale = 0.7,
                 s1_1  = 12, s1_2 = 1.5,
                 s2_1 = 2, s2_2 = 0.8,
                 mu1_1 = 4, mu1_2 = 5,
                 mu2_1 = 1.6, mu2_2 = 7,
                 sd1_1 = 2.8, sd1_2 = 11,
                 sd2_1 = 3.6, sd2_2 = 20,
                 sp1_1 = 1.6, sp1_2 = 25,
                 sp2_1 = 2.3, sp2_2 = 5,
                 sc1_1 = 14, sc1_2 = 6,
                 sc2_1 = 25, sc2_2 = 7)

est_pow_2samp(n1 = 20, n2 = 30, alpha = 0.05, nsim = 20, modes = 1, dist = "norm", params = params_4, tests = "Kolmogorov-Smirnov", nperm = 30)   # returns NULL
est_pow_2samp(n1 = 10, n2 = 30, alpha = 0.05, nsim = 20, modes = 2, dist = "beta", params = params_4, tests = "Cramer-von Mises", nperm = 30)     # returns NULL
est_pow_2samp(n1 = 20, n2 = 30, alpha = 0.05, nsim = 20, modes = 1, dist = "weib", params = params_4, tests = "DTS", nperm = 30)                  # returns NULL
est_pow_2samp(n1 = 20, n2 = 30, alpha = 0.05, nsim = 20, modes = 1, dist = "norm", params = params_4, tests = "Anderson-Darling", nperm = 30)     # returns NULL
est_pow_2samp(n1 = 20, n2 = 30, alpha = 0.05, nsim = 20, modes = 2, dist = "beta", params = params_4, tests = "ANOVA", nperm = 30)  
est_pow_2samp(n1 = 20, n2 = 30, alpha = 0.05, nsim = 20, modes = 2, dist = "weib", params = params_4, tests = "Non-Parametric ANOVA", nperm = 30) # returns NULL


#####################################
######## est_pow_bin ########
#####################################

est_pow_bin(n = 15, p = 0.5, alpha = 0.05, nsim = 20)

#####################################
######## est_pow_het ########
#####################################
install.packages("olsrr")
library(olsrr)
est_pow_het(n = 50, p = 0.5, alpha = 0.05, x = 12, nsim = 20)


#####################################
######## find_dist ########
#####################################
find_dist(n1 = 20, n2 = 30, alpha = 0.05, nsim = 20)



