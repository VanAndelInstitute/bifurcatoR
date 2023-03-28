install.packages("remotes")
install.packages("BiocManager")
BiocManager::install("VanAndelInstitute/bifurcatoR", dependencies=c("Depends", "Imports", "LinkingTo", "Suggests"))

library(remotes)
library(BiocManager)
library(bifurcatoR)
library(pwr)
library(dplyr)

# test = calc_power(n = 200, p1 = 0.2, p2 = 0.5, shift = 0.01, sel = 20, CI = TRUE, alpha = 0.05, nsim=100)
calc_power(n = 20, p1 = 0.25, p2 = 0.5, sel='Bodyweight', shift = 15, CI = 0)



help(dxsmall) # try firing up iSEE per the example!

est_pow(n = 50, alpha = 0.05, nsim = 20, dist = "norm", params, tests)

est_pow_bin(n = 20, p = 0.8, alpha = 0.05, nsim = 20)


set.seed(1234)
dat <- rbeta(n=1000, 17, 39)
parameters_beta(dat)

set.seed(1234)
dat <- rnorm(n=1000, mean=6)
stopifnot(all(dat > 0)) # fitdistr fails if any(dat <= 0)
parameters_weibull(dat)



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
######## Test calc_power ########
################################
calc_power(n = 20, p1 = 0.25, p2 = 0.5, sel= 'Bodyweight', shift = 15, CI = 0)
calc_power(n = 550, p1 = 0.2, p2 = 0.9, sel= 'Fat Trim', shift = 30, CI = 1)
calc_power(n = 11050, p1 = 0.9, p2 = 0.6, sel= 'Fat NNAT', shift = 120, CI = 1)

################################
######## Test est_pow ########
################################
params_1 = list (p = 0.5, mu1 = 2, sd1 = 0.1, mu2 = 3, sd2 = 0.4, s1 = 1.5, s2 = 2.3, sp1 = 12, sp2 = 63, sc1 = 2, sc2 = 5)
est_pow(n = 20, alpha = 0.05, nsim = 20, dist = "norm", params = params_1,  tests = "Hartigans' dip test")
est_pow(n = 500, alpha = 0.05, nsim = 20, dist = "beta", params = params_1,  tests = "mclust")
est_pow(n = 1000, alpha = 0.05, nsim = 20, dist = "weib", params = params_1,  tests = "Mouse Trap")



### Test est_pow_2samp
est_pow_2samp(n1 = 20, n2 = 30, alpha = 0.05, nsim = 20, modes = 1, dist = "norm", params = 0.02, tests = "ks", nperm = 50)
