# NOTE: this is an analysis script, not package code. It used to live in R/, where
# every file is sourced when the package is built/installed, so installing would
# have run these simulations (and failed on the hard-coded paths below).
# Edit the paths for your machine before running it by hand.

.libPaths(c("/varidata/research/projects/bbc/zach/R/x86_64-pc-linux-gnu-library/4.6",.libPaths()))
library(parallel)
library(dplyr)
library(data.table)
library(bifurcatoR)
library(mclust)
library(diptest)
library(mousetrap)
library(LaplacesDemon)
library(multimode)
library(Hmisc)
library(twosamples)
library(mixR)
data(MLL)

  ## Need to take the data and estimate params based on the dist
  ## The sim multiple sample sizes
  ## Then down sample randomly
  x <- MLL$OS
  dist = "weibull"
  fit <- mixR::mixfit(x,ncomp = 2, family=dist)
  mu1 <- fit$mu[1]; mu2 <- fit$mu[2]
  s1 <- fit$sd[1]; s2 <- fit$sd[2]
  params <- list(list(mean = mu1, sd = s1),list(mean = mu2, sd = s2))  
  ns <- c(8,16,32,64,128,256,512)
  all.tests <- c("mclust_E", "mclust_V" , "mt","dip", "NmixR","LNmixR","WmixR","GmixR","ACR")
  for(n in ns){
    n1 <- floor(fit$pi[1] * n)
    n2 <- n - n1 
    sims <- est_pow_bimodal(n = c(n1,n2) ,
                            alpha = 0.05,
                            nsim = 2000,
                            dist = dist,
                            params = params,
                            tests = all.tests,
                            run_null = F)
    
    write.csv(file = paste0("/varidata/research/projects/bbc/research/POSA_20230314_TR01Shiny_VBCS-718/bifurcatoR_main/sims/survival/weibull_bimodal_grid_sim",n,".csv"),
              data.frame(sims,n = n))

  }
  
  


