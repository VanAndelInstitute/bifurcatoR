.libPaths(c("/varidata/research/projects/bbc/zach/R/x86_64-pc-linux-gnu-library/4.5",.libPaths()))
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

mus <- c(0,2,4)
sdx <- c(1,2,4)
dists <- c("norm","beta","weibull","gamma","lnorm")
total_n <- c(8,16,32,64,128,256,512,1048)
s_ratios <- c(0.25,0.5,0.75) #sampling ratios

all.tests <-   test_labels <- c("mclust_E", "mclust_V" , "mt",
                                "dip","SI","CH","HY","ACR","FM",
                                "WmixR","NmixR","GmixR","LNmixR")

## params need to be set based off the distribution.
## If normal, one mean should be 0 for standard normal and the other can just be mus
## If Gamma, Weibull, or log-normal one mean should be 1 and the others should be mus*1, SDs will be the same as normal
## If beta, the upper limit for SD is ~0.289. The reference for beta is shapes 1, 1, so that will be our starting point
## This has a mean of 0.5 and SD ~0.289. This is actually the max sd, so we will actually go the other way for fc
## 1/sdx

params.sim <- expand.grid(mus = mus, sdx = sdx,dists=dists,s_ratios=s_ratios,total_n=total_n)
params.sim$dists <- as.character(params.sim$dists)

mu1 <- case_when(params.sim$dists == "norm" ~ 0,
                 params.sim$dists == "beta" ~ 0.5,
                 TRUE ~ 1)

sd1 <- case_when(params.sim$dists == "beta" ~ 0.288,
                 TRUE ~ 1)

sim_mean <- function(i){
  
  mu2 <- ifelse(params.sim[i,]$dist == "norm", params.sim$mus[i] + mu1[i],
                ifelse(params.sim[i,]$dist == "beta", 1/max(params.sim$mus[i],1) *mu1[i],
                       max(params.sim$mus[i],1) *mu1[i])) 
  
  sd2 <- ifelse(params.sim[i,]$dist == "beta", 1/params.sim$sdx[i] *sd1[i],
                params.sim$sdx[i] *sd1[i]) 
  
  
  params <- list(list(mean = mu1[i],sd=sd1[i]),list(mean = mu2,sd=sd2))
  
  #beta max sd is 0.28867; max mean is 0.999999
  n1 = params.sim$total_n[i] * params.sim$s_ratios[i]
  n2 =  params.sim$total_n[i] - n1
  sims <- est_pow_bimodal(n =c(n1,n2) ,
                             alpha = 0.05,
                             nsim = 1000,
                             dist = params.sim$dists[i],
                             params = params,
                             tests = all.tests)
  
  return(data.frame(sims,
                    delta = params.sim$mus[i],
                    sd_fc = params.sim$sdx[i],
                    dist = params.sim$dists[i],
                    s_ratios = params.sim$s_ratios[i]))
  
}

ncore <- ceiling(parallel::detectCores()*0.9)
mean.sims <- rbindlist(mclapply(1:nrow(params.sim),function(x) sim_mean(x) ,mc.cores = ncore))

write.csv(mean.sims,file = paste0("/varidata/research/projects/bbc/research/POSA_20230314_TR01Shiny_VBCS-718/bifurcatoR_main/sims/2000_bimodal_grid_sim.csv"),row.names = F)

