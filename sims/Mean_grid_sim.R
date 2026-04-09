library(parallel)
library(dplyr)
library(bifurcatoR)

# Set up the grid 
# Run the calcs
# Make the plots

mus <- c(0,2,4)
sdx <- c(1,2,4)
dists <- c("norm","beta","weibull","gamma","lnorm")
total_n <- c(8,16,32,64,128,256,512,1048)
s_ratios <- c(0.25,0.5,0.75) #sampling ratios

all.tests <-   test_labels <- c("welch", "ANOVA" , "Non-parametric ANOVA", "Permutations (Raw)" )

## params need to be set based off the distribution.
## If normal, one mean should be 0 for standard normal and the other can just be mus
## If Gamma, Weibull, or log-normal one mean should be 1 and the others should be mus*1, SDs will be the same as normal
## If beta, the upper limit for SD is ~0.289. The reference for beta is shapes 1, 1, so that will be our starting point
## This has a mean of 0.5 and SD ~0.289. This is actually the max sd, so we will actually go the other way for fc
## 1/sdx

params.sim <- expand.grid(mus = mus, sdx = sdx,dists=dists,s_ratios=s_ratios,total_n=total_n)
 

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
    est_pow_2_unimodes(n =c(n1,n2) ,
                       alpha = 0.05,
                       nsim = 2000,
                       dist = params.sim$dists[i],
                       params = params,
                       tests = all.tests)
    
}

lapply(1:3,function(x) sim_mean(x) )

