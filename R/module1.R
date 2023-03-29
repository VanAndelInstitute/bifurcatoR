devtools::install_github("VanAndelInstitute/bifurcatoR",force=T)

suppressPackageStartupMessages({
  library(bifurcatoR)
  library(doParallel)
  library(dplyr)
  library(mixdist)
  library(parallel)
  library(data.table)
  library(Hmisc)
  library(matrixStats)
})


total_cores <- round(parallel::detectCores()*.95)

params_grid <- expand.grid(
  Distribution = c("weib","norm"),
  Alpha = .05,
  Sim = 5000,
  SampleSize = c(20, 100, 500),
  Prop = c(0.1, 0.25, 0.5,.75,.9),
  Modes = 1,
  mu1 = 0,
  sd1 = 1,
  mu2 = c(0, 1, 2, 4),
  sd2 = c(1, 2, 4),
  Perm = c(5000))

process_sims_w_n = function(row){
  params_list <- list(mean = params_grid$mu2[row],
                      v_scale = params_grid$sd2[row])
  
  n2 <- floor(params_grid$Prop[row] * params_grid$SampleSize[row])
  n1 <- params_grid$SampleSize[row] - n2
  
  output <- bifurcatoR::est_pow_2samp(
    n1 = n1,
    n2 = n2,
    alpha = params_grid$Alpha[row],
    nsim = params_grid$Sim[row],
    modes = params_grid$Modes[row],
    dist = params_grid$Distribution[row],
    params = params_list,

    tests = c("ANOVA","Permutations (Raw)","Levene", "Permutations (MAD)","Permutations (SD)","Permutations (GiniMd)"),

    
    nperm = params_grid$Perm[row]
  )
  
  return(cbind(params_grid[row, ], output))
}

start.time <- Sys.time()
results=rbindlist(mclapply(1:nrow(params_grid), function(x) process_sims_w_n(x),mc.cores=total_cores))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# results
saveRDS(results, '/data/home/zachary.madaj/module2.rds')
