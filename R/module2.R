suppressPackageStartupMessages({
  library(bifurcatoR)
  library(doParallel)
  library(dplyr)
  library(data.table)
  library(parallel)
})

total_cores <- round(parallel::detectCores()*.95)


params_grid <- expand.grid(
  Distribution = c("norm","weib"),
  Alpha = .05,
  Sim = 1000,
  SampleSize = c(20,40,80,100,500),
  Prop = c(0.1, 0.25, 0.5,.75,.9),
  mu1 = 0,
  sd1 = 1,
  mu2 = c(1, 2, 4, 8),
  sd2 = c(1,  2, 4)
)

process_sims_w_n = function(row){  
  
  if(params_grid$Distribution[row] == "norm"){
    params_list <- list(p = params_grid$Prop[row],
                        mu1 = params_grid$mu1[row],
                        sd1 = params_grid$sd1[row],
                        mu2 = params_grid$mu2[row],
                        sd2 = params_grid$sd2[row])
    
  } else if(params_grid$Distribution[row] == "weib"){
    params_list <- list(p = params_grid$Prop[row],
                        sp1 = mixdist::weibullpar(params_grid$mu1[row],params_grid$sd1[row], loc = 0)$shape,
                        sc1 = mixdist::weibullpar(params_grid$mu1[row],params_grid$sd1[row], loc = 0)$scale,
                        sp2 = mixdist::weibullpar(params_grid$mu2[row],params_grid$sd2[row], loc = 0)$shape,
                        sc2 = mixdist::weibullpar(params_grid$mu2[row],params_grid$sd2[row], loc = 0)$scale)
  }
  
  
  
  output <- bifurcatoR::est_pow(
    n = params_grid$SampleSize[row],
    alpha = params_grid$Alpha[row],
    nsim = params_grid$Sim[row],
    dist = params_grid$Distribution[row],
    params = params_list,
    tests = c("dip", "mclust", "mt")
    # tests = c("mclust")
    
  )
  
  return(cbind(params_grid[row, ], output))
}

start.time <- Sys.time()
results=rbindlist(mclapply(1:nrow(params_grid), function(x) process_sims_w_n(x),mc.cores=total_cores))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# results
saveRDS(results,'/module2_set2_n_w_mclust.rds')


params_grid <- expand.grid(
  Distribution = c("beta"),
  Alpha = .05,
  Sim = 100,
  SampleSize = c(20,40,80,100,500),
  Prop = c(0.1, 0.25, 0.5,.75,.9),
  s1 = c(0.25,0.5),
  s2 = c(0.5,.75)
)


process_sims_b = function(row){  
  
  
  params_list <- list(s1 = params_grid$s1[row],
                      s2 = params_grid$s2[row])
  
  output <- bifurcatoR::est_pow(
    n = params_grid$SampleSize[row],
    alpha = params_grid$Alpha[row],
    nsim = params_grid$Sim[row],
    dist = params_grid$Distribution[row],
    params = params_list,
    tests = c("dip", "mclust", "mt")
    # tests = c("mclust")
    
  )
  
  return(cbind(params_grid[row, ], output))
}

start.time <- Sys.time()
results=rbindlist(mclapply(1:nrow(params_grid), function(x) process_sims_b(x),mc.cores=total_cores))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# results
saveRDS(results, 'module2_set2_b_mclust.rds')


