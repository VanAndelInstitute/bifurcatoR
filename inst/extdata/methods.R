fit_test <- function(which_test, ...){
  #' @description Runs test specified by which_test.
  #' @param which_test str. Options: levene.
  
  params <- list(...)
  balanced <- params$balanced
  distrib <- params$distrib
  mean_size <- params$mean_size
  n_sim <- params$n_sim
  n <- params$n
  
  # Initialize arrays to store count tests which are significant at the 0.05 threshold.
  
  if(which_test %in% c('kruskal', 'lm', 'levene', 'diptest')){
    counts <- array(0, dim=c(n_model, n_sim))
    iter_length <- n_model 
  }
  
  if(which_test %in% c('mclust')){
    n_clust <- params$n_clust
    # 2*n_clust if specifying modelNames=c('E', 'V') in mClust()
    # counts <- array(0, dim=c(n_model, n_sim, 2*n_clust)) # TBD
    counts <- array(0, dim=c(n_model, n_sim))
    iter_length <- n_model
  }
  
  if(which_test %in% c('dbscan')){
    epsilon <- params$epsilon
    min_pts <- params$min_pts
    counts <- array(0, dim=c(n_model, n_sim))
    iter_length <- n_model
  }
  
  if(which_test %in% c('dglm')){
    # Second dimension = 2 because dglm tests mean and dispersion (variance) simultaneously.
    counts <- array(0, dim=c(n_model, 2, n_sim))
    dimnames(counts)[[2]] <- c('Mean', 'Var')
    iter_length <- n_model
  }
  
  if(which_test %in% c('lmp', 'aovp', 'permanova', 'anova')){
    counts <- array(0, dim=c(n_model*2, n_sim)) # For MAD model outputs
    iter_length <- n_model*2
  }
  
  # WARNING: Very slow with large n_perm and/or n_sim. 
  if(which_test == 'permanova'){
    print(paste('Method:', which_test))
    print(paste('Reducing n_sim from', n_sim, 'to', round(.2*n_sim), '(new n_sim=nperm)'))
    n_perm <- n_sim
    n_sim <- round(.2*n_sim)
    print(paste('Number of permutations for perm.anova function is:', n_perm))
    iter_length <- n_model*2
  }
  
  for(i in 1:n_sim){
    
    set.seed(i)
    
    output <- generate_data(i=i, ...)
    X <- output[9, ] # Use dimnames(output)[[1]] for reference. 
    y_arr <- output[1:8, ] # Array with both raw and MAD data.
    raw_idx <- grep('Raw', dimnames(output)[[1]]) # Array indices with raw data.
    mad_idx <- grep('MAD', dimnames(output)[[1]]) # Array indices with MAD data
    
    if(iter_length == n_model) indices <- raw_idx
    if(iter_length == n_model*2) indices <- 1:(length(dimnames(output)[[1]])-1)
    
    output_idx <- 0
    
    for(idx in indices){
      
      # Need this because indices to access y_arr different from indices to store results.
      #   In particular, certain tests are run with MAD data so indices run from 1:8 and in
      #   this case output_idx = idx; but if running only raw data, then output_idx != idx.
      output_idx <- output_idx + 1 
      
      if(which_test == 'lm'){
        lm_fit <- summary(lm(y_arr[idx, ] ~ X))
        counts[output_idx, i] <- ifelse(lm_fit$coef[2,4] < sig_level, 1, 0)
      } # END lm CLAUSE
      
      if(which_test == 'kruskal'){
        kw_fit <- kruskal.test(y_arr[idx, ], X)
        counts[output_idx, i] <- ifelse(kw_fit$p.value < sig_level, 1, 0) 
      } # END kruskal CLAUSE
      
      if(which_test == 'levene'){
        lev_fit <- leveneTest(y_arr[idx, ] ~ factor(X))
        counts[output_idx, i] <- ifelse(lev_fit$`Pr(>F)`[1] < sig_level, 1, 0)
      } # END levene CLAUSE
      
      if(which_test == 'lmp'){
        lmp_fit <- summary(lmp(y_arr[idx, ] ~ X, perm='Prob', settings=FALSE))
        counts[output_idx, i] <- ifelse(lmp_fit$coef[2,3] < sig_level, 1, 0)
      } # END lmp CLAUSE
      
      if(which_test == 'aovp'){
        aovp_fit <- summary(aovp(y_arr[idx, ] ~ X, perm='Prob', settings=FALSE))
        counts[output_idx, i] <- ifelse(aovp_fit[[1]]$`Pr(Prob)`[1] < sig_level, 1, 0)
      } # END aovp CLAUSE
      
      if(which_test == 'anova'){
        anova_fit <- anova(lm(y_arr[idx, ] ~ X))
        counts[output_idx, i] <- ifelse(anova_fit$`Pr(>F)`[1] < sig_level, 1, 0)
      } # END aovp CLAUSE
      
      if(which_test == 'permanova'){
        # OPTION 1: perm.anova; OPTION 2: PERMANOVA
        # But PERMANOVA is not available on the R server due to R version incompatibility with package.
        # OPTION 1:
        perm_fit <- perm.anova(y_arr[idx, ] ~ X, nperm=n_perm, progress=FALSE)
        counts[output_idx, i] <- ifelse(perm_fit$`Pr(>F)`[1] < sig_level, 1, 0)
        
        # OPTION 2: 
        # perm_fit <- PERMANOVA(DistContinuous(y_arr[idx, ]), factor(X), nperm=n_perm)
        # counts[output_idx, i] <- ifelse(perm_fit$pvalue < sig_level, 1, 0) 
      }
      
      if(which_test == 'dglm'){
        dglm_fit = dglm(y_arr[idx, ] ~ X, dformula=~X)
        dglm_mean = summary.glm(dglm_fit)
        # For mean effects, do $coef  (short for coefficients)
        counts[output_idx, 1, i] = ifelse(dglm_mean$coef[2,4] < sig_level, 1, 0)
        # For variance effects, see dispersion model
        dglm_var = summary(dglm_fit$dispersion.fit)
        counts[output_idx, 2, i] = ifelse(dglm_var$coef[2,4] < sig_level, 1, 0)
      } # END dglm CLAUSE
      
      if(which_test == 'mclust'){
        mclust_fit <- Mclust(y_arr[idx, ], G=1:n_clust, modelNames=c('E', 'V'), verbose=FALSE)
        # counts[output_idx, i, ] <- as.vector(as.matrix(mclust_fit$BIC))
        counts[output_idx, i] <- mclust_fit$G # Returns optimal no. of clusters
      } # END mclust CLAUSE
      
      if(which_test == 'diptest'){
        dt_fit <- dip.test(y_arr[idx, ], simulate.p.value=TRUE, B=n_perm)
        counts[output_idx, i] <- ifelse(dt_fit$p.value < sig_level, 1, 0)
      } # END diptest CLAUSE
      
      if(which_test == 'dbscan'){
        dbscan_fit <- dbscan(as.matrix(y_arr[idx, ]), eps=epsilon, minPts=min_pts)
        dbscan_tbl <- table(dbscan_fit$cluster)
        if(length(dbscan_tbl) == 1){
          counts[output_idx, i] <- names(dbscan_tbl)
        }
        if(length(dbscan_tbl) > 1){
          # Remove 0th cluster
          dbscan_tbl <- dbscan_tbl[-1]
          dbscan_cluster <- as.numeric(names(sort(dbscan_tbl, decreasing=TRUE))[1])
          counts[output_idx, i] <- dbscan_cluster
        }
      } # END dbscan CLAUSE
      
    } # END LOOPING OVER SIMULATED DATA (I.E. MODELS)
  } # END LOOPING OVER n_sim
  
  if(iter_length == 2*n_model){
    dimnames(counts)[[1]] <- dimnames(output)[[1]][1:(n_model*2)]
  }
  if(iter_length == n_model){
    dimnames(counts)[[1]] <- dimnames(output)[[1]][raw_idx]
    
    # if(which_test == 'mclust'){
    #   dimnames(counts)[[3]] <- sort(apply(expand.grid(c('E', 'V'), 1:n_clust), 
    #                                       1, paste, collapse=''))
    # }
  }
  
  return(counts)
}

# DGLM --------------------------------------------------------------------

# Only for Normal distribution

execute_dglm <- function(){
  
  method <- 'DGLM'
  
  dglm_tests <- lapply(1:nrow(config$config), function(row)
    fit_test('dglm',
             balanced=balanced_design,
             distrib=distribution,
             n=config$config[row, 1],
             n_gene=0,
             n_sim=n_sim,
             mean_size=config$config[row, 2],
             sd=config$config[row, 3]))
  # print(dglm_tests)
  dglm_output <- lapply(1:length(dglm_tests),
                        function(l) apply(dglm_tests[[l]], c(1,2), mean))
  # Reformat from a 4 models x 2 columns (1 for Mean, 1 for Var) into a list.
  dglm_output_list <- lapply(1:length(dglm_output), function(l){
    dglm_rownames <- rownames(dglm_output[[l]])
    mean_vals <- dglm_output[[l]][, 1]
    names(mean_vals) <- paste(dglm_rownames, 'Mean', sep='-')
    var_vals <- dglm_output[[l]][, 2]
    names(var_vals) <- paste(dglm_rownames, 'Var', sep='-')
    return(list(c(mean_vals, var_vals)))
  })
  
  # print(dglm_output)
  dglm_tbl <- tibble(do.call(rbind, dglm_output_list))
  colnames(dglm_tbl) <- 'values'
  
  dglm_tbl <- dglm_tbl %>% 
    tidyr::unnest_wider(values)
  dglm_tbl <- as.data.frame(dglm_tbl)
  
  names(dglm_tests) <- names(dglm_output) <- names(dglm_output_list) <- 
    rownames(dglm_tbl) <- config$combinations
  
  saveRDS(dglm_tests, paste0(method, '-', n_sim, 'Sims-', config$filekey, '.rds'))
  saveRDS(dglm_tbl, paste0(method, '-', n_sim, 'Sims-', config$filekey, '-Table.rds'))
  
}

# LEVENE'S ----------------------------------------------------------------

run_levene <- function(...){
  
  args <- as.list(...)
  
  if(distribution == 'norm'){
    
    levene_tests <- lapply(1:nrow(config$config), function(row) 
      fit_test('levene',
               balanced=balanced_design,
               distrib=distribution,
               n=config$config[row, 1],
               n_gene=n_gene,
               n_sim=n_sim,
               mean_size=config$config[row, 2],
               sd=config$config[row, 3]))}
  
  levene_output <- lapply(1:length(levene_tests),
                          function(l) apply(levene_tests[[l]], 1, mean))
  names(levene_tests) <- names(levene_output) <- config$combinations
  levene_tbl <- data.frame(do.call(rbind, levene_output))
  
  # saveRDS(levene_tests, paste0(method, '-', n_sim, 'Sims-', config$filekey, '.rds'))
  # saveRDS(levene_tbl, paste0(method, '-', n_sim, 'Sims-', config$filekey, '-Table.rds'))
  
}

# HARTIGAN'S DIP TEST -----------------------------------------------------

execute_diptest <- function(){
  
  method <- 'DIPTEST'
  
  if(balanced_design == TRUE) design = 'Balanced'
  if(balanced_design == FALSE) design = 'Unbalanced'
  
  if(distribution == 'norm'){
    diptest_tests <- lapply(1:nrow(config$config), function(row)
      fit_test('diptest',
               balanced=balanced_design,
               distrib=distribution,
               n=config$config[row, 1],
               n_gene=n_gene,
               n_sim=n_sim,
               mean_size=config$config[row, 2],
               sd=config$config[row, 3]))}
  
  diptest_output <- lapply(1:length(diptest_tests),
                           function(l) apply(diptest_tests[[l]], 1, mean))
  diptest_tbl <- data.frame(do.call(rbind, diptest_output))
  
  names(diptest_tests) <- names(diptest_output) <- 
    rownames(diptest_tbl) <- config$combinations
  
  # saveRDS(diptest_tests, paste0(design, '-', method, '-', n_sim, 'Sims-', config$filekey, '.rds'))
  # saveRDS(diptest_tbl, paste0(design, '-', method, '-', n_sim, 'Sims-', config$filekey, '-Table.rds'))
}

# MCLUST ------------------------------------------------------------------

extract_cluster_counts <- function(list_to_unpack){
  #' @description Find number of most frequently occurring cluster based on input table.
  #' @details If a table has only one value, then extracting the value can be done by using as.numeric, but it takes additional steps to unpack the list when there are multiple values in the table.
  #' @param list_to_unpack A list containing tabulated cluster assignments for each of the four models, to be unpacked into a single vector.
  
  counts <- vector(length=n_model)
  
  for(i in 1:length(list_to_unpack)){ # length(list_to_unpack) should be n_model
    if(is.integer(list_to_unpack)){
      counts[i] <- list_to_unpack[[i]]
    }
    
    if(is.list(list_to_unpack)){
      cluster_names <- names(list_to_unpack[[i]])
      most_frequent_cluster <- cluster_names[1]
      counts[i] <- as.numeric(most_frequent_cluster)
    }
  }
  return(counts)
}

execute_mclust <- function(){
  
  method <- 'MCLUST'
  
  if(balanced_design == TRUE) design = 'Balanced'
  if(balanced_design == FALSE) design = 'Unbalanced'
  
  if(distribution == 'norm'){
    mclust_tests <- lapply(1:nrow(config$config), function(row)
      fit_test('mclust',
               balanced=balanced_design,
               distrib=distribution,
               n=config$config[row, 1],
               n_gene=n_gene,
               n_sim=n_sim,
               mean_size=config$config[row, 2],
               sd=config$config[row, 3],
               n_clust=n_clust))}
  
  # Find most frequently occurring clusters
  mclust_tbl <- lapply(1:length(mclust_tests),
                       function(t) {
                         # Tabulate cluster assignments
                         tbl <- lapply(1:nrow(mclust_tests[[t]]), function(m) table(mclust_tests[[t]][m, ]))
                         # Sort to get most frequent
                         tbl <- lapply(1:length(tbl), function(t) sort(tbl[[t]], decreasing=TRUE))
                       })
  mclust_tbl <- t(sapply(1:length(mclust_tbl), function(l2) extract_cluster_counts(mclust_tbl[[l2]])))
  
  rownames(mclust_tbl) <- config$combinations
  colnames(mclust_tbl) <- models
  
  # saveRDS(mclust_tests, paste0(design, '-', method, '-', n_sim, 'Sims-', config$filekey, '.rds'))
  # saveRDS(mclust_tbl, paste0(design, '-', method, '-', n_sim, 'Sims-', config$filekey, '-Table.rds'))
}

# DBSCAN ------------------------------------------------------------------

execute_dbscan <- function(){
  
  method <- 'DBSCAN'
  
  if(balanced_design == TRUE) design = 'Balanced'
  if(balanced_design == FALSE) design = 'Unbalanced'
  
  # If using DBSCAN, add additional parameters to fit
  config$config <- expand.grid.df(config$config, data.frame(Eps=eps), 
                                  data.frame(MinPts=min_pts))
  config$combinations <- sapply(1:nrow(config$config), 
                                function(row) gsub('\\.', 'p', paste(paste(names(config$config), 
                                                                           config$config[row, ], sep='='), 
                                                                     collapse=' ')))
  if(distribution == 'norm'){
    dbscan_tests <- lapply(1:nrow(config$config), function(row)
      fit_test('dbscan',
               balanced=balanced_design,
               distrib=distribution,
               n=config$config[row, 1],
               n_gene=n_gene,
               n_sim=n_sim,
               mean_size=config$config[row, 2],
               sd=config$config[row, 3],
               epsilon=config$config[row, 4],
               min_pts=config$config[row, 5]))}
  
  # Find most frequently occurring clusters
  dbscan_tbl <- lapply(1:length(dbscan_tests),
                       function(t) {
                         # Tabulate cluster assignments
                         tbl <- lapply(1:nrow(dbscan_tests[[t]]), function(m) table(dbscan_tests[[t]][m, ]))
                         # Sort to get most frequent
                         tbl <- lapply(1:length(tbl), function(t) sort(tbl[[t]], decreasing=TRUE))
                       })
  dbscan_tbl <- t(sapply(1:length(dbscan_tbl), function(l2) extract_cluster_counts(dbscan_tbl[[l2]])))
  
  rownames(dbscan_tbl) <- config$combinations
  colnames(dbscan_tbl) <- models
  
  names(dbscan_tests) <- 
    # names(dbscan_output) <-
    rownames(dbscan_tbl) <-
    config$combinations
  
  # saveRDS(dbscan_tests, paste0(design, '-', method, '-', n_sim, 'Sims-', config$filekey, '.rds'))
  # saveRDS(dbscan_tbl, paste0(design, '-', method, '-', n_sim, 'Sims-', config$filekey, '-Table.rds'))
}