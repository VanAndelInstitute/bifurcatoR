# invisible(sapply(list.files(
#   pattern = 'utils',
#   recursive = TRUE,
#   full.names = TRUE),
#   function(f)
#     sys.source(f, env = attach(NULL))))

invisible(sys.source('/home/ckhoo@mmcf.mehealth.org/TR01/utils.R', env = attach(NULL)))

# GENERATE DATA -----------------------------------------------------------
# Create an array of simulated data.

configure_params <- function(param_group) {
  #' @description Given a set of parameters, convert into list of parameters to easily generate distributions.
  #' @param param_group data frame or matrix with specific column names.
  
  # print(class(param_group))
  
  if ((class(param_group) != 'data.frame') ||
      (c('Distribution', 'nGroups') %notin% names(param_group))) {
    return('Input has missing columns.')
    
  }
  
  else {
    distribution <- param_group$Distribution
    n_groups <- param_group$nGroups
    nSubjects <- param_group$nSubjects
    X_labels <- paste0('X=', 0:(n_groups - 1))
    
    params_list <- vector('list', length = n_groups)
    names(params_list) <- X_labels
    # Here, we want:
    # - (i) default values per distribution (unless we have to specify a value)
    # - (ii) the values that will simulate mean and/or variance effect per distribution
    params_list <-
      lapply(setNames(X_labels, X_labels), function(l) {
        if (distribution == 'beta') {
          # if(l == 'X=0') return(list('n' = nSubjects,
          #                            'shape1' = param_group$shape1,
          #                            'shape2' = param_group$shape2))
          return(
            list(
              'n' = nSubjects,
              'shape1' = param_group$shape1,
              'shape2' = param_group$shape2
            )
          )
          
        } # END beta
        
        if (distribution == 'cauchy') {
          if (l == 'X=0')
            return(list('n' = nSubjects))
          if (l == 'X=1')
            return(
              list(
                'n' = nSubjects,
                'location' = param_group$location,
                'scale' = param_group$scale
              )
            )
          if (l == 'X=2')
            return(
              list(
                'n' = nSubjects,
                'location' = param_group$location,
                'scale' = param_group$scale ** 2
              )
            )
          
        } # END cauchy
        
        if (distribution == 'norm') {

          if (l == 'X=0')
            return(list('n' = nSubjects))
          if (l == 'X=1')
            return(list('n' = nSubjects,
                        'sd' = param_group$sd))
          if (l == 'X=2')
            return(list('n' = nSubjects,
                        'sd' = param_group$sd ** 2))
          
        } # END norm
        
        if (distribution == 'unif') {
          if (l == 'X=0')
            return(list('n' = nSubjects))
          if (l == 'X=1')
            return(list(
              'n' = nSubjects,
              'min' = param_group$min,
              'max' = param_group$max
            ))
          # Check?
          if (l == 'X=2')
            return(list(
              'n' = nSubjects,
              'min' = param_group$min,
              'max' = 2 * param_group$max
            ))
          
        } # END unif
        
        if (distribution == 'weibull') {
          if (l == 'X=0')
            return(list('n' = nSubjects,
                        'shape' = 1))
          if (l == 'X=1')
            return(
              list(
                'n' = nSubjects,
                'scale' = param_group$scale,
                'shape' = param_group$shape
              )
            )
          if (l == 'X=2')
            return(
              list(
                'n' = nSubjects,
                'scale' = param_group$scale ** 2,
                'shape' = param_group$shape
              )
            )
          
        } # END weibull
      })
    
    return(params_list)
  }
}

X_sim <- function(i, ...) {
  #' @description Generate X based on simulation design.
  #' @param i integer. Denotes iteration index.
  #' @return vector.

  args <- as.list(...)
  balanced <- args$BalancedDesign
  distribution <- args$Distribution
  nSubjects <- args$nSubjects
  n_groups <- args$nGroups
  
  # # if (n_groups >= 3) {
  # #   maf <- args$MAF
  # # }
  # 
  # if (distribution == 'beta') {
  #   params <- list('shape1' = args$shape1,
  #                  'shape2' = args$shape2)
  # }
  # 
  # if (distribution == 'cauchy') {
  #   params <- list('location' = args$location,
  #                  'scale' = args$scale)
  # }

  if (distribution == 'norm') {
    params <- list('mean' = args$mean,
                   'sd' = args$sd)
  }
  
  # if (distribution == 'weibull') {
  #   params <- list('scale' = args$scale,
  #                  'shape' = args$shape)
  # }
  # 
  # if (distribution == 'uniform') {
  #   params <- list('min' = args$min,
  #                  'max' = args$max)
  # }

  set.seed(i * nSubjects)

  # Balanced design.
  if (balanced == TRUE) {
    if (n_groups == 2) {
      # Strain effect only.
      X <-
        sample(rep(0:1, each = nSubjects / 2)) # Want proportion to be exactly 0.5.
    }
  #   # if (n_groups == 3) {
  #   #   f <-
  #   #     sample(rep(0:1, each = nSubjects / 2)) # Contribution from the father
  #   #   m <-
  #   #     sample(rep(0:1, each = nSubjects / 2)) # Contribution from the mother
  #   #   X <- f + m
  #   # }
  }

  # # Unbalanced design.
  # if (!balanced) {
  #   if (n_groups == 2) {
  #     # Strain effect only.
  #     if ('prob' %notin% names(params))
  #       prob <- .25
  #     if ('prob' %in% names(params))
  #       prob <- params$prob
  #     X <- rbinom(nSubjects, 1, prob)
  #   }
  #   
  #   # if (n_groups == 3) {
  #   #   f <- rbinom(nSubjects, 1, maf) # Contribution from the father
  #   #   m <- rbinom(nSubjects, 1, maf) # Contribution from the mother
  #   #   X <- f + m
  #   # }
  #   
  # }

  return(X)
}

y_sim <- function(i, X, ...) {
  #' @description Currently working for Normal distribution only.
  
  args <- as.list(...)

  distribution <- args$Distribution
  idx0 <- args[['idx0']]
  idx1 <- args[['idx1']]
  models <- c('Null', 'Mean', 'Var', 'MeanVar')
  nSubjects <- args$nSubjects
  
  set.seed(i * nSubjects)
  
  y_arr <- array(NA, dim = c(2 * length(models), nSubjects))
  rownames(y_arr) <- c(paste(models, 'Raw', sep='-'), paste(models, 'MAD', sep='-'))
  
  if (distribution == 'norm') {
    mean_size <- args$sd
  }

  # Null model - Raw data
  y_arr['Null-Raw', ] <-
    do.call(paste0('r', distribution), args[['Null']])

  # Mean model - Raw data
  y_arr['Mean-Raw', ] <- X * mean_size + do.call(paste0('r', distribution), args[['Null']])

  # Variance model - Raw data
  y_arr['Var-Raw', idx0] <- do.call(paste0('r', distribution), args[['X=0']])
  y_arr['Var-Raw', idx1] <- do.call(paste0('r', distribution), args[['X=1']])

  # Mean-variance model - Raw data
  y_arr['MeanVar-Raw', idx0] <- do.call(paste0('r', distribution), args[['X=0']])
  y_arr['MeanVar-Raw', idx1] <-
    mean_size + do.call(paste0('r', distribution), args[['X=1']])
  
  # MAD data
  y_arr['Null-MAD', ] <- compute_mad(y_arr['Null-Raw', ])
  y_arr['Mean-MAD', idx0] <- compute_mad(y_arr['Mean-Raw', idx0])
  y_arr['Mean-MAD', idx1] <- compute_mad(y_arr['Mean-Raw', idx1])
  y_arr['Var-MAD', idx0] <- compute_mad(y_arr['Var-Raw', idx0])
  y_arr['Var-MAD', idx1] <- compute_mad(y_arr['Var-Raw', idx1])
  y_arr['MeanVar-MAD', idx0] <- compute_mad(y_arr['MeanVar-Raw', idx0])
  y_arr['MeanVar-MAD', idx1] <- compute_mad(y_arr['MeanVar-Raw', idx1])

  # print(y_arr)
  return(y_arr)
}


generate_data <- function(i, param_group) {
  #' @param i integer. Denotes iteration index.
  
  args <- as.list(param_group)
  nGroups <- args$nGroups
  # args <- lapply(1:length(args), function(l)
  #   unique(args[[l]]))

  X <- X_sim(i, param_group)
  
  get_params <- configure_params(param_group)

  args[['Null']] <- list('n' = args$nSubjects)
  indices <- vector('list', nGroups)

  for (idx in 0:(nGroups - 1)) {

    label <- paste0('X=', idx)
    args[[label]] <- get_params[[label]]
    args[[label]] <- modifyList(args[[label]], list('n' = length(which(X == idx))))
    indices[[idx+1]] <- which(X == idx)

  }

  names(indices) <- paste0('idx', 0:(nGroups-1))
  args <- c(args, indices)
  y_arr <- y_sim(i, X, args)

  data <- data.frame(y_arr)
  data <- rbind(data, 'X' = X)
  data <- t(data)
  colnames(data) <- c(rownames(y_arr), 'X')

  return(data)
}

# param_grid <- readRDS(paste0(sim_dir, 'ParamGrid.rds'))
# tmpgrid <- param_grid[1, ]
# tmpsim <- generate_data(1, tmpgrid)
# tmpsim[['y']][1, ]
