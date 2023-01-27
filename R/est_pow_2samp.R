#' est_pow_2samp
#' 
#' @param   n1      number of [somethings] in group 1
#' @param   n2      number of [somethings] in group 2 
#' @param   alpha   default significance level (0.05)
#' @param   nsim    number of simulations (20) 
#' @param   modes   modes of the two groups 
#' @param   dist    generating distributions
#' @param   params  parameters for the generating distributions
#' @param   tests   names of tests to run between the two groups 
#' @param   nperm   number of permutations to run
#'
#' @return          a power estimate
#' 
#' @import          twosamples 
#' @import          car
#'
#' @export




est_pow_2samp = function(n1,n2,alpha,nsim,modes,dist,params,tests,nperm){
  set.seed(42)
  n.dfs=list()
  a.dfs=list()
  if(dist=="norm"){
    if(modes == 2){
      n1_1 = floor(params$p_1*n1)
      n2_1 = floor((1-params$p_1)*n1)
      
      n1_2 = floor(params$p_2*n2)
      n2_2 = floor((1-params$p_2)*n2)
      
      n.dfs[[1]] = lapply(1:nsim,function(x) c(rnorm(n1_1,params$mu1_1,params$sd1_1),
                                               rnorm(n2_1,params$mu2_1,params$sd2_1)))
      
      n.dfs[[2]] = lapply(1:nsim,function(x) c(rnorm(n1_2,params$mu1_2,params$sd1_2),
                                               rnorm(n2_2,params$mu2_2,params$sd2_2)))
      
      p = (params$p_1*n1+params$p_2*n2)/(n1+n2)
      mu1 =   (params$mu1_1*n1_1+params$mu1_2*n1_2)/(n1_1+n1_2)
      mu2 =   (params$mu2_1*n2_1+params$mu2_2*n2_2)/(n2_1+n2_2)
      sd1 = sqrt((params$sd1_1^2*(n1_1-1)+params$sd1_2^2*(n1_2-1))/(n1_1+n1_2-2))
      sd2 = sqrt((params$sd2_1^2*(n2_1-1)+params$sd2_2^2*(n2_2-1))/(n2_1+n2_2-2))
      
      n1_1 = floor(p*n1)
      n2_1 = floor((1-p)*n1)
      
      n1_2 = floor(p*n2)
      n2_2 = floor((1-p)*n2)
      
      
      a.dfs[[1]] = lapply(1:nsim,function(x) c(rnorm(n1_1,mu1,sd1),rnorm(n2_1,mu2,sd2)))
      a.dfs[[2]] = lapply(1:nsim,function(x) c(rnorm(n1_2,mu1,sd1),rnorm(n2_2,mu2,sd2)))
    } else {
      if(modes == 1){
        n.dfs[[1]] = lapply(1:nsim,function(x) rnorm(n1,0,1))
        n.dfs[[2]] = lapply(1:nsim,function(x) rnorm(n2,0+params$mean,1*params$v_scale))
        
        
        a.dfs[[1]] = lapply(1:nsim,function(x) rnorm(n1,(n2*params$mean)/(n1+n2),sqrt((1*(n1-1)+params$v_scale^2*(n2-1))/(n1+n2-2))))
        a.dfs[[2]] = lapply(1:nsim,function(x) rnorm(n2,(n2*params$mean)/(n1+n2),sqrt((1*(n1-1)+params$v_scale^2*(n2-1))/(n1+n2-2))))
      }
    }
    
  } else {
    ## As of right now beta is being used only for module 2 and 3
    if(dist=="beta"){
      n.dfs[[1]] = lapply(1:nsim,function(x) rbeta(n1,params$s1_1,params$s2_1))
      n.dfs[[2]] = lapply(1:nsim,function(x) rbeta(n2,params$s1_2,params$s2_2))
      
      a.dfs[[1]] = lapply(1:nsim,function(x) rbeta(n1,(n1*params$s1_1+n2*params$s1_2)/(n1+n2),(n1*params$s2_1+n2*params$s2_2)/(n1+n2)))
      
      a.dfs[[2]] = lapply(1:nsim,function(x) rbeta(n2,(n1*params$s1_1+n2*params$s1_2)/(n1+n2),(n1*params$s2_1+n2*params$s2_2)/(n1+n2)))
      
      
    } else {
      
      # need to find suitable support for standard normal and std normal + mean and var*var
      if(dist == "weib"){
        if(modes == 1){
          
          shape = mixdist::weibullpar(1+params$mean,params$v_scale,loc=0)$shape
          scale = mixdist::weibullpar(1+params$mean,params$v_scale,loc=0)$scale
          n.dfs[[1]] = lapply(1:nsim,function(x) rweibull(n1,1,1))
          n.dfs[[2]] = lapply(1:nsim,function(x) rweibull(n2,shape=shape,scale=scale))
          
          #Create a targically over-simplified mixture of the two by averaging the means
          #and pooling the variances
          mu.pool = (n1+n2*(1+params$mean))/(n1+n2)
          sd.pool = sqrt(((n1-1)*1^2 + (n2-1)*(params$v_scale)^2)/(n1+n2-2))
          shape.p = mixdist::weibullpar(mu.pool,sd.pool,loc=0)$shape
          scale.p = mixdist::weibullpar(mu.pool,sd.pool,loc=0)$scale
          
          a.dfs[[1]] = lapply(1:nsim,function(x) rweibull(n1,shape=shape.p,scale=scale.p))
          a.dfs[[2]] = lapply(1:nsim,function(x) rweibull(n2,shape=shape.p,scale=scale.p))
          
        } else {
          if(modes == 2){
            n1_1 = floor(params$p_1*n1)
            n2_1 = floor((1-params$p_1)*n1)
            
            n1_2 = floor(params$p_2*n2)
            n2_2 = floor((1-params$p_2)*n2)
            
            n.dfs[[1]] = lapply(1:nsim,function(x) c(rweibull(n1_1,shape = params$sp1_1,scale = params$sc1_1),
                                                     rweibull(n2_1,shape = params$sp2_1,scale = params$sc2_1)))
            
            n.dfs[[2]] = lapply(1:nsim,function(x) c(rweibull(n1_2,shape = params$sp1_2,scale = params$sc1_2),
                                                     rweibull(n2_2,shape = params$sp2_2,scale = params$sc2_2)))
            
            p = (params$p_1*n1+params$p_2*n2)/(n1+n2)
            sp1 = (params$sp1_1*n1_1+params$sp1_2*n1_2)/(n1_1+n1_2)
            sp2 = (params$sp2_1*n2_1+params$sp2_2*n2_2)/(n2_1+n2_2)
            sc1 = (params$sc1_1*n1_1+params$sc1_2*n1_2)/(n1_1+n1_2)
            sc2 = (params$sc2_1*n2_1+params$sc2_2*n2_2)/(n2_1+n2_2)
            
            n1_1 = floor(p*n1)
            n2_1 = floor((1-p)*n1)
            
            n1_2 = floor(p*n2)
            n2_2 = floor((1-p)*n2)
            
            
            a.dfs[[1]] = lapply(1:nsim,function(x) c(rweibull(n1_1,shape = sp1,scale = sc1),rweibull(n2_1,shape = sp2,scale = sc2)))
            a.dfs[[2]] = lapply(1:nsim,function(x) c(rweibull(n1_2,shape = sp1,scale = sc1),rweibull(n2_2,shape = sp2,scale = sc2)))
          }
        }
        
      }
      
      
    }
    
  }
  pwr.df = NULL
  if("ks" %in% tests){
    pwr.df = rbind(pwr.df,data.frame( Test = "Kolmogorov-Smirnov",
                                      power = sum(sapply(1:nsim, function(s) I(ks.test(n.dfs[[1]][[s]],n.dfs[[2]][[s]])$p.value<alpha)))/nsim,
                                      FP = sum(sapply(1:nsim, function(s) I(ks.test(a.dfs[[1]][[s]],a.dfs[[2]][[s]])$p.value<alpha)))/nsim))
  }
  
  
  if("cvm" %in% tests){
    pwr.df = rbind(pwr.df,data.frame( Test = "Cramer-von Mises",
                                      power = sum(sapply(1:nsim, function(s) I(twosamples::cvm_test(n.dfs[[1]][[s]],n.dfs[[2]][[s]],nboots=nsim)[2]<alpha)))/nsim,
                                      FP = sum(sapply(1:nsim, function(s) I(twosamples::cvm_test(a.dfs[[1]][[s]],a.dfs[[2]][[s]],nboots=nsim)[2]<alpha)))/nsim))
  }
  
  
  if("dts" %in% tests){
    pwr.df = rbind(pwr.df,data.frame( Test = "DTS",
                                      power = sum(sapply(1:nsim, function(s) I(twosamples::dts_test(n.dfs[[1]][[s]],n.dfs[[2]][[s]],nboots=nsim)[2]<alpha)))/nsim,
                                      FP = sum(sapply(1:nsim, function(s) I(twosamples::dts_test(a.dfs[[1]][[s]],a.dfs[[2]][[s]],nboots=nsim)[2]<alpha)))/nsim))
  }
  
  if("ad" %in% tests){
    pwr.df = rbind(pwr.df,data.frame( Test = "Anderson-Darling",
                                      power = sum(sapply(1:nsim, function(s) I(twosamples::ad_test(n.dfs[[1]][[s]],n.dfs[[2]][[s]],nboots=nsim)[2]<alpha)))/nsim,
                                      FP = sum(sapply(1:nsim, function(s) I(twosamples::ad_test(a.dfs[[1]][[s]],a.dfs[[2]][[s]],nboots=nsim)[2]<alpha)))/nsim))
  }
  
  if("Levene" %in% tests){
    
    pwr.df = rbind(pwr.df,data.frame( Test = "Levene",
                                      power = sum(sapply(1:nsim, function(s) I(car::leveneTest(c(n.dfs[[1]][[s]],n.dfs[[2]][[s]]),as.factor(c(rep("1",length(n.dfs[[1]][[s]])),rep("2",length(n.dfs[[2]][[s]])))))$"Pr(>F)"[1]<alpha)))/nsim,
                                      FP = sum(sapply(1:nsim, function(s) I(car::leveneTest(c(a.dfs[[1]][[s]],a.dfs[[2]][[s]]),as.factor(c(rep("1",length(a.dfs[[1]][[s]])),rep("2",length(a.dfs[[2]][[s]])))))$"Pr(>F)"[1]<alpha)))/nsim))
  }
  
  
  if("ANOVA" %in% tests){
    
    pwr.df = rbind(pwr.df,data.frame( Test = "ANOVA",
                                      power = sum(sapply(1:nsim, function(s) I(anova(lm(c(n.dfs[[1]][[s]],n.dfs[[2]][[s]]) ~ c(rep("C",length(n.dfs[[1]][[s]])),rep("T",length(n.dfs[[2]][[s]])))))$'Pr(>F)'[1]<alpha)))/nsim,
                                      FP = sum(sapply(1:nsim, function(s) I(anova(lm(c(a.dfs[[1]][[s]],a.dfs[[2]][[s]]) ~ c(rep("C",length(a.dfs[[1]][[s]])),rep("T",length(a.dfs[[2]][[s]])))))$'Pr(>F)'[1]<alpha)))/nsim))
  }
  
  
  if("Non-parametric ANOVA" %in% tests){
    
    pwr.df = rbind(pwr.df,data.frame( Test = "Non-Parametric ANOVA",
                                      power = sum(sapply(1:nsim, function(s) I(anova(lm(rank(c(n.dfs[[1]][[s]],n.dfs[[2]][[s]])) ~ c(rep("C",length(n.dfs[[1]][[s]])),rep("T",length(n.dfs[[2]][[s]])))))$'Pr(>F)'[1]<alpha)))/nsim,
                                      FP = sum(sapply(1:nsim, function(s) I(anova(lm(rank(c(a.dfs[[1]][[s]],a.dfs[[2]][[s]])) ~ c(rep("C",length(a.dfs[[1]][[s]])),rep("T",length(a.dfs[[2]][[s]])))))$'Pr(>F)'[1]<alpha)))/nsim))
  }
  
  #### Insert all other functions here
  if ( any(tests %in% c("Permutations (Raw)",
                        "Permutations (MAD)",
                        "Permutations (SD)",
                        "Permutations (GiniMd"))) {

    
          #Should move these eventually
      meanDiff <- function(y, X){
        abs(mean(y[X == 1]) - mean(y[X == 0]))
      }

      madDiff <- function(y, X){
        abs((mad(y[X == 1], constant = 1) - mad(y[X == 0], constant = 1)))
      }


      giniDiff <- function(y, X){
        abs((Hmisc::GiniMd(y[X == 1]) - Hmisc::GiniMd(y[X == 0])))
      }

      sdDiff <- function(y, X){
        abs((sd(y[X == 1]) - sd(y[X == 0])))
      }


      perms <- function(temp_df,test.l){

      actual_mean_diff = NA
      actual_mad_diff = NA
      actual_sd_diff = NA
      actual_gini_diff = NA
      shuffle_mean_diff = NA
      shuffle_mad_diff = NA
      shuffle_sd_diff = NA
      shuffle_gini_diff = NA


      # Get actual absolute stats.
      if("Permutations (Raw)" %in% test.l){
        actual_mean_diff <- meanDiff(temp_df$y, temp_df$X)
      }

      if("Permutations (MAD)" %in% test.l){
        actual_mad_diff <- madDiff(temp_df$y, temp_df$X)
      }

      if("Permutations (SD)" %in% test.l){
        actual_sd_diff <- sdDiff(temp_df$y, temp_df$X)
      }

      if("Permutations (GiniMd)" %in% test.l){
        actual_gini_diff <- giniDiff(temp_df$y, temp_df$X)
      }

      actual_diff = data.frame(actual_mean_diff,
                               actual_mad_diff,
                               actual_sd_diff,
                               actual_gini_diff)
      if(any(is.na(actual_diff))){
        actual_diff <- actual_diff[,colSums(is.na(actual_diff))<nrow(actual_diff), drop=FALSE]
      }

      # Find shuffled stats.
      shuffled_diff <- as.data.frame(data.table::rbindlist(lapply(1:nperm, function(p){
        shuffled_idx <- sample(1:nrow(temp_df))
        shuffled_X <- temp_df$X[shuffled_idx]

        if("Permutations (Raw)" %in% test.l){
          shuffle_mean_diff <- meanDiff(temp_df$y, shuffled_X)
        }

        if("Permutations (MAD)" %in% test.l){
          shuffle_mad_diff <- madDiff(temp_df$y, shuffled_X)
        }

        if("Permutations (SD)" %in% test.l){
          shuffle_sd_diff <- sdDiff(temp_df$y, shuffled_X)
        }

        if("Permutations (GiniMd)" %in% test.l){
          shuffle_gini_diff <- giniDiff(temp_df$y, shuffled_X)
        }

        return(data.frame('Permutations (RAW)' = shuffle_mean_diff,
                          'Permutations (MAD)' = shuffle_mad_diff,
                          'Permutations (SD)' = shuffle_sd_diff,
                          'Permutations (GiniMd)' = shuffle_gini_diff))

      })))

      if(any(is.na(shuffled_diff))){
        shuffled_diff <- shuffled_diff[,colSums(is.na(shuffled_diff))<nrow(shuffled_diff), drop=FALSE]
      }
      quantile_val <- sapply(1:ncol(shuffled_diff),function(x) {quantile(shuffled_diff[,x], 1-(alpha/2))})
      if_sig = shuffled_diff[1,]
      if_sig[1,] <- as.numeric(I(quantile_val < actual_diff))
      return(as.data.frame(if_sig))
    }

    # Calculate power
    power_sig <- rbindlist(lapply(1:nsim, function(s) {

      temp_df <- data.frame(
        y = c(n.dfs[[1]][[s]], n.dfs[[2]][[s]]),
        X = c(rep(0, length(n.dfs[[1]][[s]])), rep(1, length(n.dfs[[2]][[s]])))
      )
      
      return(perms(temp_df,tests))
      
    }))
    
    # if_sig <- unlist(tmp)
    # num_sig <- sum(as.numeric(as.character(if_sig)))
    # ave_pval <- sum(as.numeric(as.character(if_sig)))/nsim
    
    # Calculate FDR
    fp_sig <- rbindlist(lapply(1:nsim, function(s) {
      
      temp_df <- data.frame(
        y = c(a.dfs[[1]][[s]], a.dfs[[2]][[s]]),
        X = c(rep(0, length(a.dfs[[1]][[s]])), rep(1, length(a.dfs[[2]][[s]])))
      )
      
      return(perms(temp_df,tests))
      
    }))
    
    pwr.df <- rbind(pwr.df, data.frame(Test = colnames(power_sig),
                                       power = colSums2(power_sig)/nsim,
                                       FP = colSums2(fp_sig)/nsim))
  }
  
  
  return(pwr.df)
  
}
