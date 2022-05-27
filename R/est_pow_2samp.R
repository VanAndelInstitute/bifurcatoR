est_pow_2samp = function(n1,n2,alpha,nsim,modes,dist,params,tests,nperm){
  n.dfs=list()
  a.dfs=list()
  if(dist=="norm"){
    if(modes == 2){
      n1_1 = floor(params$p_1*n1)
      n2_1 = floor((1-params$p_1)*n1)
      
      n1_2 = floor(params$p_2*n2)
      n2_2 = floor((1-params$p_2)*n2)
      
      n.dfs[[1]] = lapply(1:nsim,function(x) c(rnorm(n1_1,params$mu1_1,params$sd1_1),rnorm(n2_1,params$mu2_1,params$sd2_1)))
      
      n.dfs[[2]] = lapply(1:nsim,function(x) c(rnorm(n1_2,params$mu1_2,params$sd1_2),rnorm(n2_2,params$mu2_2,params$sd2_2)))
      
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
      if(dist == "weibull"){
        if(modes == 1){
          
          # n.dfs[[1]] = lapply(1:nsim,function(x) rnorm(n1,0,1))
          # n.dfs[[2]] = lapply(1:nsim,function(x) rnorm(n2,0+mean,1*var))
          # 
          # a.dfs[[1]] = lapply(1:nsim,function(x) rnorm(n1,0,1))
          # a.dfs[[2]] = lapply(1:nsim,function(x) rnorm(n2,0,1))
          
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
                                      power = sum(sapply(1:nsim, function(s) I(anova(lm(c(n.dfs[[1]][[s]],n.dfs[[2]][[s]]) ~ c(rep("C",n1),rep("T",n2))))$'Pr(>F)'[1]<alpha)))/nsim,
                                      FP = sum(sapply(1:nsim, function(s) I(anova(lm(c(a.dfs[[1]][[s]],a.dfs[[2]][[s]]) ~ c(rep("C",n1),rep("T",n2))))$'Pr(>F)'[1]<alpha)))/nsim))
  }
  
  #### Insert all other functions here
  
  return(pwr.df)
  
}
