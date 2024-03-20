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
#' @import          data.table
#'
#' @export




est_pow_2samp = function(n1,n2,alpha,nsim,modes,dist,params,tests,nperm){
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

      } else {
        if (dist == "lnorm") {
          if(modes == 1){
            logmean = EpiNow2::convert_to_logmean(exp(0.5) + params$mean, params$v_scale*exp((1/2)*1)*sqrt(exp(1) - 1) )
            logsd = EpiNow2::convert_to_logsd(exp(0.5) + params$mean, params$v_scale*exp((1/2)*1)*sqrt(exp(1) - 1) )
            n.dfs[[1]] = lapply(1:nsim,function(x) rlnorm(n1,meanlog=0,sdlog=1))
            n.dfs[[2]] = lapply(1:nsim,function(x) rlnorm(n2,meanlog=logmean,sdlog=logsd))

            #Create a tragically over-simplified mixture of the two by averaging the logmeans
            #and pooling the logsds
            mu.pool = (n1+n2*(1+logmean))/(n1+n2)
            sd.pool = sqrt(((n1-1)*1^2 + (n2-1)*(logsd)^2)/(n1+n2-2))

            a.dfs[[1]] = lapply(1:nsim,function(x) rlnorm(n1,meanlog = mu.pool,sdlog =sd.pool))
            a.dfs[[2]] = lapply(1:nsim,function(x) rlnorm(n2,meanlog = mu.pool,sdlog =sd.pool))
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

  if("Permutations (Raw)" %in% tests){


    power = sum(sapply(1:nsim, function(s){

    temp_df <- data.frame(
      y = c(n.dfs[[1]][[s]], n.dfs[[2]][[s]]),
      X = c(rep(0, length(n.dfs[[1]][[s]])), rep(1, length(n.dfs[[2]][[s]])))
    )

    return(I(unlist(permutation_tests(temp_df,nperm,"meanDiff",alpha)$p)<alpha))

    }))/nsim

    FP = sum(sapply(1:nsim, function(s){

      temp_df <- data.frame(
        y = c(a.dfs[[1]][[s]], a.dfs[[2]][[s]]),
        X = c(rep(0, length(a.dfs[[1]][[s]])), rep(1, length(a.dfs[[2]][[s]])))
      )

      return(I(unlist(permutation_tests(temp_df,nperm,"meanDiff",alpha)$p)<alpha))

    }))/nsim

    pwr.df = rbind(pwr.df,data.frame( Test = "Permutations (Raw)",power,FP))

  }

  if("Permutations (SD)" %in% tests){


    power = sum(sapply(1:nsim, function(s){

      temp_df <- data.frame(
        y = c(n.dfs[[1]][[s]], n.dfs[[2]][[s]]),
        X = c(rep(0, length(n.dfs[[1]][[s]])), rep(1, length(n.dfs[[2]][[s]])))
      )

      return(I(unlist(permutation_tests(temp_df,nperm,"sdDiff",alpha)$p)<alpha))

    }))/nsim

    FP = sum(sapply(1:nsim, function(s){

      temp_df <- data.frame(
        y = c(a.dfs[[1]][[s]], a.dfs[[2]][[s]]),
        X = c(rep(0, length(a.dfs[[1]][[s]])), rep(1, length(a.dfs[[2]][[s]])))
      )

      return(I(unlist(permutation_tests(temp_df,nperm,"sdDiff",alpha)$p)<alpha))

    }))/nsim

    pwr.df = rbind(pwr.df,data.frame( Test = "Permutations (SD)",power,FP))

  }

  if("Permutations (MAD)" %in% tests){

    power = sum(sapply(1:nsim, function(s){

      temp_df <- data.frame(
        y = c(n.dfs[[1]][[s]], n.dfs[[2]][[s]]),
        X = c(rep(0, length(n.dfs[[1]][[s]])), rep(1, length(n.dfs[[2]][[s]])))
      )

      return(I(unlist(permutation_tests(temp_df,nperm,"madDiff",alpha)$p)<alpha))

    }))/nsim

    FP = sum(sapply(1:nsim, function(s){

      temp_df <- data.frame(
        y = c(a.dfs[[1]][[s]], a.dfs[[2]][[s]]),
        X = c(rep(0, length(a.dfs[[1]][[s]])), rep(1, length(a.dfs[[2]][[s]])))
      )

      return(I(unlist(permutation_tests(temp_df,nperm,"madDiff",alpha)$p)<alpha))

    }))/nsim

    pwr.df = rbind(pwr.df,data.frame( Test = "Permutations (MAD)",power,FP))

  }

  if("Permutations (Gini)" %in% tests){


    power = sum(sapply(1:nsim, function(s){

      temp_df <- data.frame(
        y = c(n.dfs[[1]][[s]], n.dfs[[2]][[s]]),
        X = c(rep(0, length(n.dfs[[1]][[s]])), rep(1, length(n.dfs[[2]][[s]])))
      )

      return(I(unlist(permutation_tests(temp_df,nperm,"giniDiff",alpha)$p)<alpha))

    }))/nsim

    FP = sum(sapply(1:nsim, function(s){

      temp_df <- data.frame(
        y = c(a.dfs[[1]][[s]], a.dfs[[2]][[s]]),
        X = c(rep(0, length(a.dfs[[1]][[s]])), rep(1, length(a.dfs[[2]][[s]])))
      )

      return(I(unlist(permutation_tests(temp_df,nperm,"giniDiff",alpha)$p)<alpha))

    }))/nsim

    pwr.df = rbind(pwr.df,data.frame( Test = "Permutations (Gini)",power,FP))

  }

  return(pwr.df)

}
