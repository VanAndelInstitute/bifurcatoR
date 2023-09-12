#' est_pow
#'
#' @param   n       number of [somethings]
#' @param   alpha   default significance level (0.05)
#' @param   nsim    number of simulations (20)
#' @param   dist    generating distribution
#' @param   params  parameters for the generating distribution
#' @param   tests   names of tests to run
#'
#' @return          a power estimate
#'
#' @import          mclust
#' @import          diptest
#' @import          mousetrap
#' @import          LaplacesDemon
#' @import          multimode
#' @import          Hmisc
#' @import          twosamples
#' @import          mixR
#'
#' @export
est_pow = function(n,alpha,nsim,dist,params,tests,nboot){
  if(dist=="norm"){

    n1 = floor(params$p*n)
    n2 = floor((1-params$p)*n)

    n.dfs = lapply(1:nsim,function(x) c(rnorm(n1,params$mu1,params$sd1),rnorm(n2,params$mu2,params$sd2)))


    a.dfs = lapply(1:nsim,function(x) c(rnorm(n,
                                              (n1 * params$mu1 + n2 * params$mu2)/n,
                                              sqrt(((n1-1)*params$sd1^2 + (n2-1)*params$sd2^2)/(n-2)))))

  } else {
    if(dist=="beta"){
      n.dfs = lapply(1:nsim,function(x) rbeta(n,params$s1,params$s2))

      a.dfs = lapply(1:nsim,function(x) rbeta(n,2,2))

    } else {
      if(dist=="weib"){
      #print(params)
      n1 = floor(params$p*n)
      n2 = floor((1-params$p)*n)

      n.dfs = lapply(1:nsim,function(x) c(rweibull(n1,shape=params$sp1,scale=params$sc1),rweibull(n2,shape=params$sp2,scale=params$sc2)))


      a.dfs = lapply(1:nsim,function(x) c(rweibull(n,
                                                shape = (n1 * params$sp1 + n2 * params$sp2)/n,
                                                scale = (n1 * params$sc1 + n2 * params$sc2)/n)))

     }
    }
  }
  pwr.df = NULL


  if("mclust" %in% tests){
    pwr.df = rbind(pwr.df,data.frame(N = n, Test = "Mclust",
                                     power = sum(sapply(n.dfs, function(s) I(mclust::mclustBootstrapLRT(as.data.frame(s),modelName="E",verbose=F,maxG=1,nboot=nboot)$p.value<alpha)))/nsim,
                                     FP = sum(sapply(a.dfs, function(s) I(mclust::mclustBootstrapLRT(as.data.frame(s),modelName="E",verbose=F,maxG=1,nboot=nboot)$p.value<alpha)))/nsim ))
  }


  if("WmixR" %in% tests){
    pwr.df = rbind(pwr.df,data.frame(N = n, Test = "Weibull mixR",
                                     power = sum(sapply(n.dfs, function(s) I(bs_lrt(unlist(s), H0=1, H1=2, family="weibull", nboot=nboot)$pvalue<alpha)))/nsim,
                                     FP = sum(sapply(a.dfs, function(s) I(bs_lrt(unlist(s), H0=1, H1=2, family="weibull", nboot=nboot)$p.value<alpha)))/nsim ))
  }


  if("GmixR" %in% tests){
    pwr.df = rbind(pwr.df,data.frame(N = n, Test = "Gaussian mixR",
                                     power = sum(sapply(n.dfs, function(s) I(bs_lrt(unlist(s), H0=1, H1=2, family="normal", nboot=nboot)$pvalue<alpha)))/nsim,
                                     FP = sum(sapply(a.dfs, function(s) I(bs_lrt(unlist(s), H0=1, H1=2, family="normal", nboot=nboot)$p.value<alpha)))/nsim ))
  }


  if("GamixR" %in% tests){
    pwr.df = rbind(pwr.df,data.frame(N = n, Test = "Gamma mixR",
                                     power = sum(sapply(n.dfs, function(s) I(bs_lrt(unlist(s), H0=1, H1=2, family="gamma", nboot=nboot)$pvalue<alpha)))/nsim,
                                     FP = sum(sapply(a.dfs, function(s) I(bs_lrt(unlist(s), H0=1, H1=2, family="gamma", nboot=nboot)$p.value<alpha)))/nsim ))
  }

  #Pulling bootstrapped BC until we establish a cut-off
  # if("mt" %in% tests){
  #   pwr.df = rbind(pwr.df,data.frame(N = n, Test = "Bimodality Coefficient",
  #                                    power = sum(sapply(n.dfs, function(s)  I(BC_boot(s,nboot) < 0.05)))/nsim,
  #                                    FP = sum(sapply(a.dfs, function(s)  I(BC_boot(s,nboot) < 0.05)))/nsim))
  # }

  if("mt" %in% tests){
    pwr.df = rbind(pwr.df,data.frame(N = n, Test = "Bimodality Coefficient",
                                     power = sum(sapply(n.dfs, function(s)  I(mousetrap::mt_check_bimodality(as.data.frame(s),method="BC")$BC > (5/9))))/nsim,
                                     FP = sum(sapply(a.dfs, function(s)  I(mousetrap::mt_check_bimodality(as.data.frame(s),method="BC")$BC > (5/9))))/nsim))
  }

  if("dip" %in% tests){
    pwr.df = rbind(pwr.df,data.frame(N = n, Test = "Hartigans' dip test",
                                     power = sum(sapply(n.dfs, function(s)  I(multimode::modetest(s,mod0 = 1,method = "HH",B=nboot)$p.value<alpha)))/nsim,
                                     FP = sum(sapply(a.dfs, function(s)  I(multimode::modetest(s,mod0 = 1,method = "HH",B=nboot)$p.value<alpha)))/nsim))
  }

  if("SI" %in% tests){
    pwr.df = rbind(pwr.df,data.frame(N = n, Test = "Silverman Bandwidth test",
                                     power = sum(sapply(n.dfs, function(s)  I(multimode::modetest(s,mod0 = 1,method = "SI",B=nboot)$p.value<alpha)))/nsim,
                                     FP = sum(sapply(a.dfs, function(s)  I(multimode::modetest(s,mod0 = 1,method = "SI",B=nboot)$p.value<alpha)))/nsim))
  }


  if("HY" %in% tests){
    pwr.df = rbind(pwr.df,data.frame(N = n, Test = "Hall and York Bandwidth test",
                                     power = sum(sapply(n.dfs, function(s)  I(multimode::modetest(s,mod0 = 1,method = "HY",B=nboot)$p.value<alpha)))/nsim,
                                     FP = sum(sapply(a.dfs, function(s)  I(multimode::modetest(s,mod0 = 1,method = "HY",B=nboot)$p.value<alpha)))/nsim))
  }


  if("CH" %in% tests){
    pwr.df = rbind(pwr.df,data.frame(N = n, Test = "Cheng and Hall Excess Mass",
                                     power = sum(sapply(n.dfs, function(s)  I(multimode::modetest(s,mod0 = 1,method = "CH",B=nboot)$p.value<alpha)))/nsim,
                                     FP = sum(sapply(a.dfs, function(s)  I(multimode::modetest(s,mod0 = 1,method = "CH",B=nboot)$p.value<alpha)))/nsim))
  }

  if("ACR" %in% tests){
    pwr.df = rbind(pwr.df,data.frame(N = n, Test = "Ameijeiras-Alonso et al. Excess Mass",
                                     power = sum(sapply(n.dfs, function(s)  I(multimode::modetest(s,mod0 = 1,method = "ACR",B=nboot)$p.value<alpha)))/nsim,
                                     FP = sum(sapply(a.dfs, function(s)  I(multimode::modetest(s,mod0 = 1,method = "ACR",B=nboot)$p.value<alpha)))/nsim))
  }

  if("FM" %in% tests){
    pwr.df = rbind(pwr.df,data.frame(N = n, Test = "Fisher and Marron Carmer-von Mises",
                                     power = sum(sapply(n.dfs, function(s)  I(multimode::modetest(s,mod0 = 1,method = "FM",B=nboot)$p.value<alpha)))/nsim,
                                     FP = sum(sapply(a.dfs, function(s)  I(multimode::modetest(s,mod0 = 1,method = "FM",B=nboot)$p.value<alpha)))/nsim))
  }



  return(pwr.df)

}
