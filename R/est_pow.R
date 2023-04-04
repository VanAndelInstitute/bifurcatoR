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
  if("dip" %in% tests){
    pwr.df = rbind(pwr.df,data.frame(N = n, Test = "Hartigans' dip test",
                                     power = sum(sapply(n.dfs, function(s) I(diptest::dip.test(s)$p.value<alpha)))/nsim,
                                     FP = sum(sapply(a.dfs, function(s) I(diptest::dip.test(s)$p.value<alpha)))/nsim))
  }

  if("mclust" %in% tests){
    pwr.df = rbind(pwr.df,data.frame(N = n, Test = "Mclust",
                                     power = sum(sapply(n.dfs, function(s) I(mclust::mclustBootstrapLRT(as.data.frame(s),modelName="E",verbose=F,maxG=1,nboot=nboot)$p.value<alpha)))/nsim,
                                     FP = sum(sapply(a.dfs, function(s) I(mclust::mclustBootstrapLRT(as.data.frame(s),modelName="E",verbose=F,maxG=1,nboot=nboot)$p.value<alpha)))/nsim ))
  }

  ## sigclust is giving me issues so I've dropped it for now.
  # if("sigclust" %in% tests){
  #   pwr.df = rbind(pwr.df,data.frame(N = n, Test = "2-Mean cluster",
  #                                    power = sum(sapply(n.dfs, function(s) I(sigclust::sigclust(as.data.frame(s),nsim=999)@pval<alpha)))/nsim,
  #                                    FP = sum(sapply(a.dfs, function(s) I(sigclust::sigclust(as.data.frame(s),nsim=999)@pval<alpha)))/nsim))
  # }

  ## Laplace's Demon is a useful method for bimodality, but requires the user to input the minimum propotion of the sample needed to define a mode
  ## This is being removed for now to limit the number of parameters a user must input.
  # if("isbimo" %in% tests){
  #  pwr.df = rbind(pwr.df,data.frame(N = n, Test = "Laplace's Demon",
  #                                   power = sum(sapply(n.dfs, function(s) LaplacesDemon::is.multimodal(s)))/nsim,
  #                                   FP =  sum(sapply(a.dfs, function(s) LaplacesDemon::is.multimodal(s)))/nsim))
  #}

  if("mt" %in% tests){
    pwr.df = rbind(pwr.df,data.frame(N = n, Test = "Mouse Trap",
                                     power = sum(sapply(n.dfs, function(s)  I(mousetrap::mt_check_bimodality(as.data.frame(s))$BC > 0.555)))/nsim,
                                     FP = sum(sapply(a.dfs, function(s)  I(mousetrap::mt_check_bimodality(as.data.frame(s))$BC > 0.555)))/nsim))
  }

  return(pwr.df)

}
