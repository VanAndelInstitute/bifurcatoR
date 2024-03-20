#' bifurcatoR_Analysis
#'
#' @param   data    data frame with 2 columns, a two-level group column and numeric value column
#' @param   alpha   default significance level (0.05)
#' @param   nboot   number of bootstraps or permutations
#' @param   tests   names of tests to run
#'
#' @return          a data frame where each row corresponds to the results of test, p-values, test stats, and confidence intervals where possible
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


bifurcatoR_Analysis = function(data,tests,nboot,alpha){
  res <- data.frame(Test = character(),
                    nboot = numeric(),
                    p.value = numeric(),
                    Stat = numeric(),
                    CI = numeric()
  )

  if("mclust" %in% tests){
    tmp = mclust::mclustBootstrapLRT(data$value,modelName="E",verbose=F,maxG=1,nboot=nboot)
    res = rbind(res,data.frame(Test = "Mclust", nboot = nboot,p.value = tmp$p.value,Stat = tmp$obs ,CI = paste(round(quantile(tmp$boot,p=c(alpha/2,1-alpha/2)),floor(log10(nboot)) + 1),collapse=", " )))
  }

  if("mt" %in% tests){

    s = as.data.frame(data$value)

    tmp = mousetrap::mt_check_bimodality(s,method="BC")$BC

    tmp.boot = unname(unlist(lapply(1:nboot, function(x) mousetrap::mt_check_bimodality(as.data.frame(s[sample(1:nrow(s),replace=T),]),method="BC")$BC)))

    res = rbind(res,data.frame(Test = "Bimodality Coeficient", nboot = nboot,p.value =  as.numeric(I(tmp < (5/9))),Stat = unname(tmp) ,CI = paste(round(quantile(tmp.boot,p=c(alpha/2,1-alpha/2)),floor(log10(nboot)) + 1),collapse=", " )))

  }

  if("SI" %in% tests){
    tmp = multimode::modetest(data$value,mod0 = 1,method = "SI",B=nboot)

    res = rbind(res,data.frame(Test = tmp$method, nboot = nboot,p.value = tmp$p.value,Stat = unname(tmp$statistic) ,CI = "Not yet available"))

  }

  if("dip" %in% tests){
    tmp = multimode::modetest(data$value,mod0 = 1,method = "HH",B=nboot)

    res = rbind(res,data.frame(Test = tmp$method, nboot = nboot,p.value = tmp$p.value,Stat = unname(tmp$statistic) ,CI = "Not yet available"))

  }

  if("HY" %in% tests){
    tmp = multimode::modetest(data$value,mod0 = 1,method = "HY",B=nboot)

    res = rbind(res,data.frame(Test = tmp$method, nboot = nboot,p.value = tmp$p.value,Stat = unname(tmp$statistic) ,CI = "Not yet available"))

  }

  if("CH" %in% tests){
    tmp = multimode::modetest(data$value,mod0 = 1,method = "CH",B=nboot)

    res = rbind(res,data.frame(Test = tmp$method, nboot = nboot,p.value = tmp$p.value,Stat = unname(tmp$statistic) ,CI = "Not yet available"))

  }

  if("ACR" %in% tests){
    tmp = multimode::modetest(data$value,mod0 = 1,method = "ACR",B=nboot)

    res = rbind(res,data.frame(Test = tmp$method, nboot = nboot,p.value = tmp$p.value,Stat = unname(tmp$statistic) ,CI = "Not yet available"))

  }

  if("FM" %in% tests){
    tmp = multimode::modetest(data$value,mod0 = 1,method = "FM",B=nboot)

    res = rbind(res,data.frame(Test = tmp$method, nboot = nboot,p.value = tmp$p.value,Stat = unname(tmp$statistic) ,CI = "Not yet available"))

  }

  if("ks" %in% tests){

    tmp = twosamples::ks_test(data$value[data$group == unique(data$group)[1]] ,data$value[data$group == unique(data$group)[2]],nboots=nboot,keep.boots=T)

    res = rbind(res,data.frame(Test = "Kolmogorov-Smirnov Test", nboot = nboot,p.value = tmp[[2]],Stat = tmp[[1]] ,CI = paste(round(quantile(attributes(tmp)$bootstraps,p=c(alpha/2,1-alpha/2)),floor(log10(nboot)) + 1),collapse=", " )))


  }


  if("cvm" %in% tests){


    tmp = twosamples::cvm_test(data$value[data$group == unique(data$group)[1]] ,data$value[data$group == unique(data$group)[2]],nboots=nboot,keep.boots=T)

    res = rbind(res,data.frame(Test = "Cramer-von Mises Test", nboot = nboot,p.value = tmp[[2]],Stat = tmp[[1]] ,CI = paste(round(quantile(attributes(tmp)$bootstraps,p=c(alpha/2,1-alpha/2)),floor(log10(nboot)) + 1),collapse=", " )))

  }


  if("dts" %in% tests){

    tmp = twosamples::dts_test(data$value[data$group == unique(data$group)[1]] ,data$value[data$group == unique(data$group)[2]],nboots=nboot,keep.boots=T)

    res = rbind(res,data.frame(Test = "DTS Test", nboot = nboot,p.value = tmp[[2]],Stat = tmp[[1]] ,CI = paste(round(quantile(attributes(tmp)$bootstraps,p=c(alpha/2,1-alpha/2)),floor(log10(nboot)) + 1),collapse=", " )))

  }

  if("ad" %in% tests){

    tmp = twosamples::ad_test(data$value[data$group == unique(data$group)[1]] ,data$value[data$group == unique(data$group)[2]],nboots=nboot,keep.boots=T)

    res = rbind(res,data.frame(Test = "Aderson-Darling Test", nboot = nboot,p.value = tmp[[2]],Stat = tmp[[1]] ,CI = paste(round(quantile(attributes(tmp)$bootstraps,p=c(alpha/2,1-alpha/2)),floor(log10(nboot)) + 1),collapse=", " )))

  }

  if("Levene" %in% tests){

    tmp = car::leveneTest(lm(value~as.factor(data$group),data=data))

    res = rbind(res,data.frame(Test = "Levene's Test", nboot = NA ,p.value =tmp$'Pr(>F)',Stat = tmp$'F value' ,CI = "Not yet available"))

  }



  if ("Permutations (Raw)" %in% tests) {

    temp_df = data.frame(
      y = data$value,
      X= as.numeric(as.factor(data$group))-1
    )

    tmp = permutation_tests(temp_df,nboot,"meanDiff",alpha)

    res = rbind(res,data.frame(Test = "Permutations (Raw)", nboot = nboot ,p.value = tmp$p ,Stat = tmp$diff ,CI = paste(round(quantile(tmp$crit,p=c(alpha/2,1-alpha/2)),floor(log10(nboot)) + 1),collapse=", " )))

  }

  if ("Permutations (SD)" %in% tests) {

    temp_df = data.frame(
      y = data$value,
      X= as.numeric(as.factor(data$group))-1
    )

    tmp = permutation_tests(temp_df,nboot,"sdDiff",alpha)

    res = rbind(res,data.frame(Test = "Permutations (SD)", nboot = nboot ,p.value = tmp$p ,Stat = tmp$diff ,CI = paste(round(quantile(tmp$crit,p=c(alpha/2,1-alpha/2)),floor(log10(nboot)) + 1),collapse=", " )))

  }

  if ("Permutations (MAD)" %in% tests) {

    temp_df = data.frame(
      y = data$value,
      X= as.numeric(as.factor(data$group))-1
    )

    tmp = permutation_tests(temp_df,nboot,"madDiff",alpha)

    res = rbind(res,data.frame(Test = "Permutations (MAD)", nboot = nboot ,p.value = tmp$p ,Stat = tmp$diff ,CI = paste(round(quantile(tmp$crit,p=c(alpha/2,1-alpha/2)),floor(log10(nboot)) + 1),collapse=", " )))

  }


  if ("Permutations (GiniMd)" %in% tests) {

    temp_df = data.frame(
      y = data$value,
      X= as.numeric(as.factor(data$group))-1
    )

    tmp = permutation_tests(temp_df,nboot,"giniDiff",alpha)

    res = rbind(res,data.frame(Test = "Permutations (GiniMd)", nboot = nboot ,p.value = tmp$p ,Stat = tmp$diff ,CI = paste(round(quantile(tmp$crit,p=c(alpha/2,1-alpha/2)),floor(log10(nboot)) + 1),collapse=", " )))

  }



  if("ANOVA" %in% tests){

    tmp = lm(value~as.factor(data$group),data=data)

    res = rbind(res,data.frame(Test = "ANOVA", nboot = NA ,p.value = summary(tmp)$coefficients[2,4] ,Stat = tmp$coefficients[2] ,CI = paste(round(confint(tmp)[2,],floor(log10(nboot)) + 1),collapse=", " )))

  }


  if("Non-parametric ANOVA" %in% tests){

    tmp = lm(rank(value)~as.factor(data$group),data=data)

    res = rbind(res,data.frame(Test = "Non-parametric ANOVA", nboot = NA ,p.value = summary(tmp)$coefficients[2,4] ,Stat = tmp$coefficients[2] ,CI = paste(round(confint(tmp)[2,],floor(log10(nboot)) + 1),collapse=", " )))


  }


  if("WmixR" %in% tests){


    tmp = bs_lrt(data$value, H0=1, H1=2, family="weibull", nboot=nboot)

    res = rbind(res,data.frame(Test = "Weibull mixR", nboot = nboot ,p.value = tmp$pvalue ,Stat = tmp$w0 ,CI = paste(round(quantile(tmp$w1,p=c(alpha/2,1-alpha/2)),floor(log10(nboot)) + 1),collapse=", " )))


  }

  if("LNmixR" %in% tests){

    tmp = bs_lrt(data$value, H0=1, H1=2, family="lnorm", nboot=nboot)
    res = rbind(res,data.frame(Test = "Lognormal mixR", nboot = nboot ,p.value = tmp$pvalue ,Stat = tmp$w0 ,CI = paste(round(quantile(tmp$w1,p=c(alpha/2,1-alpha/2)),floor(log10(nboot)) + 1),collapse=", " )))

  }


  if("GmixR" %in% tests){

    tmp = bs_lrt(data$value, H0=1, H1=2, family="normal", nboot=nboot)
    res = rbind(res,data.frame(Test = "Gaussian mixR", nboot = nboot ,p.value = tmp$pvalue ,Stat = tmp$w0 ,CI = paste(round(quantile(tmp$w1,p=c(alpha/2,1-alpha/2)),floor(log10(nboot)) + 1),collapse=", " )))

  }


  if("GamixR" %in% tests){

    tmp = bs_lrt(data$value, H0=1, H1=2, family="gamma", nboot=nboot)
    res = rbind(res,data.frame(Test = "Gamma mixR", nboot = nboot ,p.value = tmp$pvalue ,Stat = tmp$w0 ,CI = paste(round(quantile(tmp$w1,p=c(alpha/2,1-alpha/2)),floor(log10(nboot)) + 1),collapse=", " )))

  }



  return(res)
}
