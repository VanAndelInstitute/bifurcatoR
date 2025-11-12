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

mclust <- function(data, nboot, alpha) {
  tmp = mclust::mclustBootstrapLRT(
    data$value,
    modelName = "E",
    verbose = F,
    maxG = 1,
    nboot = nboot
  )
  q <- quantile(tmp$boot, p = c(alpha / 2, 1 - alpha / 2))
  ci <- round(q, floor(log10(nboot)) + 1)
  data.frame(
    Test = "Mclust",
    nboot = nboot,
    p.value = tmp$p.value,
    Stat = tmp$obs,
    CI = paste(ci, collapse = ", ")
  )
}

mt <- function(data, nboot, alpha) {
  s = as.data.frame(data$value)
  tmp = mousetrap::mt_check_bimodality(s, method = "BC")$BC
  boot.list <- lapply(1:nboot, function(x) {
    mousetrap::mt_check_bimodality(
      as.data.frame(s[sample(1:nrow(s), replace = T), ]),
      method = "BC"
    )$BC
  })
  tmp.boot = unname(unlist(boot.list))
  q <- quantile(tmp.boot, p = c(alpha / 2, 1 - alpha / 2))
  ci <- round(q, floor(log10(nboot)) + 1)
  data.frame(
    Test = "Bimodality Coefficient",
    nboot = nboot,
    p.value = as.numeric(I(tmp < (5 / 9))),
    Stat = unname(tmp),
    CI = paste(ci, collapse = ", ")
  )
}

SI <- function(data, nboot) {
  tmp = multimode::modetest(data$value, mod0 = 1, method = "SI", B = nboot)
  data.frame(
    Test = "Silverman Bandwidth test",
    nboot = nboot,
    p.value = tmp$p.value,
    Stat = unname(tmp$statistic),
    CI = "Not yet available"
  )
}

dip <- function(data, nboot) {
  tmp = multimode::modetest(data$value, mod0 = 1, method = "HH", B = nboot)
  res = rbind(
    res,
    data.frame(
      Test = "Hartigans' dip test",
      nboot = nboot,
      p.value = tmp$p.value,
      Stat = unname(tmp$statistic),
      CI = "Not yet available"
    )
  )
}

HY <- function(data, nboot) {
  tmp = multimode::modetest(data$value, mod0 = 1, method = "HY", B = nboot)
  data.frame(
    Test = "Hall and York Bandwidth test",
    nboot = nboot,
    p.value = tmp$p.value,
    Stat = unname(tmp$statistic),
    CI = "Not yet available"
  )
}

CH <- function(data, nboot) {
  tmp = multimode::modetest(data$value, mod0 = 1, method = "CH", B = nboot)
  data.frame(
    Test = "Cheng and Hall Excess Mass",
    nboot = nboot,
    p.value = tmp$p.value,
    Stat = unname(tmp$statistic),
    CI = "Not yet available"
  )
}

ACR <- function(data, nboot) {
  tmp = multimode::modetest(data$value, mod0 = 1, method = "ACR", B = nboot)
  data.frame(
    Test = "Ameijeiras-Alonso et al. Excess Mass",
    nboot = nboot,
    p.value = tmp$p.value,
    Stat = unname(tmp$statistic),
    CI = "Not yet available"
  )
}

FM <- function(data, nboot) {
  tmp = multimode::modetest(data$value, mod0 = 1, method = "FM", B = nboot)
  data.frame(
    Test = "Fisher and Marron Carmer-von Mises",
    nboot = nboot,
    p.value = tmp$p.value,
    Stat = unname(tmp$statistic),
    CI = "Not yet available"
  )
}

ks <- function(data, nboot, alpha) {
  tmp = twosamples::ks_test(
    data$value[data$group == unique(data$group)[1]],
    data$value[data$group == unique(data$group)[2]],
    nboots = nboot,
    keep.boots = T
  )
  q <- quantile(attributes(tmp)$bootstraps, p = c(alpha / 2, 1 - alpha / 2))
  ci <- round(q, floor(log10(nboot)) + 1)
  data.frame(
    Test = "Kolmogorov-Smirnov Test",
    nboot = nboot,
    p.value = tmp[[2]],
    Stat = tmp[[1]],
    CI = paste(ci, collapse = ", ")
  )
}

cvm <- function(data, nboot, alpha) {
  tmp = twosamples::cvm_test(
    data$value[data$group == unique(data$group)[1]],
    data$value[data$group == unique(data$group)[2]],
    nboots = nboot,
    keep.boots = T
  )
  q <- quantile(attributes(tmp)$bootstraps, p = c(alpha / 2, 1 - alpha / 2))
  ci <- round(q, floor(log10(nboot)) + 1)
  data.frame(
    Test = "Cramer-von Mises Test",
    nboot = nboot,
    p.value = tmp[[2]],
    Stat = tmp[[1]],
    CI = paste(ci, collapse = ", ")
  )
}

dts <- function(data, nboot, alpha) {
  tmp = twosamples::dts_test(
    data$value[data$group == unique(data$group)[1]],
    data$value[data$group == unique(data$group)[2]],
    nboots = nboot,
    keep.boots = T
  )
  q <- quantile(attributes(tmp)$bootstraps, p = c(alpha / 2, 1 - alpha / 2))
  ci <- round(q, floor(log10(nboot)) + 1)
  data.frame(
    Test = "DTS Test",
    nboot = nboot,
    p.value = tmp[[2]],
    Stat = tmp[[1]],
    CI = paste(ci, collapse = ", ")
  )
}

ad <- function(data, nboot, alpha) {
  tmp = twosamples::ad_test(
    data$value[data$group == unique(data$group)[1]],
    data$value[data$group == unique(data$group)[2]],
    nboots = nboot,
    keep.boots = T
  )
  q <- quantile(attributes(tmp)$bootstraps, p = c(alpha / 2, 1 - alpha / 2))
  ci <- round(q, floor(log10(nboot)) + 1)
  data.frame(
    Test = "Aderson-Darling Test",
    nboot = nboot,
    p.value = tmp[[2]],
    Stat = tmp[[1]],
    CI = paste(ci, collapse = ", ")
  )
}

Levene <- function(data, nboot) {
  tmp = car::leveneTest(lm(value ~ as.factor(data$group), data = data))
  data.frame(
    Test = "Levene's Test",
    nboot = NA,
    p.value = tmp$'Pr(>F)'[1],
    Stat = tmp$'F value'[1],
    CI = "Not yet available"
  )
}

`Permutations (Raw)` <- function(data, nboot, alpha) {
  temp_df = data.frame(
    y = data$value,
    X = as.numeric(as.factor(data$group)) - 1
  )
  tmp = permutation_tests(temp_df, nboot, "meanDiff", alpha)
  q <- quantile(tmp$crit, p = c(alpha / 2, 1 - alpha / 2))
  ci <- round(q, floor(log10(nboot)) + 1)
  data.frame(
    Test = "Permutations (Raw)",
    nboot = nboot,
    p.value = tmp$p,
    Stat = tmp$diff,
    CI = paste(ci, collapse = ", ")
  )
}

`Permutations (SD)` <- function(data, nboot, alpha) {
  temp_df = data.frame(
    y = data$value,
    X = as.numeric(as.factor(data$group)) - 1
  )
  tmp = permutation_tests(temp_df, nboot, "sdDiff", alpha)
  q <- quantile(tmp$crit, p = c(alpha / 2, 1 - alpha / 2))
  ci <- round(q, floor(log10(nboot)) + 1)
  data.frame(
    Test = "Permutations (SD)",
    nboot = nboot,
    p.value = tmp$p,
    Stat = tmp$diff,
    CI = paste(ci, collapse = ", ")
  )
}


`Permutations (MAD)` <- function(data, nboot, alpha) {
  temp_df = data.frame(
    y = data$value,
    X = as.numeric(as.factor(data$group)) - 1
  )
  tmp = permutation_tests(temp_df, nboot, "madDiff", alpha)
  q <- quantile(tmp$crit, p = c(alpha / 2, 1 - alpha / 2))
  ci <- round(q, floor(log10(nboot)) + 1)
  data.frame(
    Test = "Permutations (MAD)",
    nboot = nboot,
    p.value = tmp$p,
    Stat = tmp$diff,
    CI = paste(ci, collapse = ", ")
  )
}

`Permutations (GiniMd)` <- function(data, nboot, alpha) {
  temp_df = data.frame(
    y = data$value,
    X = as.numeric(as.factor(data$group)) - 1
  )
  tmp = permutation_tests(temp_df, nboot, "giniDiff", alpha)
  q <- quantile(tmp$crit, p = c(alpha / 2, 1 - alpha / 2))
  ci <- round(q, floor(log10(nboot)) + 1)
  data.frame(
    Test = "Permutations (GiniMd)",
    nboot = nboot,
    p.value = tmp$p,
    Stat = tmp$diff,
    CI = paste(ci, collapse = ", ")
  )
}


