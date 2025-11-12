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
bifurcatoR_Analysis = function(data, tests, nboot, alpha) {
  res_list <- lapply(tests, function(i) {
    eval(call(i, data, nboot, alpha))
  })
  do.call(rbind, res_list)
}

.ci <- function(x, alpha, nboot) {
  q <- quantile(x, p = c(alpha / 2, 1 - alpha / 2))
  round(q, floor(log10(nboot)) + 1)
}

mclust <- function(data, nboot, alpha) {
  tmp = mclust::mclustBootstrapLRT(
    data$value,
    modelName = "E",
    verbose = F,
    maxG = 1,
    nboot = nboot
  )
  data.frame(
    Test = "Mclust",
    nboot = nboot,
    p.value = tmp$p.value,
    Stat = tmp$obs,
    CI = paste(.ci(tmp$boot, alpha, nboot), collapse = ", ")
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
  data.frame(
    Test = "Bimodality Coefficient",
    nboot = nboot,
    p.value = as.numeric(I(tmp < (5 / 9))),
    Stat = unname(tmp),
    CI = paste(.ci(tmp.boot, alpha, nboot), collapse = ", ")
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
  data.frame(
    Test = "Kolmogorov-Smirnov Test",
    nboot = nboot,
    p.value = tmp[[2]],
    Stat = tmp[[1]],
    CI = paste(.ci(attributes(tmp)$bootstraps, alpha, nboot), collapse = ", ")
  )
}

cvm <- function(data, nboot, alpha) {
  tmp = twosamples::cvm_test(
    data$value[data$group == unique(data$group)[1]],
    data$value[data$group == unique(data$group)[2]],
    nboots = nboot,
    keep.boots = T
  )
  data.frame(
    Test = "Cramer-von Mises Test",
    nboot = nboot,
    p.value = tmp[[2]],
    Stat = tmp[[1]],
    CI = paste(.ci(attributes(tmp)$bootstraps), collapse = ", ")
  )
}

dts <- function(data, nboot, alpha) {
  tmp = twosamples::dts_test(
    data$value[data$group == unique(data$group)[1]],
    data$value[data$group == unique(data$group)[2]],
    nboots = nboot,
    keep.boots = T
  )
  data.frame(
    Test = "DTS Test",
    nboot = nboot,
    p.value = tmp[[2]],
    Stat = tmp[[1]],
    CI = paste(.ci(attributes(tmp)$bootstraps), collapse = ", ")
  )
}

ad <- function(data, nboot, alpha) {
  tmp = twosamples::ad_test(
    data$value[data$group == unique(data$group)[1]],
    data$value[data$group == unique(data$group)[2]],
    nboots = nboot,
    keep.boots = T
  )
  data.frame(
    Test = "Aderson-Darling Test",
    nboot = nboot,
    p.value = tmp[[2]],
    Stat = tmp[[1]],
    CI = paste(.ci(attributes(tmp)$bootstraps), collapse = ", ")
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
  data.frame(
    Test = "Permutations (Raw)",
    nboot = nboot,
    p.value = tmp$p,
    Stat = tmp$diff,
    CI = paste(.ci(tmp$crit, alpha, nboot), collapse = ", ")
  )
}

`Permutations (SD)` <- function(data, nboot, alpha) {
  temp_df = data.frame(
    y = data$value,
    X = as.numeric(as.factor(data$group)) - 1
  )
  tmp = permutation_tests(temp_df, nboot, "sdDiff", alpha)
  data.frame(
    Test = "Permutations (SD)",
    nboot = nboot,
    p.value = tmp$p,
    Stat = tmp$diff,
    CI = paste(.ci(tmp$crit), collapse = ", ")
  )
}


`Permutations (MAD)` <- function(data, nboot, alpha) {
  temp_df = data.frame(
    y = data$value,
    X = as.numeric(as.factor(data$group)) - 1
  )
  tmp = permutation_tests(temp_df, nboot, "madDiff", alpha)
  data.frame(
    Test = "Permutations (MAD)",
    nboot = nboot,
    p.value = tmp$p,
    Stat = tmp$diff,
    CI = paste(.ci(tmp$crit), collapse = ", ")
  )
}

`Permutations (GiniMd)` <- function(data, nboot, alpha) {
  temp_df = data.frame(
    y = data$value,
    X = as.numeric(as.factor(data$group)) - 1
  )
  tmp = permutation_tests(temp_df, nboot, "giniDiff", alpha)
  data.frame(
    Test = "Permutations (GiniMd)",
    nboot = nboot,
    p.value = tmp$p,
    Stat = tmp$diff,
    CI = paste(.ci(tmp$crit), collapse = ", ")
  )
}


ANOVA <- function(data, nboot, alpha) {
  tmp = lm(value ~ as.factor(data$group), data = data)
  ci <- round(confint(tmp)[2, ], floor(log10(nboot)) + 1)
  data.frame(
    Test = "ANOVA",
    nboot = NA,
    p.value = summary(tmp)$coefficients[2, 4],
    Stat = tmp$coefficients[2],
    CI = paste(ci, collapse = ", ")
  )
}


`Non-parametric ANOVA` <- function(data, nboot, alpha) {
  tmp = lm(rank(value) ~ as.factor(data$group), data = data)
  ci <- round(confint(tmp)[2, ], floor(log10(nboot)) + 1)
  data.frame(
    Test = "Non-parametric ANOVA",
    nboot = NA,
    p.value = summary(tmp)$coefficients[2, 4],
    Stat = tmp$coefficients[2],
    CI = paste(ci, collapse = ", ")
  )
}

mixR <- function(family, data, nboot, alpha) {
  model <- switch(family,
    WmixR = c(name = "Weibull", dist = "weibull"),
    LNmixR = c(name = "Lognormal", dist = "lnorm"),
    GmixR = c(name = "Gaussian", dist = "normal"),
    GamixR = c(name = "Gamma", dist = "gamma"),
  )

  model_name <- paste(model['name'], "mixR")
  model_dist <- model['dist']

  tmp = bs_lrt(data$value, H0 = 1, H1 = 2, family = model_dist, nboot = nboot)
  data.frame(
    Test = model_name,
    nboot = nboot,
    p.value = tmp$pvalue,
    Stat = tmp$w0,
    CI = paste(.ci(tmp$w1, alpha, nboot), collapse = ", ")
  )
}

WmixR <- function(data, nboot, alpha) {
  mixR("WmixR", data, nboot, alpha)
}

LNmixR <- function(data, nboot, alpha) {
  mixR("LNmixR", data, nboot, alpha)
}

GmixR <- function(data, nboot, alpha) {
  mixR("GmixR", data, nboot, alpha)
}

GamixR <- function(data, nboot, alpha) {
  mixR("GamixR", data, nboot, alpha)
}
