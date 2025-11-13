#' bifurcatoR_Analysis
#'
#' @param   data    data frame with 2 columns, a two-level group column and numeric value column
#' @param   alpha   default significance level (0.05)
#' @param   nboot   number of bootstraps or permutations
#' @param   tests   names of tests to run. See details below for supported tests.
#'
#' @details
#' - `mclust`
#' - `mt`
#'
#' - Modetests
#'   - `SI`
#'   - `dip`
#'   - `HY`
#'   - `CH`
#'   - `ACR`
#'   - `FM`
#'
#' - Permutation tests
#'   - `perm.raw`
#'   - `perm.sd`
#'   - `perm.mad`
#'   - `perm.ginimd`
#'
#' - ANOVA
#'   - `anova.p`: Parametric ANOVA
#'   - `anova.np`: Non-parametric ANOVA
#'
#' - mixR
#'   - `WmixR`: Weibull
#'   - `LNmixR`: Lognormal
#'   - `GmixR`: Gaussian
#'   - `GamixR`: Gamma
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

modetester <- function(type, data, nboot) {
  model_name <- switch(type,
    SI = "Silverman Bandwidth test",
    HH = "Hartigans' dip test",
    HY = "Hall and York Bandwidth test",
    CH = "Cheng and Hall Excess Mass",
    ACR = "Ameijeiras-Alonso et al. Excess Mass",
    FM = "Fisher and Marron Carmer-von Mises"
  )

  tmp = multimode::modetest(data$value, mod0 = 1, method = type, B = nboot)
  data.frame(
    Test = model_name,
    nboot = nboot,
    p.value = tmp$p.value,
    Stat = unname(tmp$statistic),
    CI = "Not yet available"
  )
}

SI <- function(data, nboot, ...) {
  modetester("SI", data, nboot)
}

dip <- function(data, nboot, ...) {
  modetester("HH", data, nboot)
}

HY <- function(data, nboot, ...) {
  modetester("HY", data, nboot)
}

CH <- function(data, nboot, ...) {
  modetester("CH", data, nboot)
}

ACR <- function(data, nboot, ...) {
  modetester("ACR", data, nboot)
}

FM <- function(data, nboot, ...) {
  modetester("FM", data, nboot)
}

twosample_tester <- function(type, data, nboot, alpha) {
  model <- switch(type,
    ks = c(fun = twosamples::ks_test, name = "Kolmogorov-Smirnov Test"),
    cvm = c(fun = twosamples::cvm_test, name = "Cramer-von Mises Test"),
    dts = c(fun = twosamples::dts_test, name = "DTS Test"),
    ad = c(fun = twosamples::ad_test, name = "Aderson-Darling Test")
  )
  twosample_test <- model['fun']
  tmp = twosample_test(
    data$value[data$group == unique(data$group)[1]],
    data$value[data$group == unique(data$group)[2]],
    nboots = nboot,
    keep.boots = T
  )
  data.frame(
    Test = model['name'],
    nboot = nboot,
    p.value = tmp[[2]],
    Stat = tmp[[1]],
    CI = paste(.ci(attributes(tmp)$bootstraps, alpha, nboot), collapse = ", ")
  )
}

ks <- function(data, nboot, alpha) {
  twosample_tester("ks", data, nboot, alpha)
}

cvm <- function(data, nboot, alpha) {
  twosample_tester("cvm", data, nboot, alpha)
}

dts <- function(data, nboot, alpha) {
  twosample_tester("dts", data, nboot, alpha)
}

ad <- function(data, nboot, alpha) {
  twosample_tester("ad", data, nboot, alpha)
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

permutation_tester <- function(type, data, nboot, alpha) {
  difftype <- switch(type,
    Raw = "meanDiff",
    SD = "sdDiff",
    MAD = "madDiff",
    GiniMd = "giniDiff"
  )

  temp_df = data.frame(
    y = data$value,
    X = as.numeric(as.factor(data$group)) - 1
  )

  tmp = permutation_tests(temp_df, nboot, difftype, alpha)
  data.frame(
    Test = paste0("Permutations (", type, ")"),
    nboot = nboot,
    p.value = tmp$p,
    Stat = tmp$diff,
    CI = paste(.ci(tmp$crit, alpha, nboot), collapse = ", ")
  )
}

`perm.raw` <- function(data, nboot, alpha) {
  permutation_tester("Raw", data, nboot, alpha)
}

`perm.sd` <- function(data, nboot, alpha) {
  permutation_tester("SD", data, nboot, alpha)
}

`perm.mad` <- function(data, nboot, alpha) {
  permutation_tester("MAD", data, nboot, alpha)
}

`perm.ginimd` <- function(data, nboot, alpha) {
  permutation_tester("GiniMd", data, nboot, alpha)
}

anova_tester <- function(type, data, nboot, alpha) {
  tmp <- switch(type,
    `Parametric ANOVA` = lm(value ~ as.factor(data$group), data = data),
    `Non-parametric ANOVA` = lm(rank(value) ~ as.factor(data$group), data = data)
  )
  ci <- round(confint(tmp)[2, ], floor(log10(nboot)) + 1)
  data.frame(
    Test = type,
    nboot = NA,
    p.value = summary(tmp)$coefficients[2, 4],
    Stat = tmp$coefficients[2],
    CI = paste(ci, collapse = ", ")
  )
}

anova.p <- function(data, nboot, alpha) {
  anova_tester("Parametric ANOVA", data, nboot, alpha)
}

anova.np <- function(data, nboot, alpha) {
  anova_tester("Non-parametric ANOVA", data, nboot, alpha)
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
