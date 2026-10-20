#' bifurcatoR_Analysis
#'
#' @param   data    data frame with 2 columns, a group column and numeric value column.
#'                  Tests that compare groups (two-sample tests, permutation tests, ANOVA,
#'                  Levene) need exactly two groups; bimodality tests ignore `group`.
#' @param   tests   names of tests to run. See details below for supported tests.
#' @param   nboot   number of bootstraps or permutations
#' @param   alpha   significance level (0.05)
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
#' - Two-sample distribution tests
#'   - `ks`, `cvm`, `dts`, `ad`
#'
#' - Variance
#'   - `Levene`
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
#' - mixR (bootstrap likelihood ratio test via `mixR::bs.test`, 1 vs 2 components)
#'   - `NmixR`: Normal (Gaussian)
#'   - `WmixR`: Weibull
#'   - `LNmixR`: Lognormal
#'   - `GmixR`: Gamma
#'
#' For the bimodality coefficient (`mt`), `p.value` is by design an indicator: 1 when the
#' coefficient is below 5/9 (consistent with unimodality) and 0 otherwise.
#'
#' If an individual test fails, its row is returned with `NA` results and the reason in
#' the `CI` column (plus a warning) so the other requested tests are not lost.
#'
#' @return          a data frame where each row corresponds to the results of test, p-values, test stats, and confidence intervals where possible
#'
#' @import          diptest
#' @import          LaplacesDemon
#' @import          Hmisc
#' @importFrom      stats confint lm quantile
#'
#' @export
bifurcatoR_Analysis = function(data, tests, nboot, alpha) {
  stopifnot(is.data.frame(data), all(c("group", "value") %in% names(data)))
  stopifnot(is.character(tests), length(tests) > 0)
  stopifnot(is.numeric(nboot), length(nboot) == 1, nboot >= 1)
  stopifnot(is.numeric(alpha), length(alpha) == 1, alpha > 0, alpha < 1)

  registry <- .analysis_registry()
  unknown <- setdiff(tests, names(registry))
  if (length(unknown) > 0) {
    stop("Unknown test(s): ", paste(unknown, collapse = ", "),
         ". Supported tests: ", paste(names(registry), collapse = ", "), call. = FALSE)
  }

  res_list <- lapply(tests, function(i) {
    tryCatch(
      registry[[i]](data, nboot, alpha),
      error = function(e) {
        warning("Test '", i, "' failed: ", conditionMessage(e), call. = FALSE)
        data.frame(
          Test = i,
          nboot = nboot,
          p.value = NA_real_,
          Stat = NA_real_,
          CI = paste("Failed:", conditionMessage(e))
        )
      }
    )
  })
  do.call(rbind, res_list)
}

# Test name -> function. Looked up by name instead of building a call with
# eval(call(i, data, ...)), which pasted the whole data frame into the call.
# (Also avoids naming a function `anova.p`, which roxygen/R treat as an S3
# method for stats::anova.)
.analysis_registry <- function() {
  list(
    mclust      = mclust,
    mt          = mt,
    SI          = SI,
    dip         = dip,
    HY          = HY,
    CH          = CH,
    ACR         = ACR,
    FM          = FM,
    ks          = ks,
    cvm         = cvm,
    dts         = dts,
    ad          = ad,
    Levene      = Levene,
    perm.raw    = `perm.raw`,
    perm.sd     = `perm.sd`,
    perm.mad    = `perm.mad`,
    perm.ginimd = `perm.ginimd`,
    anova.p     = function(data, nboot, alpha) anova_tester("ANOVA", data, nboot, alpha),
    anova.np    = function(data, nboot, alpha) anova_tester("Non-parametric ANOVA", data, nboot, alpha),
    NmixR       = NmixR,
    WmixR       = WmixR,
    LNmixR      = LNmixR,
    GmixR       = GmixR
  )
}

.ci <- function(x, alpha, nboot) {
  q <- quantile(x, probs = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
  round(q, floor(log10(nboot)) + 1)
}

# The two groups, in factor-level order. Group-comparison tests need exactly 2.
.two_groups <- function(data) {
  grp <- droplevels(as.factor(data$group))
  if (nlevels(grp) != 2) {
    stop("this test compares exactly two groups, but the data contain ", nlevels(grp), call. = FALSE)
  }
  grp
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
    # not a true p-value: 1 if BC < 5/9 (unimodal-like), 0 otherwise
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
    FM = "Fisher and Marron Cramer-von Mises"
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
  # `c(fun = <function>, name = "...")` makes a list, and `model['fun']` is then
  # a length-1 *list*, not a function, so build the spec explicitly.
  spec <- switch(type,
    ks  = list(fun = twosamples::ks_test,  name = "Kolmogorov-Smirnov Test"),
    cvm = list(fun = twosamples::cvm_test, name = "Cramer-von Mises Test"),
    dts = list(fun = twosamples::dts_test, name = "DTS Test"),
    ad  = list(fun = twosamples::ad_test,  name = "Anderson-Darling Test")
  )

  grp <- .two_groups(data)
  lev <- levels(grp)

  tmp = spec$fun(
    data$value[grp == lev[1]],
    data$value[grp == lev[2]],
    nboots = nboot,
    keep.boots = T
  )
  data.frame(
    Test = spec$name,
    nboot = nboot,
    p.value = unname(tmp[[2]]),
    Stat = unname(tmp[[1]]),
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

Levene <- function(data, nboot, alpha) {
  grp <- .two_groups(data)
  tmp = car::leveneTest(data$value, grp)
  data.frame(
    Test = "Levene's Test",
    nboot = NA,
    p.value = tmp$'Pr(>F)'[1],
    Stat = tmp$'F value'[1],
    CI = "Not yet available"
  )
}

permutation_tester <- function(type, data, nboot, alpha) {
  # label understood by permutation_tests()
  fxn <- switch(type,
    Raw = "Permutations (Raw)",
    SD = "Permutations (SD)",
    MAD = "Permutations (MAD)",
    GiniMd = "Permutations (Gini)"
  )

  grp <- .two_groups(data)
  X <- as.integer(grp) - 1L

  tmp = permutation_tests(
    y = data$value,
    X = X,
    nperm = nboot,
    fxn_names = fxn,
    alpha = alpha
  )
  data.frame(
    Test = paste0("Permutations (", type, ")"),
    nboot = nboot,
    p.value = unname(tmp$p[[fxn]]),
    Stat = unname(tmp$diff[[fxn]]),
    CI = paste(round(tmp$crit[[fxn]], floor(log10(nboot)) + 1), collapse = ", ")
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
  grp <- .two_groups(data)
  d <- data.frame(value = data$value, grp = grp)

  tmp <- switch(type,
    `ANOVA` = lm(value ~ grp, data = d),
    `Non-parametric ANOVA` = lm(rank(value) ~ grp, data = d)
  )
  ci <- round(confint(tmp)[2, ], floor(log10(nboot)) + 1)
  data.frame(
    Test = type,
    nboot = NA,
    p.value = summary(tmp)$coefficients[2, 4],
    Stat = unname(tmp$coefficients[2]),
    CI = paste(ci, collapse = ", ")
  )
}

mixR <- function(family, data, nboot, alpha) {
  model <- switch(family,
    NmixR = c(name = "Normal", dist = "normal"),
    WmixR = c(name = "Weibull", dist = "weibull"),
    LNmixR = c(name = "Lognormal", dist = "lnorm"),
    GmixR = c(name = "Gamma", dist = "gamma")
  )

  model_name <- paste(model[["name"]], "mixR")
  model_dist <- model[["dist"]]

  tmp = mixR::bs.test(data$value, ncomp = c(1, 2), family = model_dist, B = nboot)

  w1 <- tmp$w1
  has_boot <- !is.null(w1) && any(is.finite(w1))
  data.frame(
    Test = model_name,
    nboot = nboot,
    p.value = as.numeric(tmp$pvalue),
    Stat = if (is.null(tmp$w0)) NA_real_ else as.numeric(tmp$w0),
    CI = if (has_boot) paste(.ci(w1, alpha, nboot), collapse = ", ") else "Not available"
  )
}

NmixR <- function(data, nboot, alpha) {
  mixR("NmixR", data, nboot, alpha)
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
