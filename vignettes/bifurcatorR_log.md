---
title: "bifurcatoR working log"
author: "Mao Ding"
date: "2023/3/28"
output:
  pdf_document:
    toc: yes
  html_document:
    fig_width: 8
    code_folding: hide
    highlight: tango
    toc: yes
    keep_md: true
    toc_float:
      collapsed: yes
---



## Objective
Aim to test functions in the bifurcatoR package

### Functions Test

```r
#devtools::install_github("carmkhoo/experimentalRepo")
library(remotes)
library(BiocManager)
library(pwr)
library(dplyr)
library(olsrr)
library(bifurcatoR)
```


```r
################################
######## calc_power ########
################################
calc_power(n = 20, p1 = 0.25, p2 = 0.5, sel= 'Bodyweight', shift = 15, CI = 0)
```

```
##    N      Test       Power
## 1 20 Frequency 0.380637791
## 2 20     Shift 0.997091145
## 3 20        KS 1.000000000
```

```r
calc_power(n = 550, p1 = 0.2, p2 = 0.9, sel= 'Fat Trim', shift = 30, CI = 1)
```

```
##     N      Test Power
## 1 550 Frequency     1
## 2 550     Shift     1
## 3 550        KS     1
```

```r
calc_power(n = 11050, p1 = 0.9, p2 = 0.6, sel= 'Fat NNAT', shift = 120, CI = 1)
```

```
##       N      Test Power
## 1 11050 Frequency     1
## 2 11050     Shift     1
## 3 11050        KS     1
```

```r
################################
######## est_pow ########
################################
params_1 = list (p = 0.5, mu1 = 0.2, sd1 = 0.5, mu2 = 3, sd2 = 0.4, s1 = 1.2, s2 = 2.3, sp1 = 1.2, sp2 = 1.7, sc1 = 2, sc2 = 0.5)
params_2 = list (p = 0.2, mu1 = 0.1, sd1 = 0.6, mu2 = 5.3, sd2 = 15, s1 = 0.3, s2 = 6.7, sp1 = 5.1, sp2 = 9, sc1 = 12, sc2 = 3)
params_3 = list (p = 0.5, mu1 = 0.1, sd1 = 0.2, mu2 = 3.6, sd2 = 1.8, s1 = 9.2, s2 = 2.6, sp1 = 2.7, sp2 = 1.5, sc1 = 0.2, sc2 = 0.7)

est_pow(n = 20, alpha = 0.05, nsim = 20, dist = "norm", params = params_1,  tests = "Hartigans' dip test")  # input multiple random parameters, returns NULL
```

```
## NULL
```

```r
est_pow(n = 20, alpha = 0.05, nsim = 20, dist = "beta", params = params_2,  tests = "mclust") 
```

```
##    N   Test power FP
## 1 20 Mclust     1  0
```

```r
est_pow(n = 20, alpha = 0.02, nsim = 20, dist = "weib", params = params_3,  tests = "Mouse Trap") # input multiple random parameters, returns NULL
```

```
## NULL
```

```r
#####################################
######## est_pow_2samp ########
####################################

params_4 = list (p_1 = 0.5, p_2 = 0.7,
                 mean = 5, v_scale = 0.7,
                 s1_1  = 12, s1_2 = 1.5,
                 s2_1 = 2, s2_2 = 0.8,
                 mu1_1 = 4, mu1_2 = 5,
                 mu2_1 = 1.6, mu2_2 = 7,
                 sd1_1 = 2.8, sd1_2 = 11,
                 sd2_1 = 3.6, sd2_2 = 20,
                 sp1_1 = 1.6, sp1_2 = 25,
                 sp2_1 = 2.3, sp2_2 = 5,
                 sc1_1 = 14, sc1_2 = 6,
                 sc2_1 = 25, sc2_2 = 7)

est_pow_2samp(n1 = 20, n2 = 30, alpha = 0.05, nsim = 20, modes = 1, dist = "norm", params = params_4, tests = "Kolmogorov-Smirnov", nperm = 30)   # returns NULL
```

```
## NULL
```

```r
est_pow_2samp(n1 = 10, n2 = 30, alpha = 0.05, nsim = 20, modes = 2, dist = "beta", params = params_4, tests = "Cramer-von Mises", nperm = 30)     # returns NULL
```

```
## NULL
```

```r
est_pow_2samp(n1 = 20, n2 = 30, alpha = 0.05, nsim = 20, modes = 1, dist = "weib", params = params_4, tests = "DTS", nperm = 30)                  # returns NULL
```

```
## Error in loadNamespace(x): there is no package called 'mixdist'
```

```r
est_pow_2samp(n1 = 20, n2 = 30, alpha = 0.05, nsim = 20, modes = 1, dist = "norm", params = params_4, tests = "Anderson-Darling", nperm = 30)     # returns NULL
```

```
## NULL
```

```r
est_pow_2samp(n1 = 20, n2 = 30, alpha = 0.05, nsim = 20, modes = 2, dist = "beta", params = params_4, tests = "ANOVA", nperm = 30)  
```

```
##    Test power   FP
## 1 ANOVA     1 0.05
```

```r
est_pow_2samp(n1 = 20, n2 = 30, alpha = 0.05, nsim = 20, modes = 2, dist = "weib", params = params_4, tests = "Non-Parametric ANOVA", nperm = 30) # returns NULL
```

```
## NULL
```

```r
#####################################
######## est_pow_bin ########
#####################################

est_pow_bin(n = 15, p = 0.5, alpha = 0.05, nsim = 20)
```

```
## [1] 0
```


```r
#####################################
######## est_pow_het ########
#####################################
est_pow_het(n = 50, p = 0.5, alpha = 0.05, x = 12, nsim = 20)
```

```
## [1] 1
```


```r
#####################################
######## find_dist ########
#####################################
find_dist(n1 = 20, n2 = 30, alpha = 0.05, nsim = 20)
```

```
##    Distance power
## 1      0.25  0.00
## 2      0.50  0.00
## 3      0.75  0.00
## 4      1.00  0.00
## 5      1.25  0.05
## 6      1.50  0.00
## 7      1.75  0.00
## 8      2.00  0.00
## 9      2.25  0.05
## 10     2.50  0.00
## 11     2.75  0.05
## 12     3.00  0.00
## 13     3.25  0.00
## 14     3.50  0.15
## 15     3.75  0.10
## 16     4.00  0.20
## 17     4.25  0.50
## 18     4.50  0.50
## 19     4.75  0.65
## 20     5.00  0.90
## 21     5.25  0.85
## 22     5.50  0.85
## 23     5.75  1.00
## 24     6.00  1.00
## 25     6.25  1.00
## 26     6.50  1.00
## 27     6.75  1.00
## 28     7.00  1.00
## 29     7.25  1.00
## 30     7.50  1.00
## 31     7.75  1.00
## 32     8.00  1.00
## 33     8.25  1.00
## 34     8.50  1.00
## 35     8.75  1.00
## 36     9.00  1.00
## 37     9.25  1.00
## 38     9.50  1.00
## 39     9.75  1.00
## 40    10.00  1.00
```
  
