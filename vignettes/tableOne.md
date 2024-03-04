---
title: "Table One for COG merged MLL/NSD1 cohort slices"
author: "Timothy J. Triche, Jr. (on behalf of all authors)"
date: "2023-2-1"
output:
  html_document:
    fig_width: 8
    code_folding: hide
    highlight: tango
    toc: yes
    keep_md: true
    toc_float:
      collapsed: yes
  pdf_document:
    toc: yes
    toc_depth: 3
---



# Covariates and outcomes for MLL and NSD1 fusion patients

Patient number 368 does not have an annotated sex or age in the COG CDEs. 



## Aggregate 

Tabulate age group, sex, protocol, and outcome overall.
NA will be tabulated whenever present. 


```
##                           
##                            Overall         
##   n                            470         
##   AgeGroup (%)                             
##      AYA                        77 (16.4)  
##      Child                     220 (46.8)  
##      Infant                    172 (36.6)  
##      NA                          1 ( 0.2)  
##   Sex (%)                                  
##      Female                    211 (44.9)  
##      Male                      258 (54.9)  
##      Unknown                     1 ( 0.2)  
##   FAB (%)                                  
##      M0                         10 ( 2.1)  
##      M1                         27 ( 5.7)  
##      M2                         33 ( 7.0)  
##      M4                        129 (27.4)  
##      M5                        112 (23.8)  
##      M6                          2 ( 0.4)  
##      M7                         10 ( 2.1)  
##      NA                        147 (31.3)  
##   BlastPercent (mean (SD))   61.31 (27.71) 
##   fusion (%)                               
##      KMT2A-ABI1                  2 ( 0.4)  
##      KMT2A-AFF1                  4 ( 0.9)  
##      KMT2A-AFF3                  1 ( 0.2)  
##      KMT2A-ARAP1                 1 ( 0.2)  
##      KMT2A-ARHGAP26              1 ( 0.2)  
##      KMT2A-ARHGEF37              1 ( 0.2)  
##      KMT2A-C9orf72               1 ( 0.2)  
##      KMT2A-CCNJL                 1 ( 0.2)  
##      KMT2A-DCP1A                 1 ( 0.2)  
##      KMT2A-ELL                  40 ( 8.5)  
##      KMT2A-EPS15                 2 ( 0.4)  
##      KMT2A-FNBP1                 2 ( 0.4)  
##      KMT2A-LASP1                 4 ( 0.9)  
##      KMT2A-LYN                   1 ( 0.2)  
##      KMT2A-MLLT1                19 ( 4.0)  
##      KMT2A-MLLT10               83 (17.7)  
##      KMT2A-MLLT11                7 ( 1.5)  
##      KMT2A-MLLT3               119 (25.3)  
##      KMT2A-MLLT3                 2 ( 0.4)  
##      KMT2A-MLLT4                43 ( 9.1)  
##      KMT2A-MLLT6                 2 ( 0.4)  
##      KMT2A-MYO1F                 3 ( 0.6)  
##      KMT2A-MYOCD                 1 ( 0.2)  
##      KMT2A-PALM2.AKAP2           1 ( 0.2)  
##      KMT2A-PICALM                1 ( 0.2)  
##      KMT2A-SARNP                 1 ( 0.2)  
##      KMT2A-SEPT2                 1 ( 0.2)  
##      KMT2A-SEPT6                12 ( 2.6)  
##      KMT2A-SEPT9                 4 ( 0.9)  
##      KMT2A-USP2                  1 ( 0.2)  
##      KMT2A-X                     4 ( 0.9)  
##      MLLT10-KMT2A                1 ( 0.2)  
##      NUP98-KDM5A                 0 ( 0.0)  
##      NUP98-NSD1                103 (21.9)  
##   FusionGroup = NSD1 (%)       103 (21.9)  
##   OS (mean (SD))           1093.64 (851.56)
##   OSI (mean (SD))             0.51 (0.50)  
##   Protocol (%)                             
##      AAML03P1                   10 ( 2.1)  
##      AAML0531                  134 (28.5)  
##      AAML1031                  307 (65.3)  
##      CCG-2961                   19 ( 4.0)
```

## Per trial

Tabulate age group, sex, and outcome per trial  


```
##                           Stratified by Protocol
##                            AAML03P1          AAML0531          AAML1031        
##   n                             10               134               307         
##   AgeGroup (%)                                                                 
##      AYA                         2 ( 20.0)        21 ( 15.7)        52 ( 16.9) 
##      Child                       7 ( 70.0)        62 ( 46.3)       137 ( 44.6) 
##      Infant                      1 ( 10.0)        51 ( 38.1)       117 ( 38.1) 
##      NA                          0 (  0.0)         0 (  0.0)         1 (  0.3) 
##   Sex (%)                                                                      
##      Female                      5 ( 50.0)        65 ( 48.5)       138 ( 45.0) 
##      Male                        5 ( 50.0)        69 ( 51.5)       168 ( 54.7) 
##      Unknown                     0 (  0.0)         0 (  0.0)         1 (  0.3) 
##   FAB (%)                                                                      
##      M0                          0 (  0.0)         3 (  2.2)         7 (  2.3) 
##      M1                          1 ( 10.0)         9 (  6.7)        15 (  4.9) 
##      M2                          2 ( 20.0)         9 (  6.7)        14 (  4.6) 
##      M4                          6 ( 60.0)        90 ( 67.2)        24 (  7.8) 
##      M5                          0 (  0.0)         0 (  0.0)       112 ( 36.5) 
##      M6                          0 (  0.0)         2 (  1.5)         0 (  0.0) 
##      M7                          0 (  0.0)         3 (  2.2)         7 (  2.3) 
##      NA                          1 ( 10.0)        18 ( 13.4)       128 ( 41.7) 
##   BlastPercent (mean (SD))     NaN (NA)          NaN (NA)        61.31 (27.71) 
##   fusion (%)                                                                   
##      KMT2A-ABI1                  0 (  0.0)         1 (  0.7)         1 (  0.3) 
##      KMT2A-AFF1                  0 (  0.0)         3 (  2.2)         1 (  0.3) 
##      KMT2A-AFF3                  0 (  0.0)         0 (  0.0)         1 (  0.3) 
##      KMT2A-ARAP1                 0 (  0.0)         0 (  0.0)         1 (  0.3) 
##      KMT2A-ARHGAP26              0 (  0.0)         1 (  0.7)         0 (  0.0) 
##      KMT2A-ARHGEF37              0 (  0.0)         0 (  0.0)         1 (  0.3) 
##      KMT2A-C9orf72               0 (  0.0)         0 (  0.0)         1 (  0.3) 
##      KMT2A-CCNJL                 0 (  0.0)         0 (  0.0)         1 (  0.3) 
##      KMT2A-DCP1A                 0 (  0.0)         0 (  0.0)         1 (  0.3) 
##      KMT2A-ELL                   3 ( 30.0)        14 ( 10.4)        21 (  6.8) 
##      KMT2A-EPS15                 0 (  0.0)         1 (  0.7)         1 (  0.3) 
##      KMT2A-FNBP1                 0 (  0.0)         1 (  0.7)         1 (  0.3) 
##      KMT2A-LASP1                 0 (  0.0)         1 (  0.7)         3 (  1.0) 
##      KMT2A-LYN                   0 (  0.0)         0 (  0.0)         1 (  0.3) 
##      KMT2A-MLLT1                 0 (  0.0)         4 (  3.0)        14 (  4.6) 
##      KMT2A-MLLT10                1 ( 10.0)        26 ( 19.4)        55 ( 17.9) 
##      KMT2A-MLLT11                0 (  0.0)         1 (  0.7)         6 (  2.0) 
##      KMT2A-MLLT3                 1 ( 10.0)        26 ( 19.4)        92 ( 30.0) 
##      KMT2A-MLLT3                 0 (  0.0)         2 (  1.5)         0 (  0.0) 
##      KMT2A-MLLT4                 3 ( 30.0)        14 ( 10.4)        26 (  8.5) 
##      KMT2A-MLLT6                 0 (  0.0)         1 (  0.7)         1 (  0.3) 
##      KMT2A-MYO1F                 0 (  0.0)         0 (  0.0)         3 (  1.0) 
##      KMT2A-MYOCD                 0 (  0.0)         0 (  0.0)         1 (  0.3) 
##      KMT2A-PALM2.AKAP2           0 (  0.0)         0 (  0.0)         1 (  0.3) 
##      KMT2A-PICALM                0 (  0.0)         0 (  0.0)         1 (  0.3) 
##      KMT2A-SARNP                 0 (  0.0)         1 (  0.7)         0 (  0.0) 
##      KMT2A-SEPT2                 0 (  0.0)         0 (  0.0)         1 (  0.3) 
##      KMT2A-SEPT6                 0 (  0.0)         4 (  3.0)         8 (  2.6) 
##      KMT2A-SEPT9                 0 (  0.0)         2 (  1.5)         2 (  0.7) 
##      KMT2A-USP2                  0 (  0.0)         0 (  0.0)         1 (  0.3) 
##      KMT2A-X                     0 (  0.0)         2 (  1.5)         2 (  0.7) 
##      MLLT10-KMT2A                0 (  0.0)         1 (  0.7)         0 (  0.0) 
##      NUP98-KDM5A                 0 (  0.0)         0 (  0.0)         0 (  0.0) 
##      NUP98-NSD1                  2 ( 20.0)        28 ( 20.9)        58 ( 18.9) 
##   FusionGroup = NSD1 (%)         2 ( 20.0)        28 ( 20.9)        58 ( 18.9) 
##   OS (mean (SD))           1751.90 (1393.02) 1268.34 (1000.77) 1008.12 (717.31)
##   OSI (mean (SD))             0.50 (0.53)       0.52 (0.50)       0.49 (0.50)  
##   Protocol (%)                                                                 
##      AAML03P1                   10 (100.0)         0 (  0.0)         0 (  0.0) 
##      AAML0531                    0 (  0.0)       134 (100.0)         0 (  0.0) 
##      AAML1031                    0 (  0.0)         0 (  0.0)       307 (100.0) 
##      CCG-2961                    0 (  0.0)         0 (  0.0)         0 (  0.0) 
##                           Stratified by Protocol
##                            CCG-2961         p      test
##   n                            19                      
##   AgeGroup (%)                               0.354     
##      AYA                        2 ( 10.5)              
##      Child                     14 ( 73.7)              
##      Infant                     3 ( 15.8)              
##      NA                         0 (  0.0)              
##   Sex (%)                                    0.248     
##      Female                     3 ( 15.8)              
##      Male                      16 ( 84.2)              
##      Unknown                    0 (  0.0)              
##   FAB (%)                                   <0.001     
##      M0                         0 (  0.0)              
##      M1                         2 ( 10.5)              
##      M2                         8 ( 42.1)              
##      M4                         9 ( 47.4)              
##      M5                         0 (  0.0)              
##      M6                         0 (  0.0)              
##      M7                         0 (  0.0)              
##      NA                         0 (  0.0)              
##   BlastPercent (mean (SD))    NaN (NA)          NA     
##   fusion (%)                                   NaN     
##      KMT2A-ABI1                 0 (  0.0)              
##      KMT2A-AFF1                 0 (  0.0)              
##      KMT2A-AFF3                 0 (  0.0)              
##      KMT2A-ARAP1                0 (  0.0)              
##      KMT2A-ARHGAP26             0 (  0.0)              
##      KMT2A-ARHGEF37             0 (  0.0)              
##      KMT2A-C9orf72              0 (  0.0)              
##      KMT2A-CCNJL                0 (  0.0)              
##      KMT2A-DCP1A                0 (  0.0)              
##      KMT2A-ELL                  2 ( 10.5)              
##      KMT2A-EPS15                0 (  0.0)              
##      KMT2A-FNBP1                0 (  0.0)              
##      KMT2A-LASP1                0 (  0.0)              
##      KMT2A-LYN                  0 (  0.0)              
##      KMT2A-MLLT1                1 (  5.3)              
##      KMT2A-MLLT10               1 (  5.3)              
##      KMT2A-MLLT11               0 (  0.0)              
##      KMT2A-MLLT3                0 (  0.0)              
##      KMT2A-MLLT3                0 (  0.0)              
##      KMT2A-MLLT4                0 (  0.0)              
##      KMT2A-MLLT6                0 (  0.0)              
##      KMT2A-MYO1F                0 (  0.0)              
##      KMT2A-MYOCD                0 (  0.0)              
##      KMT2A-PALM2.AKAP2          0 (  0.0)              
##      KMT2A-PICALM               0 (  0.0)              
##      KMT2A-SARNP                0 (  0.0)              
##      KMT2A-SEPT2                0 (  0.0)              
##      KMT2A-SEPT6                0 (  0.0)              
##      KMT2A-SEPT9                0 (  0.0)              
##      KMT2A-USP2                 0 (  0.0)              
##      KMT2A-X                    0 (  0.0)              
##      MLLT10-KMT2A               0 (  0.0)              
##      NUP98-KDM5A                0 (  0.0)              
##      NUP98-NSD1                15 ( 78.9)              
##   FusionGroup = NSD1 (%)       15 ( 78.9)   <0.001     
##   OS (mean (SD))           883.21 (1050.48)  0.001     
##   OSI (mean (SD))            0.84 (0.37)     0.026     
##   Protocol (%)                              <0.001     
##      AAML03P1                   0 (  0.0)              
##      AAML0531                   0 (  0.0)              
##      AAML1031                   0 (  0.0)              
##      CCG-2961                  19 (100.0)
```


## By fusion

Tabulate age group, sex, protocol, and outcome by fusion group


```
##                           Stratified by FusionGroup
##                            MLL              NSD1            p      test
##   n                            367             103                     
##   AgeGroup (%)                                              <0.001     
##      AYA                        56 (15.3)       21 ( 20.4)             
##      Child                     144 (39.2)       76 ( 73.8)             
##      Infant                    166 (45.2)        6 (  5.8)             
##      NA                          1 ( 0.3)        0 (  0.0)             
##   Sex (%)                                                    0.034     
##      Female                    176 (48.0)       35 ( 34.0)             
##      Male                      190 (51.8)       68 ( 66.0)             
##      Unknown                     1 ( 0.3)        0 (  0.0)             
##   FAB (%)                                                   <0.001     
##      M0                          7 ( 1.9)        3 (  2.9)             
##      M1                         14 ( 3.8)       13 ( 12.6)             
##      M2                         10 ( 2.7)       23 ( 22.3)             
##      M4                        105 (28.6)       24 ( 23.3)             
##      M5                         97 (26.4)       15 ( 14.6)             
##      M6                          0 ( 0.0)        2 (  1.9)             
##      M7                          7 ( 1.9)        3 (  2.9)             
##      NA                        127 (34.6)       20 ( 19.4)             
##   BlastPercent (mean (SD))   60.83 (28.74)   63.63 (22.31)   0.513     
##   fusion (%)                                                   NaN     
##      KMT2A-ABI1                  2 ( 0.5)        0 (  0.0)             
##      KMT2A-AFF1                  4 ( 1.1)        0 (  0.0)             
##      KMT2A-AFF3                  1 ( 0.3)        0 (  0.0)             
##      KMT2A-ARAP1                 1 ( 0.3)        0 (  0.0)             
##      KMT2A-ARHGAP26              1 ( 0.3)        0 (  0.0)             
##      KMT2A-ARHGEF37              1 ( 0.3)        0 (  0.0)             
##      KMT2A-C9orf72               1 ( 0.3)        0 (  0.0)             
##      KMT2A-CCNJL                 1 ( 0.3)        0 (  0.0)             
##      KMT2A-DCP1A                 1 ( 0.3)        0 (  0.0)             
##      KMT2A-ELL                  40 (10.9)        0 (  0.0)             
##      KMT2A-EPS15                 2 ( 0.5)        0 (  0.0)             
##      KMT2A-FNBP1                 2 ( 0.5)        0 (  0.0)             
##      KMT2A-LASP1                 4 ( 1.1)        0 (  0.0)             
##      KMT2A-LYN                   1 ( 0.3)        0 (  0.0)             
##      KMT2A-MLLT1                19 ( 5.2)        0 (  0.0)             
##      KMT2A-MLLT10               83 (22.6)        0 (  0.0)             
##      KMT2A-MLLT11                7 ( 1.9)        0 (  0.0)             
##      KMT2A-MLLT3               119 (32.4)        0 (  0.0)             
##      KMT2A-MLLT3                 2 ( 0.5)        0 (  0.0)             
##      KMT2A-MLLT4                43 (11.7)        0 (  0.0)             
##      KMT2A-MLLT6                 2 ( 0.5)        0 (  0.0)             
##      KMT2A-MYO1F                 3 ( 0.8)        0 (  0.0)             
##      KMT2A-MYOCD                 1 ( 0.3)        0 (  0.0)             
##      KMT2A-PALM2.AKAP2           1 ( 0.3)        0 (  0.0)             
##      KMT2A-PICALM                1 ( 0.3)        0 (  0.0)             
##      KMT2A-SARNP                 1 ( 0.3)        0 (  0.0)             
##      KMT2A-SEPT2                 1 ( 0.3)        0 (  0.0)             
##      KMT2A-SEPT6                12 ( 3.3)        0 (  0.0)             
##      KMT2A-SEPT9                 4 ( 1.1)        0 (  0.0)             
##      KMT2A-USP2                  1 ( 0.3)        0 (  0.0)             
##      KMT2A-X                     4 ( 1.1)        0 (  0.0)             
##      MLLT10-KMT2A                1 ( 0.3)        0 (  0.0)             
##      NUP98-KDM5A                 0 ( 0.0)        0 (  0.0)             
##      NUP98-NSD1                  0 ( 0.0)      103 (100.0)             
##   FusionGroup = NSD1 (%)         0 ( 0.0)      103 (100.0)  <0.001     
##   OS (mean (SD))           1132.77 (852.67) 951.81 (836.42)  0.059     
##   OSI (mean (SD))             0.49 (0.50)     0.59 (0.49)    0.061     
##   Protocol (%)                                              <0.001     
##      AAML03P1                    8 ( 2.2)        2 (  1.9)             
##      AAML0531                  106 (28.9)       28 ( 27.2)             
##      AAML1031                  249 (67.8)       58 ( 56.3)             
##      CCG-2961                    4 ( 1.1)       15 ( 14.6)
```

## By fusion, by trial 

Tabulate age group, sex, protocol, and outcome by fusion group and trial. 


```
##                           Stratified by FusionGroup:Protocol
##                            MLL:AAML03P1      NSD1:AAML03P1    MLL:AAML0531     
##   n                              8                 2              106          
##   AgeGroup (%)                                                                 
##      AYA                         2 ( 25.0)         0 (  0.0)       16 ( 15.1)  
##      Child                       5 ( 62.5)         2 (100.0)       40 ( 37.7)  
##      Infant                      1 ( 12.5)         0 (  0.0)       50 ( 47.2)  
##      NA                          0 (  0.0)         0 (  0.0)        0 (  0.0)  
##   Sex (%)                                                                      
##      Female                      5 ( 62.5)         0 (  0.0)       57 ( 53.8)  
##      Male                        3 ( 37.5)         2 (100.0)       49 ( 46.2)  
##      Unknown                     0 (  0.0)         0 (  0.0)        0 (  0.0)  
##   FAB (%)                                                                      
##      M0                          0 (  0.0)         0 (  0.0)        3 (  2.8)  
##      M1                          1 ( 12.5)         0 (  0.0)        5 (  4.7)  
##      M2                          1 ( 12.5)         1 ( 50.0)        5 (  4.7)  
##      M4                          5 ( 62.5)         1 ( 50.0)       78 ( 73.6)  
##      M5                          0 (  0.0)         0 (  0.0)        0 (  0.0)  
##      M6                          0 (  0.0)         0 (  0.0)        0 (  0.0)  
##      M7                          0 (  0.0)         0 (  0.0)        2 (  1.9)  
##      NA                          1 ( 12.5)         0 (  0.0)       13 ( 12.3)  
##   BlastPercent (mean (SD))     NaN (NA)          NaN (NA)         NaN (NA)     
##   fusion (%)                                                                   
##      KMT2A-ABI1                  0 (  0.0)         0 (  0.0)        1 (  0.9)  
##      KMT2A-AFF1                  0 (  0.0)         0 (  0.0)        3 (  2.8)  
##      KMT2A-AFF3                  0 (  0.0)         0 (  0.0)        0 (  0.0)  
##      KMT2A-ARAP1                 0 (  0.0)         0 (  0.0)        0 (  0.0)  
##      KMT2A-ARHGAP26              0 (  0.0)         0 (  0.0)        1 (  0.9)  
##      KMT2A-ARHGEF37              0 (  0.0)         0 (  0.0)        0 (  0.0)  
##      KMT2A-C9orf72               0 (  0.0)         0 (  0.0)        0 (  0.0)  
##      KMT2A-CCNJL                 0 (  0.0)         0 (  0.0)        0 (  0.0)  
##      KMT2A-DCP1A                 0 (  0.0)         0 (  0.0)        0 (  0.0)  
##      KMT2A-ELL                   3 ( 37.5)         0 (  0.0)       14 ( 13.2)  
##      KMT2A-EPS15                 0 (  0.0)         0 (  0.0)        1 (  0.9)  
##      KMT2A-FNBP1                 0 (  0.0)         0 (  0.0)        1 (  0.9)  
##      KMT2A-LASP1                 0 (  0.0)         0 (  0.0)        1 (  0.9)  
##      KMT2A-LYN                   0 (  0.0)         0 (  0.0)        0 (  0.0)  
##      KMT2A-MLLT1                 0 (  0.0)         0 (  0.0)        4 (  3.8)  
##      KMT2A-MLLT10                1 ( 12.5)         0 (  0.0)       26 ( 24.5)  
##      KMT2A-MLLT11                0 (  0.0)         0 (  0.0)        1 (  0.9)  
##      KMT2A-MLLT3                 1 ( 12.5)         0 (  0.0)       26 ( 24.5)  
##      KMT2A-MLLT3                 0 (  0.0)         0 (  0.0)        2 (  1.9)  
##      KMT2A-MLLT4                 3 ( 37.5)         0 (  0.0)       14 ( 13.2)  
##      KMT2A-MLLT6                 0 (  0.0)         0 (  0.0)        1 (  0.9)  
##      KMT2A-MYO1F                 0 (  0.0)         0 (  0.0)        0 (  0.0)  
##      KMT2A-MYOCD                 0 (  0.0)         0 (  0.0)        0 (  0.0)  
##      KMT2A-PALM2.AKAP2           0 (  0.0)         0 (  0.0)        0 (  0.0)  
##      KMT2A-PICALM                0 (  0.0)         0 (  0.0)        0 (  0.0)  
##      KMT2A-SARNP                 0 (  0.0)         0 (  0.0)        1 (  0.9)  
##      KMT2A-SEPT2                 0 (  0.0)         0 (  0.0)        0 (  0.0)  
##      KMT2A-SEPT6                 0 (  0.0)         0 (  0.0)        4 (  3.8)  
##      KMT2A-SEPT9                 0 (  0.0)         0 (  0.0)        2 (  1.9)  
##      KMT2A-USP2                  0 (  0.0)         0 (  0.0)        0 (  0.0)  
##      KMT2A-X                     0 (  0.0)         0 (  0.0)        2 (  1.9)  
##      MLLT10-KMT2A                0 (  0.0)         0 (  0.0)        1 (  0.9)  
##      NUP98-KDM5A                 0 (  0.0)         0 (  0.0)        0 (  0.0)  
##      NUP98-NSD1                  0 (  0.0)         2 (100.0)        0 (  0.0)  
##   FusionGroup = NSD1 (%)         0 (  0.0)         2 (100.0)        0 (  0.0)  
##   OS (mean (SD))           1399.62 (1334.80) 3161.00 (169.71) 1325.12 (1017.87)
##   OSI (mean (SD))             0.62 (0.52)       0.00 (0.00)      0.51 (0.50)   
##   Protocol (%)                                                                 
##      AAML03P1                    8 (100.0)         2 (100.0)        0 (  0.0)  
##      AAML0531                    0 (  0.0)         0 (  0.0)      106 (100.0)  
##      AAML1031                    0 (  0.0)         0 (  0.0)        0 (  0.0)  
##      CCG-2961                    0 (  0.0)         0 (  0.0)        0 (  0.0)  
##                           Stratified by FusionGroup:Protocol
##                            NSD1:AAML0531    MLL:AAML1031     NSD1:AAML1031  
##   n                             28              249              58         
##   AgeGroup (%)                                                              
##      AYA                         5 ( 17.9)       38 ( 15.3)      14 ( 24.1) 
##      Child                      22 ( 78.6)       97 ( 39.0)      40 ( 69.0) 
##      Infant                      1 (  3.6)      113 ( 45.4)       4 (  6.9) 
##      NA                          0 (  0.0)        1 (  0.4)       0 (  0.0) 
##   Sex (%)                                                                   
##      Female                      8 ( 28.6)      111 ( 44.6)      27 ( 46.6) 
##      Male                       20 ( 71.4)      137 ( 55.0)      31 ( 53.4) 
##      Unknown                     0 (  0.0)        1 (  0.4)       0 (  0.0) 
##   FAB (%)                                                                   
##      M0                          0 (  0.0)        4 (  1.6)       3 (  5.2) 
##      M1                          4 ( 14.3)        8 (  3.2)       7 ( 12.1) 
##      M2                          4 ( 14.3)        4 (  1.6)      10 ( 17.2) 
##      M4                         12 ( 42.9)       18 (  7.2)       6 ( 10.3) 
##      M5                          0 (  0.0)       97 ( 39.0)      15 ( 25.9) 
##      M6                          2 (  7.1)        0 (  0.0)       0 (  0.0) 
##      M7                          1 (  3.6)        5 (  2.0)       2 (  3.4) 
##      NA                          5 ( 17.9)      113 ( 45.4)      15 ( 25.9) 
##   BlastPercent (mean (SD))     NaN (NA)       60.83 (28.74)   63.63 (22.31) 
##   fusion (%)                                                                
##      KMT2A-ABI1                  0 (  0.0)        1 (  0.4)       0 (  0.0) 
##      KMT2A-AFF1                  0 (  0.0)        1 (  0.4)       0 (  0.0) 
##      KMT2A-AFF3                  0 (  0.0)        1 (  0.4)       0 (  0.0) 
##      KMT2A-ARAP1                 0 (  0.0)        1 (  0.4)       0 (  0.0) 
##      KMT2A-ARHGAP26              0 (  0.0)        0 (  0.0)       0 (  0.0) 
##      KMT2A-ARHGEF37              0 (  0.0)        1 (  0.4)       0 (  0.0) 
##      KMT2A-C9orf72               0 (  0.0)        1 (  0.4)       0 (  0.0) 
##      KMT2A-CCNJL                 0 (  0.0)        1 (  0.4)       0 (  0.0) 
##      KMT2A-DCP1A                 0 (  0.0)        1 (  0.4)       0 (  0.0) 
##      KMT2A-ELL                   0 (  0.0)       21 (  8.4)       0 (  0.0) 
##      KMT2A-EPS15                 0 (  0.0)        1 (  0.4)       0 (  0.0) 
##      KMT2A-FNBP1                 0 (  0.0)        1 (  0.4)       0 (  0.0) 
##      KMT2A-LASP1                 0 (  0.0)        3 (  1.2)       0 (  0.0) 
##      KMT2A-LYN                   0 (  0.0)        1 (  0.4)       0 (  0.0) 
##      KMT2A-MLLT1                 0 (  0.0)       14 (  5.6)       0 (  0.0) 
##      KMT2A-MLLT10                0 (  0.0)       55 ( 22.1)       0 (  0.0) 
##      KMT2A-MLLT11                0 (  0.0)        6 (  2.4)       0 (  0.0) 
##      KMT2A-MLLT3                 0 (  0.0)       92 ( 36.9)       0 (  0.0) 
##      KMT2A-MLLT3                 0 (  0.0)        0 (  0.0)       0 (  0.0) 
##      KMT2A-MLLT4                 0 (  0.0)       26 ( 10.4)       0 (  0.0) 
##      KMT2A-MLLT6                 0 (  0.0)        1 (  0.4)       0 (  0.0) 
##      KMT2A-MYO1F                 0 (  0.0)        3 (  1.2)       0 (  0.0) 
##      KMT2A-MYOCD                 0 (  0.0)        1 (  0.4)       0 (  0.0) 
##      KMT2A-PALM2.AKAP2           0 (  0.0)        1 (  0.4)       0 (  0.0) 
##      KMT2A-PICALM                0 (  0.0)        1 (  0.4)       0 (  0.0) 
##      KMT2A-SARNP                 0 (  0.0)        0 (  0.0)       0 (  0.0) 
##      KMT2A-SEPT2                 0 (  0.0)        1 (  0.4)       0 (  0.0) 
##      KMT2A-SEPT6                 0 (  0.0)        8 (  3.2)       0 (  0.0) 
##      KMT2A-SEPT9                 0 (  0.0)        2 (  0.8)       0 (  0.0) 
##      KMT2A-USP2                  0 (  0.0)        1 (  0.4)       0 (  0.0) 
##      KMT2A-X                     0 (  0.0)        2 (  0.8)       0 (  0.0) 
##      MLLT10-KMT2A                0 (  0.0)        0 (  0.0)       0 (  0.0) 
##      NUP98-KDM5A                 0 (  0.0)        0 (  0.0)       0 (  0.0) 
##      NUP98-NSD1                 28 (100.0)        0 (  0.0)      58 (100.0) 
##   FusionGroup = NSD1 (%)        28 (100.0)        0 (  0.0)      58 (100.0) 
##   OS (mean (SD))           1053.39 (918.60) 1040.41 (732.52) 865.14 (631.97)
##   OSI (mean (SD))             0.57 (0.50)      0.47 (0.50)     0.55 (0.50)  
##   Protocol (%)                                                              
##      AAML03P1                    0 (  0.0)        0 (  0.0)       0 (  0.0) 
##      AAML0531                   28 (100.0)        0 (  0.0)       0 (  0.0) 
##      AAML1031                    0 (  0.0)      249 (100.0)      58 (100.0) 
##      CCG-2961                    0 (  0.0)        0 (  0.0)       0 (  0.0) 
##                           Stratified by FusionGroup:Protocol
##                            MLL:CCG-2961      NSD1:CCG-2961    p      test
##   n                              4               15                      
##   AgeGroup (%)                                                <0.001     
##      AYA                         0 (  0.0)        2 ( 13.3)              
##      Child                       2 ( 50.0)       12 ( 80.0)              
##      Infant                      2 ( 50.0)        1 (  6.7)              
##      NA                          0 (  0.0)        0 (  0.0)              
##   Sex (%)                                                      0.050     
##      Female                      3 ( 75.0)        0 (  0.0)              
##      Male                        1 ( 25.0)       15 (100.0)              
##      Unknown                     0 (  0.0)        0 (  0.0)              
##   FAB (%)                                                     <0.001     
##      M0                          0 (  0.0)        0 (  0.0)              
##      M1                          0 (  0.0)        2 ( 13.3)              
##      M2                          0 (  0.0)        8 ( 53.3)              
##      M4                          4 (100.0)        5 ( 33.3)              
##      M5                          0 (  0.0)        0 (  0.0)              
##      M6                          0 (  0.0)        0 (  0.0)              
##      M7                          0 (  0.0)        0 (  0.0)              
##      NA                          0 (  0.0)        0 (  0.0)              
##   BlastPercent (mean (SD))     NaN (NA)         NaN (NA)       0.513     
##   fusion (%)                                                     NaN     
##      KMT2A-ABI1                  0 (  0.0)        0 (  0.0)              
##      KMT2A-AFF1                  0 (  0.0)        0 (  0.0)              
##      KMT2A-AFF3                  0 (  0.0)        0 (  0.0)              
##      KMT2A-ARAP1                 0 (  0.0)        0 (  0.0)              
##      KMT2A-ARHGAP26              0 (  0.0)        0 (  0.0)              
##      KMT2A-ARHGEF37              0 (  0.0)        0 (  0.0)              
##      KMT2A-C9orf72               0 (  0.0)        0 (  0.0)              
##      KMT2A-CCNJL                 0 (  0.0)        0 (  0.0)              
##      KMT2A-DCP1A                 0 (  0.0)        0 (  0.0)              
##      KMT2A-ELL                   2 ( 50.0)        0 (  0.0)              
##      KMT2A-EPS15                 0 (  0.0)        0 (  0.0)              
##      KMT2A-FNBP1                 0 (  0.0)        0 (  0.0)              
##      KMT2A-LASP1                 0 (  0.0)        0 (  0.0)              
##      KMT2A-LYN                   0 (  0.0)        0 (  0.0)              
##      KMT2A-MLLT1                 1 ( 25.0)        0 (  0.0)              
##      KMT2A-MLLT10                1 ( 25.0)        0 (  0.0)              
##      KMT2A-MLLT11                0 (  0.0)        0 (  0.0)              
##      KMT2A-MLLT3                 0 (  0.0)        0 (  0.0)              
##      KMT2A-MLLT3                 0 (  0.0)        0 (  0.0)              
##      KMT2A-MLLT4                 0 (  0.0)        0 (  0.0)              
##      KMT2A-MLLT6                 0 (  0.0)        0 (  0.0)              
##      KMT2A-MYO1F                 0 (  0.0)        0 (  0.0)              
##      KMT2A-MYOCD                 0 (  0.0)        0 (  0.0)              
##      KMT2A-PALM2.AKAP2           0 (  0.0)        0 (  0.0)              
##      KMT2A-PICALM                0 (  0.0)        0 (  0.0)              
##      KMT2A-SARNP                 0 (  0.0)        0 (  0.0)              
##      KMT2A-SEPT2                 0 (  0.0)        0 (  0.0)              
##      KMT2A-SEPT6                 0 (  0.0)        0 (  0.0)              
##      KMT2A-SEPT9                 0 (  0.0)        0 (  0.0)              
##      KMT2A-USP2                  0 (  0.0)        0 (  0.0)              
##      KMT2A-X                     0 (  0.0)        0 (  0.0)              
##      MLLT10-KMT2A                0 (  0.0)        0 (  0.0)              
##      NUP98-KDM5A                 0 (  0.0)        0 (  0.0)              
##      NUP98-NSD1                  0 (  0.0)       15 (100.0)              
##   FusionGroup = NSD1 (%)         0 (  0.0)       15 (100.0)   <0.001     
##   OS (mean (SD))           1228.25 (1282.76) 791.20 (1011.49) <0.001     
##   OSI (mean (SD))             0.75 (0.50)      0.87 (0.35)     0.060     
##   Protocol (%)                                                <0.001     
##      AAML03P1                    0 (  0.0)        0 (  0.0)              
##      AAML0531                    0 (  0.0)        0 (  0.0)              
##      AAML1031                    0 (  0.0)        0 (  0.0)              
##      CCG-2961                    4 (100.0)       15 (100.0)
```
