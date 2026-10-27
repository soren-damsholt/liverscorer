
<!-- README.md is generated from README.Rmd. Please edit that file -->

# liverscoreR

[![R-CMD-check](https://github.com/soren-damsholt/liverscorer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/soren-damsholt/liverscorer/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/soren-damsholt/liverscorer/graph/badge.svg)](https://app.codecov.io/gh/soren-damsholt/liverscorer)

Collection of severity scores in general hepatology. Currently i have
created fully vectorized functions for calculating:

- MELD score

- MELD-Na score

- MELD 3.0 score

- CLIF-C-AD score

- CLIF-C-ACLF score

- CLIF-C-OF score

- Liver frailty index

- ELF score

- FIB4 index

## Installation

You can install the development version of liverscoreR from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("soren-damsholt/liverscorer")
```

## Example

This package contains several

``` r
library(liverscoreR)
## basic calculations of singular values
meld(creatinine = 120, bilirubin = 80, inr = 2, dialysis = FALSE, unit = "SI")
#> [1] 23

lfi(grip1 = 23, grip2 = 25, grip3 = 22, time_to_stand = 14, side_balance = 10,
    semi_tandem = 10, tandem = 7, sex = "male")
#> [1] 4.38

## However it can also work with large vectors.

meld_data <- data.frame(creatinine = sample(150, 20),
           bilirubin = sample(120, 20),
           inr = sample(2.5, 20, replace = TRUE))

meld_data$meld <- meld(creatinine = meld_data$creatinine, 
                        bilirubin = meld_data$bilirubin,
                        inr = meld_data$inr, dialysis = FALSE,
                        unit = "SI")

## This should also work seemlessly with the tidyverse functions
library(tidyverse)
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.2.1     ✔ readr     2.2.0
#> ✔ forcats   1.0.1     ✔ stringr   1.6.0
#> ✔ ggplot2   4.0.3     ✔ tibble    3.3.1
#> ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
#> ✔ purrr     1.2.2     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

meld_data %>% 
  mutate(meld2 = meld(creatinine = creatinine,
                      bilirubin = bilirubin, 
                      inr = inr, dialysis = FALSE,
                      unit = "SI"))
#>    creatinine bilirubin inr meld meld2
#> 1         134       115   2   25    25
#> 2          98        55   1   12    12
#> 3          33        36   2   17    17
#> 4          62        46   2   18    18
#> 5         143        76   1   17    17
#> 6          58       119   1   14    14
#> 7         148        78   1   17    17
#> 8          52        19   2   15    15
#> 9          50        96   2   21    21
#> 10         84        40   2   17    17
#> 11          6        67   2   19    19
#> 12        146       103   2   26    26
#> 13         31        53   2   18    18
#> 14        119        34   1   12    12
#> 15         74        54   1   11    11
#> 16         44        51   1   11    11
#> 17         97       101   2   22    22
#> 18         47        84   2   20    20
#> 19         78        65   2   19    19
#> 20          3        12   1    6     6


## You will notice that here dialysis is a scalar set to "no". However you can also pass a vector of "yes" and "no" per subject in case you have that information. Several functions have buildt in possibility for using binary setting as both vectors and scalars. See documentation for the specific functions.

meld_data %>% 
  mutate(dialysis = sample(c(TRUE, FALSE), 20, replace = TRUE),
         meld2 = meld(creatinine, bilirubin, inr, dialysis = dialysis,
                     unit = "SI"))
#>    creatinine bilirubin inr meld dialysis meld2
#> 1         134       115   2   25    FALSE    25
#> 2          98        55   1   12    FALSE    12
#> 3          33        36   2   17     TRUE    30
#> 4          62        46   2   18    FALSE    18
#> 5         143        76   1   17    FALSE    17
#> 6          58       119   1   14     TRUE    27
#> 7         148        78   1   17    FALSE    17
#> 8          52        19   2   15     TRUE    28
#> 9          50        96   2   21     TRUE    34
#> 10         84        40   2   17     TRUE    31
#> 11          6        67   2   19    FALSE    19
#> 12        146       103   2   26     TRUE    34
#> 13         31        53   2   18    FALSE    18
#> 14        119        34   1   12    FALSE    12
#> 15         74        54   1   11     TRUE    24
#> 16         44        51   1   11    FALSE    11
#> 17         97       101   2   22    FALSE    22
#> 18         47        84   2   20    FALSE    20
#> 19         78        65   2   19     TRUE    33
#> 20          3        12   1    6     TRUE    20
```
