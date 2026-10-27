
# liverscoreR <a href="https://github.com/soren-damsholt/liverscoreR"><img src="man/figures/logo.png" align="right" height="138" /></a>

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
set.seed(123456)
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
#> 1          60        67   1   12    12
#> 2          42        98   2   21    21
#> 3         113       111   2   24    24
#> 4         126        70   2   23    23
#> 5          74        63   2   19    19
#> 6           3        48   1   10    10
#> 7         128        29   2   20    20
#> 8         109        46   2   20    20
#> 9          94        38   1   10    10
#> 10          2        54   2   19    19
#> 11         80        50   2   18    18
#> 12         85        62   1   11    11
#> 13         65       119   1   14    14
#> 14        127        30   2   20    20
#> 15        144        73   2   24    24
#> 16         46        10   2   14    14
#> 17        125       115   2   25    25
#> 18         35         4   2   14    14
#> 19        140        26   2   20    20
#> 20         87        82   2   20    20


## You will notice that here dialysis is a scalar set to "no". However you can also pass a vector of "yes" and "no" per subject in case you have that information. Several functions have buildt in possibility for using binary setting as both vectors and scalars. See documentation for the specific functions.

meld_data %>% 
  mutate(dialysis = sample(c(TRUE, FALSE), 20, replace = TRUE),
         meld2 = meld(creatinine, bilirubin, inr, dialysis = dialysis,
                     unit = "SI"))
#>    creatinine bilirubin inr meld dialysis meld2
#> 1          60        67   1   12    FALSE    12
#> 2          42        98   2   21     TRUE    34
#> 3         113       111   2   24    FALSE    24
#> 4         126        70   2   23    FALSE    23
#> 5          74        63   2   19    FALSE    19
#> 6           3        48   1   10    FALSE    10
#> 7         128        29   2   20     TRUE    29
#> 8         109        46   2   20    FALSE    20
#> 9          94        38   1   10     TRUE    23
#> 10          2        54   2   19     TRUE    32
#> 11         80        50   2   18    FALSE    18
#> 12         85        62   1   11     TRUE    25
#> 13         65       119   1   14     TRUE    27
#> 14        127        30   2   20     TRUE    30
#> 15        144        73   2   24     TRUE    33
#> 16         46        10   2   14     TRUE    27
#> 17        125       115   2   25    FALSE    25
#> 18         35         4   2   14     TRUE    27
#> 19        140        26   2   20    FALSE    20
#> 20         87        82   2   20     TRUE    33
```
