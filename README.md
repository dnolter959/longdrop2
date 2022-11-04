
# longdrop2

<!-- badges: start -->
<!-- badges: end -->

The goal of longdrop2 is to …

## Installation

You can install the development version of longdrop2 like so:

``` r
# install.packages("devtools")
devtools::install_github("dnolter959/longdrop")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(longdrop2)

MAR_model = fit_lmm(data)
id_var = "uid"
timepoints = c(0, 2, 4, 6, 8, 12, 16, 20)
idx_of_coef_of_interest = 4
K = c(0)
M = 5

longdrop(MAR_model, id_var, timepoints, idx_of_coef_of_interest, K, M)
```
