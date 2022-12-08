
# longdrop2

<!-- badges: start -->
<!-- badges: end -->

The goal of longdrop2 is to:

## Installation

You can install the development version of longdrop2 like so:

``` r
# install.packages("devtools")
devtools::install_github("dnolter959/longdrop2")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(longdrop2)
MAR_model = lmer(WHO5 ~ arm + week + arm*week + (1 + week | uid), data = data)
time_var = "week"
idx_of_coef_of_interest = 4
K = c(-1, 0, 1)
M = 3

ld = longdrop(MAR_model, time_var, idx_of_coef_of_interest, K, M)
```

``` r
plot(longdrop)
```

![](man/figures/effect-by-k.png)
