
# longdrop2

## Overview

This package implements a statistical method proposed by Moreno-Betancur
and Chavance in their 2013
[paper](https://journals.sagepub.com/doi/10.1177/0962280213490014),
which facilitates sensitivity analyses of Mixed Model results to various
departures from the Missing at Random (MAR) assumption, in the presence
of drop-out.

## Description

Mixed Models are commonly used to make inference in the longitudinal
setting. In the event of study drop-out, one must proceed to
characterize whether data is Missing Completely at Random (MCAR),
Missing at Random (MAR), or Missing Not at Random (MNAR). In the event
that data is MCAR or MAR, no adjustments need to be made and one may
interpret fixed effects in the same way as if there was no missingness;
this does not hold in the case of MNAR.

A fundamental problem is that there is no way to know for sure whether
data is MNAR or MAR; and so it is common to proceed to conduct a
sensitivity analysis which assumes MNAR in a variety of ways, and
compares these results to those assuming MAR.

This package facilitates conducting such sensitivity analyses by
allowing a user to specify departures from the MAR assumption by choosing
multiple values of k, each of which characterizes the distributional
differences between the observed and missing outcomes via an affine
shift. For each value of k, the package uses multiple imputation (M defaults to 20) to make 
corresponding random draws of the longitudinal outcomes; results are then combined 
using Rubin’s rules as specified in the original Moreno-Betancur and Chavance paper
linked above.

The user can then plot results across many values of k (k=0 represents
the MAR case) to observe sensitivity of effects to departures from the
MAR assumption.

## Installation

You can install the development version of longdrop2 like so:

``` r
install.packages("devtools")
devtools::install_github("dnolter959/longdrop2")
```

## Example

``` r
library(longdrop2)
MAR_model = lmer(outcome ~ arm + week + arm*week + (1 + week | uid), data = data)
time_var = "week"
idx_of_coef_of_interest = 4
K = c(-20, -15, -10, 3, 2, 1, 0, 1, 2, 3, 10, 15, 20)
M = 20

ld = longdrop(MAR_model, time_var, idx_of_coef_of_interest, K, M)
```

In this setting a user has a dataset called `data`.

- They first fit a MAR model using the `lmer` function from the `lme4`
  package
- Then they specify:
  - The time variable,
  - The index of the coefficient of interest (in this case it is 4
    because the user is interested in the coefficient on the interaction
    term)
  - A set, K, of k values to test
  - M, the number of random samples imputed)

The `longdrop` function returns a `hash` object with inferential results
for each value of k (point estimate, 95% CI, standard errors, p-value)

If the user calls `plot` on the longdrop object, they can see a plot of
the effect size of interest and associated confidence intervals for each
value of k.

``` r
plot(ld)
```

![](man/figures/effect-by-k.png) In this case, the treatment effect
doesn’t differ much for different values of k, suggesting low
sensitivity to departures from MAR in this case.
