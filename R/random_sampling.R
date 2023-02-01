#' Draw random Beta M, for fixed effects
#'
#' @param uids A vector of uids in the provided dataset
#' @param mod An instance of class lmerMod from the package lme4 (fit using `lmer`)
#'
#' @return A random draw of Beta_m for use in multiple imputation
#' @export
#'
#' @examples
generate_random_Beta_m = function(uids, mod){
  Beta_hat = mod@beta
  Vcov_hat = vcov(mod)

  # Sample Beta_m
  Beta_m = MASS::mvrnorm(1, Beta_hat, Vcov_hat)
  return(matrix(Beta_m))
}

#' Draw random beta M, for random effects
#'
#' @param uids A vector of uids in the provided dataset
#' @param mod An instance of class lmerMod from the package lme4 (fit using `lmer`)
#'
#' @return A hash containing random draws of random intercepts/slopes for each participant, for use in multiple imputation
#' @export
#'
#' @examples
generate_random_beta_i_ms = function(uids, mod){
  beta_i_hats = cbind(uids, ranef(mod)[[1]])
  idx = dim(beta_i_hats)[2]

  # Random intercepts and slopes
  if (idx == 3) {
    vcov_beta_i = matrix(unclass(VarCorr(MAR_model))[[1]], 2, 2)
  }
  # Random intercepts only
  else if (idx == 2) {
    vcov_beta_i = as.numeric(unclass(VarCorr(MAR_model))[[1]])
  }

  beta_i_ms = hash::hash()
  for (uid in uids) {
    beta_i_hat = as.numeric(beta_i_hats[which(uids == uid), 2:idx])
    beta_i_m = MASS::mvrnorm(1, beta_i_hat, vcov_beta_i)
    beta_i_ms[[toString(uid)]] = beta_i_m
  }
  return(beta_i_ms)
}
