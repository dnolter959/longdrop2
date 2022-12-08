#' Title
#'
#' @param uids
#' @param mod
#'
#' @return
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

#' Title
#'
#' @param uids
#' @param mod
#'
#' @return
#' @export
#'
#' @examples
generate_random_beta_i_ms = function(uids, mod){
  beta_i_hats = cbind(uids, ranef(mod)[[1]])
  vcov_beta_i = matrix(unclass(VarCorr(MAR_model))[[1]], 2, 2)
  beta_i_ms = hash::hash()
  for (uid in uids) {
    beta_i_hat = as.numeric(beta_i_hats[which(uids == uid), 2:3])
    beta_i_m = MASS::mvrnorm(1, beta_i_hat, vcov_beta_i)
    beta_i_ms[[toString(uid)]] = beta_i_m
  }
  return(beta_i_ms)
}
