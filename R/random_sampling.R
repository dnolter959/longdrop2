generate_random_Beta_m = function(uids, mod){
  Beta_hat = coef(summary(mod))[, 1]
  Vcov_hat = vcov(mod)

  # Sample Beta_m
  Beta_m = mvrnorm(1, Beta_hat, Vcov_hat)
  return(matrix(Beta_m))
}

generate_random_beta_i_ms = function(uids, mod){
  beta_i_hats = cbind(uids, ranef(mod))
  beta_i_ms = hash()
  for (uid in uids) {
    beta_i_hat = as.numeric(beta_i_hats[which(uids == uid), 2:3])
    vocv_beta_i = getVarCov(mod, individual = uid, type = "random.effects")
    beta_i_m = mvrnorm(1, beta_i_hat, vocv_beta_i)
    beta_i_ms[[toString(uid)]] = beta_i_m
  }
  return(beta_i_ms)
}
