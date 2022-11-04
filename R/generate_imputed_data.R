calculate_epsilon = function(N, trace_of_hat_matrix, sigma_hat_squared){
  q = N - trace_of_hat_matrix
  random_chi_squared_q = rchisq(1, q)
  sigma_squared_m = q*sigma_hat_squared/random_chi_squared_q
  epsilon = matrix(rnorm(N, 0, sqrt(sigma_squared_m)))
  return(epsilon)
}

construct_Z = function(timepoints, q){
  block = matrix(c(rep(1, length(timepoints)), timepoints), ncol=2)
  Z = .bdiag(rep(list(block), q))
  return(Z)
}

construct_u = function(uids, beta_i_ms) {
  u = c()
  for (i in uids){
    u = append(u, beta_i_ms[[toString(i)]])
  }
  return(matrix(u))
}

generate_complete_dataset_using_multiple_imputation = function(X, uids, timepoints, q, Beta_m, beta_i_ms, trace_of_hat_matrix, sigma_hat_squared){
  n = length(timepoints)
  N = q*n
  Z = construct_Z(timepoints, q)
  u = construct_u(uids, beta_i_ms)
  R = rep(1, n*q)
  epsilon = calculate_epsilon(N, trace_of_hat_matrix, sigma_hat_squared)
  imputed_values = X %*% Beta_m + Z %*% u + k*R + epsilon

  full_imputed_data = data.frame(
    uid = sort(rep(uids, n)),
    arm = X[, 2],
    week = X[, 3],
    WHO5_imputed = as.vector(imputed_values)
  ) %>% mutate(arm = factor(arm, levels = c(0, 1)))
  colnames(full_imputed_data) = c("uid", "arm", "week", "WHO5_imputed")

  complete_dataset = left_join(full_imputed_data, data, by = c("uid", "week", "arm")) %>%
    mutate(WHO5 = ifelse(is.na(WHO5), WHO5_imputed, WHO5)) %>%
    dplyr::select(-WHO5_imputed) %>%
    arrange(uid, week)
  return(complete_dataset)
}
