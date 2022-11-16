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

generate_complete_dataset_using_multiple_imputation = function(MAR_model, design_X_full, k, uids, timepoints, Beta_m, beta_i_ms){
  q = length(unique(uids))
  trace_of_hat_matrix = sum(hatvalues(MAR_model))
  var_cor = as.data.frame(VarCorr(MAR_model))
  sigma_hat_squared = var_cor[nrow(var_cor), ncol(var_cor)]^2
  n = length(timepoints)
  N = q*n
  Z = construct_Z(timepoints, q)
  u = construct_u(uids, beta_i_ms)
  R = rep(1, n*q)
  epsilon = calculate_epsilon(N, trace_of_hat_matrix, sigma_hat_squared)
  imputed_values = design_X_full %*% Beta_m + Z %*% u + k*R + epsilon

  full_imputed_data = data.frame(
    uid = as.numeric(as.character(sort(rep(uids, n)))),
    arm = design_X_full[, 2],
    week = design_X_full[, 3],
    WHO5_imputed = as.vector(imputed_values)
  )
  colnames(full_imputed_data) = c("uid", "arm", "week", "WHO5_imputed")

  complete_dataset = left_join(full_imputed_data, MAR_model@frame, by = c("uid", "week")) %>%
    mutate(WHO5 = ifelse(is.na(WHO5), WHO5_imputed, WHO5)) %>%
    dplyr::select(-WHO5_imputed, -arm.y) %>%
    dplyr::rename(arm = arm.x) %>%
    arrange(uid, week)
  return(complete_dataset)
}
