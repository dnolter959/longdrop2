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

generate_complete_dataset_using_multiple_imputation = function(MAR_model, design_X_full, k, uids, time_var, outcome_var, timepoints, Beta_m, beta_i_ms){
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

  full_dataset = generate_design_X_full(MAR_model, uids, timepoints, time_var, outcome_var, "dataframe")
  full_dataset$imputed_values = as.vector(imputed_values)
  full_dataset[, "observed_val"] = full_dataset[, outcome_var]
  full_dataset[, outcome_var] = ifelse(full_dataset$missing == 1, full_dataset$imputed_values, full_dataset$observed_val)
  full_dataset = full_dataset %>% dplyr::select(-c(observed_val, -imputed_values))

  return(full_dataset)
}
