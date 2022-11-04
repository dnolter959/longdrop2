longdrop = function(MAR_model, id_var, timepoints, idx_of_coef_of_interest, K, M=20){
  # Extract objects from model for use in imputation
  uids = unique(MAR_model$data$id_var)
  q = length(unique(uids))
  trace_of_hat_matrix = sum(hatvalues(fit_lmm(MAR_model$data, "lme4")))
  design_X_full = generate_design_X_full(MAR_model$data, uids, timepoints)
  sigma_hat_squared = MAR_model$sigma^2

  # Calculate results hash
  results_by_k = hash()
  for (k in K){
    complete_datasets = hash()
    for (m in 1:M){
      Beta_m            = generate_random_Beta_m(uids, MAR_model)
      beta_i_ms         = generate_random_beta_i_ms(uids, MAR_model)
      complete_dataset  = generate_complete_dataset_using_multiple_imputation(design_X_full, uids, timepoints, q, Beta_m, beta_i_ms, trace_of_hat_matrix, sigma_hat_squared)
      complete_datasets[[toString(m)]] = complete_dataset
    }
    results_by_k[[toString(k)]] = aggregate_results_using_Rubins_rules(M, complete_datasets, idx_of_coef_of_interest)
  }

  return(results_by_k)
}



