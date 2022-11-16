longdrop = function(MAR_model, time_var, idx_of_coef_of_interest, K, M=20){
  # Extract objects from model for use in imputation
  uids = unique(as.numeric(as.character(MAR_model@flist[[1]])))
  timepoints = sort(unique(MAR_model@frame[, which(colnames(MAR_model@frame) == time_var)]))
  design_X_full = generate_design_X_full(MAR_model, uids, timepoints, time_var)
  var_cor = as.data.frame(VarCorr(MAR_model))
  sigma_hat_squared = var_cor[nrow(var_cor), ncol(var_cor)]^2

  # Calculate results hash
  results_by_k = hash()
  for (k in K){
    complete_datasets = hash()
    for (m in 1:M){
      Beta_m            = generate_random_Beta_m(uids, MAR_model)
      beta_i_ms         = generate_random_beta_i_ms(uids, MAR_model)
      complete_dataset  = generate_complete_dataset_using_multiple_imputation(MAR_model, design_X_full, k, uids, timepoints, Beta_m, beta_i_ms)
      complete_datasets[[toString(m)]] = complete_dataset
    }
    results_by_k[[toString(k)]] = aggregate_results_using_Rubins_rules(MAR_model, M, complete_datasets, idx_of_coef_of_interest)
  }

  return(results_by_k)
}
