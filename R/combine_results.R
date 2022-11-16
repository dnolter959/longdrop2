calculate_theta_and_variance_hats = function(cal, M, complete_datasets, idx_of_coef_of_interest, call){
  theta_hats = c()
  variance_hats = c()

  for (m in 1:M){
    complete_dataset = complete_datasets[[toString(m)]]
    lmm = lmer(cal, complete_dataset)
    theta_hat = extract_inferential_statistics_from_lmm(lmm, idx_of_coef_of_interest)[["point-estimate"]]
    variance_hat = extract_inferential_statistics_from_lmm(lmm, idx_of_coef_of_interest)[["standard-error"]]^2
    theta_hats = append(theta_hats, theta_hat)
    variance_hats = append(variance_hats, variance_hat)
  }
  return(list(theta_hats, variance_hats))
}

aggregate_results_using_Rubins_rules = function(MAR_model, M, complete_datasets, idx_of_coef_of_interest, null_value = 0, alpha = 0.05){
  var_and_theta_hats =calculate_theta_and_variance_hats(MAR_model@call[[2]], M, complete_datasets, idx_of_coef_of_interest)
  theta_hats = var_and_theta_hats[[1]]
  variance_hats = var_and_theta_hats[[2]]

  # Calculate theta_hat_star
  theta_hat_star = mean(theta_hats)

  # Calculate variance_hat_star
  fun = function(theta_hat) {(theta_hat - theta_hat_star)^2}
  W_hat = 1/M*sum(variance_hats)
  B_hat = sum(sapply(theta_hats, fun))/(M-1)
  variance_hat_star = W_hat + (1 + 1/M)*B_hat

  # Calculate CI
  results = hash()
  t_stat = (theta_hat_star - null_value)/sqrt(variance_hat_star)
  df = (M-1)*(1 + W_hat/((1 + 1/M)*B_hat))
  p_val = ifelse(t_stat >= 0, 2*(1-pt(t_stat, df)), 2*pt(t_stat, df))
  results[["point_estimate"]] = theta_hat_star; results[["standard-error"]] = sqrt(variance_hat_star)
  results[["t-statistic"]] = t_stat; results[["p-value"]] = p_val
  return(results)
}
