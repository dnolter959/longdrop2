#' Performs MI, combines results, returns a longdrop object
#'
#' @param MAR_model An instance of class lmerMod from the package lme4 (fit using `lmer`)
#' @param time_var The time-related variable in the longitudinal dataset
#' @param idx_of_coef_of_interest Index of the fixed effect coefficient of interest for analysis (corresponds to the variable order provided in the lmer call, or the `summary` of the model fixed effects)
#' @param K A vector defining the set of affine offsets characterizing the distirbution of the missing outcomes (with respect to those observed)
#' @param M The number of imputed datasets per value of K
#'
#' @return A `hash` object with inferential statistics on the coefficient of interest for each value of k.
#' @export
#'
#' @examples
longdrop = function(MAR_model, time_var, idx_of_coef_of_interest, K, M=20){
  # Extract objects from model for use in imputation
  outcome_var = all.vars(as.formula(MAR_model@call))[1]
  uids = unique(as.numeric(as.character(MAR_model@flist[[1]])))
  timepoints = sort(unique(MAR_model@frame[, which(colnames(MAR_model@frame) == time_var)]))
  design_X_full = generate_design_X_full(MAR_model, uids, timepoints, time_var, outcome_var)
  var_cor = as.data.frame(VarCorr(MAR_model))
  sigma_hat_squared = var_cor[nrow(var_cor), ncol(var_cor)]^2

  # Calculate results hash
  results_by_k = hash::hash()
  for (k in K){
    complete_datasets = hash::hash()
    for (m in 1:M){
      Beta_m            = generate_random_Beta_m(uids, MAR_model)
      beta_i_ms         = generate_random_beta_i_ms(uids, MAR_model)
      complete_dataset  = generate_complete_dataset_using_multiple_imputation(MAR_model, design_X_full, k, uids, time_var, outcome_var, timepoints, Beta_m, beta_i_ms)
      complete_datasets[[toString(m)]] = complete_dataset
    }
    results_by_k[[toString(k)]] = aggregate_results_using_Rubins_rules(MAR_model, M, complete_datasets, idx_of_coef_of_interest)
  }

  out = list(
    results = results_by_k,
    call = match.call(),
    args = list(model = MAR_model, time_var = time_var, idx = idx_of_coef_of_interest, K = K, M = M)
  )

  class(out) = "longdrop"
  return(out)
}
