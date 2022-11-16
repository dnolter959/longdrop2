extract_inferential_statistics_from_lmm = function(mod, idx_of_coef_of_interest){
  results_hash = hash()
  results_hash[["point-estimate"]] = round(coef(summary(mod))[idx_of_coef_of_interest, 1], 3)
  results_hash[["standard-error"]] = round(coef(summary(mod))[idx_of_coef_of_interest, 2], 3)

  return(results_hash)
}

generate_design_X_full = function(MAR_model, uids, timepoints, time_var){
  n = length(timepoints)
  q = length(uids)

  design_X_full = data.frame(ones = rep(1, n*q), uid = sort(rep(uids, n)), time_var = rep(timepoints, q))
  colnames(design_X_full) = c("Intercept", "uid", time_var)

  covariate_lookup = distinct(data.frame(uid = as.numeric(as.character(MAR_model@flist[[1]])), cbind(MAR_model@pp$X[, 2])))
  colnames(covariate_lookup) = c("uid", "armfull")

  design_X_full_comp = left_join(design_X_full, covariate_lookup, by = c("uid")) %>%
    mutate(armfull_week = armfull*week) %>%
    dplyr::select(Intercept, armfull, week, armfull_week) %>% data.matrix()

  return(design_X_full_comp)
}
