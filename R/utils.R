extract_inferential_statistics_from_lmm = function(mod, idx_of_coef_of_interest){
  results_hash = hash()
  results_hash[["point-estimate"]] = round(coef(summary(mod))[idx_of_coef_of_interest, 1], 3)
  results_hash[["standard-error"]] = round(coef(summary(mod))[idx_of_coef_of_interest, 2], 3)

  return(results_hash)
}

generate_design_X_full = function(MAR_model, uids, timepoints, time_var, type="matrix"){
  # generate fully expanded id-time
  outcome_var = all.vars(as.formula(MAR_model@call))[1]
  uid_var = names(MAR_model@flist[1])

  id_time = expand.grid(uids, timepoints)
  names(id_time) = c(uid_var, time_var)
  id_time = id_time[order(id_time[,uid_var], id_time[,time_var]), ]

  # extract non id/time data
  # create a = uid | group (unique), all baseline covariates
  # left join id_time to a by uid
  # left join a
  full_data = left_join(id_time, MAR_model@frame, by=c(uid_var, time_var))
  full_data$missing = ifelse(is.na(full_data[,outcome_var]), 1, 0)
  full_data = full_data %>% group_by_at(uid_var) %>%
    fill(everything())

  if (type == "dataframe"){return(full_data)}

  # extract full design matrix
  tmp = model.matrix(as.formula(MAR_model@call), data = full_data)
  full_mod = lmer(as.formula(MAR_model@call), data = full_data)
  design_X_full_comp = model.matrix(full_mod)

  return(design_X_full_comp)
}

