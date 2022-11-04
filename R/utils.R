fit_lmm = function(data, type = "nlme"){
  if (type == "nlme") {
    mod = lme(
      fixed = WHO5 ~ arm + week + arm*week,
      control = lmeControl(opt = "optim"),
      data = data,
      random = ~1 + week | uid,
      method = "ML"
    )
  }
  if (type == "lme4"){
    mod = lmer(
      WHO5 ~ arm + week + arm*week + (1 + week | uid),
      data = data
    )
  }
  return(mod)
}

extract_inferential_statistics_from_lmm = function(mod, idx_of_coef_of_interest){
  results_hash = hash()
  results_hash[["point-estimate"]] = round(coef(summary(mod))[idx_of_coef_of_interest, 1], 3)
  results_hash[["standard-error"]] = round(coef(summary(mod))[idx_of_coef_of_interest, 2], 3)
  results_hash[["t-statistic"]] = round(coef(summary(mod))[idx_of_coef_of_interest, 4], 3)
  results_hash[["p-value"]] = round(coef(summary(mod))[idx_of_coef_of_interest, 5], 3)

  return(results_hash)
}

generate_design_X_full = function(data, uids, timepoints){
  n = length(timepoints)
  q = length(uids)
  design_X_full = data.frame(
    ones     = rep(1, n*q),
    uid      = sort(rep(uids, n)),
    week     = rep(timepoints, q)
  )

  arm_lookup = data %>% dplyr::select(uid, arm) %>% distinct(uid, arm)
  design_X_full = left_join(design_X_full, arm_lookup, by = "uid") %>%
    mutate(arm_i = ifelse(arm == "full", 1, 0)) %>%
    mutate(arm_i_x_week = arm_i*week) %>%
    dplyr::select(ones, arm_i, week, arm_i_x_week) %>% data.matrix()

  return(design_X_full)
}
