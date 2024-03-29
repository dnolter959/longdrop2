#' Extract point estimates and standard errors from model
#'
#' @param mod An instance of class lmerMod from the package lme4 (fit using `lmer`)
#' @param idx_of_coef_of_interest Index of the fixed effect coefficient of interest for analysis (corresponds to the variable order provided in the lmer call, or the `summary` of the model fixed effects)
#'
#' @return A `hash` object with inferential statistics on the coefficient of interest
#' @export
#'
#' @examples
extract_inferential_statistics_from_lmm = function(mod, idx_of_coef_of_interest){
  results_hash = hash::hash()
  results_hash[["point-estimate"]] = round(coef(summary(mod))[idx_of_coef_of_interest, 1], 3)
  results_hash[["standard-error"]] = round(coef(summary(mod))[idx_of_coef_of_interest, 2], 3)

  return(results_hash)
}

#' Generate full design matrix from model
#'
#' @param MAR_model An instance of class lmerMod from the package lme4 (fit using `lmer`)
#' @param uids A vector of uids in the provided dataset
#' @param timepoints A vector of timepoints in the provided dataset
#' @param time_var The time-related variable in the longitudinal dataset
#' @param outcome_var The outcome variable in the provided dataset
#' @param type Specifies whether to return a data frame ("dataframe") or matrix
#'
#' @return Full design matrix (in a world of no missingness)
#' @export
#'
#' @examples
generate_design_X_full = function(MAR_model, uids, timepoints, time_var, outcome_var, type="matrix"){
  # generate fully expanded id-time
  uid_var = names(MAR_model@flist[1])

  id_time = expand.grid(uids, timepoints)
  names(id_time) = c(uid_var, time_var)
  id_time = id_time[order(id_time[,uid_var], id_time[,time_var]), ]

  full_data = dplyr::left_join(id_time, MAR_model@frame, by=c(uid_var, time_var))
  full_data$missing = ifelse(is.na(full_data[,outcome_var]), 1, 0)
  full_data = full_data %>% dplyr::group_by_at(uid_var) %>%
    tidyr::fill(everything(), .direction = "updown")

  if (type == "dataframe"){return(full_data)}

  # extract full design matrix
  tmp = model.matrix(as.formula(MAR_model@call), data = full_data)
  full_mod = lme4::lmer(as.formula(MAR_model@call), data = full_data)
  design_X_full_comp = model.matrix(full_mod)

  return(design_X_full_comp)
}

