% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{generate_design_X_full}
\alias{generate_design_X_full}
\title{Generate full design matrix from model}
\usage{
generate_design_X_full(
  MAR_model,
  uids,
  timepoints,
  time_var,
  outcome_var,
  type = "matrix"
)
}
\arguments{
\item{MAR_model}{An instance of class lmerMod from the package lme4 (fit using \code{lmer})}

\item{uids}{A vector of uids in the provided dataset}

\item{timepoints}{A vector of timepoints in the provided dataset}

\item{time_var}{The time-related variable in the longitudinal dataset}

\item{outcome_var}{The outcome variable in the provided dataset}

\item{type}{Specifies whether to return a data frame ("dataframe") or matrix}
}
\value{
Full design matrix (in a world of no missingness)
}
\description{
Generate full design matrix from model
}
