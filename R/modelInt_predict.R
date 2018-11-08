#' @title Prediction interval for a fitted \code{merMod} object. See \code{\link{lmer}}, See \code{\link{glmer}}.
#' @description This function uses \code{\link{bootMer}} to bootstrap a mixed model and calculate
#' prediction intervals.
#' @param model fitted \code{merMod} object
#' @param df data frame with new combination of fixed effect values to generate prediction
#' @param n_sim number of bootstrap simulation
#' @param prob probability for the predicted interval
#' @param cluster logical. TRUE / FALSE for whether to use parallele computing. See \code{\link{bootMer}}
#' @param cluster_name if cluster = TRUE, a user-defined cluster name
#' @param ... see \code{\link{bootMer}} for more argument options
#' @details See \code{\link{bootMer}} and \code{\link{boot}} for further details.
#' information about the presence/absence of the incubating individual in the nest.
#' @return Data frame with prediction interval
#' @export
modelInt_predict <- function(model, df, n_sim = 100, prob = 0.95, cluster = NULL, cluster_name = NULL, ...){

  if(base::is.null(df)) {
    stop("Please, provide a new data frame for predictions, 'df'.")
  }

  if(base::is.null(model)) {
    stop("No 'model' provided. Please, specify one")
  }

  predFun <- function(model = model, dataPredict  = df) {
    stats::predict(object = model, newdata = dataPredict, re.form = NA, type = "response")
  }

  if(is.null(cluster)){
  bb <- lme4::bootMer(x = model,
                      nsim=n_sim,
                      FUN=predFun,
                      seed=101,
                      PBargs = list(style=3), ...) # bootMer
  } else {
    bb <- lme4::bootMer(x = model,
                        nsim=n_sim,
                        FUN=predFun,
                        seed=101,
                        PBargs = list(style=3),
                        cl = cluster_name, ...) # bootMer
  }
  # store and manipulate results
  boot_rep <- dplyr::tbl_df(data.frame(bb$t))

  # calculating intervals
  one <- 1 - ((1 - prob)/2)
  two <- 0 + ((1 - prob)/2)
  PI.arm <- base::data.frame(
    fit=base::apply(boot_rep, 2, function(x) stats::quantile(x, 0.500)),
    upr=base::apply(boot_rep, 2, function(x) stats::quantile(x, one)),
    lwr=base::apply(boot_rep, 2, function(x) stats::quantile(x, two))
  )
  return(base::cbind(df, PI.arm))
}

