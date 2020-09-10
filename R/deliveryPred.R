#' deliveryPred
#'
#' description
#'
#' details
#'
#' @param df details
#' @param model details
#'
#' @return details
#'
#' @examples
#' @export

deliveryPred <- function(df, model=c("RF", "NN", "GLM")) {
  model <- match.arg(model)
  pdat <- deliveryPreprocess(df=df)
  feats <- deliveryFeatures(df=pdat)
  predRes <- deliveryPrediction(df=pdat, feats=feats, model=model)
  return(predRes)
}
