#' Wrapper Function for Accelerometry data Preprocessing, Feature Extraction, and Delivery Prediction
#'
#' The function is a wrapper function that performs preprocessing, feature extraction, and delivery day prediction 
#' of an accelerometry dataset. The prediction model can be selected from one of three models, 
#' a Random Forest, a logistic regression, and a convolutional neural network (default: Random Forest).
#'
#' Function works for data consisting of one or multiple unique trials.
#'
#' @param df A dataframe. The source accelerometry dataset, in dataframe format.
#' @param model A character. Indicates which prediction model to use.
#' \sQuote{RF} is a Random Forest. \sQuote{GLM} is a logistic regression, and
#' \sQuote{NN} is a convolutional neural network.
#'
#' @return A dataframe is returned with a predicted probability of each day being a delivery activity day.
#'
#' @note The input dataframe should have the following columns: 
#' \sQuote{TimeStamp}, \sQuote{axis1}, \sQuote{axis2}, \sQuote{axis3}, \sQuote{vm},
#' where \sQuote{vm} is the vector magnitude of axes 1, 2, and 3. 
#' Dataframe should also be formatted to 60 second epoch. 
#' 
#' The function uses the default preprocessing criteria used in the development of the predictive models. 
#'
#' @templateVar author ryancolechoi
#' @template auth
#'
#' @seealso \code{\link{deliveryPreprocess}}, \code{\link{deliveryFeatures}}, \code{\link{deliveryPrediction}}
#'
#' @examples
#' data(deliveryData)
#' 
#' predictions <- deliveryPred(df = deliveryData, model = "GLM")
#' 
#' @export

deliveryPred <- function(df, model=c("RF", "NN", "GLM")) {
  model <- match.arg(model)
  pdat <- deliveryPreprocess(df=df)
  feats <- deliveryFeatures(df=pdat)
  predRes <- deliveryPrediction(df=pdat, feats=feats, model=model)
  return(predRes)
}
