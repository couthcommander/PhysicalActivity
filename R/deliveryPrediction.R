#' Predict Delivery Days in Accelerometry Data
#'
#' The function predicts the probability of each day in an accelerometry dataset being 
#' caused from delivery activity instead of human activity. 
#' The prediction model can be selected from one of three models, 
#' a Random Forest, a logistic regression, and a convolutional neural network (default: Random Forest).
#'
#' Function works for data consisting of one or multiple unique trials.
#'
#' @param df A dataframe. The source accelerometry dataset, in dataframe format.
#' @param feats A dataframe. Features output from the \code{\link{deliveryFeatures}} function.
#' @param model A character. Indicates which prediction model to use.
#' \sQuote{RF} is a Random Forest. \sQuote{GLM} is a logistic regression, and
#' \sQuote{NN} is a convolutional neural network.
#' @param \dots not used at this time
#'
#' @return A dataframe is returned with a predicted probability of each day being a delivery activity day.
#'
#' @note The input dataframe should have the following columns: 
#' \sQuote{TimeStamp}, \sQuote{axis1}, \sQuote{axis2}, \sQuote{axis3}, \sQuote{vm},
#' where \sQuote{vm} is the vector magnitude of axes 1, 2, and 3. 
#' Dataframe should also be formatted to 60 second epoch. 
#' 
#' @templateVar author ryancolechoi
#' @template auth
#' 
#' @seealso \code{\link{deliveryFeatures}}, \code{\link{deliveryPred}}
#' 
#' @examples
#' data(deliveryData)
#'
#' deliveryDataProcessed <- deliveryPreprocess(df = deliveryData)
#' deliveryDataFeats <- deliveryFeatures(df = deliveryDataProcessed)
#' deliveryPrediction(deliveryDataProcessed, deliveryDataFeats)
#'
#' @export

deliveryPrediction <- function(df, feats, model = c('RF','GLM','NN'), ...) {
  model <- match.arg(model)
  xnames <- names(df)
  misscol <- setdiff(c('TimeStamp','axis1','axis2','axis3','vm'), xnames)
  if(length(misscol) > 0) {
    stop(sprintf('columns missing from data set: %s', paste(misscol, collapse = ', ')))
  }

  if(model == "RF") {
    if(!requireNamespace("randomForest", quietly = TRUE)) {
      stop("deliveryPrediction requires the randomForest package, please install it.",
        call. = FALSE)
    }
    model_RF <- NULL
    load(system.file("delivery_models", "model_RF.rda", package = "PhysicalActivity"))
    preds <- as.vector(stats::predict(model_RF, newdata=feats, type="prob")[,1])
  } else if(model == "GLM") {
    if(!requireNamespace("rms", quietly = TRUE)) {
      stop("deliveryPrediction requires the rms package, please install it.",
        call. = FALSE)
    }
    model_GLM <- NULL
    load(system.file("delivery_models", "model_GLM.rda", package = "PhysicalActivity"))
    ns <- loadNamespace('rms')
    predict.lrm <- getFromNamespace('predict.lrm', ns)
    preds <- as.vector(predict.lrm(model_GLM, feats, type="fitted"))
    preds <- 1 - preds #Using predict function, 1 label for human wear. Reverse here
  } else if(model == "NN") {
    if(!requireNamespace("reticulate", quietly = TRUE)) {
      stop("deliveryPrediction requires the reticulate package, please install it.",
        call. = FALSE)
    }
    if(!reticulate::py_module_available('tensorflow')) {
      stop("deliveryPrediction requires the tensorflow Python module, please install it.",
        call. = FALSE)
    }
    if(!requireNamespace("keras", quietly = TRUE)) {
      stop("deliveryPrediction requires the keras package, please install it.",
        call. = FALSE)
    }
    reticulate::import('tensorflow')
    weights <- readRDS(system.file("delivery_models", "model_CRNN_W", package = "PhysicalActivity"))
    arch <- readRDS(system.file("delivery_models", "model_CRNN_A", package = "PhysicalActivity"))
    model_CRNN <- keras::model_from_json(arch)
    model_CRNN <- keras::set_weights(model_CRNN, weights) 
    dtAvailable <- requireNamespace("data.table", quietly = TRUE)
    orthdf <- addDayIndex(df, ...)

    if(dtAvailable) {
      # ensure expected behaviour
      class(orthdf) <- 'data.frame'
    }
    axesCols <- c('axis1','axis2','axis3')
    axes_scale <- vapply(orthdf[,axesCols], base::scale, numeric(nrow(orthdf)))
    #Format data as list of dt's by ID_day
    dt <- split.data.frame(axes_scale, orthdf[['ID_day']])
    #Reshape list into 3-D array for input into neural net
    dim1 <- length(dt)
    dim2 <- nrow(axes_scale) / dim1 # this should be 1440
    arr <- array(NA, c(dim1, dim2, 3))
    dimnames(arr) <- list(NULL,NULL,axesCols)
    #rearrange array to be: (depth, rows, cols)
    for(i in seq(dim1)) {
      arr[i,,] <- dt[[i]]
    }
    #Model 
    preds <- stats::predict(model_CRNN, arr)
  }
  results <- cbind(feats[,c('.id','TimeStamp','day')], preds)
  return(results)
}
