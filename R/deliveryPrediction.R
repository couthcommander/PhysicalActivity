#' deliveryPrediction
#'
#' description
#'
#' details
#'
#' @param df details
#' @param feats details
#' @param model details
#' @param \dots possibly used to create datetime variable from TimeStamp
#'
#' @return details
#'
#' @examples
#' @export

deliveryPrediction <- function(df, feats, model = c('RF','GLM','NN'), ...) {
  model <- match.arg(model)
  xnames <- names(df)
  misscol <- setdiff(c('TimeStamp','axis1','axis2','axis3','vm'), xnames)
  if(length(misscol) > 0) {
    stop(sprintf('columns missing from data set: %s', paste(misscol, collapse = ', ')))
  }

  if(model == "RF") {
    model_RF <- NULL
    load(system.file("delivery_models", "model_RF.rda"))
    preds <- as.vector(stats::predict(model_RF, newdata=feats, type="prob")[,1])
  } else if(model == "GLM") {
    model_GLM <- NULL
    load(system.file("delivery_models", "model_GLM.rda"))
    preds <- as.vector(stats::predict(model_GLM, feats, type="fitted"))
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
    model_CRNN <- keras::load_model_tf(system.file("delivery_models", "model_CRNN"))
    dtAvailable <- requireNamespace("data.table", quietly = TRUE)
    orthdf <- addDayIndex(df, ...)

    axesCols <- c('axis1','axis2','axis3')
    if(dtAvailable) {
      ..axesCols <- NULL
      axes_scale <- vapply(orthdf[,..axesCols], base::scale, numeric(nrow(orthdf)))
    } else {
      axes_scale <- vapply(orthdf[,axesCols], base::scale, numeric(nrow(orthdf)))
    }
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
