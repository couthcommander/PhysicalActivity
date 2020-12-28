#' Delivery Features
#'
#' The function extracts multiple statistical features relevant for classification of days as delivery or human wear. 
#' The extracted features are: mean, variance, maximum, absolute change, absolute energy, 
#' proportion of trial completed, 95th quantile, skewness, and kurtosis.
#'
#' Function works for data consisting of one or multiple unique trials.
#'
#' @param df A dataframe. The source accelerometry dataset, in dataframe format.
#' @param \dots not used at this time
#'
#' @return A dataframe is returned with a row for each unique day and a column for each feature.
#' 
#' @note The input dataframe should have the following columns: 
#' \sQuote{TimeStamp}, \sQuote{axis1}, \sQuote{axis2}, \sQuote{axis3}, \sQuote{vm},
#' where \sQuote{vm} is the vector magnitude of axes 1, 2, and 3. 
#' Dataframe should also be formatted to 60 second epoch. 
#' 
#' @templateVar author ryancolechoi
#' @template auth
#' 
#' @seealso \code{\link{deliveryPred}}
#' 
#' @examples
#' data(deliveryData)
#' 
#' deliveryDataProcessed <- deliveryPreprocess(df = deliveryData)
#' deliveryDataFeats <- deliveryFeatures(df = deliveryDataProcessed)
#' 
#' @export

deliveryFeatures <- function(df, ...) {
  xnames <- names(df)
  misscol <- setdiff(c('TimeStamp','vm'), xnames)
  if(length(misscol) > 0) {
    stop(sprintf('columns missing from data set: %s', paste(misscol, collapse = ', ')))
  }
  dtAvailable <- requireNamespace("data.table", quietly = TRUE)
  orthdf <- addDayIndex(df, ...)
  if(!requireNamespace("e1071", quietly = TRUE)) {
    stop("deliveryFeatures requires the e1071 package, please install it.",
      call. = FALSE)
  }

  # Calculate and scale other features
  cols1 <- c('.id','TimeStamp','ID','day','ID_day','propDay')
# data.table isn't working for some reason
#   if(dtAvailable) {
#     ### R check fix
#     .id <- NULL
#     TimeStamp <- NULL
#     ID <- NULL
#     day <- NULL
#     ID_day <- NULL
#     vm <- NULL
#     propDay <- NULL
#     `:=` <- NULL
#     ###
#     feats <- orthdf[, list(
#       .id = .id[1],
#       TimeStamp = TimeStamp[1],
#       ID = ID[1],
#       day = day[1],
#       ID_day = ID_day[1],
#       mean = mean(vm),
#       variance = stats::var(vm),
#       max = max(vm),
#       absChange = sum(abs(diff(vm))),
#       absEnergy = c(vm %*% vm),
#       propDay = propDay[1],
#       q95 = quantile(vm, 0.95),
#       skewness = e1071::skewness(vm),
#       kurtosis = e1071::kurtosis(vm)
#     ), by = ID_day]
#     feats[, TimeStamp := format(TimeStamp, '%Y-%m-%d')]
#     feats <- as.data.frame(feats)[,-1]
#   } else {
  orthdf$ID_day <- factor(orthdf$ID_day, levels=unique(orthdf$ID_day))
  dt <- split(orthdf, orthdf[['ID_day']])
  first <- function(df, j) df[[j]][1]
  feats <- data.frame(
    .id = sapply(dt, first, '.id'),
    TimeStamp = sapply(dt, function(df){ format(df$TimeStamp[1], '%Y-%m-%d') }),
    ID = sapply(dt, first, 'ID'),
    day = sapply(dt, first, 'day'),
    ID_day = sapply(dt, first, 'ID_day'),
    mean = sapply(dt, function(df){mean(df$vm)}),
    variance = sapply(dt, function(df){stats::var(df$vm)}),
    max = sapply(dt, function(df){max(df$vm)}),
    absChange = sapply(dt, function(df){sum(abs(diff(df$vm)))}),
    absEnergy = sapply(dt, function(df){df$vm %*% df$vm}),
    propDay = sapply(dt, first, 'propDay'),
    q95 = sapply(dt, function(df){unname(quantile(df$vm, 0.95))}),
    skewness = sapply(dt, function(df){e1071::skewness(df$vm)}), 
    kurtosis = sapply(dt, function(df){e1071::kurtosis(df$vm)})
  )
#   }
  feats[is.nan(feats[['skewness']]), 'skewness'] <- 0 #If day is all 0, nan occurs. Make nan 0
  feats[is.nan(feats[['kurtosis']]), 'kurtosis'] <- 0
  
  cols2 <- setdiff(names(feats), cols1)
  feats_scale <- vapply(feats[,cols2], base::scale, numeric(nrow(feats)))
  feats <- cbind(feats[,cols1], feats_scale)
}
