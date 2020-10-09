#' Accelerometer Data Example
#'
#' Approximately 3 days of accelerometer data collected with 1-sec epoch in the
#' correct data format that can be used by \code{wearingMarking} to classify
#' wear and nonwear time.
#'
#' @usage data(dataSec)
#'
#' @format A data frame with 238140 observations on the following 2 required
#' variables.
#' \describe{
#'   \item{TimeStamp}{A character vector, timestamp of accelerometer measurements}
#'   \item{counts}{A numeric vector, counts as accelerometer measurements}
#' }
#'
#' @template ref2011
#'
#' @keywords datasets
#'
#' @examples
#' data(dataSec)
"dataSec"
