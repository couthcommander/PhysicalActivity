#' Data Example for Mail Delivery Day Classification 
#'
#' Approximately 15 days of 3-axis accelerometer data collected with 1-minute epoch.
#'
#' @format A data frame with 20987 observations on the following variables.
#' \describe{
#'   \item{TimeStamp}{A character vector, timestamp of accelerometer measurements}
#'   \item{axis1}{A numeric vector, counts from axis1 of 3-axis accelerometer}
#'   \item{axis2}{A numeric vector, counts from axis2 of 3-axis accelerometer}
#'   \item{axis3}{A numeric vector, counts from axis3 of 3-axis accelerometer}
#'   \item{steps}{A numeric vector, the number of steps}
#'   \item{vm}{A numeric vector, the vector magnitude calculated from counts of axis1, axis2 and axis3}
#' }
#'
#' @keywords datasets
#'
#' @templateVar author colechoi
#' @template auth
#'
#' @examples
#' data(deliveryData)
"deliveryData"
