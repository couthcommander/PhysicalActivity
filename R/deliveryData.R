#' Delivery Classification Data Example
#'
#' Approximately 15 days of 3-axis accelerometer data collected with 1-minute epoch.
#'
#' @format A data frame with 20987 observations on the following variables.
#' \describe{
#'   \item{TimeStamp}{a character vector, timestamp of accelerometer measurements}
#'   \item{axis1}{a numeric vector, counts from axis1 of 3-axis accelerometer}
#'   \item{axis2}{a numeric vector, counts from axis2 of 3-axis accelerometer}
#'   \item{axis3}{a numeric vector, counts from axis3 of 3-axis accelerometer}
#'   \item{steps}{a numeric vector, the number of steps}
#'   \item{vm}{a numeric vector, the vector magnitude calculated from counts of axis1, axis2 and axis3}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' data(deliveryData)
"deliveryData"
