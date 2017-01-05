#' Mark Days
#'
#' This function adds a "day" variable to the source dataset. The day is marked
#' in numeric order, according to the timestamp variable.
#'
#' @param dataset The source dataset, in dataframe format, that needs to be
#' marked.
#' @param timestamp The column name in the dataset that will be used as
#' timestamp.
#' @param startTime Define the starting time of a day.  It must be in the format
#' of "hh:mm:ss".
#' @param endTime Define the ending time of a day.  It must be in the format of
#' "hh:mm:ss".
#'
#' @return A dataframe with an extra day marking column.
#'
#' @template ref2010
#'
#' @templateVar author liu
#' @template auth
#' @export

markingTime <- function(dataset, timestamp, startTime = "00:00:00",
                        endTime = "23:59:59") {
    cadval <- dataset[,timestamp]
    alltime <- as.POSIXct(cadval, tz = "GMT")
    size <- length(cadval)
    day1 <- format(min(alltime), "%Y-%m-%d")
    daystart <- as.POSIXct(paste(day1, startTime), tz = "GMT")
    ts <- difftime(alltime, daystart, tz = "GMT", units = "days")
    dayMarking <- floor(as.numeric(ts)) + 1
    dataset[,'days'] <- dayMarking
    dataset
}
