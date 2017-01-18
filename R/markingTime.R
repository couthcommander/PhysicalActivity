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
#' @param tz Local time zone, defaults to UTC.
#'
#' @return A dataframe with an extra day marking column.
#'
#' @template ref2010
#'
#' @templateVar author liu
#' @template auth
#' @export

markingTime <- function(dataset, timestamp, startTime="00:00:00", tz="UTC") {
    cadval <- dataset[,timestamp]
    if(!inherits(cadval, "POSIXt")) {
        cadval <- as.POSIXct(cadval, format = "%Y-%m-%d %H:%M:%S", tz = tz)
    }
    day1 <- format(min(cadval), "%Y-%m-%d")
    daystart <- as.POSIXct(paste(day1, startTime), tz = tz)
    sameday <- as.POSIXct(paste(day1, format(cadval, "%H:%M:%S")), tz = tz)
    doffset <- ifelse(sameday - daystart < 0, 1, 0)
    cadate <- as.Date(format(cadval, "%Y-%m-%d"))
    cadiff <- as.numeric(difftime(cadate, as.Date(day1), units = 'days'))
    dayMarking <- cadiff + 1 - doffset
#     ts <- difftime(cadval, daystart, tz = tz, units = "days")
#     dayMarking <- floor(as.numeric(ts)) + 1
    dataset[,'days'] <- dayMarking
    dataset
}
