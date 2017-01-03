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
    if(is.numeric(timestamp)){
        cadval = as.vector(dataset[,timestamp])
    }else {
        cadval = as.vector(dataset[,c(names(dataset)== timestamp)])
    }
    size = length(cadval)

    daystart = paste(substring(as.POSIXlt(cadval[1], tz = "GMT"),1,10), startTime)
    dayend = paste(substring(as.POSIXlt(cadval[1], tz = "GMT"),1,10), endTime)

    days = 1;
    dayMarking = rep(NA, size)
    while(as.POSIXlt(dayend, tz = "GMT")  < 60*60*24 + as.POSIXlt(cadval[size], tz = "GMT"))
    {
        dayMarking[as.POSIXlt(cadval, tz = "GMT")>= as.POSIXlt(daystart, tz = "GMT") & 
                   as.POSIXlt(cadval, tz = "GMT")<= as.POSIXlt(dayend, tz = "GMT")] = days
        
        days = days+1
        daystart = as.POSIXlt(dayend, tz = "GMT")+ 1 
        dayend = as.POSIXlt(dayend, tz = "GMT")+ 60*60*24
    }
    temp = cbind(dataset, days = dayMarking) 
    return(temp)
}
