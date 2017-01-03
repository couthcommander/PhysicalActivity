#' Collapse Accelerometer Data to a Dataset with a Longer Epoch
#'
#' The function collapses counts in data collected with a short epoch to make a 
#' data set with a longer epoch. For example, this function collapses data with
#' 1-sec epoch to 10-sec epoch or 1-min epoch data.
#'
#' @param dataset The source dataset, in dataframe format, that needs to be
#' collapsed.
#' @param TS The column name for timestamp.
#' @param by Epoch in seconds for a collapsed dataset. For example, to collapse
#' second data to minute data, set by = 60; to collapse 10-second data to minute
#' data, set by = 60.
#' @param col The column name for counts.
#' @param func A method for collapsing counts. The default is the summation of
#' counts.
#' @param \dots Argument settings that to be used by user-defined "func"
#' setting.
#'
#' @return A collapsed data with user specified epoch.
#'
#' @template ref2010
#'
#' @templateVar author liu
#' @template auth
#'
#' @examples
#' data(dataSec)
#'
#' ## collapse 1-sec epoch data to 10-sec epoch data
#' mydata10s = dataCollapser(dataSec, TS = "TimeStamp", col = "counts", by = 10)
#'
#' ## collapse 1-sec epoch data to 1-min epoch data
#' mydata1m = dataCollapser(dataSec, TS = "TimeStamp", col = "counts", by = 60)
#' @export

dataCollapser <- function(dataset, TS, by, col, func = sum, ...) {
    ts = as.vector(dataset[,TS])
    ct = as.numeric(dataset[,col])

    timeRange = range(as.vector(ts))
    epoch = as.numeric(as.POSIXlt(ts[2]) - as.POSIXlt(ts[1]))
    ratio = by/epoch

    newrange = c(0: (ceiling(length(ts)/ratio)-1))*by
    step1 = rep(as.POSIXlt(timeRange[1], tz = "GMT"), length(newrange))
    newts = gsub(" GMT", "", step1+ newrange)
    newct = rep(NA, length(newrange))

    i = 1
    prevts = 
    while(i <= length(newts))
    {
        start = (i-1)*ratio +1
        end = i*ratio
        if(end > length(ct))
        {    end = length(ct)} 

        newct[i] = func(ct[start:end], ...)
        
        i = i+1
    }

    tf = data.frame(timestamp = newts, counts = newct)
    names(tf) = c(TS, col)
    return(tf)
}
