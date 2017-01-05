#' Classify Wear and Nonwear Time for Accelerometer Data
#'
#' This function classifies wear and nonwear time status for accelerometer
#' data by epoch-by-epoch basis.
#'
#' A detailed description of the algorithm implemented in this function is
#' described in Choi, L., Liu, Z., Matthews, C. E. and Buchowski, M. S. (2010).
#'
#' @param dataset The source dataset, in dataframe format, which needs to be
#' marked.
#' @param frame The size of time interval to be considered; Window 1 described
#' in Choi, L., Liu, Z., Matthews, C. E. and Buchowski, M. S. (2010). The
#' default is 90.
#' @param perMinuteCts The number of data rows per minute. The default is 1-sec
#' epoch (perMinuteCts = 60). For examples: for data with 10-sec epoch, set
#' perMinuteCts = 6; for data with 1-min epoch, set perMinuteCts = 1.
#' @param TS The column name for timestamp. The default is "TimeStamp".
#' @param cts The column name for counts. The default is "counts".
#' @param streamFrame The size of time interval that the program will look back
#' or forward if activity is detected; Window 2 described in Choi, L., Liu, Z.,
#' Matthews, C. E. and Buchowski, M. S. (2010). The default is the half of the
#' frame.
#' @param allowanceFrame The size of time interval that zero counts are allowed;
#' the artifactual movement interval described in Choi, L., Liu, Z., Matthews,
#' C. E. and Buchowski, M. S. (2010). The default is 2.
#' @param newcolname The column name for classified wear and nonwear status. The
#' default is "wearing".  After the data is processed, a new field will be added
#' to the original dataframe.  This new field is the wearing /nowwearing
#' indicator.
#' @param getMinuteMarking Return minute data with wear and nonwear
#' classification. If the source is not a minute dataset, the function will
#' collapse it into minute data. The default is FALSE.
#' @param dayStart Define the starting time of day. The default is the midnight,
#' "00:00:00". It must be in the format of "hh:mm:ss".
#' @param dayEnd Define the ending time of day. The default is "23:59:59". It
#' must be in the format of "hh:mm:ss".
#' @param \dots Parameter settings that will be used in
#' \code{\link{dataCollapser}} function.
#'
#' @return A dataframe with the column for wear and nonwear classification
#' indicator by epoch-by-epoch basis.
#'
#' @template ref2010
#'
#' @templateVar author all
#' @template auth
#'
#' @note Warning: It will be very slow if accelerometer data with 1-sec epoch
#' for many days are directly classified. We recommend to collapse a dataset
#' with 1-sec epoch to 1-min epoch data using \code{\link{dataCollapser}} and
#' then classify wear and nonwear status using a dataset with a larger epoch.
#'
#' @seealso \code{\link{readCountsData}}, \code{\link{sumVct}}
#'
#' @examples
#' data(dataSec)
#'
#' ## mark data with 1-min epoch
#' mydata1m = dataCollapser(dataSec, TS = "TimeStamp", col = "counts", by = 60)
#' 
#' data1m = wearingMarking(dataset = mydata1m,
#'                        frame = 90, 
#'                        perMinuteCts = 1,
#'                        TS = "TimeStamp",
#'                        cts = "counts", 
#'                        streamFrame = NULL, 
#'                        allowanceFrame= 2, 
#'                        newcolname = "wearing")
#' 
#' sumVct(data1m, id="dataid")
#' 
#' ## mark data with 1-sec epoch
#' \dontrun{
#' data1s = wearingMarking(dataset = dataSec,
#'                        frame = 90, 
#'                        perMinuteCts = 60,
#'                        TS = "TimeStamp",
#'                        cts = "counts", 
#'                        streamFrame = NULL, 
#'                        allowanceFrame= 2, 
#'                        newcolname = "wearing",
#'                        getMinuteMarking = FALSE)
#' 
#' sumVct(data1s, id="dataid")
#' sumVct(data1s, id="dataid", markingString = "nw")
#' }
#' @export

wearingMarking <- function(dataset, 
                          frame = 90, 
                          perMinuteCts = 60,
                          TS = "TimeStamp",
                          cts = "counts", 
                          streamFrame = NULL, 
                          allowanceFrame= 2, 
                          newcolname = "wearing",
                          getMinuteMarking = FALSE,
                          dayStart = "00:00:00",
                          dayEnd = "23:59:59",
                          ...) {
    if(perMinuteCts != 1) {
        #not a minute data run collapse
        data2 <- dataCollapser(dataset, TS=TS, by = 60, col = cts, ...)
    } else {
        data2 <- dataset
    }
    data3 <- marking(data2, frame = frame, cts = cts, streamFrame = streamFrame,
                     allowanceFrame = allowanceFrame, newcolname = newcolname)

    if(!getMinuteMarking) {
        data4 <- dataset
        key1 <- substring(data4[,TS], 1, 16)
        key2 <- substring(data3[,TS], 1, 16)
        data4[,newcolname] <- data3[match(key1, key2), newcolname]
    } else {
        data4 <- data3
    }

    ts <- as.POSIXct(data4[,TS], format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
    data4[,'weekday'] <- weekdays(ts)
    markingTime(data4, TS, dayStart, dayEnd)
}
