#' Plot Accelerometer Data over Time
#'
#' This function makes plot for accelerometer collected data (counts) over time
#' for the whole monitor period, or a user specified time period or day with a
#' midnight marking to separate monitored days.
#'
#' @param data Data with classified wear and nonwear status from
#' \code{\link{wearingMarking}}.
#' @param day A part of data during a user specified day for plot.
#' @param start Define a starting time for plot.
#' @param end Define a ending time for plot.
#' @param cts The name of the counts column. The default is "axis1".
#' @param TS The column name for timestamp. The default is "TimeStamp".
#'
#' @return Plot with midnight marking.
#'
#' @templateVar author choi
#' @template auth
#'
#' @seealso \code{\link{sumVct}}, \code{\link{wearingMarking}}
#'
#' @examples
#' data(dataSec)
#'
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
#' ## plot the whole data
#' plotData(data=data1m, cts = "counts")
#'
#' ## plot the data from 60 min to 900 min
#' plotData(data=data1m, start=60, end=900, cts = "counts")
#'
#' ## plot the data for day 2
#' plotData(data=data1m, day=2, cts = "counts")
#' @export

plotData <- function(data, day=NULL, start=NULL, end=NULL, cts='axis1',
                     TS = "TimeStamp") {
    stopifnot('days' %in% names(data))
    findMidnight <- function(data) {
        mm <- c(0, diff(data[,'days']))
        which(mm == 1)
    }
    if(is.null(start)) {
        start <- 1
    }
    if(is.null(end)) {
        end <- nrow(data)
    }
    if(!is.null(day)) {
        dd <- data[data[,'days']==day,]
    } else {
        dd <- data[seq(start,end),]
    }
    midnightMark <- findMidnight(dd)
    plot(dd[,cts], type="l", xlab="Time", ylab="Counts")
    abline(v=midnightMark, lty=2, lwd=1.5, col=4)
    text(midnightMark, 0, pos=1, "0 AM", cex=0.8, col=4)
    tzs <- as.POSIXlt(dd[,TS])$zone
    if(length(table(tzs)) > 1) {
        yval <- max(dd[,cts])
        dst <- which(tzs != c(tzs[-1], tzs[length(tzs)]))
        abline(v=dst+0.5, lty=3, lwd=2, col='red')
        text(dst+0.5, yval, pos=2, tzs[dst], cex=0.8, col='red')
        text(dst+0.5, yval, pos=4, tzs[dst+1], cex=0.8, col='red')
    }
}
