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
#' plotData(data=data1m)
#'
#' ## plot the data from 60 min to 900 min
#' plotData(data=data1m, start=60, end=900)
#'
#' ## plot the data for day 2
#' plotData(data=data1m, day=2)
#' @export

plotData <- function(data, day=NULL, start=NULL, end=NULL) {
    findMidnight <- function(data){
    n <- length(data[,1])
            mm <- 0
            for(i in 1:(n-1)){
                    mm <- c(mm, ifelse(data$days[i]==data$days[i+1], 0, 1))
                    }
            data.midnight <- data[mm==1,]
            first.rowname <- as.numeric(row.names(data[1,]))
            midnightStart <- as.numeric(row.names(data.midnight) ) - first.rowname + 1
            return(midnightStart)
    }
    if(is.null(start)==1){
            start <- 1
            }
    if(is.null(end)==1){
            end <- length(data[,1])
            }
    if(is.null(day)!=1){
            dd <- data[data$days==day,]
            midnightMark <- findMidnight(dd)
            } else{
                    dd <- data[start:end,]
                    midnightMark <- findMidnight(dd)
                    }
    plot(dd$counts, type="l", xlab="Time", ylab="Counts")
    abline(v=midnightMark, lty=2, lwd=1.5, col=4)
    text(midnightMark, 0, pos=1,"0 AM", cex=0.8, col=4)
}
