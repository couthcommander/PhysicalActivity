#' Summarize Wear and Nonwear Time Interval
#'
#' This function summarizes the classified wear (nonwear) time by interval basis
#' from the epoch-by-epoch classified wear (nonwear) status classified by
#' \code{\link{wearingMarking}}.
#'
#' @param datavct Data with classified wear (nonwear) status classified by
#' \code{\link{wearingMarking}}.
#' @param wearing The column name for classified wear and nonwear status. The
#' default is "wearing".
#' @param TS The column name for timestamp. The default is "TimeStamp".
#' @param markingString Option for summarizing wear (markingString="w") or
#' nonwear time interval (markingString="nw").
#' @param by A sequence of days for classified wear (nonwear) time intervals.
#' @param id Optional output for subject identification or file name.
#'
#' @return The summary data for wear and nonwear time intervals.
#'
#' @template ref2011
#'
#' @templateVar author all
#' @template auth
#'
#' @seealso \code{\link{wearingMarking}}, \code{\link{summaryData}}
#'
#' @examples
#'
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
#' sumVct(data1m, id="sdata1m")
#' sumVct(data1m, id="sdata1m", markingString = "nw")
#' @export

sumVct <- function(datavct, wearing = "wearing", TS = getOption('pa.timeStamp'),
                        markingString = "w", by = "days", id = NULL) {
    len <- as.numeric(table(datavct[,by]))
    len <- c(0, cumsum(len))
    zz <- split(datavct, datavct[,by])
    allrst <- vector('list', length(zz))
    for(i in seq_along(zz)) {
        smalldatavct <- zz[[i]]
        loc <- which(smalldatavct[,wearing] == markingString)
        if(length(loc)) {
            ix <- which(diff(loc) != 1)
            pos <- sort(c(loc[1], loc[ix], loc[ix+1], loc[length(loc)]))
            rst <- data.frame(matrix(pos, ncol=2, byrow=TRUE))
            names(rst) <- c('start', 'end')
            rst[,'duration'] <- rst[,'end'] - rst[,'start'] + 1
            rst[,'startTimeStamp'] <- smalldatavct[rst[,'start'], TS]
            rst[,'endTimeStamp'] <- smalldatavct[rst[,'end'], TS]
            rst[,'days'] <- smalldatavct[1,by]
            rst[,'start'] <- rst[,'start'] + len[i]
            rst[,'end'] <- rst[,'end'] + len[i]
            rst[,'weekday'] <- weekdays(rst[,'startTimeStamp'])
            allrst[[i]] <- rst
        }
    }
    collist <- c("startTimeStamp", "endTimeStamp", "days", "weekday", "start",
                 "end", "duration")
    allrst <- do.call(rbind, allrst)[,collist]
    if(!is.null(id)) {
        allrst <- cbind(id = id, allrst)
    }
    allrst
}
