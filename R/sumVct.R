#' Summarize Wearing or Nonwearing Time Interval
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
#' @return The summary data for wearing or nonwearing time intervals.
#'
#' @template ref2010
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

sumVct <- function(datavct, wearing = "wearing", TS = "TimeStamp",
                        markingString = "w", by = "days", id = NULL) {
    len = as.numeric(table(datavct[, c(by)]))
    if(length(len) > 1){
        for( j in 2:length(len)){
            len[j] = len[j]+ len[j-1]
        }
    }
    len=c(0, len)

    d = unique(datavct[, c(by)])
    allrst = NULL
    for(i in 1:length(d)){
        smalldatavct = datavct[datavct[,c(by)] == d[i],]
        temp = as.vector(smalldatavct[,c(wearing)])
         
        loc = 1:length(temp)
        loc = loc[temp == markingString]
        if(length(loc)>0){
            loc1= loc[1:(length(loc)-1)]
            loc2= loc[2:length(loc)]
 
            tempdf = data.frame(diff = (loc1 == loc2-1), loc1, loc2)
            pos = sort(c(loc[1], tempdf[tempdf$diff == FALSE,]$loc1, tempdf[tempdf$diff == FALSE,]$loc2, loc[length(loc)]))
            start = pos[1:(length(pos)/2)*2-1] 
            end = pos[1:(length(pos)/2)*2]
            size = end-start+1
            rst = data.frame(start, end, duration = size)
            rst$startTimeStamp = smalldatavct[rst$start, TS]
            rst$endTimeStamp = smalldatavct[rst$end, TS]
            rst$days = d[i]
            if(!is.null(id)){
                rst$id = id
            }
            rst$start = rst$start +len[i]
            rst$end = rst$end +len[i]
            rst$weekday = weekdays(as.Date(rst$startTimeStamp))
            allrst = rbind(allrst,rst)
        }
    }
    collist = c("startTimeStamp", "endTimeStamp", "days", "weekday", "start", "end", "duration")
    if(!is.null(id)){
        collist = c("id", collist)
    }

    allrst = allrst[collist]
    return(allrst)
}
