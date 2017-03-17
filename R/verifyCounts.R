#' Verify Daily Counts
#'
#' Some days may be incorrectly classified as valid.  This function attempts
#' to verify those dates.
#'
#' @param data Data with classified wear (nonwear) status by
#' \code{\link{wearingMarking}}.
#' @param summary List output of \code{\link{summaryData}} function.
#' @param verifyFUN Function to describe daily counts.  The default is
#' \code{mean}.
#' @param cts The name of the counts column. The default is "axis1".
#' @param markingString Option for summarizing wear (markingString = "w") or
#' nonwear time (markingString = "nw").
#' @param nonzero Option to exclude zero value counts from the summary.
#' @param conf Confidence interval, defaulting to 95%.  Use NA to return
#' probabilites.
#'
#' @return Logical vector or vector of probabilites if conf is NA.
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
#' sumdat <- summaryData(data=data1m)
#' verifyCounts(data1m, sumdat, verifyFUN = mean, cts = "counts")
#' verifyCounts(data1m, sumdat, verifyFUN = median, cts = "counts",
#'              nonzero = TRUE, conf = NA)
#' @export

verifyCounts <- function(data, summary, verifyFUN=mean, cts='axis1',
                         markingString = "w", nonzero = FALSE, conf = 0.95) {
    stopifnot('days' %in% names(data), is.function(verifyFUN))
    daylist <- names(summary$validWearTimeByDay)
    dd <- data[data[,'wearing'] == markingString & data[,'days'] %in% daylist,]
    if(nonzero) {
        dd <- dd[dd[,cts] > 0,]
    }
    x <- tapply(dd[,cts], dd[,'days'], verifyFUN)
    n <- length(x)
    ans <- pt((x - mean(x)) / (sd(x) / sqrt(n)), df=n-1)
    if(!is.na(conf)) {
        prob <- (1 - conf) / 2
        ans <- ans >= prob
    }
    ans
}
