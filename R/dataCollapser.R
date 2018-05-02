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
#' @param col The column name(s) to collapse.  If not provided, will default to
#' all numeric columns.
#' @param func A method for collapsing counts. The default is the summation of
#' counts.
#' @param \dots Argument settings that to be used by user-defined "func"
#' setting.
#'
#' @return A collapsed data with user specified epoch.
#'
#' @template ref2011
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
    nr <- nrow(dataset)
    ts <- dataset[,TS]
    if(!inherits(ts, "POSIXt")) {
        ts <- as.POSIXct(ts, tz = 'UTC')
    }
    if(missing(col)) {
        col <- which(vapply(dataset, is.numeric, logical(1)))
    }
    ct <- dataset[,col, drop=FALSE]
    attrMeta <- attr(dataset, 'metadata')

    minTime <- min(ts)
    epoch <- as.numeric(difftime(ts[2], ts[1], units='secs'))
    ratio <- by / epoch
    if(ratio < 1) {
        stop("'by' set to value less than current epoch", call. = FALSE)
    }

    newrange <- seq(0, ceiling(nr / ratio) - 1) * by
    tf <- data.frame(ts = minTime + newrange)
    summ <- matrix(0, length(newrange), length(col))
    for(i in seq_along(newrange)) {
        start <- (i-1) * ratio + 1
        end <- min(i * ratio, nr)
        ix <- seq.int(start, end)
        for(j in seq_along(col)) {
            summ[i,j] <- func(ct[ix,j], ...)
        }
    }
    tf <- cbind(tf, summ)
    names(tf) <- c(TS, names(ct))
    if(!is.null(attrMeta)) {
        attr(tf, 'metadat') <- attrMeta
    }
    tf
}
