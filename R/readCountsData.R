#' Convert Accelerometer Output Data to a Correct Data Format
#'
#' This function converts accelerometer output data to a correct data format to
#' classify wear and nonwear time using \code{\link{wearingMarking}}. This
#' function can accept accelerometer output data with various epochs (for
#' example, 1-sec, 10-sec or 1-min).
#'
#' @param filename A filename of accelerometer output to be read.
#' @param ctPerSec Data collection epoch. This argument tells the program the
#' number of counting will be performed in every second. For examples: for 1-sec
#' epoch data, set ctPerSec = 1; for 10-sec epoch data, set ctPerSec = 1/10; for
#' 1-min epoch data, set ctPerSec = 1/60.
#' @param mode The mode of the ActiLife dat file.  Defaults to 0, and should be
#' listed in the file header.
#'
#' @return a data frame with the correct format (TimeStamp, counts) to be used for
#' \code{\link{wearingMarking}}.
#'
#' @template ref2011
#'
#' @templateVar author liu
#' @template auth
#'
#' @note Warning: It can be very slow if accelerometer data were collected with
#' 1-sec epoch for many days.
#'
#' @seealso \code{\link{wearingMarking}}
#'
#' @examples
#' ###############################################################################
#' ## Read accelerometer output and convert to a correct format (TimeStamp, counts)
#' ## Suppose "rawActigraphOutput.dat" is an Actigraph output with header as follows:
#' ###############################################################################
#' ## --- Data File Created By ActiGraph GT1M ActiLife v4.4.1 Firmware v7.2.0 ---
#' ## Serial Number: LYN2B21080027
#' ## Start Time 16:15:00
#' ## Start Date 6/16/2010
#' ## Epoch Period (hh:mm:ss) 00:00:01
#' ## Download Time 09:50:23
#' ## Download Date 6/22/2010
#' ## Current Memory Address: 983038
#' ## Current Battery Voltage: 4.01     Mode = 0
#' ## --------------------------------------------------
#' ###############################################################################
#' ## This raw data with 1-sec epoch can be converted to a correct data format to
#' ## classify wear and nonwear time using "wearingMarking" by the following code:
#'
#' \dontrun{mydata1s = readCountsData("rawActigraphOutput.dat", ctPerSec=1)}
#' @export

readCountsData <- function(filename, ctPerSec, mode = 0) {
    print("Please wait while I am reading your source data ...")
    #reading raw data
    Tfile <- file(filename, "r")
    if(isOpen(Tfile, "r")) #  TRUE
    {
        seek(Tfile, 0, rw="r") # reset to beginning
        lines <- readLines(Tfile)
        close(Tfile)
    }

    skipPos <- max(grep("-----", lines))
    heading <- lines[seq(skipPos)]
    lines <- lines[-seq(skipPos)]
    metaVals <- agdMetaKeys()
    metaKeys <- names(metaVals)
    for(i in seq_along(metaVals)) {
        key <- gsub("[(]", "\\\\(", metaKeys[i])
        regex <- sprintf("^.*%s[:= ]+([^ ]+).*$", key)
        metaVals[i] <- sub(regex, "\\1", grep(key, heading, value=TRUE))
    }
    # reformat date string as Y-m-d
    startDate <- sub("([0-9]+)/([0-9]+)/([0-9]+)", "\\3-\\1-\\2",
                     metaVals['Start Date'])
    # add leading zeroes where necessary
    startDate <- sub("-([0-9])$", "-0\\1",
                     sub("-([0-9])-", "-0\\1-", startDate))
    ts <- paste(startDate, metaVals['Start Time'], sep = " ")
    rawTimeStamp <- as.POSIXct(ts, tz = 'UTC')

    rawdata <- as.numeric(unlist(lapply(strsplit(lines, "[ ]+"), function(i) {
        i[i != ""]
    })))

    altmode <- as.numeric(metaVals['Mode'])
    if(!is.na(altmode) & altmode != mode) {
        warning(sprintf("mode provided [%s] differs from header [%s]",
                        mode, altmode), call. = FALSE)
    }
    colNames <- modeNames(mode)
    dat <- data.frame(matrix(rawdata, ncol=length(colNames), byrow=TRUE))
    names(dat) <- colNames
    if(all(c('axis1', 'axis2', 'axis3') %in% colNames)) {
        dat[,'vm'] <- ceiling(sqrt(rowSums(dat[,c('axis1','axis2','axis3')]^2)))
    }
    dat <- cbind(TimeStamp = NA, dat)
    nl <- nrow(dat)

    if(ctPerSec > 1) {
        rst <- rawTimeStamp + seq(0, nl/ctPerSec)
        rst <- rep(rst, each=ctPerSec)[seq(nl)]
    } else {
        rst <- rawTimeStamp + seq(0, nl-1) / ctPerSec
    }
    dat[,'TimeStamp'] <- rst
    attr(dat, 'metadata') <- as.data.frame(metaVals, stringsAsFactors=FALSE)
    dat
}
