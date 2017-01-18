#' Read ActiGraph Accelerometer Data
#'
#' This function reads an ActiGraph agd file into R as a data.frame.
#' Accelerometer data has three axes which are used to create vector magnitude.
#' Observations can be classified with wear and nonwear time using
#' \code{\link{wearingMarking}}.
#'
#' agd files are SQLite databases.  This function requires the \code{RSQLite}
#' package.
#'
#' @param datfile An agd file.
#' @param convertTime Convert the time stamp from a character string into
#' POSIXct.
#'
#' @return data.frame with accelerometer data.
#'
#' @seealso \code{\link{wearingMarking}}, \code{\link{queryActigraph}}
#'
#' @examples
#' \dontrun{
#' dat <- readActigraph("actfile.agd")
#' dat1s <- wearingMarking(dataset = dat,
#'                        frame = 90,
#'                        perMinuteCts = 1,
#'                        TS = "TimeStamp",
#'                        cts = "axis1",
#'                        streamFrame = NULL,
#'                        allowanceFrame= 2,
#'                        newcolname = "wearing",
#'                        getMinuteMarking = FALSE)
#' }
#' @export

readActigraph <- function(datfile, convertTime=TRUE) {
    meta <- agdSettings(datfile)
    colNames <- modeNames(meta['Mode',1])
    # dataTimestamp is 64bit, which R doesn't like
    select <- paste('CAST(dataTimestamp AS TEXT) AS TimeStamp', 
                    paste(colNames, collapse=", "), sep=', ')
    qry <- sprintf("SELECT %s FROM data", select)
    dat <- queryActigraph(datfile, qry)
    if(all(c('axis1', 'axis2', 'axis3') %in% colNames)) {
        dat[,'vm'] <- ceiling(sqrt(rowSums(dat[,c('axis1','axis2','axis3')]^2)))
    }
    if(convertTime) {
        # timezone will be UTC
        dat[,'TimeStamp'] <- timeFromYear1(as.numeric(dat[,'TimeStamp']))
    }
    attr(dat, 'metadata') <- meta
    dat
}

#' Create Time From Seconds Since 0001-01-01
#'
#' There are 62,135,596,800 seconds from 0001-01-01 until 1970-01-01.
#' The last 1e7 digits of x are unnecessary microseconds.
#'
#' @keywords internal
timeFromYear1 <- function(x, base=62135596800, tz = 'UTC') {
    as.POSIXct(x %/% 1e7 - base, origin="1970-01-01", tz = tz)
}

#' Define Metadata Vector
#'
#' @keywords internal
agdMetaKeys <- function() {
    metaKeys <- c('Serial Number', 'Start Time', 'Start Date',
                  'Epoch Period (hh:mm:ss)', 'Download Time', 'Download Date',
                  'Current Memory Address', 'Current Battery Voltage', 'Mode')
    metaVals <- character(length(metaKeys))
    names(metaVals) <- metaKeys
    metaVals
}

#' Retrieve ActiGraph Settings
#'
#' Query ActiGraph (agd) SQLite file for settings.
#'
#' @keywords internal
agdSettings <- function(datfile) {
    qry <- "SELECT settingID, settingName, settingValue FROM settings"
    res <- queryActigraph(datfile, qry)
    sqlFields <- c('deviceserial', 'startdatetime', 'epochlength',
                   'downloaddatetime', 'batteryvoltage', 'modenumber',
                   'addresspointer')
    sqlValues <- res[match(sqlFields, res[,'settingName']), 'settingValue']
    starttime <- timeFromYear1(as.numeric(sqlValues[2]))
    downtime <- timeFromYear1(as.numeric(sqlValues[4]))
    metaVals <- agdMetaKeys()
    epoch <- as.POSIXct("1970-01-01", tz="UTC") + as.numeric(sqlValues[3])
    metaVals[1] <- sqlValues[1]
    metaVals[2] <- format(starttime, "%H:%M:%S")
    metaVals[3] <- format(starttime, "%m/%d/%Y")
    metaVals[4] <- format(epoch, "%H:%M:%S")
    metaVals[5] <- format(downtime, "%H:%M:%S")
    metaVals[6] <- format(downtime, "%m/%d/%Y")
    metaVals[7] <- sqlValues[7]
    metaVals[8] <- sqlValues[5]
    metaVals[9] <- sqlValues[6]
    as.data.frame(metaVals, stringsAsFactors=FALSE)
}

#' Retrieve ActiGraph Column Names
#'
#' Column names should match the mode number.
#'
#' https://actigraph.desk.com/customer/en/portal/articles/
#' 2515800-what-do-the-different-mode-numbers-mean-in-a-csv-or-dat-file-
#' @keywords internal

modeNames <- function(mode) {
    mode <- as.numeric(mode)
    modes <- expand.grid(steps=0:1, hr=0:1, axis2=0:1, axis3=0:1, lux=0:1,
                         incline=0:1)
    nm <- nrow(modes)
    stopifnot(mode >= 0, mode < nm)
    modes <- modes[,rev(names(modes))]
    modes <- cbind(mode=seq(nm)-1, binary=apply(modes, 1, paste, collapse=''),
                   modes)
    mymode <- unlist(modes[mode+1,])
    modename <- c('axis1', 'axis2', 'axis3', 'steps', 'hr', 'lux', 'incline')
    # can't confirm hr/lux/incline
    active <- c(1, mymode[c('axis2','axis3','steps')], 0, 0, 0) == 1
    modename[active]
}
