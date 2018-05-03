#' Summarize Classified Wear Time by Daily Basis
#'
#' This function summarizes accelerometer data and the classified wear or nonwear
#' time by daily basis.
#'
#' This function summarizes the total number of days, weekdays and weekends in
#' accelerometer data. Also this function provides the total number of valid
#' days, valid weekdays and valid weekend days based on a user defined cutoff
#' for the total minutes of classified monitor wear time per day. This function
#' also summarizes the classified wear (nonwear) time by day and by valid day,
#' and the mean wear (nonwear) time for valid days during weekday and weekends,
#' and for overall valid days. If mail delivery days are classified by 
#' \code{\link{markDelivery}}, it also summarizes the classified delivery
#' (non-delivery) days with argument \dQuote{delivery}.
#' If \dQuote{pai} column is present in the data, 
#' which can be created by \code{\link{markPAI}}, then physical activity intensity
#' (PAI) level will be summarized in the output.
#' 
#' @param data Data with classified wear (nonwear) status by
#' \code{\link{wearingMarking}}.
#' @param validCut A cutoff for the total minutes of classified monitor wear
#' time per day to be considered as a valid monitor day.
#' @param perMinuteCts The number of data rows per minute. The default is 1-min
#' epoch (perMinuteCts = 1) and we recommend to use 1-min epoch data for this
#' summary. For examples: for data with 10-sec epoch, set perMinuteCts = 6; for
#' data with 1-sec epoch, set perMinuteCts = 60.
#' @param markingString Option for summarizing wear (markingString = \dQuote{w}) or
#' nonwear time (markingString = \dQuote{nw}).
#' @param TS The column name for timestamp. The default is \dQuote{TimeStamp}.
#' @param cts The name of the counts column. The default is \dQuote{axis1}.
#' @param delivery data.frame. Delivery information created by
#' \code{\link{markDelivery}}.
#'
#' @return
#' \item{unit}{epoch for data.}
#' \item{totalNumDays}{the total number of days in accelerometer data.}
#' \item{totalNumWeekWeekend}{the total number of weekdays and weekend days in
#' accelerometer data.}
#' \item{validCut}{a user defined cutoff for the total minutes of classified
#' monitor wear time per day to be considered as a valid monitor day.}
#' \item{totalValidNumDays}{the total number of valid days based on the user defined 
#' cutoff (\dQuote{validCut}) for the total minutes of wear time and the classified wear time.}
#' \item{totalValidNumWeekWeekend}{the total number of valid weekdays and valid
#' weekend days based on the user defined cutoff (\dQuote{validCut}) for the total minutes of
#' classified monitor wear time per day.}
#' \item{wearTimeByDay}{the classified total wear (nonwear) time by day.}
#' \item{deliveryDays}{marked delivery days.}
#' \item{validWearTimeByDay}{the classified total wear (nonwear) time by valid
#' day.}
#' \item{meanWeartimeValidDays}{the mean wear (nonwear) time for valid days
#' during weekdays and weekends.}
#' \item{meanWeartimeOverallValidDays}{the mean wear (nonwear) time for overall
#' valid days.}
#' \item{dayInfo}{information about wear time and mean counts for each day.}
#' \item{intensity}{conditional output, the total time in hours of physical activity intensity by day.}
#' \item{meanValidIntensity}{conditional output, the mean physical activity intensity (PAI) level for valid days.}
#'
#' @template ref2011
#'
#' @templateVar author colechoi
#' @template auth
#'
#' @seealso \code{\link{wearingMarking}}, \code{\link{sumVct}},
#' \code{\link{markPAI}}, \code{\link{markDelivery}}
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
#' summaryData(data=data1m, validCut=600, perMinuteCts=1, markingString = "w", cts = "counts")
#'
#' data(deliveryData)
#' options(pa.cts = "vm")
#' wm <- wearingMarking(dataset = deliveryData)
#' dd <- markDelivery(wm)
#' summaryData(wm, delivery = dd)
#'
#' pai.data <- markPAI(data = wm)
#' dd <- markDelivery(pai.data)
#' summaryData(pai.data, delivery = dd)
#'
#' @export

summaryData <- function(data, validCut = getOption('pa.validCut'),
                        perMinuteCts = 1, markingString = "w",
                        TS = getOption('pa.timeStamp'),
                        cts = getOption('pa.cts'), delivery = NULL) {
    stopifnot('weekday' %in% names(data))
    usePAI <- 'pai' %in% names(data)
    rmDel <- !is.null(delivery) && 'delivery' %in% names(delivery)
    epoch <- 60 / perMinuteCts
    if(perMinuteCts==1) {
        unit <- "1 min"
    } else if(perMinuteCts==60) {
        unit <- "1 sec"
    } else {
        unit <- paste(epoch, "sec")
    }
    catWeekend <- function(day) {
        factor(day %in% c('Saturday', 'Sunday'), levels = c(FALSE, TRUE),
               labels = c('weekday', 'weekend'))
    }
    minPerDay <- function(timerange) {
        dx <- epoch / 60
        as.numeric(difftime(timerange[2], timerange[1], units='mins')) + dx
    }
    validCut <- validCut * perMinuteCts
    data[,'weekend'] <- catWeekend(data[,'weekday'])
    dayLabel <- unique(data[,c('days','weekday','weekend')])
    totalNumDays <- nrow(dayLabel)
    ts <- data[,TS]
    minCnts <- sapply(tapply(ts, data[,'days'], range), minPerDay)
    obsCnts <- tapply(seq_len(nrow(data)), data[,'days'], length)
    ix <- match(dayLabel[,'days'], names(minCnts))
    dayLabel <- cbind(dayLabel, minutes=minCnts[ix], obs=obsCnts[ix])
    # total number of week and weekend days
    totWWe <- tapply(seq_len(totalNumDays), dayLabel[,'weekend'], length)
    wearTime <- sumVct(data, markingString = markingString)
    wearTime[,'weekend'] <- catWeekend(wearTime[,'weekday'])
    wearTimeByDay <- tapply(wearTime[,'duration'], wearTime[,'days'],
                            sum, na.rm=TRUE)
    ix <- match(dayLabel[,'days'], names(wearTimeByDay))
    dayLabel <- cbind(dayLabel, wearTime=wearTimeByDay[ix])
    dayLabel[is.na(dayLabel[,'wearTime']),'wearTime'] <- 0
    ddAll <- data[data[, "wearing"] == markingString,]
    mean.cnt <- round(tapply(ddAll[, cts], ddAll[, "days"], 'mean'), 1)
    ix <- match(dayLabel[,'days'], names(mean.cnt))
    dayLabel <- cbind(dayLabel, cnt.mean=mean.cnt[ix])
    wearTimeByDay <- dayLabel[,'wearTime']
    names(wearTimeByDay) <- dayLabel[,'days']
    validWearTimeByDay <- wearTimeByDay[wearTimeByDay >= validCut]
    vDayLabel <- dayLabel[dayLabel[,'wearTime'] >= validCut,]
    # remove delivery dates
    rmDelDays <- NULL
    if(rmDel) {
        delDay <- delivery[delivery[,'delivery'] == 1, 'day']
        delDay1 <- intersect(delDay, vDayLabel[,'days'])
        delDay2 <- intersect(delDay, dayLabel[,'days'])
        if(length(delDay1)) {
            dix <- match(delDay1, vDayLabel[,'days'])
            validWearTimeByDay <- validWearTimeByDay[-dix]
            vDayLabel <- vDayLabel[-dix,]
        }
        if(length(delDay2)) {
            dix <- match(delDay2, dayLabel[,'days'])
            rmDelDays <- wearTimeByDay[dix]
        }
    }
    # total number of days for valid days
    totalValidNumDays <- nrow(vDayLabel)
    # total number of week and weekend days for valid days
    vtotWWe <- tapply(seq_len(totalValidNumDays), vDayLabel[,'weekend'], length)
    if(!is.na(totWWe['weekday']) && is.na(vtotWWe['weekday'])) {
        vtotWWe['weekday'] <- 0
    }
    if(!is.na(totWWe['weekend']) && is.na(vtotWWe['weekend'])) {
        vtotWWe['weekend'] <- 0
    }
    # wear time duration by day on valid days
    meanWeartime <- tapply(vDayLabel[,'wearTime'],
                                    vDayLabel[,'weekend'], mean, na.rm=TRUE)
    meanWeartimeOverall <- mean(vDayLabel[,'wearTime'], na.rm=TRUE)
    res <- list(unit=unit, totalNumDays=totalNumDays,
        totalNumWeekWeekend=totWWe,
        validCut=validCut,
        totalValidNumDays=totalValidNumDays,
        totalValidNumWeekWeekend=vtotWWe,
        wearTimeByDay=wearTimeByDay,
        deliveryDays = rmDelDays,
        validWearTimeByDay=validWearTimeByDay,
        meanWeartimeValidDays=meanWeartime,
        meanWeartimeOverallValidDays=meanWeartimeOverall,
        dayInfo=dayLabel
    )
    if(usePAI) {
        intLevel <- do.call(rbind, tapply(data[,'pai'], data[,'days'], table))
        getPerc <- function(x) x / sum(x, na.rm = TRUE)
        intLevelPerc <- round(t(apply(intLevel, 1, getPerc)), 2)
        colnames(intLevelPerc) <- paste0(colnames(intLevelPerc), '.perc')
        intLevels <- cbind(intLevel, intLevelPerc)
        if(totalValidNumDays > 0) {
            vix <- match(vDayLabel[,'days'], rownames(intLevels))
            cntCols <- !grepl('.perc$', colnames(intLevels))
            validIntLevel <- colMeans(intLevels[vix, cntCols, drop=FALSE],
                                      na.rm = TRUE)
            vilp <- getPerc(validIntLevel)
            names(vilp) <- colnames(intLevelPerc)
            validIntLevels <- round(c(validIntLevel, vilp), 2)
        } else {
            validIntLevels <- NULL
        }
        res[['intensity']] <- intLevels
        res[['meanValidIntensity']] <- validIntLevels
    }
    res
}
