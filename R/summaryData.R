#' Summarize Classified Wear Time by Daily Basis
#'
#' This function summarizes accelerometer data and classified wear or nonwear
#' time by daily basis.
#'
#' This function summarizes the total number of days, weekdays and weekends in
#' accelerometer data. Also this function provides the total number of valid
#' days, valid weekdays and valid weekend days based on a user defined cutoff
#' for the total minutes of classified monitor wear time per day. This function
#' also summarizes the classified wear (nonwear) time by day and by valid day,
#' and the mean wear (nonwear) time for valid days during weekday and weekends,
#' and for overall valid days.
#'
#' @param data Data with classified wear (nonwear) status by
#' \code{\link{wearingMarking}}.
#' @param validCut A cutoff for the total minutes of classified monitor wear
#' time per day to be considered as a valid monitor day.
#' @param perMinuteCts The number of data rows per minute. The default is 1-min
#' epoch (perMinuteCts = 1) and we recommend to use 1-min epoch data for this
#' summary. For examples: for data with 10-sec epoch, set perMinuteCts = 6; for
#' data with 1-sec epoch, set perMinuteCts = 60.
#' @param markingString Option for summarizing wear (markingString = "w") or
#' nonwear time (markingString = "nw").
#'
#' @return
#' \item{unit}{epoch for data.}
#' \item{totalNumDays}{the total number of days in accelerometer data.}
#' \item{totalNumWeekWeekend}{the total number of weekdays and weekend days in
#' accelerometer data.}
#' \item{validCut}{a user defined cutoff for the total minutes of classified
#' monitor wear time per day to be considered as a valid monitor day.}
#' \item{totalValidNumDays}{the total number of valid days based on the user
#' defined cutoff for total minutes of wearing and the classified wearing time.}
#' \item{totalValidNumWeekWeekend}{the total number of valid weekdays and valid
#' weekend days based on a user defined cutoff for the total minutes of
#' classified monitor wear time per day.}
#' \item{wearTimeByDay}{the classified total wear (nonwear) time by day.}
#' \item{validWearTimeByDay}{the classified total wear (nonwear) time by valid
#' day.}
#' \item{meanWeartimeValidDays}{the mean wear (nonwear) time for valid days
#' during weekdays and weekends.}
#' \item{meanWeartimeOverallValidDays}{the mean wear (nonwear) time for overall
#' valid days.}
#'
#' @template ref2010
#'
#' @templateVar author choi
#' @template auth
#'
#' @seealso \code{\link{wearingMarking}}, \code{\link{sumVct}}
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
#' summaryData(data=data1m, validCut=600, perMinuteCts=1, markingString = "w")
#' @export

summaryData <- function(data, validCut = 600, perMinuteCts = 1,
                        markingString = "w") {
    stopifnot('weekday' %in% names(data))
    if(perMinuteCts==1) {
        unit <- "1 min"
    } else if(perMinuteCts==60) {
        unit <- "1 sec"
    } else {
        epoch <- 60 / perMinuteCts
        unit <- paste(epoch, "sec")
    }
    catWeekend <- function(day) {
        factor(day %in% c('Saturday', 'Sunday'), levels = c(FALSE, TRUE),
               labels = c('weekday', 'weekend'))
    }
    ctsPerDay <- 1440 * perMinuteCts
    validCut <- validCut * perMinuteCts
    data[,'weekend'] <- catWeekend(data[,'weekday'])
    # total number of week and weekend days
    totWWe <- tapply(data[,'weekend'], data[,'weekend'], length) / ctsPerDay
    totWWe[is.na(totWWe)] <- 0
    # total number of days
    totalNumDays <- nrow(data) / ctsPerDay
    totalNumDays[is.na(totalNumDays)] <- 0
    wearTime <- sumVct(data, markingString = markingString)
    wearTime[,'weekend'] <- catWeekend(wearTime[,'weekday'])
    wearTimeByDay <- tapply(wearTime[,'duration'], wearTime[,'days'],
                            sum, na.rm=TRUE)
    validWearTimeByDay <- wearTimeByDay[wearTimeByDay > validCut]
    valid.days <- as.numeric(names(validWearTimeByDay))
    valid.wearTime <- wearTime[wearTime[,'days'] %in% valid.days,]
    # valid wear time
    val.WT <- wearTime[wearTime[,'days'] %in% valid.days,]
    # weekend indicator for valid time
    valid.WE <- data[data[,'days'] %in% valid.days, 'weekend']
    # total number of week and weekend days for valid days
    totalValidNumWeekWeekend <- tapply(valid.WE, valid.WE, length) / ctsPerDay
    totalValidNumWeekWeekend[is.na(totalValidNumWeekWeekend)] <- 0
    # total number of days for valid days
    totalValidNumDays <- length(valid.WE) / ctsPerDay
    totalValidNumDays[is.na(totalValidNumDays)] <- 0
    # wear time duration by day on valid days
    val.WTD <- tapply(val.WT[,'duration'], val.WT[,'weekend'], sum, na.rm=TRUE)
    meanWeartimeValidDays <- val.WTD / totalValidNumWeekWeekend
    meanWeartimeValidDays[is.na(meanWeartimeValidDays)] <- 0
    meanWeartimeOverallValidDays <- sum(val.WTD, na.rm=TRUE) / totalValidNumDays
    meanWeartimeOverallValidDays[is.na(meanWeartimeOverallValidDays)] <- 0
    list(unit=unit, totalNumDays=totalNumDays,
        totalNumWeekWeekend=totWWe,
        validCut=validCut,
        totalValidNumDays=totalValidNumDays,
        totalValidNumWeekWeekend=totalValidNumWeekWeekend,
        wearTimeByDay=wearTimeByDay,
        validWearTimeByDay=validWearTimeByDay,
        meanWeartimeValidDays=meanWeartimeValidDays,
        meanWeartimeOverallValidDays=meanWeartimeOverallValidDays
    )
}
