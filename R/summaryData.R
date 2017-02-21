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
#' @param TS The column name for timestamp. The default is "TimeStamp".
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
#' \item{dayInfo}{information about each day of wear.}
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
                        markingString = "w", TS = "TimeStamp") {
    stopifnot('weekday' %in% names(data))
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
    obsCnts <- tapply(seq(nrow(data)), data[,'days'], length)
    ix <- match(dayLabel[,'days'], names(minCnts))
    dayLabel <- cbind(dayLabel, minutes=minCnts[ix], obs=obsCnts[ix])
    # total number of week and weekend days
    totWWe <- tapply(seq(totalNumDays), dayLabel[,'weekend'], length)
    wearTime <- sumVct(data, markingString = markingString)
    wearTime[,'weekend'] <- catWeekend(wearTime[,'weekday'])
    wearTimeByDay <- tapply(wearTime[,'duration'], wearTime[,'days'],
                            sum, na.rm=TRUE)
    ix <- match(dayLabel[,'days'], names(wearTimeByDay))
    dayLabel <- cbind(dayLabel, wearTime=wearTimeByDay[ix])
    dayLabel[is.na(dayLabel[,'wearTime']),'wearTime'] <- 0
    wearTimeByDay <- setNames(dayLabel[,'wearTime'], dayLabel[,'days'])
    validWearTimeByDay <- wearTimeByDay[wearTimeByDay >= validCut]
    vDayLabel <- dayLabel[dayLabel[,'wearTime'] >= validCut,]
    # total number of days for valid days
    totalValidNumDays <- nrow(vDayLabel)
    # total number of week and weekend days for valid days
    vtotWWe <- tapply(seq(totalValidNumDays), vDayLabel[,'weekend'], length)
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
    list(unit=unit, totalNumDays=totalNumDays,
        totalNumWeekWeekend=totWWe,
        validCut=validCut,
        totalValidNumDays=totalValidNumDays,
        totalValidNumWeekWeekend=vtotWWe,
        wearTimeByDay=wearTimeByDay,
        validWearTimeByDay=validWearTimeByDay,
        meanWeartimeValidDays=meanWeartime,
        meanWeartimeOverallValidDays=meanWeartimeOverall,
        dayInfo=dayLabel
    )
}
