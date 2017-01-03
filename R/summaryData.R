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

summaryData <- function(data, validCut=600, perMinuteCts=1, markingString = "w") {
    if(perMinuteCts==1) {
        unit = "1 min"
    } else if(perMinuteCts==60) {
        unit = "1 sec"
    } else {
        epoch <- 60/perMinuteCts
        unit = paste(epoch, "sec")
    }
    validCut <- validCut*perMinuteCts
    data$weekend <- ifelse(data$weekday=="Saturday" | data$weekday=="Sunday", 1, 0)
    data$weekend <- factor(data$weekend, levels=c(0,1), labels=c("weekday", "weekend") )
    # total number of week and weekend days
    totalNumWeekWeekend <- tapply(data$weekend, data$weekend, length)/(1440*perMinuteCts)
    totalNumWeekWeekend[is.na(totalNumWeekWeekend)==1] <- 0
    # total number of days
    totalNumDays <- (length(data[,1])/(1440*perMinuteCts))
    totalNumDays[is.na(totalNumDays)==1] <- 0
    wearTime <- sumVct(data, markingString = markingString)
    wearTime$weekend <- ifelse(wearTime$weekday=="Saturday" | wearTime$weekday=="Sunday", 1, 0)
    wearTime$weekend <- factor(wearTime$weekend, levels=c(0,1), labels=c("weekday", "weekend") )
    wearTimeByDay <- tapply(wearTime$duration, wearTime$days, sum, na.rm=T)
    wearTimeByDay
    validWearTimeByDay <- wearTimeByDay[ifelse(wearTimeByDay > validCut, 1, 0)==1]
    validWearTimeByDay
    valid.days <- as.numeric(names(validWearTimeByDay))
    valid.days
    valid.wearTime <- wearTime[wearTime$days%in%valid.days,]
    valid.wearTime
    valid.data <- data[data$days%in%valid.days,]
    valid.data$weekend <- ifelse(valid.data$weekday=="Saturday" | valid.data$weekday=="Sunday", 1, 0)
    valid.data$weekend <- factor(valid.data$weekend, levels=c(0,1), labels=c("weekday", "weekend") )
    # total number of week and weekend days for valid days
    totalValidNumWeekWeekend <- tapply(valid.data$weekend, valid.data$weekend, length)/(1440*perMinuteCts)
    totalValidNumWeekWeekend[is.na(totalValidNumWeekWeekend)==1] <- 0
    # total number of days for valid days
    totalValidNumDays <- (length(valid.data[,1])/(1440*perMinuteCts))
    totalValidNumDays[is.na(totalValidNumDays)==1] <- 0
    meanWeartimeValidDays <- tapply(valid.wearTime$duration, valid.wearTime$weekend, sum, na.rm=T)/totalValidNumWeekWeekend
    meanWeartimeValidDays
    meanWeartimeValidDays[is.na(meanWeartimeValidDays)==1] <- 0
    meanWeartimeOverallValidDays <- sum(tapply(valid.wearTime$duration, valid.wearTime$weekend, sum, na.rm=T), na.rm=T)/totalValidNumDays
    meanWeartimeOverallValidDays
    meanWeartimeOverallValidDays[is.na(meanWeartimeOverallValidDays)==1] <- 0
    return(list(unit=unit, totalNumDays=totalNumDays,totalNumWeekWeekend=totalNumWeekWeekend, validCut=validCut, totalValidNumDays=totalValidNumDays, totalValidNumWeekWeekend=totalValidNumWeekWeekend, wearTimeByDay=wearTimeByDay, validWearTimeByDay=validWearTimeByDay, meanWeartimeValidDays=meanWeartimeValidDays, meanWeartimeOverallValidDays=meanWeartimeOverallValidDays) )
}
