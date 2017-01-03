#' Convert Accelerometer Output Data to Correct Data Format
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
#'
#' @return Data with the correct format (TimeStamp, counts) to be used for
#' \code{\link{wearingMarking}}.
#'
#' @template ref2010
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
#' ## Read accelerometer output and convert to correct format (TimeStamp, counts).
#' ## As example, "rawActigraphOutput.dat" is an Actigraph output with header:
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

readCountsData <- function(filename, ctPerSec) {
    print("Please wait while I am reading your source data ...")
    #reading raw data
    Tfile <- file(filename, "r")
    if(isOpen(Tfile, "r")) #  TRUE
    {
        seek(Tfile, 0, rw="r") # reset to beginning
        lines = readLines(Tfile)
        close(Tfile)
    }

    skipPos = grep("-----", lines)[2]  #number of skip lines
    startTPos = grep("Start Time", lines)  #start time

    #get start date
    startTime = gsub("Start Time ", "", lines[startTPos])
    startTime = gsub("[[:blank:]]", "", startTime)
    startDatePos = grep("Start Date ", lines)  #startdate
    startDate = gsub("Start Date ", "", lines[startDatePos])
    startDate = gsub("[[:blank:]]", "", startDate)
    startDate = strsplit(startDate, "/")[[1]]
    if(nchar(startDate[1]) == 1){
        startDate[1] = paste("0", startDate[1], sep ="")
    }
    if(nchar(startDate[2]) == 1){
        startDate[2] = paste("0", startDate[2], sep ="")
    }
    startDate = paste(startDate[3], startDate[1], startDate[2], sep = "-")
    #end of getting startdate
    rawTimeStamp1  = paste(startDate, startTime, sep = " ")
 
    #get epochtime
    #if(is.null(ctPerSec)){
    #    ctPerSec = getEpoch(filename, unit = "sec")
    #}
    #end of getting epoch time

    startline = skipPos+1
    endline = length(lines)

    rawdata = c()
    timeline = c()
    for(i in startline: endline){
        temp0 = gsub("[[:blank:]]+", " ",lines[i])
        temp = strsplit(temp0, " ")[[1]]
        temp = temp[temp != ""]
        rawdata = c(rawdata, temp)
    }

    if(ctPerSec >1){
        timeline = rep(0:as.integer(length(rawdata)/ctPerSec), each = ctPerSec)[1:length(rawdata)]
    }else{  
        timeline = (0:as.integer(length(rawdata)-1)/ctPerSec)
    }

    rawTimeStamp = rep(rawTimeStamp1, length(rawdata))
    rst = gsub(" GMT", "", as.POSIXlt(rawTimeStamp, tz = "GMT")+ timeline)
    data.frame(TimeStamp = as.vector(rst), counts = as.numeric(as.vector(rawdata)))
}
