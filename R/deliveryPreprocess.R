#' Preprocess Accelerometry Data
#'
#' This function preprocesses accelerometry data by removing days based on a total
#' activity count (default: less than 5000) or total time with activity (default: less than 10 minutes). 
#' Additionally, the function has an option to zeropad truncated days such that that days that do not 
#' have a whole day of 1440 minutes of data spanning from 00:00 to 23:59 (default: TRUE).
#'
#' Function works for dataframes from one or multiple unique trials.
#'
#' @param df A dataframe. The source accelerometry dataset, in dataframe format. 
#' @param minLow Numeric. The minimum total counts of movement for a day to not be removed.
#' @param minTime Numeric. The minimum number of minutes of activity for a day to not be removed.
#' @param zeropad Boolean value for truncated days to be zeropadded.
#' @param \dots not used at this time
#'
#' @return The dataframe is returned with days fulfilling the dropping criteria removed and truncated days zeropadded. 
#' A new column indicating which day is added to the dataframe.
#' 
#' @note The input dataframe should have the following columns: 
#' \sQuote{TimeStamp}, \sQuote{axis1}, \sQuote{axis2}, \sQuote{axis3}, \sQuote{vm},
#' where \sQuote{vm} is the vector magnitude of axes 1, 2, and 3. 
#' Dataframe should also be formatted to 60 second epoch. 
#'
#' @templateVar author ryancolechoi
#' @template auth
#' 
#' @seealso \code{\link{deliveryPred}}
#'
#' @examples
#' data(deliveryData)
#' 
#' deliveryDataProcessed <- deliveryPreprocess(df = deliveryData)
#' 
#' @export

deliveryPreprocess <- function(df, minLow = 5000, minTime = 10, zeropad = TRUE, ...) {
  `%notin%` <- Negate(`%in%`)
  xnames <- names(df)
  misscol <- setdiff(c('TimeStamp','axis1','axis2','axis3','vm'), xnames)
  if(length(misscol) > 0) {
    stop(sprintf('columns missing from data set: %s', paste(misscol, collapse = ', ')))
  }
  if("steps" %notin% xnames) {
    df[,"steps"] <- rep(0, ncol(df))
    warning("steps unspecified - dummy steps created")
  }
  if('.id' %notin% xnames) {
    # create a dummy ID
    df[['.id']] <- 1  
    warning(".id unspecified - dummy ID created")
  }
  # create time as user specifies, but force UTC
  df[,'TimeStamp'] <- convertTZ(createTime(df[,'TimeStamp'], ...))
  df[,'day'] <- format(df[,'TimeStamp'], format = "%m-%d")

  # Make dataframe into list indexed by trial ID
  df$.id <- factor(df$.id, levels=unique(df$.id))
  dat <- split(df, df$.id)

  #Check counts per minute. Give warning/dataCollapse if != 60
  checkCounts <- function(df) {
    ctsPerMin <- as.numeric(difftime(df[2,'TimeStamp'], df[1,'TimeStamp'], units="secs"))
    if(ctsPerMin != 60) {
      df <- dataCollapser(df, TS = "TimeStamp", by = 60,
                          col = c("axis1", "axis2", "axis3", "vm"))
      warning('data does not contain 60 counts per minute - collapsed to 60 second epoch')
    }
    df
  }
  dat <- lapply(dat, checkCounts)

  # Index days numerically
  dayIndex <- function(df) {
    uday <- unique(df[,'day'])
    df[,'day'] <- match(df[,'day'], uday)
    return(df)
  }
  dat <- lapply(dat, dayIndex)

  #Removes days with less than 5000 total vm or 10 minutes of minutes
  removeLow <- function(df, min_low = minLow, min_time=minTime) {
    dayFactor <- as.factor(df[,'day'])
    vmByDay <- tapply(df[,'vm'], dayFactor, sum)
    nzByDay <- tapply(df[,'vm'] != 0, dayFactor, sum)
    ix1 <- names(Filter(function(x) x < min_low, vmByDay))
    ix2 <- names(Filter(function(x) x < min_time, nzByDay))
    ix <- union(ix1, ix2)
    df <- df[df[,'day'] %notin% ix,]
    return(df)
  }
  dat <- lapply(dat, removeLow)

  #Zeropad all days in a trial is less than 1440 observations
  if(zeropad == TRUE) {
    zeroPatch <- function(df) {
      #For loop over all days with incomplete data
      counts <- table(df[,'day'])
      days <- as.numeric(names(counts))
      ix <- which(counts < 1440)
      if(length(ix) == 0) return(df)
      curid <- df[1,'.id']
      newdat <- vector('list', length(ix))
      for(i in seq_along(ix)) {
        dayno <- days[ix[i]]
        ts <- df[df[,'day'] == dayno, 'TimeStamp']
        #Create dataframe of 0s for accelerometer data for all times not in day but preserve id/day
        yearmonthday <- format(ts[1], '%Y-%m-%d')
        timerange <- seq.POSIXt(as.POSIXct(paste0(yearmonthday, " 00:00:00"), tz = 'UTC'), by = 'min', length.out = 60 * 24)
        tmiss <- timerange %notin% ts
        tmp2 <- data.frame(
          .id = curid,
          TimeStamp = timerange[tmiss],
          axis1 = 0,
          axis2 = 0,
          axis3 = 0,
          steps = 0,
          vm = 0,
          day = dayno
        )
        newdat[[i]] <- tmp2
      }
      moredat <- do.call(rbind, newdat)
      xnames <- names(df)
      moredat[,setdiff(xnames, names(moredat))] <- NA
      moredat <- moredat[,xnames]
      fulldf <- rbind(df, moredat)
      fulldf <- fulldf[order(fulldf[,'TimeStamp']),]
      rownames(fulldf) <- NULL
      fulldf
    }
    dat <- lapply(dat, zeroPatch)
  }
  return_data <- l2df(dat)
  return(return_data)
}
