#' Internal functions
#' 
#' code{l2df}: convert list to data.frame
#'
#' code{convertTZ}: force a new timezone on datetime
#'
#' code{createTime}: create datetime variable
#'
#' code{addDayIndex}: create ID_day variable from timestamp
#'
#' @name pa-internal
#' @aliases l2df convertTZ createTime addDayIndex
#' @keywords internal
NULL

l2df <- function(l, keepDT = FALSE) {
  if(requireNamespace("data.table", quietly = TRUE)) {
    x <- data.table::rbindlist(l)
    if(!keepDT) x <- as.data.frame(x)
  } else {
    x <- do.call(rbind, c(l, make.row.names = FALSE))
  }
  x
}

convertTZ <- function(x, tz = 'UTC') {
  stopifnot(inherits(x, 'POSIXt'))
  day1 <- x[1]
  day2 <- as.POSIXct(as.character(day1), tz = tz)
  if(difftime(day1, day2, units='secs') != 0) {
    offset <- difftime(x, day1, units='secs')
    x <- day2 + offset
  }
  x
}

createTime <- function(x, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC', ...) {
  if(inherits(x, 'POSIXt')) return(x)
  as.POSIXct(x, format = format, tz = tz)
}

addDayIndex <- function(df, ...) {
  `%notin%` <- Negate(`%in%`)
  xnames <- names(df)
  misscol <- setdiff('TimeStamp', xnames)
  if(length(misscol) > 0) {
    stop(sprintf('columns missing from data set: %s', paste(misscol, collapse = ', ')))
  }
  if('.id' %notin% xnames) {
    # create a dummy ID
    df[['.id']] <- 1
    warning(".id unspecified - dummy ID created")
  }

  df[,'TimeStamp'] <- convertTZ(createTime(df[,'TimeStamp'], ...))
  df[,'day'] <- format(df[,'TimeStamp'], format = "%m-%d")

  # Split by ID
  df$.id <- factor(df$.id, levels=unique(df$.id))
  dat <- split(df, df$.id)

  # Index days numerically
  dayIndex <- function(df) {
    uday <- unique(df[,'day'])
    df[,'day'] <- match(df[,'day'], uday)
    df
  }
  dat <- lapply(dat, dayIndex)

  #Extract day proportion
  propDay <- function(df) {
    df$propDay <- df$day/max(df$day)
    return(df)
  }
  dat <- lapply(dat, propDay)

  #Collapse list of dataframes into large dataframe and create .id+day index
  orthdf  <- l2df(dat, keepDT = TRUE)
  uids <- unique(orthdf[['.id']])
  ids <- seq_along(uids)
  ix <- match(orthdf[['.id']], uids)
  orthdf[['ID']] <- ids[ix] 
  id_day <- paste0(orthdf[['ID']], "_", orthdf[['day']])
  orthdf[['ID_day']] <- factor(id_day, levels=unique(id_day)) #Makes split keep order
  orthdf
}
