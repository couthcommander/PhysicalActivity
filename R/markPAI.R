#' Mark Physical Activity Intensity (PAI) Level
#'
#' This function adds a physical activity intensity level variable to the
#' source dataset.  "pai" is an ordered factor variable.  It will be NA for
#' nonwear times.
#'
#' @param data Data with classified wear (nonwear) status by
#' \code{\link{wearingMarking}}.
#' @param cts The name of the counts column. The default is "axis1".
#' @param markingString Option for summarizing wear (markingString = "w") or
#' nonwear time (markingString = "nw").
#' @param breaks A numeric vector of cut-points. The default cut-points are based on Matthews et al. (2016).
#' @param labels A character vector labelling intensity levels.
#'
#' @return A dataframe with an additional pai-level column.
#'
#' @references Matthews CE, Keadle SK, Troiano RP, Kahle L, Koster A, Brychta R, 
#' Van Domelen D, Caserotti P, Chen KY, Harris TB, Berrigan D. 
#' Accelerometer-measured dose-response for physical activity, sedentary time, and mortality in US adults.
#' Am J Clin Nutr. 2016 Nov;104(5):1424-1432.
#' 
#' @examples
#' data(dataSec)
#'
#' mydata1m = dataCollapser(dataSec, TS = "TimeStamp", col = "counts", by = 60)
#'
#' data1m = wearingMarking(dataset = mydata1m,
#'                        perMinuteCts = 1,
#'                        cts = "counts")
#'
#' markPAI(data = data1m, cts = 'counts')[1:10,]
#' @export

markPAI <- function(data, cts = getOption('pa.cts'), markingString = "w",
        breaks = c(-Inf, 100, 760, 2020, Inf), 
        labels = c('sedentary', 'light', 'moderate', 'vigorous')) {
    stopifnot(length(breaks) == length(labels) + 1)
    ix <- data[, "wearing"] == markingString
    data[ix,'pai'] <- cut(data[ix, cts], breaks, labels,
                right = FALSE, ordered_result = TRUE)
    data
}
