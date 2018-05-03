#' Define Days Within Window
#'
#' @keywords internal
validWindow <- function(wearTime, cnt, trim = TRUE) {
    vt <- wearTime >= cnt
    window <- NA
    vtl <- length(vt)
    # remove first two and last two days
    if(trim && vtl > 10) {
        vt[c(1,2,vtl-1,vtl)] <- FALSE
    }
    # select window if there are at least 7 days
    if(vtl > 6) {
        cnts <- numeric(vtl-6)
        # calculate number of valid days for each 7-day sequence
        for(i in seq(1, vtl-6)) {
            cnts[i] <- sum(vt[seq(i, i+6)])
        }
        # select largest sequence
        window <- seq(which.max(cnts), length.out=7)
    } else {
        window <- seq(vtl)
    }
    # only return valid days coming from 7-day window
    daylist <- names(which(vt[window]))
    if(length(daylist) == 0) {
        daylist <- NULL
    }
    daylist
}

#' Delivery Count Threshold
#'
#' @keywords internal
deliveryThreshold <- function(data, daylist, cts = getOption('pa.cts'),
                              dist = c("t", "normal"),
                              stat = c("mean", "sd", "95")) {
    dist <- match.arg(dist)
    stat <- match.arg(stat)
    dd <- data[data[, "days"] %in% daylist,]
    # compute summary stat for each day
    if(stat == '95') {
        x <- tapply(dd[, cts], dd[, "days"], quantile, probs=0.95)
    } else {
        x <- tapply(dd[, cts], dd[, "days"], stat)
    }
    n <- length(x)
    # calculate lower-bound of confidence interval
    # negative values are possible (often SSS),
    # which prevents `delivery` classification
    if(n > 1) {
        crit <- switch(dist,
            t = qt(0.025, df = n - 1),
            normal = qnorm(0.025)
        )
        lb.window <- mean(x) + crit * sd(x) / sqrt(n)
    } else {
        lb.window <- 0
    }
    lb.window
}

#' Classify Mail Delivery and Non-Delivery Days for Accelerometer Data  
#'
#' This function adds an indicator variable for accelerometer delivery days based
#' on a delivery classification algorithm. The algorithm classifies each day as 
#' delivery or non-delivery day within each participant data using summary 
#' statistics of accelerometer counts for each day. As the summary statistics,
#' the 95th percentile, mean and standard deviation (sd) of accelerometer counts
#' can be used. Using the summary statistics for each day, the algorithm defines
#' a set of days that are used to estimate the 95\% confidence interval (CI)
#' based on t-distribution (default) or normal distribution. The lower bound of
#' the 95\%  CI is used to classify delivery days; if the summary statistics 
#' for a day is below the lower bound of the 95\% CI, this day is classified 
#' as delivery day. Three methods for defining a set of days are available: 
#' \code{trim} (default), \code{consecutive}, and \code{valid}.

#' @param data Data with classified wear (nonwear) status by
#' \code{\link{wearingMarking}}.
#' @param cts The name of the counts column. The default is \dQuote{axis1}.
#' @param markingString Option for summarizing wear (markingString = \dQuote{w}) or
#' nonwear time (markingString = \dQuote{nw}).
#' @param window A character. It should be one of \sQuote{trim}, \sQuote{consecutive}, 
#' or \sQuote{valid}.
#' @param method A character. It should be one of \sQuote{95}, \sQuote{mean} or \sQuote{sd}.
#' @param validCut A cutoff for the total minutes of classified monitor wear
#' time per day to be considered as a valid monitor day.
#' @param wearThreshold A numeric value specifying a pseudo-valid day cutoff
#' similar to \dQuote{validCut}, which is used to define a set of days to estimate
#' the 95\% CI.
#' @param dist Option for distribution used to calculate the 95\% CI.
#'
#' @return A data frame with summary information about daily counts.
#'
#' @templateVar author colechoi
#' @template auth
#'
#' @examples
#' data(deliveryData)
#'
#' options(pa.cts = "vm")
#' wm <- wearingMarking(dataset = deliveryData)
#'
#' markDelivery(wm)
#' plotData(data=wm) # days 1, 2, 10 - 15 are delivery or invalid days based on the result above
#' markDelivery(wm, window='valid', method='mean')
#' markDelivery(wm, method='mean')
#' markDelivery(wm, method='sd')
#' @export

markDelivery <- function(data, cts = getOption('pa.cts'), markingString = "w",
        window = c('trim', 'consecutive', 'valid'),
        method = c('95', 'mean', 'sd'), validCut = getOption('pa.validCut'), 
        wearThreshold = 300, dist = c("t", "normal")) {
    window <- match.arg(window)
    method <- match.arg(method)
    dist <- match.arg(dist)

    alldays <- sort(as.numeric(unique(data[, "days"])))
    wearTime <- numeric(length(alldays))
    names(wearTime) <- alldays
    wt <- sumVct(data, markingString = markingString)
    wearTimeByDay <- tapply(wt[,'duration'], wt[,'days'], sum, na.rm=TRUE)
    wearTime[names(wearTimeByDay)] <- wearTimeByDay
    daylist <- names(which(wearTime >= validCut))
    # data marked as "wearing"
    ddAll <- data[data[, "wearing"] == markingString,]

    wind <- switch(window,
        consecutive = validWindow(wearTime, cnt = wearThreshold, trim = FALSE),
        trim = validWindow(wearTime, cnt = wearThreshold, trim = TRUE),
        valid = daylist
    )
    if(length(wind) == 0) return(NULL)

    # define lower-bound window
    lb <- deliveryThreshold(ddAll, wind, cts, dist=dist, stat=method)

    probs <- c(95,50,5) / 100
    x.mu <- tapply(ddAll[, cts], ddAll[, "days"], 'mean')
    x.sd <- tapply(ddAll[, cts], ddAll[, "days"], 'sd')
    x.q <- do.call(rbind,
                   tapply(ddAll[, cts], ddAll[, "days"], quantile, probs=probs)
    )
    nzDay <- names(x.mu)

    val <- switch(method,
        mean = x.mu,
        sd = x.sd,
        x.q[,'95%'] # default value
    )
    # indicate delivery date
    delivery <- val < lb

    res <- data.frame(day = alldays)
    resA <- cbind(res, wearTime=wearTime[match(names(wearTime), res[,'day'])])
    resB <- cbind(cnt.mean=x.mu, cnt.sd=x.sd, x.q, delivery)
    resA[, colnames(resB)] <- NA
    ix <- match(nzDay, res[,'day'])
    resA[ix, colnames(resB)] <- resB
    resA
}
