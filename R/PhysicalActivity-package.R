#' Process Physical Activity Accelerometer Data
#'
#' This package contains functions to classify monitor wear and nonwear time
#' intervals in accelerometer data collected to assess physical activity in
#' free-living condition. The package also contains functions to make plot for
#' accelerometer data, and to obtain the summary of daily monitor wear time and
#' the mean of monitor wear time during valid days. A monitored day is
#' considered valid if the total minutes of classified monitor wear time per day
#' is greater than a user defined cutoff.
#'
#' Classify wear and nonwear time status for accelerometer data by
#' epoch-by-epoch basis by \code{\link{wearingMarking}}.
#'
#' @docType package
#'
#' @author Leena Choi \email{leena.choi@@Vanderbilt.Edu},
#' Cole Beck \email{cole.beck@vumc.org},
#' Zhouwen Liu \email{zhouwen.liu@@Vanderbilt.Edu},
#' Charles E. Matthews \email{Charles.Matthews2@@nih.gov}, and
#' Maciej S. Buchowski \email{maciej.buchowski@@Vanderbilt.Edu}
#'
#' Maintainer: Leena Choi \email{leena.choi@@Vanderbilt.Edu}
#'
#' @references Choi L, Liu Z, Matthews CE, Buchowski MS. 
#' Assessment of wear/nonwear time classification algorithms for triaxial accelerometer.
#' Med Sci Sports Exerc. 2011 Feb;43(2):357-64.
#'
#' Choi L, Ward SC, Schnelle JF, Buchowski MS. 
#' Assessment of wear/nonwear time classification algorithms for triaxial accelerometer.
#' Med Sci Sports Exerc. 2012 Oct;44(10):2009-16.
#'
#' Choi L, Chen KY, Acra SA, Buchowski MS. 
#' Distributed lag and spline modeling for predicting energy expenditure from 
#' accelerometry in youth. J Appl Physiol. 2010 Feb;108(2):314-27.
#'
#' @keywords accelerometer nonwear process
#' @importFrom graphics abline plot text mtext
#' @importFrom stats pt sd qnorm qt quantile
#'
#' @examples
#' data(dataSec)
#'
#' mydata1m = dataCollapser(dataSec, TS = "TimeStamp", col = "counts", by = 60)
#' options(pa.cts = 'counts') # change cnt variable from "axis1" to "counts"
#' data1m = wearingMarking(dataset = mydata1m, frame = 90)
#'
#' sumVct(data1m, id="sdata1m")
#'
#' plotData(data=data1m)
#'
#' summaryData(data=data1m, validCut=600, perMinuteCts=1, markingString = "w")
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
    myopts <- list(
        pa.validCut = 600,
        pa.timeStamp = "TimeStamp",
        pa.cts = "axis1"
    )
    options(myopts)
    invisible()
}
