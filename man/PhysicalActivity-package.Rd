\name{PhysicalActivity-package}
\alias{PhysicalActivity-package}
\alias{PhysicalActivity}
\docType{package}
\title{
Process Physical Activity Accelerometer Data
}
\description{
This package contains functions to classify monitor wear and nonwear time intervals in accelerometer data collected to assess physical activity in free-living condition. The package also contains functions to make plot for accelerometer data, and to obtain the summary of daily monitor wear time and the mean of monitor wear time during valid days. A monitored day is considered valid if the total minutes of classified monitor wear time per day is greater than a user defined cutoff.
}
\details{
Classify wear and nonwear time status for accelerometer data by epoch-by-epoch basis by \code{\link{wearingMarking}}.
}
\author{
Leena Choi <leena.choi@Vanderbilt.Edu>, Zhouwen Liu <zhouwen.liu@Vanderbilt.Edu>, Charles E. Matthews <Charles.Matthews2@nih.gov>, and Maciej S. Buchowski <maciej.buchowski@Vanderbilt.Edu>

Maintainer: Leena Choi <leena.choi@Vanderbilt.Edu>

}

\references{

Choi, L., Liu, Z., Matthews, C.E. and Buchowski, M.S.  (2010). Validation of Accelerometer Wear and Nonwear Time Classification Algorithm. Med Sci Sports Exerc. Jun 23 [Epub ahead of print].

Choi, L., Chen, K.Y., Acra, S.A. and Buchowski, M.S.  (2010). Distributed Lag and Spline Modeling for Predicting Energy Expenditure from Accelerometry in Youth. J Appl Physiol. 108(2):314-27.

}

\keyword{accelerometer}
\keyword{nonwear}
\keyword{process}

\examples{

data(dataSec)

mydata1m = dataCollapser(dataSec, TS = "TimeStamp", col = "counts", by = 60)
data1m = wearingMarking(dataset = mydata1m,
                       frame = 90, 
                       perMinuteCts = 1,
                       TS = "TimeStamp",
                       cts = "counts", 
                       streamFrame = NULL, 
                       allowanceFrame= 2, 
                       newcolname = "wearing")

sumVct(data1m, id="sdata1m")

plotData(data=data1m)

summaryData(data=data1m, validCut=600, perMinuteCts=1, markingString = "w")

}
