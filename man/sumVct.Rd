\name{sumVct}
\alias{sumVct}
\title{Summarize Wearing or Nonwearing Time Interval}
\description{
This function summarizes the classified wear (nonwear) time by interval basis from the epoch-by-epoch classified wear (nonwear) status classified by \code{\link{wearingMarking}}.}
\usage{
sumVct(datavct, wearing = "wearing", TS = "TimeStamp", markingString = "w", 
by = "days", id = NULL)
}
\arguments{
  \item{datavct}{data with classified wear (nonwear) status classified by \code{\link{wearingMarking}}.}
  \item{wearing}{the column name for classified wear and nonwear status. The default is "wearing".}
  \item{TS}{the column name for timestamp. The default is "TimeStamp".}  
  \item{markingString}{option for summarizing wear (markingString="w") or nonwear time interval (markingString="nw").}
  \item{by}{a sequence of days for classified wear (nonwear) time intervals.}
  \item{id}{optional output for subject identification or file name.}
}

\value{
the summary data for wearing or nonwearing time intervals.
}

\references{Choi, L., Liu, Z., Matthews, C. E. and Buchowski, M. S.  (2010). Validation of Accelerometer Wear and Nonwear Time Classification Algorithm. Med Sci Sports Exerc. Jun 23 [Epub ahead of print].}

\author{
Leena Choi <leena.choi@Vanderbilt.Edu>, Zhouwen Liu <zhouwen.liu@Vanderbilt.Edu>, Charles E. Matthews <Charles.Matthews2@nih.gov>, and Maciej S. Buchowski <maciej.buchowski@Vanderbilt.Edu>
}

\seealso{\code{\link{wearingMarking}}, \code{\link{summaryData}} 
}

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

sumVct(data1m, id="sdata1m", markingString = "nw")

}
