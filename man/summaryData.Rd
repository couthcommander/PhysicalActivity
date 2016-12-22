\name{summaryData}
\alias{summaryData}
\title{
Summarize Classified Wear Time by Daily Basis
}
\description{
This function summarizes accelerometer data and classified wear or nonwear time by daily basis.}
\usage{
summaryData(data, validCut = 600, perMinuteCts = 1, markingString = "w")
}
\arguments{
  \item{data}{
data with classified wear (nonwear) status by \code{\link{wearingMarking}}.}
  \item{validCut}{
a cutoff for the total minutes of classified monitor wear time per day to be considered as a valid monitor day.}
  \item{perMinuteCts}{the number of data rows per minute. The default is 1-min epoch (perMinuteCts = 1) and we recommend to use 1-min epoch data for this summary. For examples: for data with 10-sec epoch, set perMinuteCts = 6; for data with 1-sec epoch, set perMinuteCts = 60.}
  \item{markingString}{option for summarizing wear (markingString = "w") or nonwear time (markingString = "nw").}
}
\details{
This function summarizes the total number of days, weekdays and weekends in accelerometer data. Also this function provides the total number of valid days, valid weekdays and valid weekend days based on a user defined cutoff for the total minutes of classified monitor wear time per day. This function also summarizes the classified wear (nonwear) time by day and by valid day, and the mean wear (nonwear) time for valid days during weekday and weekends, and for overall valid days.}

\value{
\item{unit}{epoch for data.}
\item{totalNumDays}{the total number of days in accelerometer data.}
\item{totalNumWeekWeekend}{the total number of weekdays and weekend days in accelerometer data.}
\item{validCut}{a user defined cutoff for the total minutes of classified monitor wear time per day to be considered as a valid monitor day.}
\item{totalValidNumDays}{the total number of valid days based on the user defined cutoff for total minutes of wearing and the classified wearing time.}
\item{totalValidNumWeekWeekend}{the total number of valid weekdays and valid weekend days based on a user defined cutoff for the total minutes of classified monitor wear time per day.}
\item{wearTimeByDay}{the classified total wear (nonwear) time by day.}
\item{validWearTimeByDay}{the classified total wear (nonwear) time by valid day.}
\item{meanWeartimeValidDays}{the mean wear (nonwear) time for valid days during weekdays and weekends.}
\item{meanWeartimeOverallValidDays}{the mean wear (nonwear) time for overall valid days.}
}

\references{Choi, L., Liu, Z., Matthews, C.E. and Buchowski, M.S.  (2010). Validation of Accelerometer Wear and Nonwear Time Classification Algorithm. Med Sci Sports Exerc. Jun 23 [Epub ahead of print].}

\author{
Leena Choi <leena.choi@Vanderbilt.Edu>
}

\seealso{\code{\link{wearingMarking}}, \code{\link{sumVct}}
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

summaryData(data=data1m, validCut=600, perMinuteCts=1, markingString = "w")
}
