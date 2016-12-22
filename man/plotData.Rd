\name{plotData}
\alias{plotData}
\title{
Plot Accelerometer Data over Time
}
\description{
This function makes plot for accelerometer collected data (counts) over time for the whole monitor period, or a user specified time period or day with a midnight marking to separate monitored days.
}



\usage{
plotData(data, day = NULL, start = NULL, end = NULL)
}

\arguments{
  \item{data}{
data with classified wear and nonwear status from \code{\link{wearingMarking}}.
}
  \item{day}{
a part of data during a user specified day for plot.
}
  \item{start}{
define a starting time for plot.
}
  \item{end}{
define a ending time for plot.
}
}

\value{
plot with midnight marking.
}

\author{
Leena Choi <leena.choi@Vanderbilt.Edu>
}

\seealso{\code{\link{sumVct}}, \code{\link{wearingMarking}}
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

## plot the whole data
plotData(data=data1m)

## plot the data from 60 min to 900 min
plotData(data=data1m, start=60, end=900)

## plot the data for day 2
plotData(data=data1m, day=2)

}
