\name{dataCollapser}
\alias{dataCollapser}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Collapse Accelerometer Data to a Dataset with a Longer Epoch}
\description{
The function collapses counts in data collected with a short epoch to make a data set with a longer epoch. For example, this function collapses data with 1-sec epoch to 10-sec epoch or 1-min epoch data.
}

\usage{
dataCollapser(dataset, TS, by, col, func = sum, ...)
}

\arguments{
  \item{dataset}{the source dataset, in dataframe format, that needs to be collapsed.}
  \item{TS}{the column name for timestamp.}
  \item{by}{epoch in seconds for a collapsed dataset. For example, to collapse second data to minute data, set by = 60; to collapse 10-second data to minute data, set by = 60.}
  \item{col}{the column name for counts.}
  \item{func}{a method for collapsing counts. The default is the summation of counts.}
  \item{\dots}{argument settings that to be used by user-defined "func" setting.}
}
\value{
a collapsed data with user specified epoch.
}

\references{Choi, L., Liu, Z., Matthews, C.E. and Buchowski, M.S.  (2010). Validation of Accelerometer Wear and Nonwear Time Classification Algorithm. Med Sci Sports Exerc. Jun 23 [Epub ahead of print].}

\author{Zhouwen Liu <zhouwen.liu@Vanderbilt.Edu>}

\examples{

data(dataSec)

## collapse 1-sec epoch data to 10-sec epoch data
mydata10s = dataCollapser(dataSec, TS = "TimeStamp", col = "counts", by = 10)

## collapse 1-sec epoch data to 1-min epoch data
mydata1m = dataCollapser(dataSec, TS = "TimeStamp", col = "counts", by = 60)
}
