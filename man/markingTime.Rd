\name{markingTime}
\alias{markingTime}

\title{Mark Days}
\description{
  This function adds a "day" variable to the source dataset. The day is marked in numeric order, according to the timestamp variable.
}
\usage{
markingTime(dataset, timestamp, startTime = "00:00:00", endTime = "23:59:59")
}

\arguments{
  \item{dataset}{the source dataset, in dataframe format, that needs to be marked.}
  \item{timestamp}{the column name in the dataset that will be used as timestamp.}
  \item{startTime}{define the starting time of a day.  It must be in the format of "hh:mm:ss".}
  \item{endTime}{define the ending time of a day.  It must be in the format of "hh:mm:ss".}
}

\value{
  a dataframe with an extra day marking column.
}
\references{Choi, L., Liu, Z., Matthews, C. E. and Buchowski, M. S.  (2010). Validation of Accelerometer Wear and Nonwear Time Classification Algorithm. Med Sci Sports Exerc. Jun 23 [Epub ahead of print].}

\author{Zhouwen Liu <zhouwen.liu@Vanderbilt.Edu>}
