\name{dataSec}
\alias{dataSec}
\docType{data}
\title{Accelerometer Data Example}
\description{
Approximately 3 days of accelerometer data collected with 1-sec epoch in the correct data format that can be used by \code{wearingMarking} to classify wear and nonwear time.
}
\usage{data(dataSec)}
\format{
  A data frame with 238140 observations on the following 2 required variables.
  \describe{
    \item{\code{TimeStamp}}{a character vector}
    \item{\code{counts}}{a numeric vector}
  }
}

\references{Choi, L., Liu, Z., Matthews, C. E. and Buchowski, M. S.  (2010). Validation of Accelerometer Wear and Nonwear Time Classification Algorithm. Med Sci Sports Exerc. Jun 23 [Epub ahead of print].}

\examples{
data(dataSec)
}
\keyword{datasets}
