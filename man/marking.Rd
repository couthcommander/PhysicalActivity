\name{marking}
\alias{marking}
\title{Marking Data with Wearing Tags}
\description{
  This function marks the dataset with wearing/non-wearing tags.
}
\usage{
marking(dataset, frame, cts = "counts", streamFrame = NULL, allowanceFrame = 2, newcolname = "wearing")
}
\arguments{
  \item{dataset}{the source dataset, in dataframe format, which needs to be marked.}
  \item{frame}{the size of time interval to be considered.}
  \item{cts}{the name of the counts column. The default is "counts".}
  \item{streamFrame}{the size of time interval that the program will look back or forward if activity is detected.
                      The default is the half of frame.}
  \item{allowanceFrame}{the size of time interval that zero counts allowed.  The default is 2.}
  \item{newcolname}{the wearing marking column name.  The default is "wearing".  After the data is processed, a new field will 
                     be added to the original dataframe.  This new field is the wearing /nowwearing indicator.}
}

\value{
  a dataframe with an extra wearing/non-wearing marking column.
}

\references{Choi, L., Liu, Z., Matthews, C. E. and Buchowski, M. S.  (2010). Validation of Accelerometer Wear and Nonwear Time Classification Algorithm. Med Sci Sports Exerc. Jun 23 [Epub ahead of print].}

\author{Zhouwen Liu <zhouwen.liu@Vanderbilt.Edu>}

\keyword{internal}
