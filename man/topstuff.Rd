\name{topstuff}
\alias{topstuff}
\docType{data}
\title{ ~~ data name/kind ... ~~}
\description{
  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(topstuff)}
\format{
  A data frame with 566 observations on the following 6 variables.
  \describe{
    \item{\code{Experiment.Name}}{Experiment name.}
    \item{\code{Status.}}{a factor with levels \code{Auto Pass} \code{Fail} \code{Manual Pass}}
    \item{\code{Sample.Location.}}{Sample location.}
    \item{\code{Sample.Name.}}{Sample name.}
    \item{\code{Sample.Repeat.Count.}}{a numeric vector}
    \item{\code{Experiment.Call.Rate..}}{a numeric vector}
  }
}
\seealse{\code{\link{affy.code}}
}
\examples{
data(topstuff)
dim(topstuff)
}
\keyword{datasets}
