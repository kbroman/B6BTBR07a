\name{islet.count}
\alias{islet.count}
\docType{data}
\title{Number of islets harvested per F2}
\description{
Count of islets harvested at UW-Madison.
}
\usage{data(islet.count)}
\format{
  A data frame with 635 observations (mice) on the following 2 variables.
}
\describe{
  \item{\code{MouseID}}{mouse ID coded as
    \code{Mousemmmm}}
  \item{\code{harvested}}{number of islets harvested}
}
\details{
Supplied by Mary Rabaglia via Mark Keller.
}
\seealso{
  \code{\link{qtl.code}}
}
\examples{
\dontrun{
islet.count <- read.table("No islet harvested per F2.txt", header = TRUE)
names(islet.count)[2] <- "islet.count"
islet.count$sq.islet.count <- sqrt(islet.count$islet.count)

package.dir <- "~/Rlib/B6BTBR07a/data"
save("islet.count", file = file.path(package.dir, "islet.count.RData"),
     compress = TRUE)
}

data(islet.count)
myplot("sq.islet.count", newdata = islet.count)
}
\keyword{datasets}
