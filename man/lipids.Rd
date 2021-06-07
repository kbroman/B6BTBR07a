\name{lipids}
\alias{lipids}
\docType{data}
\title{CPL_Rosetta_Lipids_FINAL}
\description{
Final lipids from Rosetta.
}
\usage{data(lipids)}
\format{
  A data frame with 604 observations on the following 42 variables.
}
\describe{
  \item{\code{Mouse}}{mouse ID coded as
    \code{tidnnnnn_Mousemmmm_ST_Sex}}
  \item{\code{CPL.Tube.}}{tube number}
  \item{\code{Box..}}{box number}
  \item{\code{Location.in.Box}}{location in box (1-81)}
  \item{\code{CPL.Tracking.Number}}{internal number}
  \item{\code{Dilution...Comment}}{ignore}
  \item{\code{NEFA.mEq.L}}{NEFA in milliequivalents per liter}
  \item{\code{LDL.mg.dL}}{LDL in milligrams per deciliter}
  \item{\code{HDL.mg.dL}}{HDL in milligrams per deciliter}
  \item{\code{Total.CHOL..mg.dL}}{total cholesterol in milligrams per deciliter}
  \item{\code{Unit.of.measure}}{unit of measure}
}
\details{
  Lipids report supplied by Rosetta via Mark Keller.
}
\seealso{
  \code{\link{qtl.code}}
}
\examples{
## lipids <- read.csv("data/CPL_Rosetta_Lipids_Intranet.csv", header = TRUE)

data(lipids)
dim(lipids)
}
\keyword{datasets}
