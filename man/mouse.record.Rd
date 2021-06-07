\name{mouse.record}
\alias{mouse.record}
\docType{data}
\title{Mouse-specific record of SNP quality for B6BTBR07 Intercross.}
\description{
See affy.code for details of construction.
}
\usage{data(mouse.record)}
\format{
  A data frame with 566 observations on the following 79 variables.
  \describe{
    \item{\code{AffyTID}}{Affymetrix ID.}
    \item{\code{MouseNum}}{Mouse number as \code{Mousennnn}.}
    \item{\code{Strain}}{Strain of mouse: \code{B6}, \code{BTBR},
      \code{F1}, \code{F2}, or \code{MouseKitControl}.}
    \item{\code{Sex}}{Sex as \code{755356} (control), \code{Female}, \code{Male}.}
    \item{\code{Array}}{Overall quality score for mouse: \code{1.Best},
      \code{2.BadX}, \code{3.LowCallRate}, \code{4.NonSegMismatch},
      \code{5.CageSex}, \code{6.BadGenos}, \code{7.NoGenos},
      \code{8.NotF2}.}
    \item{\code{QC.Priority}}{QC priority determined by UW team.}
    \item{\code{QC.Status}}{RNA QC status: \code{FAIL} or \code{PASS}.}
    \item{\code{Random.Proteomic}}{Random ordering for proteomic assay
      selection (first 120 males selected).}
    \item{\code{Proteomic.OK}}{Logical indication that mouse is OK for
      proteomic assay.}
    \item{\code{Plasma.ul}}{Amount of plasma stored to nearest 25ul.}
    \item{\code{Keep}}{Logical call to keep a mouse in F2 cross.}
    \item{\code{drop.repeats}}{Repeat call from Affymetrix.}
    \item{\code{drop.calls}}{Drop call from Affymetrix.}
    \item{\code{NonSegMismatch}}{Number of mismatches for
      non-segregating SNPs (problem if >50?)}
    \item{\code{BadX}}{Number of bad SNPs on X chromosome (problem if >3?).}
    \item{\code{CO}}{Number of crossovers (problem if >40).}
    \item{\code{DC1}}{Number of double crossovers of type 1 (problem if >5?).}
    \item{\code{DC2}}{Number of double crossovers of type 2 (problem if >5?).}
    \item{\code{DD}}{Number of other double crossovers on chromosome
      \code{chr} (problem if >5?).}
    \item{\code{CO.chr}}{Number of crossovers on chromosome \code{chr}.}
    \item{\code{DC1.chr}}{Number of double crossovers of type 1 on chromosome \code{chr}.}
    \item{\code{DC2.chr}}{Number of double crossovers of type 2 on chromosome \code{chr}.}
  }
}
\details{
This is a record of mouse genotype quality. See \code{\link{affy.code}} for
details of construction.
  
Mice with \code{QC.Priority} below 503 and \code{Array} values not
\code{5.CageSex} were used for gene expression arrays. Of these 500
mice, 120 males indicated by \code{Random.Proteomic} values below 1121
were sent for proteomic assay.
}
\seealso{\code{\link{affy.code}}, \code{\link{snp.record}}}
\examples{
data(mouse.record)
dim(mouse.record)
}
\keyword{datasets}
