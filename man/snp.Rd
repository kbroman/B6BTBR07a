\name{snp}
\alias{snp}
\docType{data}
\title{Raw genotype data for B6BTBR07 cross}
\description{
Spreadsheet of genotype data directly from Affymetrix.
}
\usage{data(snp)}
\format{
  A data frame with 4853 observations on the following 574 variables.
  \describe{
    \item{\code{Assay.Id}}{Internal Affymetrix ID.}
    \item{\code{External.Id}}{SNP universal \code{rs} number.}
    \item{\code{Target.Allele}}{Target allele pair from \code{A/C},
      \code{A/G}, \code{A/T}, \code{C/G}, \code{C/T}, \code{G/T}.} 
    \item{\code{Genome.Map}}{Character string denoting \code{Mouse NCBI Build 32}.}
    \item{\code{Chrom.Name}}{Chromosome name: 1 to 19 or X}
    \item{\code{Chrom.Position}}{Chromosome position in Mb (build 32).}
    \item{\code{Gene}}{All values are \code{NA}.}
    \item{\code{Assay.Call.Rate..}}{Technical QC summary (percent of
      mice genotyped}
    \item{\dots}{SNP calls by mouse. See details.}
  }
}
\details{
Mice are identified as a chracter string consisting of \code{tidnnnnn},
\code{Mousemmmm}, \code{Strain}, \code{Sex} separated by underscores, with \code{nnnnn} the
\code{Assay.Id}, \code{mmmm} the mouse ID (from Attie lab records),
\code{Strain} being one of \code{B6}, \code{BTBR}, \code{F1} or
\code{F2}, and \code{Sex} either \code{Male} or \code{Female}.

Consider \code{Target.Allele} being \code{A/C}. The SNP calls are 0
(first of target alleles: \code{A}), 1 (both/heterozygote), 2 (second
allele: \code{C}, or 5 (missing value).
}
\seealso{ \code{\link{affy.code}}
}
\examples{
data(snp)
dim(snp)
}
\keyword{datasets}
