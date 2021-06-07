\name{snp.record}
\alias{snp.record}
\docType{data}
\title{ ~~ data name/kind ... ~~}
\description{
  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(snp.record)}
\format{
  A data frame with 4853 observations on the following 38 variables.
  \describe{
    \item{\code{Chrom.Name}}{an ordered factor with levels \code{1} < \code{2} < \code{3} < \code{4} < \code{5} < \code{6} < \code{7} < \code{8} < \code{9} < \code{10} < \code{11} < \code{12} < \code{13} < \code{14} < \code{15} < \code{16} < \code{17} < \code{18} < \code{19} < \code{X}}
    \item{\code{Chrom.Position}}{a numeric vector}
    \item{\code{Assay.Id}}{a numeric vector}
    \item{\code{External.Id}}{external \code{rs} number for SNP (Build 32)}
    \item{\code{Assay.Call.Rate..}}{a numeric vector}
    \item{\code{Target.Allele}}{a factor with levels \code{A/C} \code{A/G} \code{A/T} \code{C/G} \code{C/T} \code{G/T}}
    \item{\code{Keep}}{a logical vector}
    \item{\code{B6}}{a factor with levels \code{A} \code{C} \code{G} \code{T}}
    \item{\code{BTBR}}{a factor with levels \code{A} \code{C} \code{G} \code{T}}
    \item{\code{Segregating}}{a logical vector}
    \item{\code{HetParents}}{a logical vector}
    \item{\code{NonSegMismatch}}{a logical vector}
    \item{\code{SegMismatch}}{a logical vector}
    \item{\code{B6Match}}{a logical vector}
    \item{\code{BTBRMatch}}{a logical vector}
    \item{\code{F1Match}}{a logical vector}
    \item{\code{BadX}}{a numeric vector}
    \item{\code{n.miss}}{a numeric vector}
    \item{\code{n.AA}}{a numeric vector}
    \item{\code{n.AB}}{a numeric vector}
    \item{\code{n.BB}}{a numeric vector}
    \item{\code{SegDisorder.logP}}{a numeric vector}
    \item{\code{Build36.Chrom}}{Chromosome name as 1:19 or X.}
    \item{\code{Build36.Position}}{a numeric vector}
    \item{\code{Build36.SNP}}{external \code{rs} number for SNP (Build 36)}
    \item{\code{Build36.SNPmerge}}{a logical vector}
    \item{\code{Build36.Alleles}}{a factor with levels \code{A/C} \code{A/C/G} \code{A/G} \code{A/G/T} \code{A/T} \code{C/G} \code{C/G/T} \code{C/T} \code{G/T}}
    \item{\code{Build36.Allele.Change}}{a logical vector}
    \item{\code{Build36.Assembly}}{a factor with levels \code{C57BL/6J} \code{Celera} \code{build.32}}
    \item{\code{Build36.SegDisSolo}}{a numeric vector}
    \item{\code{CO}}{a numeric vector}
    \item{\code{DC}}{a numeric vector}
    \item{\code{COL}}{a numeric vector}
    \item{\code{COR}}{a numeric vector}
    \item{\code{DC1}}{a numeric vector}
    \item{\code{DCL}}{a numeric vector}
    \item{\code{DCR}}{a numeric vector}
    \item{\code{DD}}{a numeric vector}
  }
}
\seealso{ \code{\link{affy.code}}, \code{\link{mouse.record}}
}
\examples{
data(snp.record)
dim(snp.record)
}
\keyword{datasets}
