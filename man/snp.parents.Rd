\name{snp.parents}
\alias{snp.parents}
\docType{data}
\title{SNP raw data for parents.}
\description{
SNP raw data for parents used for genotype quality. See affy.code.
}
\usage{data(snp.parents)}
\format{
  A data frame with 4853 observations on the following 19 variables.
  \describe{
    \item{\code{Assay.Id}}{a numeric vector}
    \item{\code{External.Id}}{external \code{rs} number for SNP}
    \item{\code{Target.Allele}}{a factor with levels \code{A/C} \code{A/G} \code{A/T} \code{C/G} \code{C/T} \code{G/T}}
    \item{\code{Genome.Map}}{a factor with levels \code{Mouse NCBI Build 32}}
    \item{\code{Chrom.Name}}{an ordered factor with levels \code{1} < \code{2} < \code{3} < \code{4} < \code{5} < \code{6} < \code{7} < \code{8} < \code{9} < \code{10} < \code{11} < \code{12} < \code{13} < \code{14} < \code{15} < \code{16} < \code{17} < \code{18} < \code{19} < \code{X}}
    \item{\code{Chrom.Position}}{a numeric vector}
    \item{\code{Gene}}{a logical vector}
    \item{\code{Assay.Call.Rate..}}{a numeric vector}
    \item{\code{tid28437_Mouse2272_B6_Male}}{a numeric vector}
    \item{\code{tid28438_Mouse2273_B6_Male}}{a numeric vector}
    \item{\code{tid28426_Mouse2031_B6_Female}}{a numeric vector}
    \item{\code{tid28421_Mouse2000_BTBR_Male}}{a numeric vector}
    \item{\code{tid28424_Mouse2012_BTBR_Female}}{a numeric vector}
    \item{\code{tid28395_Mouse133_F1_Female}}{a numeric vector}
    \item{\code{tid28398_Mouse136_F1_Male}}{a numeric vector}
    \item{\code{tid28422_Mouse2001_BTBR_Male}}{a numeric vector}
    \item{\code{tid28423_Mouse2011_BTBR_Female}}{a numeric vector}
    \item{\code{tid28396_Mouse134_F1_Female}}{a numeric vector}
    \item{\code{tid28397_Mouse135_F1_Male}}{a numeric vector}
  }
}
\seealso{ \code{\link{affy.code}}
}
\examples{
data(snp.parents)
dim(snp.parents)
}
\keyword{datasets}
