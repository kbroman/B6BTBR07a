\name{B6BTBR07.bims}
\alias{B6BTBR07.bims}
\docType{data}
\title{Import permutation results for B6BTBR07 by sex.}
\seealso{\code{\link{B6BTBR07}},
  \code{\link[qtlview]{aug.scanone}}
  \code{\link[qtl]{scanone}}
}
\examples{
\dontrun{
## Identify clinical phenotypes to map.
keep <- mytrait(, B6BTBR07$pheno)
keep <- keep[-(1:4)]
keep <- keep[-grep("date", tolower(keep))]
keep <- keep[-grep("id$", tolower(keep))]
keep <- keep[-match("IL.3..Interleukin.3.",keep)]
keep <- keep[-match("Array",keep)]
keep <- keep[-grep("^DC[12].",keep)]
keep <- keep[-grep("Keep",keep)]
keep <- keep[-grep("batch",keep)]

table(sapply(B6BTBR07$pheno[,keep], class))

## Calculate genotype probabilities for qtlbim.
B6BTBR07 <- qb.genoprob(B6BTBR07, step = 2, stepwidth = "variable")

## Both sexes.
qb.scanone.clinical <- aug.scanone(keep, cross.name = B6BTBR07,
   method = "LPD")
write.csv("qb.scanone.clinical", file = "clinicalLPD.csv", 
          quote = FALSE)
}
}
\keyword{datasets}
