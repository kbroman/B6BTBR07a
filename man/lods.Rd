\name{B6BTBR07.lods}
\alias{B6BTBR07.lods}
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

## Calculate genotype probabilities.
B6BTBR07 <- calc.genoprob(B6BTBR07, step = 2, stepwidth = "variable")

## Run through all traits.
B6BTBR07.scanone.clinical <- aug.scanone(keep, cross = B6BTBR07,
  normal.scores = TRUE)
write.csv(t(B6BTBR07.scanone.clinical[,-(1:2)]), file = "clinicallod.csv",
          quote = FALSE)
## Males
B6BTBR07.scanone.clinicalM <- aug.scanone(keep, cross = B6BTBR07,
  normal.scores = TRUE, sex = "male")
write.csv(t(B6BTBR07.scanone.clinicalM[,-(1:2)]), file = "clinicallodM.csv",
          quote = FALSE)
## Females
B6BTBR07.scanone.clinicalF <- aug.scanone(keep, cross = B6BTBR07,
  normal.scores = TRUE, sex = "female")
write.csv(t(B6BTBR07.scanone.clinicalF[,-(1:2)]), file = "clinicallodF.csv",
          quote = FALSE)
}
}
\keyword{datasets}
