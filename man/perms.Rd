\name{B6BTBR07.perms}
\alias{B6BTBR07.perms}
\docType{data}
\title{Import permutation results for B6BTBR07 by sex.}
\seealso{\code{\link{B6BTBR07}},
  \code{\link[qtlview]{read.perms}}
  \code{\link[qtl]{scanone}}
}
\examples{
## Run permutations using R/qtl's scanone.
## Actually, we used simulated normal to run this,
## keeping markers and sample size the same.
## See ~yandell/p/private/diabetes3/diabetes30/LOD/miRNA
## transcripts (mRNA) in liver for 290 mice (batch effects):
## private/diabetes3/diabetes30/LOD/miRNA/290liver/perm-batch-AX.R
## miRNA in liver for both sexes (includes amp effects):
## private/diabetes3/diabetes30/LOD/miRNA/perm-batch-AX2.R
## miRNA in liver for male/female (amp effects ignored):
## private/diabetes3/diabetes30/LOD/miRNA/perm-batch-female-AX.R
## private/diabetes3/diabetes30/LOD/miRNA/perm-batch-male-AX.R
## proteomic data:
## private/diabetes3/diabetes30/LOD/penalty/proteomic/perm-batch-AX.R

sexes <- c("both","female","male")

## Permutations for full data set.
data.dir <- "~/Rlib/eqtl/data/jmoon/LOD/penalty"
filenames <- file.path(data.dir,
  paste("penalty-batch-", sexes, "-AX.RData", sep = ""))
names(filenames) <- sexes
B6BTBR07.perms <- list()
for(i in sexes) {
  load(filenames[i])
  B6BTBR07.perms[[i]] <- AX
}
B6BTBR07.perms <- read.perms(B6BTBR07, perms = B6BTBR07.perms)

## Permutations for 290 mice (miRNA-specific).
data(B6BTBR07)
filenames <- file.path(data.dir, "miRNA",
  paste("threshold-miRNA-", sexes, ".csv", sep = ""))
names(filenames) <- sexes
B6BTBR07.miRNA.perms <- read.perms(B6BTBR07, filenames, row.names = 1)

## Permutations for proteomic data (males only on subset of mice).
filenames <- file.path(data.dir, "proteomic", "penalty-male-AX.RData")
load(filenames)
B6BTBR07.prot.perms <- read.perms(B6BTBR07, perms = list(male = AX))

\dontrun{
## Save perms files into package.
package.dir <- "~/Rlib/B6BTBR07a/data"
save("B6BTBR07.perms",
  file = file.path(package.dir, "B6BTBR07.perms.RData"),
  compress = TRUE)
save("B6BTBR07.miRNA.perms",
  file = file.path(package.dir, "B6BTBR07.miRNA.perms.RData"),
  compress = TRUE)
save("B6BTBR07.prot.perms",
  file = file.path(package.dir, "B6BTBR07.prot.perms.RData"),
  compress = TRUE)
}
}
\keyword{datasets}
