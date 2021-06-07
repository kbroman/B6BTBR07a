\name{tissue.batch}
\alias{tissue.batch}
\docType{data}
\title{Batch for microarray processing.}
\description{
Batch signifies date of microarray processing. Dates with only a few
mice are recoded as "Other".
}
\usage{data(tissue.batch)}
\format{
A data frame with 541 mice and columns for MouseNum and batch identifiers.
}
\describe{
  \item{\code{MouseNum}}{mouse number coded as
    \code{Mousemmmm}}
  \item{\code{islet.batch}}{factor with 3 levels}
}
\examples{
\dontrun{
## http://attie.wisc.edu/lab/wfm/wfsh.php?dir=123465/B6BTBR07/batch.info
tmpfn <- function(tissue.name = "islet",
                  data.dir = "~/Rlib/eqtl/data/Rosetta")
{
  if(tissue.name == "miRNA")
    filename <- file.path(data.dir, "miRNA.batch.csv")
  else
    filename <- file.path(data.dir, paste("F2", tissue.name, "hybsex.csv",
                                          sep = "."))
  new.batch <- read.csv(filename, header = TRUE, row.names = 1)
  tmp2 <- rep(NA, nrow(tissue.batch))
  if(tissue.name == "miRNA") {
    tmp <- match(tissue.batch$MouseNum, row.names(new.batch),
      nomatch = 0)
    tmp2[tmp > 0] <- as.character(new.batch$miRNA.batch)[tmp]
  }
  else {
    tmp <- match(tissue.batch$MouseNum, as.character(new.batch$MouseNum),
      nomatch = 0)
    tmp2[tmp > 0] <- as.character(new.batch$Hyb.Date2)[tmp]
  }
  factor(tmp2)
}
tissue.batch <- data.frame(MouseNum = B6BTBR07$pheno$MouseNum)
tissue.batch$islet.batch <- tmpfn("islet")
tissue.batch$liver.batch <- tmpfn("liver")
tissue.batch$hypo.batch <- tmpfn("hypo")
tissue.batch$adipose.batch <- tmpfn("adipose")
tissue.batch$gastroc.batch <- tmpfn("gastroc")
tissue.batch$kidney.batch <- tmpfn("kidney")
tissue.batch$miRNA.batch <- tmpfn("miRNA")
row.names(tissue.batch) <- NULL
package.dir <- "~/Rlib/B6BTBR07a/data"
save("tissue.batch", file = file.path(package.dir, "tissue.batch.RData"),
     compress = TRUE)
}

data(tissue.batch)
table(tissue.batch$islet)
table(tissue.batch$islet, tissue.batch$liver)
}
\keyword{datasets}
