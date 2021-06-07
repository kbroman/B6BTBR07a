\name{miRNA}
\alias{miRNA}
\alias{miRNA.lod}
\alias{miRNA.pattern}
\title{miRNA data and genome scans}
\description{
miRNA data on 300 mice with ~250 miRNA elements. Some mice are parents
or F1.
}
\usage{
data(miRNA)
data(miRNA.lod)
data(miRNA.pattern)
}
\format{
  \code{miRNA} is a list with elements
  \describe{
    \item{a_gene_id}{Unique gene ID}
    \item{n.qtl}{Mean number of QTL on \code{chrom}}
    \item{chrom}{Chromosome (may be split into segments with dot)}
    \item{locus}{Locus position in cM}
    \item{variance}{Variance contribution of QTL}
  }
  \code{islet.pattern} is a list with elements
  \describe{
    \item{a_gene_id}{Unique gene ID}
    \item{pattern}{Chromosome pattern with chr separated by commas,
      epistatic pairs joined by colons}
    \item{posterior}{Posterior probability}
  }
}
\details{
  The \code{R/qtlbim} routine {qb.best} was used to find the best
  pattern of QTLs.
}
\seealso{\code{\link{best.pattern}}}
\examples{
\dontrun{
miRNA <- read.table("~/Rlib/eqtl/data/Rosetta/F2miRNAExport.txt",
  header = TRUE, row.names = 1, sep = "\t")
tmp <- names(miRNA)[-(1:2)]
tmp <- substring(tmp, 10, 18)
names(miRNA)[-(1:2)] <- tmp
names(miRNA)[1] <- "a_gene_id"
##################################################################################
## Drop miRNA with more than 200 missing values.
tmp <- apply(miRNA[, -(1:2)], 1, function(x) sum(is.na(x)))
t(t(tmp[tmp > 200]))
miRNA <- miRNA[tmp <= 200, ]
row.names(miRNA) <- paste(make.names(row.names(miRNA)), miRNA$a_gene_id, sep = ".")

write.csv(miRNA[, -(1:2)], row.names = TRUE, quote = FALSE, file = "miRNA.csv", na = "")

##################################################################################
## Mice with zeroes.
tmp <- unlist(sapply(apply(miRNA[, -(1:2)], 1,
             function(x) {
               s <- sum(x == 0, na.rm = TRUE)
               if(s)
                 which(x == 0 & !is.na(x))
               else
                 0
               }), names))
table(tmp)
table(names(tmp))

##################################################################################
## Image plot of mice by miRNA.
ot <- order(apply(normal.trans(as.matrix(miRNA[, -(1:2)])), 1, mean, na.rm = TRUE))
oa <- order(apply(normal.trans(as.matrix(miRNA[, -(1:2)])), 2, mean, na.rm = TRUE))
image(seq(ncol(miRNA) - 2), seq(nrow(miRNA)), log2(t(as.matrix(miRNA[, -(1:2)][ot,oa]))),
      xlab = "mouse", ylab = "miRNA", col = rev(gray(seq(0, .9, len = 256))))


##################################################################################
newdata <- get.raws("miRNA.csv", NULL)
## Drop Angie's markers to be compatible with other LOD files.
cross <- drop.markers(B6BTBR07, unlist(sapply(pull.map(angie), names)))
cross <- calc.genoprob(cross, step = 2, stepwidth = "variable")

cross <- add.raws(cross, newdata, toofew = 30)
miRNA.lod <- mult.scanone(names(newdata)[-1], cross = cross)
save(miRNA.lod, file = "miRNA.lod.RData", compress = TRUE)
write.csv(t(miRNA.lod[, -(1:2)]), row.names = TRUE, quote = FALSE, na = "",
          file = "miRNA.lod.csv")
##################################################################################

plot(miRNA.lod, heatmap = FALSE)

tmp <- multtrait(filename="miRNA.lod.csv")
pdf("miRNA.pdf", width = 8, height = 6)
tmp$main <- "rescale = peaks"
plot(tmp,rescale="peaks")
tmp$summary.plot <- "none"
tmp$main <- "rescale = none"
plot(tmp,rescale="none")
plot(tmp, chr = 17, heatmap = FALSE)
dev.off()


tmp <- multtrait(filename="miRNA.lod.csv", summary.plot = "none", chr = 17)

}
}
\keyword{utilities}
