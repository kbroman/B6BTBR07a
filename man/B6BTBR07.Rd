\name{B6BTBR07}
\alias{B6BTBR07}
\docType{data}
\title{Code used to construct R/qtl cross object for B6BTBR07}
\description{
B6BTBR07 cross object with SNPs from Mouse 5K SNP chip, diagnostics and
clinical phenotypes.
This is for the construction record. It can be used to rebuild B6BTBR07
from the other files.
}
\seealso{\code{\link{B6BTBR07a}}, \code{\link{B6BTBR07.maps}},
  \code{\link{build36}}, \code{\link{necropsy}},
  \code{\link{prot}}, \code{\link{rbm}},
  \code{\link{rbm.pheno}}, \code{\link{rna}}
}
\usage{
data(B6BTBR07)
}
\examples{
summary(B6BTBR07)
\dontrun{
######################################################################3
## 3 oct 2007 Brian S. Yandell
## Code to create R/qtl cross object for B6BTBR07 study.
######################################################################3
## Make cross object.
data(mouse.record)
data(snp.record)
data(snp.f2)

B6BTBR07 <- snp2cross(snp.f2[snp.record$Keep & snp.record$Segregating,
                             mouse.record$Keep[mouse.record$Strain == "F2"]],
                      mouse.record[mouse.record$Keep & mouse.record$Strain == "F2", ],
                      snp.record[snp.record$Keep & snp.record$Segregating, ])

## Make sure this is proper R/qtl cross object.
library(qtl)
summary(B6BTBR07)

##################################################################
## Linkage map in cM. Recreate if not present.
if(!exists("cM.map"))
  cM.map <- est.map(B6BTBR07) ## This takes a long time.
## Use cM.map with 0 as first location.
for(i in names(cM.map))
  cM.map[[i]] <- cM.map[[i]] - min(cM.map[[i]])

## Original map in Mb. Keep it around.
if(!exists("Mb.map"))
  Mb.map <- pull.map(B6BTBR07)

B6BTBR07 <- replace.map(B6BTBR07, cM.map)

##########################################################
## Drop bad marker(s).
tmp <- match("rs13481216", names(cM.map[[11]]))
tmp <- tmp[!is.na(tmp)]
if(length(tmp)) {
  B6BTBR07 <- drop.markers(B6BTBR07, "rs13481216")
  cM.map[[11]] <- est.map(subset(B6BTBR07, chr = 11))[[1]]
  B6BTBR07 <- replace.map(B6BTBR07, cM.map)
}
tmp <- match("rs13481216", names(Mb.map[[11]]))
tmp <- tmp[!is.na(tmp)]
if(length(tmp))
  Mb.map[[11]] <- Mb.map[[11]][-tmp]


#############################################################################
#############################################################################
## Merge RBM phenotypes.
data(rbm)

tmp <- match(B6BTBR07$pheno$MouseNum, rbm$mouse.id)
##*** Mouse 3597 is missing from RBM.
B6BTBR07$pheno$MouseNum[is.na(tmp)]
## Kludge to get right size matrix
tmp2 <- tmp
  tmp2[is.na(tmp)] <- 1

B6BTBR07$pheno <- cbind(B6BTBR07$pheno, rbm[tmp2, -(1:6)])
B6BTBR07$pheno[is.na(tmp), names(rbm)[-(1:6)]] <- NA

#############################################################################
## Merge Necropsy phenotypes.
data(necropsy)

tmp <- match(B6BTBR07$pheno$MouseNum,
             paste("Mouse", as.character(necropsy$Mouse.ID), sep = ""))
B6BTBR07$pheno <- cbind(B6BTBR07$pheno, necropsy[tmp, -(1:2)])
B6BTBR07$pheno$pepins <- B6BTBR07$pheno$X10.wk.c.peptide / B6BTBR07$pheno$X10.wk.insulin
B6BTBR07$pheno$logpepins <- log10(B6BTBR07$pheno$pepins)
for(i in c(4,6,8,10)) {
  B6BTBR07$pheno[[paste("wk", i, ".HOMA", sep = "")]] <-
    B6BTBR07$pheno[[paste("wk", i, ".GLUCOSE", sep = "")]] *
    B6BTBR07$pheno[[paste("wk", i, ".INSULIN", sep = "")]] / 22.5
}

#############################################################################
## Proteomics data (plasma amount only for now).
data(prot)

tmp <- match(paste(prot$Strain, "Mouse", prot$MouseNum, sep = ""),
             paste(B6BTBR07$pheno$Strain, B6BTBR07$pheno$MouseNum, sep = ""),
             nomatch = 0)

B6BTBR07$pheno$Plasma.ul <- prot$ul.of.plasma.remaining[tmp]

#############################################################################
## Tissue weights.
data(tissue.weights)
tmp <- match(B6BTBR07$pheno$MouseNum, tissue.weights$Mouse.ID)

## Keep only weights.
tmp2 <- names(tissue.weights)[grep("eight", names(tissue.weights))]
## Weight.mg is mostly missing--drop.
tmp2 <- tmp2[-length(tmp2)]

B6BTBR07$pheno <- cbind(B6BTBR07$pheno, tissue.weights[tmp, tmp2])

#############################################################################
## Read Lipid data.
data(lipids)
tmp <- match(B6BTBR07$pheno$MouseNum, paste("Mouse", lipids$MouseID, sep = ""))

B6BTBR07$pheno <- cbind(B6BTBR07$pheno, lipids[tmp, 9:12])

#############################################################################
## Read Islet Count data.
data(islet.count)
tmp <- match(B6BTBR07$pheno$MouseNum, as.character(islet.count$MouseID),
  nomatch = 0)
## One mouse did not have any islets harvested.
tmp2 <- seq(tmp)[tmp == 0]
tmp[tmp2] <- 1
tmp <- islet.count[tmp, 1, drop = FALSE]
tmp[tmp2, ] <- NA

B6BTBR07$pheno <- cbind(B6BTBR07$pheno, tmp)

#############################################################################
## Microarray tissue batches.
data(tissue.batch)

B6BTBR07$pheno <- cbind(B6BTBR07$pheno, tissue.batch[,-1])

#############################################################################
## Liver TG.
data(liverTG)
tmp <- match(as.character(liverTG$MouseNum), B6BTBR07$pheno$MouseNum, nomatch = 0)
tmp2 <- rep(NA, nind(B6BTBR07))
tmp2[tmp] <- liverTG$ug.TG.mg.Protein[tmp > 0]

B6BTBR07$pheno <- cbind(B6BTBR07$pheno, liver.TG = tmp2)
#############################################################################
#############################################################################
B6BTBR07.source <- rep("diag", ncol(B6BTBR07$pheno))
B6BTBR07.source[match(names(rbm)[-(1:6)], names(B6BTBR07$pheno), nomatch = 0)] <- "rbm"
tmp <- c("liver.TG", "islet.count", "wk4.HOMA", "wk6.HOMA", "wk8.HOMA", "wk10.HOMA")
B6BTBR07.source[match(c(names(necropsy)[-(1:2)], tmp), names(B6BTBR07$pheno), nomatch = 0)] <- "necr"
B6BTBR07.source[match(names(prot)[-(1:2)], names(B6BTBR07$pheno), nomatch = 0)] <- "prot"
B6BTBR07.source[match(names(tissue.weights)[-(1:4)], names(B6BTBR07$pheno), nomatch = 0)] <- "twts"
B6BTBR07.source[match(names(lipids)[9:12], names(B6BTBR07$pheno), nomatch = 0)] <- "lipids"
B6BTBR07.source[match(names(islet.count)[-1], names(B6BTBR07$pheno), nomatch = 0)] <- "necr"
B6BTBR07.source[1:4] <- "id"
names(B6BTBR07.source) <- names(B6BTBR07$pheno)
table(B6BTBR07.source)

########################################################################
B6BTBR07 <- calc.genoprob(B6BTBR07, step = 2, stepwidth = "variable")
\dontrun{
package.dir <- "~/Rlib/B6BTBR07a/data"
save("B6BTBR07.source",
     file = file.path(package.dir, "B6BTBR07.source.RData"),
     compress = TRUE)
save("B6BTBR07",
      file = file.path(package.dir, "B6BTBR07.RData"),
      compress = TRUE)
}
## Check R/qtl cross object again.
summary(B6BTBR07)
}
}
\keyword{datasets}
