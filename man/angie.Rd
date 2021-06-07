\name{B6BTBR07a}
\alias{B6BTBR07a}
\alias{angie}
\alias{patchmap}
\docType{data}
\title{Additional markers for B6BTBR7 cross from Attie lab.}
\description{
Additional markers on chr 16 and 19 from Angie Oler for B6BTBR07 F2
cross, prepared as cross object angie. The patchmap is the translation
of pseudomarkers for aligning old and new analyses.
}
\usage{
data(angie)
data(B6BTBR07a)
data(patchmap)
}
\details{
  Angie Oler added 11 markers on a subset of mice in important regions
  on chr 16 and 19. These can be used to refine QTL mapping.
}
\seealso{\code{\link[qtlview]{pull.pseudomarkers}}}
\examples{
summary(B6BTBR07a)
summary(angie)
print(patchmap)
\dontrun{
package.dir <- "~/Rlib/B6BTBR07a/data"
## Get Angie's markers.
angiemap <- read.csv(file.path("chr16","chr16angie.csv"), header = TRUE)
angie <- read.cross("csv","chr16","angiegeno.csv", geno = c("A","H","B"))

## Update Mb when known.
tmp <- !is.na(angiemap$Mb.36)
for(chr in c("16","19")) {
  map <- pull.map(angie, chr)[[1]]
  tmp2 <- angiemap$chr == chr
  map[tmp[tmp2]] <- angiemap$Mb.36[tmp & tmp2]
  angie$geno[[chr]]$map <- map
}
save(angie, file = file.path(package.dir, "angie.RData"), compress = TRUE)


########################################################################
## Add hand-done markers on chr 16 and 19 and save B6BTBR07a.
B6BTBR07a.create <- function(B6BTBR07a, angie, chr)
{
  require(qtl)
  ## Caution: angie is in Mb, so need to match using B6BTBR07.maps.
  
  ## Expand genotypes to all mice.
  ## Create new geno matrix of right size.
  newdata <- pull.geno(angie, chr)
  newgeno <- matrix(NA, nind(B6BTBR07a), ncol(newdata))

  ## Match up Mouse ID and populate geno matrix.
  tmp <- paste("Mouse", as.character(angie$pheno$Mouse.ID), sep = "")
  tmp <- match(tmp, as.character(B6BTBR07a$pheno$MouseNum), nomatch = 0)
  newgeno[tmp,] <- newdata[tmp > 0,]
  newmap <- pull.map(angie, chr)[[1]]
  colnames(newgeno) <- names(newmap)

  ## Merge old and new data.
  newgeno <- cbind(pull.geno(B6BTBR07a, chr), newgeno)

  ## Convert to cM. Need to approx position of new guys.
  map.cM <- c(B6BTBR07.maps$cM.map[[chr]],
    qm.approx(B6BTBR07.maps, "Mb", chr, newmap)$y)
  names(map.cM) <- c(names(B6BTBR07.maps$cM.map[[chr]]), names(newmap))

  ## Reorder.
  tmp <- order(map.cM)
  map.cM <- map.cM[tmp]
  newgeno <- newgeno[, tmp]

  ## Update data and map for chr in cross object.
  B6BTBR07a$geno[[chr]]$data <- newgeno
  B6BTBR07a$geno[[chr]]$map <- map.cM

  B6BTBR07a
}

## Now actually do it.
data(angie)
B6BTBR07a <- B6BTBR07
for(chr in c(16,19))
  B6BTBR07a <- B6BTBR07a.create(B6BTBR07a, angie, chr)

save(B6BTBR07a, file = file.path(package.dir, "B6BTBR07a.RData"),
  compress = TRUE)

########################################################################
## Code to drop Angie's extra markers.
tmp <- unlist(sapply(pull.map(angie), names))
cross <- drop.markers(B6BTBR07a, tmp)

########################################################################
## The routine fixmap is deprecated.
map <- pull.pseuodmarkers(calc.genoprob(B6BTBR07,
  step = 2, stepwidth = "variable"))
mapa <- pull.pseudomarkers(calc.genoprob(B6BTBR07a,
  step = 2, stepwidth = "variable"))
## Routine compmap is local to this package.
patchmap <- compmap(mapa, map, 16)
patchmap <- rbind(patchmap, compmap(mapa, map, 19))
print(patchmap)

save(patchmap, file = file.path(package.dir, "patchmap.RData"), compress = TRUE)
}
}
\keyword{datasets}
