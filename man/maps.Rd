\name{B6BTBR07.maps}
\alias{B6BTBR07.maps}
\alias{B6BTBR07a.maps}
\alias{B6BTBR07r.maps}
\alias{B6BTBR07r}
\docType{data}
\title{Segregating SNP map in cM and Mb for B6BTBR07 cross}
\description{
  List of objects of class map for cross objects B6BTBR07 and B6BTBR07a.
  Also creates B6BTBR07r if physical map is reordered within chromosomes.
}
\description{
  Data frame with elements:
  \item{cM.map}{Genetic map (in centi-Morgans) for segregating markers;
    essentially the same as pull.map(B6BTBR07).}
  \item{Mb.map}{Physical map (in Megabases) of cM.map using sequence data.}
  \item{cM.same}{Genetic map non-segregating markers (optional).}
  \item{Mb.same}{Physical map non-segregating markers (optional).}
}
\details{
  Run \code{\link[qtlview]{read.maps}} on \code{cross} object
  \code{B6BTBR07} with reference file of physical map.
  Interpolation and relationship between maps are performed using
  \code{\link[qtlview]{qm.approx}}. 
}
\usage{
data(B6BTBR07.maps)
}
\seealso{\code{\link[qtlview]{read.maps}},
  \code{\link[qtlview]{qm.approx}},
  \code{\link{B6BTBR07}}}
\examples{
summary(B6BTBR07.maps)
plot(B6BTBR07.maps)
\dontrun{
## Keep only SNPs with decent records.
keep <- snp.record$Keep
names(keep) <- snp.record$Build36.SNP

## Get old maps and creat object.
dataold.dir <- "~/Rlib/B6BTBR07/data"
load(file.path(dataold.dir, "cM.map.RData"))
load(file.path(dataold.dir, "Mb.map.RData"))
load(file.path(dataold.dir, "cM.same.RData"))
load(file.path(dataold.dir, "Mb.same.RData"))
B6BTBR07.maps <- list(cM.map = cM.map, Mb.map = Mb.map,
  cM.same = cM.same, Mb.same = Mb.same)
attr(B6BTBR07.maps, "genotypes") <- c("A","H","B")
class(B6BTBR07.maps) <- c("read.maps", "list")

package.dir <- "~/Rlib/B6BTBR07a/data"
save("B6BTBR07.maps",
  file = file.path(package.dir, "B6BTBR07.maps.RData"),
  compress = TRUE)

## Create new maps object.
data.dir <- "http://attie.wisc.edu/lab/data/data_for_intranet/snps"
B6BTBR07r.maps <- read.maps(B6BTBR07,
    file.path(data.dir, "snp_4853.txt"),
    genotypes = c("B6","Het","BTBR"),
    keep = keep)
save("B6BTBR07r.maps",
  file = file.path(package.dir, "B6BTBR07r.maps.RData"),
  compress = TRUE)

## Build 37 has slight reordering of SNPs (chr 1,2,12,14,15,17).
is.amiss <- sapply(B6BTBR07.maps$Mb.map, function(x) any(diff(x) < 0))
if(any(is.amiss)) {
  ## Create map and cross object to be reordered.
  B6BTBR07r <- B6BTBR07

  ## Reorder for chr that are amiss.
  is.amiss <- names(is.amiss[is.amiss])
  cat("Chromosomes to reorder:", paste(is.amiss, collapse = ","), "\n")
  for(chr in is.amiss) {
    cat("Reordering chr", chr, "...\n")
    o <- order(B6BTBR07r.maps$Mb.map[[chr]])
    ## Reorder by physical position.
    B6BTBR07r.maps$Mb.map[[chr]] <- B6BTBR07r.maps$Mb.map[[chr]][o]
    ## Reorder map in cross object.
    B6BTBR07r <- switch.order(B6BTBR07r, chr, o)
    ## Extract new map from cross object.
    B6BTBR07r.maps$cM.map[[chr]] <- pull.map(B6BTBR07r, chr)[[1]]
    ## Recompute cM distance for non-segregating markers.
    B6BTBR07r.maps$cM.same[[chr]] <-
      qtlview:::qm.approx(B6BTBR07r.maps, "Mb", chr, B6BTBR07r.maps$Mb.same[[chr]])$y
  }
}
save("B6BTBR07r.maps",
  file = file.path(package.dir, "B6BTBR07r.maps.RData"),
  compress = TRUE)
save("B6BTBR07r",
  file = file.path(package.dir, "B6BTBR07r.RData"),
  compress = TRUE)

## Want to keep Angie's markers for the following maps.
B6BTBR07a.maps <- read.maps(B6BTBR07,
    file.path(data.dir, "snp_4853.txt"),
    genotypes = c("B6","Het","BTBR"),
    drop.extra = FALSE, keep = keep)
save("B6BTBR07a.maps",
  file = file.path(package.dir, "B6BTBR07a.maps.RData"),
  compress = TRUE)
}
}
\keyword{datasets}
