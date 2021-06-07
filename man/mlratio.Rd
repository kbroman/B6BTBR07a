\name{add.mlratio}
\alias{add.mlratio}
\title{Add mlratio trait to cross object}
\description{
Add mlratio to cross objects.
}
\usage{
add.mlratio(cross, tissue.name, gene.symbol)
}
\arguments{
  \item{cross}{Object of class \code{cross}. See
    \code{\link[qtl]{read.cross}}.}
  \item{tissue.name}{Name of tissue, including \code{"islet"} and
    \code{"hypo"}.}
  \item{gene.symbol}{Vector of gene symbols as character strings or
    \code{a_gene_id} values as numeric.}
}
\details{
  This assumes that \code{gene.annot} and \code{islet.mlratio} are
  available. \code{gene.symbol} is matched with gene annotation to
  determine which transcripts to extract. These are pulled and added as
  new phenotypes to the \code{cross} object.
}
\examples{
\dontrun{
data(B6BTBR07)
load("B6BTBR07.mlratio/data/gene.annot.RData")
load("B6BTBR07.mlratio/data/islet.mlratio.RData")
cross <- add.mlratio(B6BTBR07, "islet", "Syne2")
cross <- add.mlratio(B6BTBR07, "islet", 10003842159)
## maybe str(add.mlratio) ; plot(add.mlratio) ...
}
}
\keyword{utilities}
