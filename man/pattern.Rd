\name{islet.pattern}
\alias{islet.best}
\alias{islet.pattern}
\alias{liver.best}
\alias{liver.pattern}
\alias{adipose.best}
\alias{adipose.pattern}
\title{Tissue pattern for best QTL model}
\description{
Islet pattern across chromosomes for best QTL model using R/qtlbim model
selection tools.
}
\usage{
data(islet.best)
data(islet.pattern)
data(liver.best)
data(liver.pattern)
data(adipose.best)
data(adipose.pattern)
}
\format{
  \code{islet.best} is a list with elements
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
data(islet.best)
data(islet.pattern)
data(liver.best)
data(liver.pattern)
data(adipose.best)
data(adipose.pattern)
\dontrun{
cross <- calc.genoprob(B6BTBR07, step = 2, stepwidth = "variable")
## Find best pattern by tissue.
made <- "~/p/private/diabetes3/diabetes30/best"
best.pattern("islet", cross, made = made)
best.pattern("liver", cross, made = made)
best.pattern("adipose", cross, made = made)
}
}
\keyword{utilities}
