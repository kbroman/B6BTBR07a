\name{B6BTBR07.annotation}
\alias{B6BTBR07.annotation}
\docType{data}
\title{Gene annotation information.}
\description{
Gene annotation curated from Rosetta.
}
\usage{data(B6BTBR07.annotation)}
\format{
  A data frame with 40573 rows (probes) on the following 40
  variables. Variables with star (*) are most useful.
  \describe{
    \item{\code{ID}}{Row index}
    \item{\code{a_substance_id}}{Substance ID number}
    %\item{\code{reporterid}}{Substance ID again?}
    %\item{\code{iIdentifier}}{internal identifier?}
    \item{\code{accession_code}}{accession code}
    %\item{\code{sequence_code}}{accession code again?}
    \item{\code{a_gene_id}}{Gene ID number}
    \item{\code{Symbol}}{Gene Symbol} %gene1 (was GeneSymbol_1 for TC)
    %\item{\code{gene_symbol_2}}{accession code again?}
    %\item{\code{GeneSymbol_3}}{Gene Symbol again?}
    \item{\code{Alternate_Symbols}}{alternate gene symbols} %gene (was gene_1 for TC)
    %\item{\code{AlternateSymbols}}{Gene Symbol synonyms again}
    %\item{\code{LocusLinkID}}{Locus Link ID}
    %\item{\code{UnigeneCluster}}{Unigene Cluster}
    \item{\code{Name}}{Gene Name} %Gene_Name
    \item{\code{Alternate_Names}}{Alternate Gene Names} %AlternateNames
    \item{\code{Description}}{Gene Description} %ProteomeShortDescription
    \item{\code{Chromosome}}{Chromosome (build 36/mm8)}
    \item{\code{Start_Coordinate}}{Start Position (build 36/mm8)} %StartCoordinate
    \item{\code{End_Coordinate}}{*End Position (build 36/mm8)} %EndCoordinate
    \item{\code{Strand}}{*Strand direction (+1 or 1)}
    \item{\code{Chromosome_Position}}{} %overwrote chromosomeMapC
    %\item{\code{chromosomeMapP}}{}
    %\item{\code{SeqDescription}}{}
    %\item{\code{KeggPath}}{}
    %\item{\code{KeggPathCode}}{}
    %\item{\code{clusterCode}}{}
    %\item{\code{Clu_go_p}}{}
    %\item{\code{Clu_go_pCode}}{}
    %\item{\code{Clu_go_f}}{}
    %\item{\code{Clu_go_fCode}}{}
    %\item{\code{Clu_go_c}}{}
    %\item{\code{Clu_go_cCode}}{}
    %\item{\code{TranscriptID}}{}
    %\item{\code{TranscriptSource}}{}
    %\item{\code{seq_go_p}}{}
    %\item{\code{seq_go_pCode}}{}
    %\item{\code{seq_go_f}}{}
    %\item{\code{seq_go_fCode}}{}
    %\item{\code{seq_go_c}}{}
    %\item{\code{seq_go_cCode}}{}
  }
}
\details{
  Supplied by Rosetta via Younjeong Choi.
}
\seealso{\code{\link[qtlview]{read.annotation}}
}
\examples{
\dontrun{
data.dir <- "~/Rlib/eqtl/data/Rosetta"
B6BTBR07.annotation <- read.annotation(file.path(data.dir, "F2.Annotation.mm8.20080208.csv",
#B6BTBR07.annotation <- read.annotation(file.path("http://attie.wisc.edu",
#    "/bxaf5/Data/DataCenter/123466/", "F2_array_data_-_mlratio/Annotation/",
#    "F2.Annotation.mm8.20080208.csv"),
  update.names = c(Symbol = "gene1", Alternate_Symbols = "gene",
    Name = "Gene_Name", Alternate_Names = "AlternateNames",
    Description = "ProteomeShortDescription",
    Chromosome = "CHROMOSOME", Start_Coordinate = "STARTCOORD",
    End_Coordinate = "ENDCOORD", Strand = "STRAND"))
package.dir <- "~/Rlib/B6BTBR07a/data"
save("B6BTBR07.annotation", file = file.path(package.dir, "B6BTBR07.annotation.RData"),
     compress = TRUE)
}
data(B6BTBR07.annotation)
str(B6BTBR07.annotation)
}
\keyword{datasets}
