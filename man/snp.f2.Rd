\name{snp.f2}
\alias{snp.f2}
\docType{data}
\title{Recoded SNP data for F2 mice.}
\description{
Recoding indicates genotype and errors.
}
\usage{data(snp.f2)}
\format{
  Matrix of 4853 rows by 554 columns.
}
\details{
Codes 1:3 now refer to genotype: 1 = B6 (BTBR if female X);
2 = Het (BTBR if male X); 3 = BTBR. Error codes are as follows:
4 = non-segregating, match with parents;
5 = non-segregating, mismatch as het;
6 = non-segregating, mismatch as other homozygote;
7 = het on male X;
8 = B6 on female X.
}
\seealso{\code{\link{snp}}, \code{\link{affy.code}}
}
\examples{
data(snp.f2)
dim(snp.f2)
}
\keyword{datasets}
