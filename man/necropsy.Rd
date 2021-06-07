\name{necropsy}
\alias{necropsy}
\alias{liverTG}
\docType{data}
\title{Necropsy data from Attie lab}
\description{
See Donnie Stapleton for details. The liverTG measurements were made
later on a subset of male mice.
}
\usage{
data(necropsy)
data(liverTG)
}
\format{
  The necropsy data is a data frame with 554 observations on the following 42 variables.
  \describe{
    \item{\code{Mouse.ID}}{a numeric vector}
    \item{\code{SEX}}{a factor with levels \code{F} \code{M}}
    \item{\code{BIRTH.DATE}}{Date of birth.}
    \item{\code{SAC.DATE}}{Date of sacrifice.}
    \item{\code{WEAN.DATE}}{Date of weaning.}
    \item{\code{D20.start.date}}{Deuterium start date.}
    \item{\code{D2O.INJECTION.WEIGHT..g.}}{Deuterium injection weight (g).}
    \item{\code{D2O.INJECTION.VOLUME..ml.}}{Deuterium injection volume (ml).}
    \item{\code{LITTER.SIZE}}{Litter size.}
    \item{\code{CAGE.ID}}{Cage identifier.}
    \item{\code{MOTHER.ID}}{Mother ID.}
    \item{\code{FATHER.ID}}{Father ID.}
    \item{\code{AGOUTI.COAT..Y.N.}}{Agouti coat (all missing).}
    \item{\code{TUFT.COAT..Y.N.}}{Tuft coat (all missing).}
    \item{\code{wk4.DATE}}{Date at week 4 measurements.}
    \item{\code{wk4.WEIGHT..g.}}{Weight at week 4 (g).}
    \item{\code{wk4.BODY.LENGTH..cm.}}{Body length at week 4 (cm).}
    \item{\code{wk4.GLUCOSE..mg.dl.}}{Glucose at week 4 (mg/dl).}
    \item{\code{wk4.INSULIN..ng.ml.}}{Insulin at week 4 (ng/ml).}
    \item{\code{wk4.TRIGLYCERIDE..mg.dl.}}{Triglyceride at week 4 (mg/dl).}
    \item{\code{wk6.DATE.1}}{Weight at week 6 (g).}
    \item{\code{wk6.WEIGHT..g..1}}{Weight at week 6 (g).}
    \item{\code{wk6.BODY.LENGTH..cm..1}}{Body length at week 6 (cm).}
    \item{\code{wk6.GLUCOSE..mg.dl..1}}{Glucose at week 6 (mg/dl).}
    \item{\code{wk6.INSULIN..ng.ml..1}}{Insulin at week 6 (ng/ml).}
    \item{\code{wk6.TRIGLYCERIDE..mg.dl..1}}{Triglyceride at week 6 (mg/dl).}
    \item{\code{wk8.DATE.2}}{Weight at week 8 (g).}
    \item{\code{wk8.WEIGHT..g..2}}{Weight at week 8 (g).}
    \item{\code{wk8.BODY.LENGTH..cm..2}}{Body length at week 8 (cm).}
    \item{\code{wk8.GLUCOSE..mg.dl..2}}{Glucose at week 8 (mg/dl).}
    \item{\code{wk8.INSULIN..ng.ml..2}}{Insulin at week 8 (ng/ml).}
    \item{\code{wk8.TRIGLYCERIDE..mg.dl..2}}{Triglyceride at week 8 (mg/dl).}
    \item{\code{wk10.DATE.3}}{Weight at week 10 (g).}
    \item{\code{wk10.WEIGHT..g..3}}{Weight at week 10 (g).}
    \item{\code{wk10.BODY.LENGTH..cm..3}}{Body length at week 10 (cm).}
    \item{\code{wk10.GLUCOSE..mg.dl..3}}{Glucose at week 10 (mg/dl).}
    \item{\code{wk10.INSULIN..ng.ml..3}}{Insulin at week 10 (ng/ml).}
    \item{\code{wk10.TRIGLYCERIDE..mg.dl..3}}{Triglyceride at week 10 (mg/dl).}
    \item{\code{X10.wk.assay.date}}{Date of 10-week assay.}
    \item{\code{X10.wk.insulin}}{Insulin at 10-week assay.}
    \item{\code{X10.wk.c.peptide}}{c-peptide measurement at 10 weeks.}
    \item{\code{insulin.c.peptide.assay.date}}{Date of c-peptide measurement}
  }
  
  The liverTG data frame has columns
  \describe{
    \item{MouseNum}{mouse ID}
    \item{Body.Weight}{in grams}
    \item{Total.Liver.Weight.g.}{total liver weight in grams (from \code{necropsy})}
    \item{X10wk.Plasma.TG}{10 week plasma TG (from \code{necropsy})}
    \item{geno}{genotype on chr 17 region (\code{B6} or \code{BTBR}).}
    \item{Project}{\code{f2}}
    \item{mg.of.liver}{mg of liver excised for TG assay (new: preserve this)}
    \item{ul.of.PBS}{ul of PBS used}
    \item{ug.of.TG}{absolute mass of liver TG in ug}
    \item{Protein.mg.ml.}{protein mg per ml}
    \item{ug.TG.ul.homogenate}{ug of TG per ml of homogenate}
    \item{ug.TG.mg.Protein}{ug of TG per mg of protein (new: preserve
      this)}
  }
}
\details{
Necropsy report supplied by Donnie Stapleton.
}
\seealso{\code{\link{qtl.code}}
}
\examples{
data(necropsy)
dim(necropsy)

data(liverTG)
dim(liverTG)

\dontrun{
## Old file was TGDonnie20080612.csv
## ... but I cannot readily find it right now.
## NB: Old data file had rather different values for 109 male mice ...
## Following file has first line removed from Donnie's XLS
## and fixed Mousennnn in first column.
data.dir <- "~/Rlib/eqtl/data"
package.dir <- "~/Rlib/B6BTBR07a/data"
liverTG <- read.csv(file.path(data.dir, "Complete F2 Liver TG Set2008dec.csv",
  comment = "", header = TRUE)
save(liverTG, file = file.path(package.dir, "liverTG.RData"),
  compress = TRUE)
}
}
\keyword{datasets}
