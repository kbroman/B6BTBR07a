\name{affy.code}
\alias{affy.code}
\docType{data}
\title{Code used to process Affymetrix chips for B6BTBR07}
\description{
This is for the record. See datasets for details.
}
\seealso{\code{\link{mouse.record}},
  \code{\link{snp}}, \code{\link{snp.f2}}, \code{\link{snp.parents}},
  \code{\link{snp.record}},  \code{\link{topstuff}},
  \code{\link{qtl.code}}
}
\examples{
\dontrun{
## 26 oct 2007 Brian S. Yandell
## Code for quality check of B6BTBR07 SNP data.
##
## NB: This list has changed below. More record entries added.
##
## Action from this code:
##   mouse.record$Array  Mice priority for microarrays.
##                       (avoid mice with Array > 3)
##     1.Best            no apparent problems  top priority
##     2.HetMaleX        male has het X        HetMaleX > 3
##     3.LowCallRate     Affy call = 0\%        drop.calls == TRUE
##                                             (Experiment.Call.Rate.. < 10)
##     4.NonSegMismatch  several bad genotypes NonSegMismatch > 30
##                                             (mostly agees with drop.calls)
##     5.CageSex         caged with other sex  Mouse3174 and Mouse3277 and mates
##     6.BadGenos        many bad genotypes    NonSegMismatch > 900
##                                             (also DC1&DC2 > 200)
##     7.NoGenos         no genotypes          drop.repeats == TRUE
##                                             (Sample.Repeat.Count. != 1)
##     8.NotF2           parent lines, check   Strain != "F2"
##
##   snp.record$Keep           SNP subset to keep.
##                             (still under development)
##                             (see snp.record$DC1,DC2)
##                             (want to compare with builds 34,36)
##
## Objects created here:
##   topstuff (566 x 6)        Stuff at top of genotypes file. Contains:
##   snp (4853 x (8 + 566))    Raw SNP genotypes
##   snp.parents (4853 x 20)   SNP data for parents (B6, F1, BTBR)
##   mouse.record (566 x 33)   Record of statistics by mouse.
##   snp.record (566 x 18)     Record of statistics by SNP.
##   snp.f2 (4853 x 554)       SNP alleles for all F2 mice.
##
## Useful Subsets:
##   Kept SNPs for kept Mice (in original order)
##     snp[snp.record$Keep, c(rep(TRUE, 8), mouse.record$Keep]
##   Kept SNPs for parents (in original order)
##     snp.parents[snp.record$Keep, ]
##   Kept SNPs that segregate for kept F2 Mice (matrix in orginal order)
##     snp.f2[snp.record$Keep & snp.record$Segregating,
##            mouse.record$Keep[mouse.record$Strain == "F2"]]
##   Reorder one of these into genome order (chromosome, position)
##     object <- snp[...] as above
##     object <- snp.parents[...] as above
##     object[order(object$Chrom.Name, object$Chrom.Position), ]
##     For snp.f2, need more care (f2geno has genotypes, object has phenotypes)
##       f2geno <- snp.f2[...] as above
##       object <- snp.record[snp.record$Keep & snp.record$Segregating, ]
##       f2geno <- f2geno[order(object$Chrom.Name, object$Chrom.Position), ]
##       object <- object[order(object$Chrom.Name, object$Chrom.Position), ]
## 
## DETAIL BY OBJECT:
##
## topstuff (566 x 6)        Stuff at top of genotypes file. Contains:
##   Experiment.Name
##   Status.
##   Sample.Location.
##   Sample.Name.
##   Sample.Repeat.Count.
##   Experiment.Call.Rate..
##
## snp (4853 x (8 + 566))    Raw SNP genotypes
##   Assay.Id                Internal Affy ID
##   External.Id             External ID: rs + number
##   Target.Allele           bases to compare: A/C, A/T, ...
##   Genome.Map              Mouse NCBI Build 32
##   Chrom.Name              1:19, X
##   Chrom.Position          Converted below to Mb.
##   Gene                    All are missing (proprietary?).
##   Assay.Call.Rate..       \% of samples with calls?
##   tid28437_Mouse2272_B6_Male Entries by mouse
##
## snp.parents (4853 x 19)   SNP data for parents (B6, F1, BTBR)
##   (columns 1:8 of snp + columns for 11 parents)
##
## mouse.record (566 x 33)   Record of mouse statistics.
##   AffyTID                 Affy TID number for mouse.
##   MouseNum                Mouse number as "Mousennnn"
##   Strain                  B6, BTBR, F1 or F2
##   Sex                     Male or Female
##***Array                   Use this mouse for microarray (***IMPORTANT COLUMN***).
##***Keep                    Keep this mouse (***IMPORTANT COLUMN***).
##   drop.repeats            Is Sample.Repeat.Count. not 1?
##   drop.calls              Is Experiment.Call.Rate.. < 10?
##   NonSegMismatch          Mismatch count for non-segregating SNPs (> 900 bad)
##   HetMaleX                  Count SNPs with het SNP (snp = 1) for male F2s
##   DC1                     Double crossover count around SNP (>200 bad)
##   DC2                     Double crossover count at SNP (>200 bad)
##   CO                      Crossover count for mouse
##   CO.1 : CO.X             Crossover count by chromosome
##
## snp.record (4853 x 20)    Record of SNP statistics.
##   Chrom.Name
##   Chrom.Position
##   Assay.Id
##   External.Id
##   Assay.Call.Rate..
##   Target.Allele
##***Keep                    Keep this SNP (***IMPORTANT COLUMN***).
##   B6                      B6 base pair allele
##   BTBR                    BTBR base pair allele
##   Segregating             Is SNP segregating? (Are B6 and BTBR alleles different?)
##   HetParents              Is either B6 or BTBR parent het (1)? bad
##   NonSegMismatch          Mismatch of F1 with parents for non-segregating SNPs
##   SegMismatch             Mismatch of F1 with parents for segregating SNPs
##                           (watch out for male X)
##   B6Match                 Match of all B6 parents.
##   BTBRMatch               Match of all BTBR parents.
##   F1Match                 Match of all F1 parents (watch out for male X).
##   HetMaleX                Count male F2 mice with het SNP (snp = 1)
##   DC1                     Double crossover count around SNP (>200 bad)
##   DC2                     Double crossover count at SNP (>200 bad)
##   CO                      Crossover count at SNP
##
## snp.f2 (4853 x 554)       SNP alleles for all F2 mice (matrix).
##   

#######################################################################
## Top stuff has mouse IDs
topstuff <- read.table("data/Genotypes18Sep07.txt", sep = "\t", nrow = 6)
topstuff <- topstuff[, -(1:9)]
topstuff <- as.matrix(topstuff)
topstuff <- t(topstuff)
dimnames(topstuff) <- list(topstuff[,4], topstuff[1,])
topstuff <- topstuff[-1,]
topstuff <- data.frame(topstuff)
topstuff[[5]] <- as.numeric(as.character(topstuff[[5]]))
topstuff[[6]] <- as.numeric(as.character(topstuff[[6]]))

###########################################################################
## SNP genotypes
snp <- read.table("data/Genotypes18Sep07.txt", sep = "\t", skip=6, header=TRUE)
## Fix column names.
tmp <- names(snp)
## Drop empty column.
snp <- snp[, -9]
tmp <- c(tmp[2:9], as.character(topstuff$Sample.Name))
names(snp) <- tmp
snp$Chrom.Name <- ordered(snp$Chrom.Name, c(1:19,"X"))
## Change to Mb.
snp$Chrom.Position <- snp$Chrom.Position * 10^-6

############################################################################
## Mouse and SNP Records: important spreadsheets.
##
## Decode mouse ID and create mouse record data frame.
mouse.record <- strsplit(as.character(topstuff$Sample.Name), "_")
mouse.record <- as.data.frame(t(as.data.frame(mouse.record)))
names(mouse.record) <- c("AffyTID","MouseNum","Strain","Sex")
row.names(mouse.record) <- NULL
mouse.record$Array <- rep("1.Best", nrow(mouse.record))
mouse.record$Keep <- rep(TRUE, nrow(mouse.record))

snp.record <- snp[, c("Chrom.Name","Chrom.Position","Assay.Id","External.Id",
                    "Assay.Call.Rate..","Target.Allele")]
snp.record$Keep <- rep(TRUE, nrow(snp.record))
############################################################################
##*** Fix mice errors.
## Mice 3273 and 3274 are Female, not Male as coded by Affy.
## Mouse 3174 is a Female, wrongly coded as Male in Clean Necropsy and RBM data.
## Mouse 3277 is a Male, not Female as coded in Affy and Clean Necropsy data.
## Mouse 3259 and 3260 are Males, not Females as coded in Affy.
mouse.record$Sex[mouse.record$MouseNum \%in\% c("Mouse3273","Mouse3274")] <-
  "Female"
mouse.record$Sex[mouse.record$MouseNum \%in\%
  c("Mouse3277","Mouse3259","Mouse3260")] <- "Male"


############################################################################
##*** Drop repeats.
mouse.record$drop.repeats <- topstuff$Sample.Repeat.Count != 1
##*** Drop calls.
mouse.record$drop.calls <- topstuff$Experiment.Call.Rate < 10

##*** ALTER mouse.record$Keep.
mouse.record$Keep <- mouse.record$Keep & !mouse.record$drop.repeats

###########################################################################
## Check SNP genotypes of Parents.
##
## Good SNPs have the following parent patterns (ignoring 5=NA):
## B6 F1 BTBR
## 0  0  0    non-segregating
## 2  2  2
##
## 0  1  2    segregating
## 2  1  0    
##            
## 0  2  2    segregating--X males with BTBR mothers
## 2  0  0
##
## 0  0  2    segregating--X males with B6 mothers
## 2  2  0

tmp <- mouse.record$Strain \%in\% c("B6","F1","BTBR")
snp.parents <- snp[, c(1:8, match(topstuff$Sample.Name[tmp], names(snp)))]

## snp.parents
## column    9 10 11 12 13 14 15 16 14 18 19
## genotype B6 B6 B6 BT BT F1 F1 BT BT F1 F1
## sex       M  M  F  M  F  F  M  M  F  F  M

is.B6 <- sapply(strsplit(names(snp.parents), "_"),
                function(x) x[3] == "B6" & !is.na(x[3]))
is.F1 <- sapply(strsplit(names(snp.parents), "_"),
                function(x) x[3] == "F1" & !is.na(x[3]))
is.BTBR <- sapply(strsplit(names(snp.parents), "_"),
                function(x) x[3] == "BTBR" & !is.na(x[3]))
is.parent <- is.B6 | is.F1 | is.BTBR
is.X <- 1 * (snp.record$Chrom.Name == "X")
is.male <- sapply(strsplit(names(snp.parents), "_"),
                function(x) x[4] == "Male" & !is.na(x[4]))

## B6 and BTBR alleles.
tmpfn <- function(x, value) all(x \%in\% value | x == 5) & !all(x == 5)
B6 <- apply(snp.parents[, is.B6], 1, tmpfn, 0)
B62 <- apply(snp.parents[, is.B6], 1, tmpfn, 2)
BTBR <- apply(snp.parents[, is.BTBR], 1, tmpfn, 0)
BTBR2 <- apply(snp.parents[, is.BTBR], 1, tmpfn, 2)

tmp <- unlist(strsplit(as.character(snp$Target.Allele), "/"))
snp.record$B6 <- factor(tmp[seq(1, 2 * nrow(snp.record), by = 2) + B6])
snp.record$B6[!(B6 | B62)] <- NA
snp.record$BTBR <- factor(tmp[seq(1, 2 * nrow(snp.record), by = 2) + BTBR])
snp.record$BTBR[!(BTBR | BTBR2)] <- NA

## Segregating SNPs as defined by B6 and BTBR.
snp.record$Segregating <- (BTBR & B62) | (BTBR2 & B6)
snp.record$Segregating[!(B6 | B62)] <- NA
snp.record$Segregating[!(BTBR | BTBR2)] <- NA

## Find bad SNPs.
## Find SNPs for B6 or BTBR with het genotypes (1).
snp.record$HetParents <- apply(snp.parents[, is.B6 | is.BTBR], 1,
                           function(x) any(x == 1))

## Find non-segregating SNPs with F1 != parents.
tmpfn <- function(x, value) any(x \%in\% value) & !all(x == 5)
snp.record$NonSegMismatch <- ((B6 & BTBR & apply(snp.parents[, is.F1], 1, tmpfn, c(1,2))) |
                        (B62 & BTBR2 & apply(snp.parents[, is.F1], 1, tmpfn, c(0,1))))

## Find segregating SNPs with code 0,2 for B6 or BTBR
## but F1 = 1 (autosome, female X) or F1 = BTBR (male X).
snp.record$SegMismatch <- (apply(snp.parents[, is.B6 | is.BTBR], 1,
                           function(x) (all(x == c(0,0,0,2,2,2,2) | x == 5) |
                                        all(x == c(2,2,2,0,0,0,0) | x == 5))) &
                     !apply(cbind(is.X, as.matrix(snp.parents[, is.parent])), 1,
                            function(x) {
                              if(as.logical(x[1])) ## X chromosome: male F1 = BTBR.
                                (all(x[-1] == c(0,0,0,2,2,1,2,2,2,1,2) | x[-1] == 5) |
                                 all(x[-1] == c(2,2,2,0,0,1,0,0,0,1,0) | x[-1] == 5))
                              else     ## Autosome: F1 = B6/BTBR.
                                (all(x[-1] == c(0,0,0,2,2,1,1,2,2,1,1) | x[-1] == 5) |
                                 all(x[-1] == c(2,2,2,0,0,1,1,0,0,1,1) | x[-1] == 5))
                            }))

snp.record$B6Match <- apply(snp.parents[, is.B6], 1,
                               function(x) !all(x == 5) &
                               (all(x == 0 | x == 5) | all(x == 2 | x == 5)))
snp.record$BTBRMatch <- apply(snp.parents[, is.BTBR], 1,
                                 function(x) !all(x == 5) &
                                 (all(x == 0 | x == 5) | all(x == 2 | x == 5)))
snp.record$F1Match <- apply(cbind(is.X, as.matrix(snp.parents[, is.F1])), 1,
                               function(x) {
                                 if(x[1]) ## X chromosome.
                                   (!all(x[-1] == 5) &
                                    (all(x[-1] == 0 | x[-1] == 5) |
                                     all(x[-1] == 2 | x[-1] == 5) |
                                     all(x[-1] == c(1,0,1,0) | x[-1] == 5) |
                                     all(x[-1] == c(1,2,1,2) | x[-1] == 5)))
                                 else
                                   (!all(x[-1] == 5) &
                                    (all(x[-1] == 0 | x[-1] == 5) |
                                     all(x[-1] == 2 | x[-1] == 5) |
                                     all(x[-1] == 1 | x[-1] == 5)))
                               })                   

################################################################################
## Remaining valid SNPs to keep.
snp.record$Keep <- (snp.record$Keep &
                    (!is.na(snp.record$Segregating) &
                     !snp.record$HetParents &
                     !snp.record$NonSegMismatch &
                     !snp.record$SegMismatch &
                     snp.record$B6Match & snp.record$BTBRMatch & snp.record$F1Match))

################################################################################



################################################################################
## Change F2 genotypes to match parents, checking for errors.
## All 5 -> NA.
## Parents     F2s
## B6 F1 BTBR  0  1  2
## 0  0  0     4  5  6  non-segregating
## 2  2  2     6  5  4
## 0  1  2     1  2  3  autosomes
## 2  1  0     3  2  1
## 0  1  2     8  2  1  X female
## 2  1  0     1  2  8
## 0  1  2     1  7  2  X male
## 2  1  0     2  7  1
##
## 1  B6   (BTBR if female X)
## 2  Het  (BTBR if male X)
## 3  BTBR
## 4  non-segregating, match with parents
## 5  non-segregating, mismatch as het
## 6  non-segregating, mismatch as other homozygote
## 7  het on male X
## 8  B6 on female X

## Pull out F2 data.
is.f2 <- match(topstuff$Sample.Name[mouse.record$Strain == "F2"], names(snp)) - 8
snp.f2 <- as.matrix(snp[, 8 + is.f2])

## Save missing value spots.
tmp <- snp.f2 == 5
## Non-segregating.
snp.f2[B6 & BTBR, ] <- c(4,5,6,NA,NA,NA)[1 + snp.f2[B6 & BTBR, ]]
snp.f2[B62 & BTBR2, ] <- c(6,5,4,NA,NA,NA)[1 + snp.f2[B62 & BTBR2, ]]
## Segregating autosomes.
snp.f2[B6 & BTBR2 & !is.X, ] <- 1 + snp.f2[B6 & BTBR2 & !is.X, ]
snp.f2[B62 & BTBR & !is.X, ] <- 3 - snp.f2[B62 & BTBR & !is.X, ]
## Segregating X males.
is.male <- (mouse.record$Sex == "Male")[is.f2]
snp.f2[B6 & BTBR2 & is.X, is.male] <-
  c(1,7,2,NA,NA,NA)[1 + snp.f2[B6 & BTBR2 & is.X, is.male]]
snp.f2[B62 & BTBR & is.X, is.male] <-
  c(2,7,1,NA,NA,NA)[1 + snp.f2[B62 & BTBR & is.X, is.male]]
## Segregating X females.
snp.f2[B6 & BTBR2 & is.X, !is.male] <-
  c(8,2,1,NA,NA,NA)[1 + snp.f2[B6 & BTBR2 & is.X, !is.male]]
snp.f2[B62 & BTBR & is.X, !is.male] <-
  c(1,2,8,NA,NA,NA)[1 + snp.f2[B62 & BTBR & is.X, !is.male]]

## Pick up missing values here.
snp.f2[tmp] <- NA

## Find mice with too many errors.
count.5 <- apply(snp.f2[snp.record$Keep, ], 2, function(x) sum(x > 4 & x < 7, na.rm = TRUE))
count.7 <- apply(snp.f2[snp.record$Keep, ], 2, function(x) sum(x > 6, na.rm = TRUE))

tmp <- rep(0, nrow(mouse.record))
tmp[is.f2] <- count.5
mouse.record$NonSegMismatch <- tmp

##*** ALTER mouse.record$Keep.
mouse.record$Keep <- mouse.record$Keep & (mouse.record$NonSegMismatch < 900)

################################################################################
## NB: Still have some invalid X genotypes in F2.
## Check for SNPs and male F2 mice with het SNP (snp = 1).
tmp <- rep(0, nrow(snp.record))
tmp[snp.record$Keep] <- apply(snp.f2[snp.record$Keep, ], 1,
                              function(x) sum(x > 6, na.rm = TRUE))
snp.record$BadX <- tmp

tmp <- rep(0, nrow(mouse.record))
tmp[is.f2] <- count.7
mouse.record$BadX <- tmp

#############################################################################
##*** Mouse priority for use with Microarrays.
##
##   mouse.record$Array  Mice priority for microarrays.
##                       (avoid mice with Array > 3)
##     1.Best            no apparent problems  top priority
##     2.BadX            X mismatch            BadX > 3
##     3.LowCallRate     Affy call = 0\%        drop.calls == TRUE
##                                             (Experiment.Call.Rate.. < 10)
##     4.NonSegMismatch  several bad genotypes NonSegMismatch > 30
##                                             (mostly agees with drop.calls)
##     5.CageSex         caged with other sex  Mouse3174 and Mouse3277 and mates
##     6.BadGenos        many bad genotypes    NonSegMismatch > 900
##                                             (also DC1&DC2 > 200)
##     7.NoGenos         no genotypes          drop.repeats == TRUE
##                                             (Sample.Repeat.Count. != 1)
##     8.NotF2           parent lines, check   Strain != "F2"

mouse.record$Array <- rep("1.Best", nrow(mouse.record))
mouse.record$Array[mouse.record$BadX > 3] <- "2.BadX"
mouse.record$Array[mouse.record$drop.calls] <- "3.LowCallRate"
mouse.record$Array[mouse.record$NonSegMismatch > 30] <- "4.NonSegMismatch"
mouse.record$Array[mouse.record$MouseNum \%in\%
                   c("Mouse3174","Mouse3277","Mouse3173","Mouse3278")] <-
  "5.CageSex"
mouse.record$Array[mouse.record$NonSegMismatch > 900] <- "6.BadGenos"
mouse.record$Array[mouse.record$drop.repeats] <- "7.NoGenos"
mouse.record$Array[mouse.record$Strain != "F2"] <- "8.NotF2"
mouse.record$Array <- factor(mouse.record$Array)

mouse.record$Keep <- mouse.record$Keep & mouse.record$Array != "5.CageSex"
#############################################################################
rm(B6,B62,BTBR,BTBR2,count.5,double.mouse,drop.repeats)
rm(tmp,tmp2,tmpfn,is.f2,is.F1,is.B6,is.BTBR,is.parent,is.male,is.female,i)
rm(is.X,find.doublecross,double.snp)


######################################################################3
## Add Segregation Disorder to snp.record

## Make cross object.
## See qtl.R for complete build--this only has mouse.record phenotypes.
B6BTBR07 <- snp2cross(snp.f2[snp.record$Keep & snp.record$Segregating,
                             mouse.record$Keep[mouse.record$Strain == "F2"]],
                      mouse.record[mouse.record$Keep & mouse.record$Strain == "F2", ],
                      snp.record[snp.record$Keep & snp.record$Segregating, ])

library(qtl)
summary(B6BTBR07)

SegDisorder <- geno.table(B6BTBR07)
map <- pull.map(B6BTBR07)
SegDisorder <- cbind(chr = SegDisorder$chr, pos = unlist(map), SegDisorder[,-1])
class(SegDisorder) <- c("scanone","data.frame")
SegDisorder$chr <- ordered(SegDisorder$chr, c(1:19,"X"))
SegDisorder$logP.value <- -log10(SegDisorder$P.value)
row.names(SegDisorder) <- unlist(lapply(map, names))

tmp <- names(SegDisorder)
tmp[3:6] <- c("n.miss","n.AA","n.AB","n.BB")
tmp[10] <- "SegDisorder.logP"
names(SegDisorder) <- tmp
for(i in names(SegDisorder)[c(3:6,10)]) {
  tmp <- rep(NA, nrow(snp.record))
  tmp[match(row.names(SegDisorder), snp.record$External.Id)] <- SegDisorder[[i]]
  snp.record[[i]] <- tmp
}

####################################################################
## Build 36 for 4853 SNPs on Affy Mouse 5K kit.
## Affy Mouse 5k kit uses NCBI Mouse Build 32.
build36 <- read.csv("data/Build36.5k.csv", header = TRUE)

## Match Affy (Build 32) with Jax MouseBlast (Build 36).
tmp2 <- match(snp.record$External.Id,
              paste("rs", build36$snp, sep = ""), nomatch = 0)
sum(tmp2 == 0)
## Pick up SNPs that were merged.
tmp3 <- match(snp.record$External.Id,
              paste("rs", build36$snpold, sep = ""), nomatch = 0)
table(tmp2>0,tmp3>0)
tmp2[tmp2 == 0] <- tmp3[tmp2 == 0]
build36 <- build36[tmp2,]

## Extract Build 36 Chromosome Name and Position.
snp.record$Build36.Chrom <- ordered(build36$chr, c(1:19,"X"))
snp.record$Build36.Position <- build36$chr.pos * 10^-6
snp.record$Build36.SNP <- factor(paste("rs", build36$snp, sep = ""))
snp.record$Build36.SNPmerge <-
  as.character(snp.record$External.Id) != as.character(snp.record$Build36.SNP)

## Find target alleles that have changed
tmp2 <- (as.character(snp.record$Target.Allele) ==
         as.character(build36$alleles))
cbind(as.character(snp.record$Target.Allele),
      as.character(build36$alleles))[!tmp2,]
build36[!tmp2,]
snp.record$Build36.Alleles <- build36$alleles
snp.record$Build36.Allele.Change <- !tmp2

## Note misspelling of assembly!
snp.record$Build36.Assembly <- build36$assemply

## Solitary Segregation Distortion.
snp.record$Build36.SegDisSolo <- find.segdischr()

snp.record$Keep <- snp.record$Keep & snp.record$Build36.SegDisSolo < 1.9
snp.record$Keep <- snp.record$Keep & snp.record$DC < 90
######################################################################
## Pattern of crossovers.
## I think this is a better measure, but needs to be tested.
## Note that some large counts of Build36.CO for snp are now 0
## since those are for CO as half of DC.
tmp <- find.patternprob(snp.record, mouse.record, find.crosspat, c("CO","DC"),
                        build = 36)
snp.record$CO <- tmp$snp.COL + tmp$snp.COR
snp.record$DC <- tmp$snp.DC1 + tmp$snp.DCL + tmp$snp.DCR + tmp$snp.DD
for(i in names(tmp)[1:6])
  snp.record[[substring(i, 5)]] <- tmp[[i]]

mouse.record$CO <- mouse.record$DC1 <- mouse.record$DC2 <- NULL
for(i in c(1:19,"X"))
  mouse.record[[paste("CO", i, sep = ".")]] <- NULL
mouse.record$CO <- apply(as.matrix(tmp$mouse.CO), 1, sum)
mouse.record$DC1 <- apply(as.matrix(tmp$mouse.DC1), 1, sum)
mouse.record$DC2 <- apply(as.matrix(tmp$mouse.DC2), 1, sum)
mouse.record$DD <- apply(as.matrix(tmp$mouse.DD), 1, sum)
for(i in names(tmp)[7:9]) {
  names(tmp[[i]]) <- substring(names(tmp[[i]]), 7)
  mouse.record <- cbind(mouse.record, tmp[[i]])
}
#############################################################################
## Add Tissue QC status and priority for RNA data to mouse.record.
rna <- read.csv("data/RNA_QC_B6xBTBR_ALL_02Oct2007rk vMKBY.csv", header = TRUE,
                na.strings = "N/A")
tmp <- c("Liver.QC.Status","Islet.QC.Status","gWAT.QC.Status","Hypo.QC.Status","Gastroc.QC.Status")
rna$QC.Status <- factor(c("PASS","FAIL")[1 + apply(as.matrix(rna[, tmp]), 1,
                                                   function(x) any(x == "FAIL" | is.na(x)))])

tmp <- rep(NA, nrow(mouse.record))
tmp2 <- match(rna$MouseNum, mouse.record$MouseNum)
tmp[tmp2] <- as.character(rna$QC.Status)
mouse.record$QC.Status <- factor(tmp)

## Reorder columns of mouse.record.
tmp <- rep(NA, nrow(mouse.record))
tmp[tmp2] <- rna$count
mouse.record$QC.Priority <- tmp

tmp <- ncol(mouse.record)
mouse.record <- mouse.record[, c(1:5, tmp, tmp - 1, 6:(tmp - 2))]
#############################################################################
## Add Protein amount.
prot <- read.csv("data/Proteonomics Study Tubes.csv", header = TRUE,
                 na.strings = "-")
tmp <- match(paste(prot$Strain, "Mouse", prot$MouseNum, sep = ""),
             paste(mouse.record$Strain, mouse.record$MouseNum, sep = ""),
             nomatch = 0)
tmp2 <- rep(NA, nrow(mouse.record))
tmp2[tmp] <- prot$ul.of.plasma.remaining[tmp > 0]
mouse.record$Plasma.ul <- tmp2
table(mouse.record$Array, mouse.record$Sex, mouse.record$Plasma.ul >= 200,
      exclude = NULL)

mouse.record$Proteomic.OK <- (mouse.record$Array == "1.Best" &
                              mouse.record$QC.Priority < 503 &
                              mouse.record$Plasma.ul >= 200)
mouse.record$Proteomic.OK[is.na(mouse.record$Proteomic.OK)] <- FALSE
tmp <- rep(NA, nrow(mouse.record))
tmp2 <- mouse.record$Proteomic.OK & mouse.record$Sex == "Male"
tmp[tmp2] <- sample(sum(tmp2)) + 1000
tmp2 <- mouse.record$Proteomic.OK & mouse.record$Sex == "Female"
tmp[tmp2] <- sample(sum(tmp2)) + 2000
mouse.record$Random.Proteomic <- tmp

tmp <- ncol(mouse.record)
mouse.record <- mouse.record[, c(1:7, tmp, tmp - 1, tmp - 2, 8:(tmp-3))]

#############################################################################
## Jackson Labs evaluations of SNPs.
ding <- read.csv("data/Jax/4853Snps_sorted_flagged.csv", header = TRUE)
tmp <- match(snp.record$Build36.SNP, paste("rs", ding$snp, sep = ""))
snp.record$BadSNPJax <- ding$bad.snp.flag[tmp]
snp.record$Keep <- snp.record$Keep & snp.record$BadSNPJax != 1

#############################################################################
## Save objects as CSV files.

write.csv(mouse.record[order(-mouse.record$QC.Priority), ],
          "mouse.record.csv", row.names = FALSE)
write.csv(snp.record, "snp.record.csv", row.names = FALSE)
write.csv(snp.parents, "snp.parents.csv", row.names = FALSE)
}
}
\keyword{datasets}
