library(B6BTBR07)
data(B6BTBR07)

## Identify clinical phenotypes to map.
keep <- qtlmult:::mytrait(, B6BTBR07$pheno)
keep <- keep[-(1:4)]
keep <- keep[-grep("date", tolower(keep))]
keep <- keep[-grep("id$", tolower(keep))]
keep <- keep[-match("IL.3..Interleukin.3.",keep)]
keep <- keep[-match("Array",keep)]
keep <- keep[-grep("^DC[12].",keep)]
keep <- keep[-grep("Keep",keep)]
keep <- keep[-grep("batch",keep)]

table(sapply(B6BTBR07$pheno[,keep], class))

tmp2 <- sapply(B6BTBR07$pheno[,keep], class)
keep[tmp2 == "factor"]

## Get needed data.
data(B6BTBR07)
data(B6BTBR07.source)

## Drop extra markers added by hand.
B6BTBR07 <- drop.markers(B6BTBR07, unlist(sapply(pull.map(angie), names)))
B6BTBR07 <- calc.genoprob(B6BTBR07, step = 2, stepwidth = "variable")

## Run through all traits.
B6BTBR07.scanone.clinical <-
  qtlmult:::mult.scanone(keep, normal.scores = TRUE)

## Save as CSV for Intranet.
write.csv(t(B6BTBR07.scanone.clinical[,-(1:2)]),
          file = "~/Rlib/eqtl/data/clinicallod.csv",
          quote = FALSE)

## Males
B6BTBR07.scanone.clinicalM <-
  qtlmult:::mult.scanone(keep, normal.scores = TRUE, sex = "male")
write.csv(t(B6BTBR07.scanone.clinicalM[,-(1:2)]),
          file = "~/Rlib/eqtl/data/clinicallodM.csv",
          quote = FALSE)
## Females
B6BTBR07.scanone.clinicalF <-
  qtlmult:::mult.scanone(keep, normal.scores = TRUE, sex = "female")
write.csv(t(B6BTBR07.scanone.clinicalF[,-(1:2)]),
          file = "~/Rlib/eqtl/data/clinicallodF.csv",
          quote = FALSE)

##save("B6BTBR07.scanone.clinical",
##     file = "B6BTBR07/data/B6BTBR07.scanone.clinical.RData",
##     compress = TRUE)

#######################################################################
## Raw data
tmp <- B6BTBR07$pheno[, keep]
tmp$QC.Status <- 1 * (tmp$QC.Status == "PASS")
tmp2 <- sapply(tmp, class)
tmp2 <- names(tmp2[tmp2 == "logical"])
for(i in tmp2)
  tmp[[i]] <- 1 * tmp[[i]]
row.names(tmp) <- as.character(B6BTBR07$pheno$MouseNum)
write.csv(t(tmp), file = "~/Rlib/eqtl/data/B6BTBR07.pheno.csv", quote = FALSE)

#######################################################################
## Check for skewness. Skip this now in favor of normal scores.
library(moments)
skew <- sapply(B6BTBR07$pheno[,keep],
               function(x) ifelse(is.numeric(x) & sum(!is.na(x)) > 50 &
                            diff(range(as.numeric(x), na.rm = TRUE)) > 0,
                                  skewness(x, na.rm = TRUE), 10))
skewlog <- sapply(B6BTBR07$pheno[,keep],
               function(x) ifelse(is.numeric(x) & sum(!is.na(x)) > 50 &
                            diff(range(as.numeric(x), na.rm = TRUE)) > 0,
                                  skewness(log10(x + 0.001), na.rm = TRUE), 10))

log10 <- ((skewlog < 10) & (abs(skew) > 4))
plot.it <- FALSE
if(plot.it) {
  plot(skew,skewlog)
  abline(v=c(-4,4), h = c(-4,4))
  abline(0,1)
  abline(0,-1)
  points(skew[log10], skewlog[log10], col = "red")
}
#######################################################################
