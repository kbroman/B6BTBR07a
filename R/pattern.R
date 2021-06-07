############################################################################
best.pattern <- function(tissue = "liver", cross = B6BTBR07,
                         made = "~/p/private/diabetes3/diabetes30/best",
                         cross.data = "~/Rlib/B6BTBR07a/data",
                         home = "~/Rlib/eqtl/data/")
{
  ## Get raw files and create R objects.
  tissue.best <- read.table(file.path(made, tissue, "best.txt"),
                            header = FALSE, row.names = NULL)
  names(tissue.best) <- c("a_gene_id", "n.qtl", "chrom", "locus", "variance")
  table(floor(tissue.best$chrom))
  
  tissue.pattern <- read.table(file.path(made, tissue, "pattern.txt"),
                              header = FALSE, row.names = NULL)
  names(tissue.pattern) <- c("a_gene_id", "pattern", "posterior")

  ## Save objects in data area of cross workspace.
  cat("Saving", tissue, "best and pattern to cross workspace.\n")
  tmp <- paste(tissue, "best", sep = ".")
  assign(tmp, tissue.best)
  save(list = tmp, file = file.path(cross.data, paste(tissue, "best.RData", sep = ".")),
       compress = TRUE)
  tmp <- paste(tissue, "pattern", sep = ".")
  assign(tmp, tissue.pattern)
  save(list = tmp, file = file.path(cross.data, paste(tissue, "pattern.RData", sep = ".")),
       compress = TRUE)

  ## Turn into scan of attenuated patterns.
  write.csv(make.bestscan(cross, tissue.best, tissue.pattern),
            file = file.path(home, paste(tissue, "bestscan.csv", sep = ".")),
            quote = FALSE, row.names = TRUE)
  invisible(tissue.bestscan)
}
