compmap <- function(news, olds, chr = 16, tol = 1)
{
  comp16 <- data.frame(newpos = news[news[,1] == chr, 2])
  comp16$newname <- dimnames(news)[[1]][news[,1] == chr]
  oldpos <- olds[olds[,1] == chr, 2]
  oldname <- dimnames(olds)[[1]][olds[,1] == chr]
  
  tmp <- match(oldname, comp16$newname, nomatch = 0)
  comp16$oldpos <- rep(NA, nrow(comp16))
  comp16$oldpos[tmp] <- oldpos[tmp > 0]
  comp16$oldname <- rep(NA, nrow(comp16))
  comp16$oldname[tmp] <- oldname[tmp > 0]
  
  tmp2 <- apply(as.matrix(oldpos[tmp == 0]), 1, function(x,y) which.min(abs(x-y)), comp16$newpos)
  comp16$matchpos <- rep(NA, nrow(comp16))
  comp16$matchname <- rep(NA, nrow(comp16))
  comp16$matchpos[tmp2] <- oldpos[tmp == 0]
  comp16$matchname[tmp2] <- oldname[tmp == 0]
  
  tmp3 <- apply(as.matrix(oldpos), 1, function(x,y) which.min(abs(x-y)), comp16$newpos)
  ## Kludge
  for(i in seq(length(tmp3) - 1)) {
    if(tmp3[i] == tmp3[i+1])
      tmp3[i+1] <- tmp3[i+1] + 1
  }
  comp16$allpos <- rep(NA, nrow(comp16))
  comp16$allname <- rep(NA, nrow(comp16))
  comp16$allpos[tmp3] <- oldpos
  comp16$allname[tmp3] <- oldname
  
  ## Now idea is to say that allname is actually a match for newname
  comp16$diff <- as.numeric((comp16$newname != comp16$allname) & !is.na(comp16$allname))
  comp16$posdiff <- comp16$newpos - comp16$allpos

  out <- comp16[comp16$diff == 1,]
  tmp <- max(abs(out$posdiff))
  if(tmp > tol)
    warning(paste("warning: map mismatch of", tmp))
  out <- out[, c("newname", "allname", "newpos", "posdiff")]
  dimnames(out)[[2]] <- c("newname","oldname","pos","offset")
  
  out
}
