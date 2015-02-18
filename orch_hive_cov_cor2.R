orch_hive_cov_cor2 <- function(terms, tabname, sample = TRUE, cor = FALSE, waitint = 3,
                                                       chunksize = 20)
{
  count <- 1
  outnames <- c()
  func <- ifelse(cor,"corr", ifelse(sample, "covar_samp","covar_pop"))
  l <- length(terms)
  resloc <- c()
  tmparr <- c()
  tab <- eval(parse(text=tabname))

  count <- 1

  cat("\n creating hive job files...")

  for (i in terms)
  {
     resloc[count] <- ORCHcore:::.orch.tmpfile()
     qry <- sprintf("insert overwrite local directory '%s' select %s from %s",
                     resloc[count], paste(func,"(",names(tab)[i],",",
                                       names(tab)[terms[count:l]],")",collapse=",",
                                       sep=""), tabname)
     tmp <- ORCHcore:::.orch.tmpfile(ext="R")
     tmpf <- ORCHcore:::.orch.open(tmp, "w", stop=T)
     cat(qry, file = tmpf)
     ORCHcore:::.orch.close(tmpf, stop=F)
     tmparr[count] <- tmp
     count <- count + 1
  }


  # submit jobs in chunks
  maxiter <-  as.integer(l/chunksize)

  if (maxiter == 0)
  {
    maxiter <- 1
    chunksize <- l
    remainder <- 0
  }
  else
    remainder <-  l%%chunksize

  cat("\nmaxiter=",maxiter,"chunksize=",chunksize,"remainder=",remainder,"\n")
 nextchunk <- 0

  existsf <- c()
  existsf[1:(count-1)] <- FALSE

  for (i in 1:(maxiter+1))
  {
    if(i==maxiter+1)
    {
     if(remainder == 0)
      break
     else
      chunksize <- remainder
    }

    chunk <- (nextchunk+1):(nextchunk+chunksize)
    nextchunk <- nextchunk+chunksize


    for ( j in chunk)
    {
      system(sprintf("hive -S -f %s &", tmparr[j]), intern=FALSE)
      cat("\n job ", j," submitted \n")
    }

    # polling
    while(!all(existsf[chunk] %in% TRUE))
    {
      Sys.sleep(waitint)
      for (i in chunk)
      {
       out <- ORCHcore:::.sh(sprintf("ls %s/000000_0", resloc[i]))
       if (ORCHcore:::.sh.error()==0)
        existsf[i] <- TRUE
      }

     cat("\nsome hive jobs still running....\n")
    }
  }

  cat("\nall hive jobs returned successfully\n")
 # get the results
  res <- list()
  for (i in 1:(count-1))
  {
     #tmpf <- ORCHcore:::.orch.open(paste(resloc[i],"/000000_0", sep=""), "r", stop=T)
     inprow <- readLines(paste(resloc[i],"/000000_0", sep=""),n=1)
     res[[i]] <- as.numeric(strsplit(inprow,'\001')[[1]])
     #ORCHcore:::.orch.close(tmpf, stop=F)
  }

  res <- unlist(res)
  covmat <- diag(l)
  covmat[lower.tri(covmat, diag=TRUE)] <- res
  covmat <- covmat + t(covmat) - diag(diag(covmat))
  rownames(covmat) <- colnames(covmat) <- names(tab)[terms]

  covmat
}
