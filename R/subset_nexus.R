#' @title Subset a nexus file
#' @description Reads a nexus file and the tips one wants to keep. It create a nexus file with the selected tips
#' @param nexus.file
#' @param tips
#' @return a nexus file with subset distant matrix
#' @export
#'
subset_nexus <- function(nexus.file, tips, outfile, fromBIGSdb=FALSE){

  if(file.exists(outfile)) {
    system(paste("rm", outfile))
  }

  file = readLines(nexus.file)
  start.MATRIX = match("MATRIX", file)
  ends = which(file == "END;")
  end.MATRIX = ends[length(ends)]-1

  L = list()
  ids=c()

  for(i in (start.MATRIX+1):(end.MATRIX-1)) {

  id=NA

  line = unlist(strsplit(file[i],"\\\t"))

  if(fromBIGSdb){

    id = unlist(strsplit(line,"\\|"))[2]
    ids = c(ids, id)

  } else {

    id=line[1]
    ids = c(ids, id)

  }

 L[[id]]=line[-1]

  }

n = length(L)

A = matrix(NA,ncol=n,nrow=n)

for(i in 1:length(L)) {
  columns = length(L[[i]])
  A[i, 1:columns] = L[[i]]
}

rownames(A) = ids
colnames(A) = ids

A_hat1 = A[rownames(A) %in% tips,]
A_hat2 = A_hat1[,colnames(A_hat1) %in% tips]

cat("#NEXUS","\n", file=outfile, append=TRUE,sep = "")

cat("BEGIN taxa;","\n", file=outfile, append=TRUE,sep = "")
dimensions = paste0("DIMENSIONS ntax = ", length(tips),";")
cat(dimensions,"\n", file=outfile, append=TRUE,sep = "")
cat("END;","\n", file=outfile, append=TRUE,sep = "")

cat("BEGIN distances;","\n", file=outfile, append=TRUE,sep = "")
cat(dimensions,"\n", file=outfile, append=TRUE,sep = "")
cat("FORMAT","\n", file=outfile, append=TRUE,sep = "")
cat("triangle=LOWER","\n", file=outfile, append=TRUE,sep = "")
cat("diagonal","\n", file=outfile, append=TRUE,sep = "")
cat("labels","\n", file=outfile, append=TRUE,sep = "")
cat("missing=?","\n", file=outfile, append=TRUE,sep = "")
cat(";","\n", file=outfile, append=TRUE,sep = "")
cat("MATRIX","\n", file=outfile, append=TRUE,sep = "")

cat("Print file","\n")

total = nrow(A_hat2)
progBar <- txtProgressBar(min = 0, max = total, style = 3)

for (i in 1:total){

  total = length(file)

  progBar <- txtProgressBar(min = 0, max = total, style = 3)

lineToPrint = c(rownames(A_hat2)[i], A_hat2[i,])

lineToPrint = lineToPrint[!is.na(lineToPrint)]
lineToPrint = paste(lineToPrint, collapse="\t")
cat(lineToPrint,"\n", file=outfile, append=TRUE,sep = "")

}

cat(";","\n", file=outfile, append=TRUE,sep = "")
cat("END;","\n", file=outfile, append=TRUE,sep = "")

} # closes fun
