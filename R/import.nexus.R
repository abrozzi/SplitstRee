#' Import the network from a nexus file produced by SplitsTree
#'
#' \code{import.nexus} reads and import into R the chunks of a nexus file: TRANSLATE, VERTICES and EDGES
#'
#' @param nexus the name of a nexus file
#'
#' @return a list of TRANSLATE, VERTICES, EDGES and FROM-TO
#' @author Alessandro Brozzi
#' @examples
#' nex = import.nexus(myfile.nexus)
#' str(nex)
#'
#' @export
#'
#'
import.nexus <- function(nexus) {

  z = readLines(nexus)

  #semiColumn = system(paste("grep -nr ';'", nexus), intern=TRUE)
  #semiColumn = as.numeric(sapply(strsplit(semiColumn,":"), function(x) x[2]))

  semiColumn = grep(";", z)

  ##### TRANSLATE

  cat("Import TRANSLATE", "\n")

  # sTRA = system(paste("grep -nr TRANSLATE", nexus), intern=TRUE)

  # start = as.numeric(unlist(strsplit(sTRA,":"))[2])

  start = grep("TRANSLATE", z)

  end = semiColumn[semiColumn>start][1]

  a0 = scan(nexus, skip = start, nlines = (end-start)-1, what="", sep = "\n", quiet = TRUE)

  a1 = strsplit(a0, " ")
  labs = sapply(a1, function(x) x[2:length(x)])
  n = sapply(labs, length)
  a2 = data.frame(VID = rep(sapply(a1, function(x) x[1]),n), LABEL = unlist(labs), stringsAsFactors = FALSE)
  a2$LABEL = gsub(x=a2$LABEL, pat="[,']", rep="", perl=TRUE)
  TRANS = a2

  ##### VERTICES

  cat("Import VERTICES", "\n")

  # sVE = system(paste("grep -nr VERTICES", nexus), intern=TRUE)

  # start = as.numeric(unlist(strsplit(sVE,":"))[2])

  start = grep("VERTICES", z)

  end = semiColumn[semiColumn>start][1]

  a0 = scan(nexus, skip = start, nlines = (end-start)-1, what="", sep = "\n", quiet = TRUE)

  a1 = strsplit(a0, " ")
  a2 = as.data.frame(do.call(rbind, a1), stringsAsFactors = FALSE)
  colnames(a2) = c("VID","X","Y")
  a2$Y = gsub(x=a2$Y, pat=",", rep="", perl=TRUE)

  a2$X = as.numeric(a2$X)

  # y axis in SplitsTree is the opposite:
  a2$Y = - as.numeric(a2$Y)

  VERT = a2

  #### EDGES

  cat("Import EDGES", "\n")

  # sED = system(paste("grep -nr EDGES", nexus), intern=TRUE)

  # start = as.numeric(unlist(strsplit(sED,":"))[2])

  start = grep("EDGES", z)

  end = semiColumn[semiColumn>start][1]

  a0 = scan(nexus, skip = start, nlines = (end-start)-1, what="", sep = "\n", quiet = TRUE)
  a1 = strsplit(a0," ")

  a1.1 = lapply(a1, function(x) x[1:3])
  a1.1 = do.call(a1.1, what="rbind")

  a1.2 = lapply(a1, function(x) x[4:length(x)])

  A = list()

  for ( k in 1:length(a1.2)) {
    s = NA
    item = a1.2[[k]]
    s.idx = grep("s=", item)
    if(length(s.idx)>0) {s = gsub(item[s.idx], pat="s=", rep="")}

    w = NA
    w.idx = grep("w=", item)
    if(length(w.idx)>0) {
      w = gsub(item[w.idx], pat="w=", rep="")
      w = gsub(w, pat=",", rep="")
      w = as.numeric(w)
    }

    fgc = c("255 0 0")
    fgc.idx = grep("fgc=", item)
    if(length(fgc.idx)>0) {
      fgc0 = gsub(item[fgc.idx], pat="fgc=", rep="")
      fgc1 = fgc0
      fgc2 = gsub(item[fgc.idx+1], pat=",", rep="")
      fgc3 = gsub(item[fgc.idx+2], pat=",", rep="")

      fgc=paste(fgc1, fgc2, fgc3)
    }

  A[[k]] = c(s,w,fgc)
  }

  A1 = cbind(
    a1.1,
    do.call(A, what="rbind")
  )

  A1           = as.data.frame(A1)
  colnames(A1) = c("ID", "FROM", "TO", "s", "w", "fgc")

  ED = A1

  FROM = VERT[match(ED$FROM, VERT$VID),c("X","Y")]

  TO = VERT[match(ED$TO, VERT$VID),c("X","Y")]

  cat("Finished.", "\n")

  RES = list(VERTICES=VERT, TRANSLATE = TRANS, EDGES=ED, FROM=FROM, TO=TO)

  return(RES)
}

