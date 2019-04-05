#' @title read a cd-hit output
#' @description import clusters done by cd-hit
#' @export
#'
read_cdhit <- function(file.clstr, pattern=NULL) {

  cls = readLines(file.clstr)

  pos = grep(">Cluster ", cls)
  pos = c(pos, (length(cls)+1))
  ncl = length(pos)

  L = list()
  for ( i in 1:(ncl-1)) {

    L[[i]] = sapply(strsplit(cls[ (pos[i]+1) : (pos[i+1]-1) ], " "), function(x) x[2])


  }
  if(!is.null(pattern)) {
    L0 = sapply(L, function(x) gsub(pattern=pattern, x=x, rep="p_"))
    L1 = sapply(L0, function(x) gsub(pattern="\\.\\.\\.", x=x, rep=""))
  } else {
    L1 = sapply(L, function(x) gsub(pattern="\\.\\.\\.", x=x, rep=""))
  }

  return(L1)

}
