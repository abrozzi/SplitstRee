#' @export
#'
read_splits <- function(file) {

  file = readLines(file)

  startTAXA = match("TAXLABELS", file)
  ends = which(file == ";")
  endTAXA = ends[ends>startTAXA][1]

  slice = file[(startTAXA+1):(endTAXA-1)]

  slice.ls = strsplit(slice, " ")

  tab = do.call( rbind, slice.ls)

  tab[,1] = gsub(x=tab[,1], pat="\\[", rep="")
  tab[,1] = gsub(x=tab[,1], pat="\\]", rep="")
  tab[,2] = gsub(x=tab[,2], pat="'", rep="")

  splits.pos = match("BEGIN Splits;", file)
  matrix.pos = grep("MATRIX", file)

  slice.sp = file[ (matrix.pos[matrix.pos>splits.pos][1]+1) : (ends[ends>splits.pos][1]-1) ]

  slice.sp.ls = strsplit(slice.sp,"\t")

  SPLITS = list()

  for (i in 1:length(slice.sp.ls)) {

item = slice.sp.ls[[i]]

u = unlist(strsplit(item[1]," "))
index = u[1]
index = gsub(x=index, rep="", pat=",")
index = gsub(x=index, rep="", pat="\\[")

size = u[2]
size = unlist(strsplit(size,"size="))[2]
size = gsub(x=size, rep="", pat="\\]")

w = gsub(x=item[2],pat=" ", rep="")

nodes = unlist(strsplit(x=item[3], " "))
nodes = gsub(x=nodes, rep="", pat=",")
nodes = nodes[nchar(nodes)>0]

labels = tab[match(nodes, tab[,1]),2]

SPLITS[[i]] = list(index=index, size=size, w=w, nodes=nodes, labels=labels)

}

  return(SPLITS)

}
