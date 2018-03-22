#' @title Get tips from dendrogram
#' @description Reads a nexus file and returns the tips
#' @param nexus.file
#' @return a dataframe with tips and verteces index
#' @examples
#' inputfile <- file.path(path.package('microbio'),'input.nexus')
#' get.nexus.tips(nexus.file=inputfile)
#' @export
#'
get.nexus.tips <- function(nexus.file,fromBIGSdb=FALSE){

  file = readLines(nexus.file)
  startTRANS = match("TRANSLATE", file)
  ends = which(file == ";")
  endTRANS = ends[ends>startTRANS][1]

  map=list()

  for(i in (startTRANS+1):(endTRANS-1)) {
    line = file[i]
    item0 = unlist(strsplit(line," "))

    key = item0[1]

    values0 = item0[2:length(item0)]

    values1 = sapply(values0, function(x) gsub(x=x, pat=",", rep=""))
    values2 = sapply(values1, function(x) gsub(x=x, pat="'", rep=""))

    if(fromBIGSdb){
      values3 = sapply(values2, function(x) unlist(strsplit(x,"\\|"))[2])
    } else {
      values3 = values2
    }

    map[[key]] = values3

  }

  map.df = data.frame(vertex_id = rep(names(map), sapply(map, length)), tips=unlist(map), stringsAsFactors = FALSE)
  map.df$vertex_id = as.character(map.df$vertex_id)
  rownames(map.df)=NULL

  return(map.df)

}
