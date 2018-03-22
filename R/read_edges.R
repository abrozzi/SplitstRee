#' @export
#'
read_edges <- function(file,fromBIGSdb = FALSE) {

  FILE = readLines(file)

  startEDGES = match("EDGES", FILE)
  ends = which(FILE == ";")
  endEDGES = ends[ends>startEDGES][1]

  slice = FILE[(startEDGES+1):(endEDGES-1)]

  slice.ls = strsplit(slice, " ")

  tab = do.call( rbind, slice.ls)

  tab[,4] = gsub(x=tab[,4], pat="s=", rep="")
  tab[,5] = gsub(x=tab[,5], pat="w=", rep="")
  tab[,5] = gsub(x=tab[,5], pat=",", rep="")

  df = data.frame(id = as.character(tab[,1]),
                  from = as.character(tab[,2]),
                  to = as.character(tab[,3]),
                  size = as.numeric(tab[,4]),
                  w = as.numeric(tab[,5]),
                  stringsAsFactors = FALSE)

  map.df = get.nexus.tips(nexus.file=file, fromBIGSdb = fromBIGSdb)

  for ( i in 1:nrow(map.df)) {

    df$from[df$from %in% map.df$vertex_id[i]] = map.df$tips[i]
    df$to[df$to %in% map.df$vertex_id[i]] = map.df$tips[i]

  }

  return(df)

}
