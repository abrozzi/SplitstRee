#' @title assign font, font color, background color to the label of a node
#' @description There are two cases: A and B. Example of a line is "77225 'p_524, p_650' x=-87 y=377 lc=0 255 0,"
#' (A) when there is only one label associate to a vertex_id the cases are only 2:
#'     (1) the vlab associated in the map.df is not an empty character
#'     (2) the vlab associated in the map.df is an empty character
#' for:
#'     (1) the syntax is VERTEXID VLAB X Y F LC LK
#'     (2) the syntax is VERTEXID VLAB X Y F LC
#' (B) when there are more than one label associate to a vertex_id the cases are:
#'    (1) vlabs are all empty
#'    (2) only one is not empty
#'    (3) more than one are not empty
#'        (3.1) the f, LC and LK are all equal
#'        (3.2) the f, LC and LK are not-all equal
#' @param line
#' @param dataframe with the features to add
#' @return a new line edited according to features
#' @export
#'
assign.vfeatures <- function(line, DF) {

# init.

  bits = unlist(strsplit(line," "))
  atomic = unlist(strsplit(line,""))
  limits = which(atomic %in% "'")
  start = limits[1]
  end = limits[2]
  vlab.orig = paste(atomic[(start+1):(end-1)],collapse="")
  vlab.orig = unlist(strsplit(vlab.orig,","))
  vertex_id = bits[1]
  x = bits[grep("x=", bits)]
  x = gsub(x=x, pat=",", rep="")
  y = bits[grep("y=", bits)]
  y = gsub(x=y, pat=",", rep="")

  entry = DF[DF$vertex_id %in% vertex_id, ]

  # case A
  if (nrow(entry) == 1) {

    vlab = entry$vlabels
    lc = paste(col2rgb(entry$lc), collapse=" ")
    lk = paste(col2rgb(entry$lk), collapse=" ")
    f = entry$f

    # case A.1
    if(nchar(vlab)!=0) {
      pieces = c(vertex_id, paste0("'",vlab,"'"), x, y, paste0("f='",f,"'"), paste0("lc=", lc), paste0("lk=", lk, ","))

    # case A.2
    } else {
      pieces = c(vertex_id, paste0("'",vlab,"'"), x, y, paste0("f='",f,"'"), paste0("lc=", lc,","))
    }

  # case B
  } else {

    vlab = entry$vlabels

    # case B.1
    if( sum(nchar(vlab)==0) == length(vlab) ) {

      f = unique(entry$f)
      lc = unique(entry$lc)

      pieces = c(vertex_id, paste0("'","'"), x, y, paste0("f='",f,"'"), paste0("lc=", lc,","))

    }

    # case B.2
    if( sum(nchar(vlab)>0)  == 1 ) {

      notempty = nchar(vlab)!=0

      vlab = entry$vlabels[notempty]
      lc = paste(col2rgb(entry$lc[notempty]), collapse=" ")
      lk = paste(col2rgb(entry$lk[notempty]), collapse=" ")
      f = entry$f[notempty]

      pieces = c(vertex_id, paste0("'",vlab,"'"), x, y, paste0("f='",f,"'"), paste0("lc=", lc), paste0("lk=", lk, ","))

    # case B.3
    }

    if( sum(nchar(vlab)>0)  > 1 ) {

      notempty = nchar(entry$vlabels)!=0

      entry = entry[notempty]

      # case B.3.1
  if ( nrow (unique(entry[,c("color", "f", "lc", "lk")])) ==1 ) {

    vlab = paste(entry$vlabel,", ")
    lc = paste(col2rgb(entry$lc[1]), collapse=" ")
    lk = paste(col2rgb(entry$lk[1]), collapse=" ")
    f = entry$f[1]

    pieces = c(vertex_id, paste0("'",vlab,"'"), x, y, paste0("f='",f,"'"), paste0("lc=", lc), paste0("lk=", lk, ","))

      # case B.3.2
  } else {

    vlab = paste(entry$vlabels,collapse=", ")
    f = "Dialog-BOLD-16"
    lc = paste(col2rgb("gray"), collapse=" ")

    pieces = c(vertex_id, paste0("'",vlab,"'"), x, y, paste0("f='",f,"'"), paste0("lc=", lc,","))

  }

    }

  }

return(pieces)

}
