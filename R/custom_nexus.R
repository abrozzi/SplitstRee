#' @title Color SplitsTree dendrograms
#' @description Reads a nexus file and adds colors to the leaves
#' @param nexus.file
#' @param tips (leaves of the dendrogram)
#' @param colors vector of the same length of the tips indicating the color of each tip
#' @param vlabels vector of labels to be plot; leave empty character "" to hide the label.
#' @param f fonts of the labels. Like "Dialog-BOLD-16" or 'Courier-PLAIN-24'. Other option are: Calibri, Arial, Times. For bold -BOLD and not -PLAIN
#' @param lc vector of label colors
#' @param lk vector of background of the label
#' @param w width of the tip. If you indicate a single number it will be recycled. Either provide a vector of widths
#' @param h height of the tip. Same for w.
#' @param edgecolor the color of the edges
#' @param s shape of the tip "o" is oval. The other option is "r" rectangle.
#' @param plot if TRUE SplitsTree is launched and the nexus displayed
#' @param Splitstree.exe path to the executable of SplitsTree
#' @return a new nexus file with colors
#' @examples
#' inputfile <- file.path(path.package('microbio'),'input.nexus')
#' outfile <- file.path(path.package('microbio'),'output.nexus')
#' tips <- c("A","R","G","B")
#' col <- c("red","blue","green","yellow")
#' s <- c("r","o","r","o")
#' ws <- c(10,20,30,40)
#' hs <- c(10,20,30,40)
#' fg <- c("blue","orange", "violet", "gray8")
#' lc <- c("gray", "cyan", "pink", "blue")
#' lk <- c("black", "blue", "black", "yellow")
#' edgecolor <- "blue"
#' f <- c("Arial-BOLD-20", "Calibri-BOLD-16", "Times-20", "Dialog-BOLDITALIC-25")
#' custom_nexus(nexus.file=inputfile, tips=tips, colors=col, vlabels=tips, w=ws, f=f, h=hs, lc=lc, lk=lk, fg= fg, s=s, edgecolor=edgecolor, outfile=outfile, plot=TRUE, SplitsTree.exe = "/Applications/SplitsTree/SplitsTree")
#' @export
#'
custom_nexus <- function(nexus.file,
                             tips,
                             colors,
                             outfile,
                             fromBIGSdb=FALSE,
                             fg,
                             vlabels,
                             w=20,
                             h=20,
                             s="r",
                             f,
                             lc,
                             lk,
                         edgecolor="gray",
                             plot,
                             SplitsTree.exe="/Applications/SplitsTree/SplitsTree"
                             ) {

  if(file.exists(outfile)) {
    system(paste("rm", outfile))
  }

  file = readLines(nexus.file)
  startTRANS = match("TRANSLATE", file)
  ends = which(file == ";")
  endTRANS = ends[ends>startTRANS][1]

  map.df = get.nexus.tips(nexus.file=nexus.file, fromBIGSdb = fromBIGSdb)

if( length(tips)!=nrow(map.df) ) {
  stop("'tips' must be a character vector of the same length of taxa")
  }

if( any(is.na(match(tips, map.df$tips))) ) {
  stop("tips and taxa don't match")
  }

# set colors
if( !is.character(colors) ) {
  stop("'colors' must be a character vector of the same length of tips")
  }

if( length(tips)!=length(colors) ) {
  stop("'colors' must be a character vector of the same length of tips")
  }

map.df$color = colors[match(map.df$tips, tips)]
map.df$color = as.character(map.df$color)

# set valabels
map.df$vlabels =NA

if(length(vlabels) != length(tips)){stop("vlabels must be a character vector of the same length of tips.")}

map.df$vlabels = vlabels[match(map.df$tips, tips)]
map.df$vlabels = as.character(map.df$vlabels)

map.df$lc =NA
map.df$lc = lc[match(map.df$tips, tips)]
map.df$lc = as.character(map.df$lc)

map.df$lk =NA
map.df$lk = lk[match(map.df$tips, tips)]
map.df$lk = as.character(map.df$lk)

map.df$f =NA
map.df$f = f[match(map.df$tips, tips)]
map.df$f = as.character(map.df$f)

# set w and h fpr each node
map.df$w = NA
map.df$h = NA
map.df$s = NA
map.df$fg = NA

# set s
if (length(s)==1) {
  map.df$s = s
} else {

  map.df$s = s[match(map.df$tips, tips)]
  map.df$s = as.character(map.df$s)

}

# set w
if (length(w)==1) {
map.df$w = w
} else {

  map.df$w = w[match(map.df$tips, tips)]
  map.df$w = as.character(map.df$w)

}

# set h
if (length(h)==1) {
  map.df$h = h
} else {
  map.df$h = h[match(map.df$tips, tips)]
  map.df$h = as.character(map.df$h)
}

# set fg
if (length(fg)==1) {
  map.df$fg = fg
} else {
  map.df$fg = fg[match(map.df$tips, tips)]
  map.df$fg = as.character(map.df$fg)
}

start.VERTICES = match("VERTICES", file)
end.VERTICES = ends[ends>start.VERTICES][1]

start.VLABELS =  match("VLABELS", file)
end.VLABELS =  ends[ends>start.VLABELS][1]

start.EDGES =  match("EDGES", file)
end.EDGES =  ends[ends>start.EDGES][1]

ecol = paste(col2rgb(edgecolor), collapse=" ")

total = length(file)

progBar <- txtProgressBar(min = 0, max = total, style = 3)

for (i in 1:total){

  setTxtProgressBar(progBar, i)

  lineToPrint = file[i]

  if(i > (start.VERTICES) & i < (end.VERTICES) ) {

    bits = unlist(strsplit(lineToPrint," "))
    vertex_id = bits[1]

    pos = match(vertex_id, map.df$vertex_id)

    if(!is.na(pos)) {

      bg = paste(col2rgb(map.df$color[pos]), collapse=" ")
      fg = paste(col2rgb(map.df$fg[pos]), collapse=" ")

      tmp0 = paste(bits[1:3], collapse = " ")
      tmp0 = gsub(x=tmp0, pat=",", rep="")
      lineToPrint = paste0(tmp0, " w=",map.df$w[pos], " h=",map.df$h[pos], " s=",map.df$s[pos], " fg=", fg," bg=", bg,",")

    } else {
      tmp0 = paste(bits[1:3], collapse = " ")
      tmp0 = gsub(x=tmp0, pat=",", rep="")
      lineToPrint = paste0(tmp0, " fg=196 196 196 bg=196 196 196,")
    }

  } #closes VERTEX BLOCK


  ################### VLABELS

    if(i > (start.VLABELS) & i < (end.VLABELS) ) {

      pieces = assign.vfeatures(line=lineToPrint, DF=map.df)

       lineToPrint = paste(pieces, collapse=" ")

    }
  ################### EDGES

  if(i > (start.EDGES) & i < (end.EDGES) ) {

    ltp = gsub(x=lineToPrint, pat=",", rep="")
    ltp = paste(ltp, paste0("fg=",ecol,","))

    lineToPrint = ltp

  }

  cat(lineToPrint,"\n", file=outfile, append=TRUE,sep = "")




} # closes for()

if(plot) {

command = paste0(SplitsTree.exe, " -i ", outfile, " & ")
system(command)

}

}
