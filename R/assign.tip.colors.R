#' @title Assign colors to tips
#' @description Given a tree of class "phylo" and a named vector with a category, returns a vector of colors to color the tips on the plots
#' @param tree a tree object of class "phylo"
#' @param tip2category a named vector. Each entry of the vector is a level and each name is a tip name.
#' @return a list of two entries: colors with the vector of colors and legend with a vector associating color to levels. Legend is useful to add the legned to the plot.
#' @examples
#' require(ape)
#' ### From Saitou and Nei (1987, Table 1):
#' x <- c(7, 8, 11, 13, 16, 13, 17, 5, 8, 10, 13,
#' 10, 14, 5, 7, 10, 7, 11, 8, 11, 8, 12,
#' 5, 6, 10, 9, 13, 8)
#' M <- matrix(0, 8, 8)
#' M[lower.tri(M)] <- x
#' M <- t(M)
#'M [lower.tri(M)] <- x
#' dimnames(M) <- list(1:8, 1:8)
#' tr <- nj(M)
#' ### Suppose that tips 1 to 4 are h.sapiens, 5 and 6 are m.musculs, 7 is NA and 8 is unassigned.
#' tip2category = c(rep(c("h.sapiens","m.musculs"),c(4,2)), NA)
#' names(tip2category) = 1:7
#' colors = assign.tip.colors(tr, tip2category, na.col="black", unassigned.col="gray")[["colors"]]
#' legenda = assign.tip.colors(tr, tip2category, na.col="black", unassigned.col="gray")[["legend"]]
#' plot(tr, "u", tip.color=colors, cex=2)
#' legend("bottomleft", legenda, pch=20, col=names(legenda))
#' @export
#'
assign.tip.colors = function(tree, tip2category, na.col="black", unassigned.col="gray", palette=NULL){
  require(plotrix)

  #tip2category = as.character(tip2category)

  if(!any(class(tree)=="phylo")){stop("tree must be an object of class phylo")}

  if(is.null(names(tip2category))) {stop("tip2category must be a character vector with names")}

  if(na.col==unassigned.col) {stop("na and unassigned colors must be different")}

  N = length(tree$tip.label)

  cols= rep(unassigned.col,N)
  cols[is.na(tip2category)]=na.col

  levels = unique(tip2category[!is.na(tip2category)])
  n = length(levels)

  if(is.null(palette)) {

    palette = rainbow(n)

      if(any(na.col==palette) | any(unassigned.col==palette)) {
        col.pal= sapply(palette,color.id)
        col.vec = unlist(col.pal)
        names(col.vec) = rep(names(col.pal), sapply(col.pal, length))
        stop(paste("na.col or unassigned.col are in conflict with one of the colors in use: ", paste(col.vec, collapse=" ")) )
  }
  }

  names(levels) = palette

  for (i in 1:n) {

    names = names(which(tip2category==levels[i]))
    cols[tree$tip.label%in% names] = palette[i]
  }

  legend=c(levels,"NA")
  names(legend)[legend=="NA"] = na.col

  legend=c(legend,"unassigned")
  names(legend)[legend=="unassigned"] = unassigned.col

  result = list(colors=cols, legend=legend)

  return(result)

}
