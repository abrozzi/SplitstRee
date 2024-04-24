#' Plot the network
#'
#' \code{plot_nexus} plot the network
#'
#' @param obj
#' @param taxa is a vector of tips you want to format. Tips are format based of defaults set by list defaults. If a subset of tips must be reformat provide a vector of tips and relative bg, col, cex, pch, text, col.bg.
#' @param defaults is a list with names: col, bg, cex, pch, text, arrange. Remember that "bg" = fill color and "col" = contour color. pch from 21 to 25 allow contour (col) and fill (bg) color
#' @param arrange can be one of the two: "back" or "front"
#' @param col.bg is the color of the box of the text
#' @param lwd is line width of the border of the points (tips)
#' @param lwd.seg is line width of the segments (edges)
#' @param bg is the fill color of the tips
#' @param col is the contour color of the tips
#' @param cex is the size of the tips
#' @param pch is the type of symbol of the tips. Please use numbers from 21 to 25 to color the fill and the contour.
#' @param text is the text you want to print in each box of each tip.
#' @return plot
#' @author Alessandro Brozzi
#' @examples
#' def = list(col="blue", bg="orange", cex=5, pch=21, text=NA, arrange="back")
#' nex = import.nexus(myfile)
#' plot_nexus(nex, defaults=def, lwd = 3, lwd.seg = 1, col.bg="white")
#' # format tip A and B
#' plot_nexus(nex, taxa = c("A", "B"), pch = c(22,22), bg = c("red", "red"), col=c("black", "black"),cex=c(3,3), text=c("A","B"), arrange="front", defaults=def, lwd = 3, lwd.seg = 1, col.bg="white")
#' @export
#'
#'
plot_nexus <- function(obj, taxa=NULL, bg, col, cex, pch, text, arrange, col.bg, defaults, ylim=NULL, xlim=NULL, main=NULL, lwd = 3, lty.seg=1, lwd.seg=1, col.segments="gray") {

  DF = data.frame(
    X = obj$VERTICES$X[match(obj$TRANSLATE$VID, obj$VERTICES$VID)],
    Y = obj$VERTICES$Y[match(obj$TRANSLATE$VID, obj$VERTICES$VID)],
    LABEL = obj$TRANSLATE$LABEL,
    stringsAsFactors = FALSE
  )

  ## xlim AND ylim

  if (is.null(xlim)) {
    something = abs(diff(range(DF$X))/2)

    xlim = c(range(DF$X)[1] - something,
             range(DF$X)[2] + something
    )
  }

  if (is.null(ylim)) {
    something = abs(diff(range(DF$Y)/2))
  ylim = c(range(DF$Y)[1] - something,
           range(DF$Y)[2] + something
  )
  }

DF$col = defaults$col
DF$bg = defaults$bg
DF$cex = defaults$cex
DF$pch = defaults$pch
DF$text = defaults$text
DF$arrange = defaults$arrange

if( !is.null(taxa)) {

  DF[match(taxa, DF$LABEL),"col"] = col
  DF[match(taxa, DF$LABEL),"bg"] = bg
  DF[match(taxa, DF$LABEL),"cex"] = cex
  DF[match(taxa, DF$LABEL),"pch"] = pch
  DF[match(taxa, DF$LABEL),"text"] = text
  DF[match(taxa, DF$LABEL),"arrange"] = arrange

  DF.b = DF[DF$arrange=="back", ]

  plot(DF.b$X,
       DF.b$Y,
       col=DF.b$col,
       bg = DF.b$bg,
       cex=DF.b$cex,
       pch=DF.b$pch,
       type="n",
       ylim=ylim,
       xlim=xlim,
       ylab="",
       xlab="",
       axes=FALSE,
       main=main
  )

  segments(x0 = obj$FROM$X, y0 = obj$FROM$Y, x1 = obj$TO$X, y1 = obj$TO$Y, col=col.segments, lty = lty.seg, lwd = lwd.seg)

  points(DF.b$X,
         DF.b$Y,
         col=DF.b$col,
         bg = DF.b$bg,
         cex=DF.b$cex,
         pch=DF.b$pch,
         ylim=ylim,
         xlim=xlim,
         ylab="",
         xlab=""
  )

  DF.f =  DF[DF$arrange=="front", ]

  points(DF.f$X,
         DF.f$Y,
         col=DF.f$col,
         bg=DF.f$bg,
         cex=DF.f$cex,
         pch=DF.f$pch,
         ylim=ylim,
         xlim=xlim,
         ylab="",
         xlab=""
  )

  idx = which(!is.na(DF$text))

  if(length(idx>0)) {

    r = col2rgb(col.bg)[1] / 255
    g = col2rgb(col.bg)[2] / 255
    b = col2rgb(col.bg)[3] / 255

    boxtext(x = DF$X[idx], y = DF$Y[idx], labels = DF$text[idx], col.bg = rgb(r,g,b,alpha=0.5), pos = 4, padding = 0.3, cex = 1.5)

  }

} else {

  plot(DF$X,
       DF$Y,
       col = DF$col,
       bg  = DF$bg,
       cex = DF$cex,
       pch = DF$pch,
       ylim = ylim,
       xlim = xlim,
       ylab = "",
       xlab = "",
       axes = FALSE,
       main = main,
       lwd = lwd
  )

  segments(x0 = obj$FROM$X, y0 = obj$FROM$Y, x1 = obj$TO$X, y1 = obj$TO$Y, col=col.segments, lty = lty.seg, lwd = lwd.seg)

  r = col2rgb(col.bg)[1] / 255
  g = col2rgb(col.bg)[2] / 255
  b = col2rgb(col.bg)[3] / 255

  boxtext(x = DF$X, y = DF$Y, labels = DF$LABEL, col.bg = rgb(r,g,b,alpha=0.5), pos = 4, padding = 0.3, cex = 2)

}

}
