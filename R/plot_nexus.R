#' Plot the network
#'
#' \code{plot_nexus} plot the network
#'
#' @param obj
#' @param defaults is a list with names: col, bg, cex, pch, text, arrange
#' @param arrange can be one of the two: "back" or "front"
#' @param col.bg is the color of the box of the text
#' @return plot
#' @author Alessandro Brozzi
#' @examples
#' nex = import.nexus(myfile.nexus)
#' plot_nexus(nex)
#'
#' @export
#'
#'
plot_nexus <- function(obj, taxa=NULL, bg, col, cex, pch, text, arrange, col.bg, defaults, ylim=NULL, xlim=NULL, main=NULL, lty=1, lwd=1, col.segments="gray") {

  DF = data.frame(
    X = obj$VERTICES$X[match(obj$TRANSLATE$VID, obj$VERTICES$VID)],
    Y = obj$VERTICES$Y[match(obj$TRANSLATE$VID, obj$VERTICES$VID)],
    LABEL = obj$TRANSLATE$LABEL,
    stringsAsFactors = FALSE
  )

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

  segments(x0 = obj$FROM$X, y0 = obj$FROM$Y, x1 = obj$TO$X, y1 = obj$TO$Y, col=col.segments, lty = lty, lwd = lwd)

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
       col=DF$col,
       bg = DF$bg,
       cex=DF$cex,
       pch=DF$pch,
       ylim=ylim,
       xlim=xlim,
       ylab="",
       xlab="",
       axes=FALSE,
       main=main
  )

  segments(x0 = obj$FROM$X, y0 = obj$FROM$Y, x1 = obj$TO$X, y1 = obj$TO$Y, col=col.segments, lty = lty, lwd = lwd)


}

}
