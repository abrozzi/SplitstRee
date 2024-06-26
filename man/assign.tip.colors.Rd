% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assign.tip.colors.R
\name{assign.tip.colors}
\alias{assign.tip.colors}
\title{Assign colors to tips}
\usage{
assign.tip.colors(
  tree,
  tip2category,
  na.col = "black",
  unassigned.col = "gray",
  palette = NULL
)
}
\arguments{
\item{tree}{a tree object of class "phylo"}

\item{tip2category}{a named vector. Each entry of the vector is a level and each name is a tip name.}
}
\value{
a list of two entries: colors with the vector of colors and legend with a vector associating color to levels. Legend is useful to add the legned to the plot.
}
\description{
Given a tree of class "phylo" and a named vector with a category, returns a vector of colors to color the tips on the plots
}
\examples{
require(ape)
### From Saitou and Nei (1987, Table 1):
x <- c(7, 8, 11, 13, 16, 13, 17, 5, 8, 10, 13,
10, 14, 5, 7, 10, 7, 11, 8, 11, 8, 12,
5, 6, 10, 9, 13, 8)
M <- matrix(0, 8, 8)
M[lower.tri(M)] <- x
M <- t(M)
M [lower.tri(M)] <- x
dimnames(M) <- list(1:8, 1:8)
tr <- nj(M)
### Suppose that tips 1 to 4 are h.sapiens, 5 and 6 are m.musculs, 7 is NA and 8 is unassigned.
tip2category = c(rep(c("h.sapiens","m.musculs"),c(4,2)), NA)
names(tip2category) = 1:7
colors = assign.tip.colors(tr, tip2category, na.col="black", unassigned.col="gray")[["colors"]]
legenda = assign.tip.colors(tr, tip2category, na.col="black", unassigned.col="gray")[["legend"]]
plot(tr, "u", tip.color=colors, cex=2)
legend("bottomleft", legenda, pch=20, col=names(legenda))
}
