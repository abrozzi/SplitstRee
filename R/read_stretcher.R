#' @export
#'
read_stretcher <- function(file) {

  RES = list()

  f1 = system(paste0("grep '1: ' ", file), intern = TRUE)
  f2 = system(paste0("grep '2: ' ", file), intern = TRUE)
  f1 = gsub(x=f1, pat="# 1: ",rep="")
  f2 = gsub(x=f2, pat="# 2: ",rep="")

  id = system(paste0("grep Identity: ", file), intern = TRUE)

  id = unlist(strsplit(id, " "))
  id = rev(id)[1]
  id = gsub(x=id, pat="\\(", rep="")
  id = as.numeric(gsub(x=id, pat="\\%\\)", rep=""))

  sim = system(paste0("grep Similarity: ", file), intern = TRUE)

  sim = unlist(strsplit(sim, " "))
  sim = rev(sim)[1]
  sim = gsub(x=sim, pat="\\(", rep="")
  sim = as.numeric(gsub(x=sim, pat="\\%\\)", rep=""))

  RES[["f1"]] = f1
  RES[["f2"]] = f2
  RES[["id"]] = id
  RES[["sim"]] = sim

  return(RES)

}
