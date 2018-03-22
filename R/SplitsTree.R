#' @export
#'
SplitsTree <- function(EXECUTE.FILE,
                       EXPORTGRAPHICS.format="JPG",
                       EXPORTGRAPHICS.file,
                       EXPORTGRAPHICS.replace="yes",
                       EXPORTGRAPHICS.textasshapes="no",
                       EXPORTGRAPHICS.title="SplitsTree",
                       EXPORT.file,
                       EXPORT.replace="yes",
                       EXPORT.format="Nexus",
                       pathToSplitsTree,
                       graphics=TRUE
                       ){

  if(file.exists("command.tmp")) {

    system("rm command.tmp")

  }

cat(file="command.tmp", "begin SplitsTree;","\n")

  cat(paste0("EXECUTE FILE=", EXECUTE.FILE,"\n"), file="command.tmp", append=TRUE)

  if(graphics){

    cat(paste0("EXPORTGRAPHICS format=", EXPORTGRAPHICS.format,
               " file=",                 EXPORTGRAPHICS.file,
               " REPLACE=",              EXPORTGRAPHICS.replace,
               " TEXTASSHAPES=",         EXPORTGRAPHICS.textasshapes,
               " TITLE=",                EXPORTGRAPHICS.title,
               "\n"
    ),
    file="command.tmp", append=TRUE
    )

  }

cat(paste0("EXPORT FILE=", EXPORT.file,
          " REPLACE=",     EXPORT.replace,
          " FORMAT=",      EXPORT.format,
          " DATA=all",
          "\n"
          ),
    file="command.tmp", append=TRUE
)

cat(paste0("QUIT", "\n"), file="command.tmp", append=TRUE)
cat(paste0("end;", "\n"), file="command.tmp", append=TRUE)

system(paste0(pathToSplitsTree, "./SplitsTree -g -c command.tmp"))

system(paste0("rm command.tmp"))

}
