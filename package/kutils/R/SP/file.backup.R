##' Create a backup version of a file by renaming it.
##' 
##' Inserts the date-time of the most recent modification at the
##' end of the file name, before the extension.
##'
##' Return is the new file name that was created, using whatever
##' path information was provided in the file's original name. However,
##' the fullpath argument can be set to TRUE, so a path with the
##' full directory name will be created and returned.
##' 
##' @param name A character string for the name of the file.
##' @param fullpath Return the full directory path to the file. Default FALSE
##' @author Shadi P-H
##' @return The name of the newly created file.
##' @importFrom base format
##' @importFrom tools file_ext
##' @importFrom tools file_path_sans_ext
##' @export
##' @examples
##' tdir <- tempdir()
##' owd <- getwd()
##' 
##' setwd(tdir)
##' system("touch test.1.txt")
##' system("touch test.2.txt")
##' system("touch test.3.txt")
##' system("touch test.4.txt")
##' list.files()
##' file.backup("test.1.txt")
##' file.backup("test.2.txt", fullpath=TRUE)
##' list.files()
##' setwd(owd)
##' file.backup(file.path(tdir, "test.3.txt"))
##' ## Should be same path because input had a full path
##' file.backup(file.path(tdir, "test.4.txt"), fullpath=TRUE)
file.backup <- function(name, fullpath = FALSE){
  date_cr <- base::format(base::file.info(name)$mtime, "%Y%m%d-%H%M")
  ext_name <- tools::file_ext(name)
  noext_name <- tools::file_path_sans_ext(name)
  new_name <- paste0(noext_name, "-", date_cr, ".", ext_name)

  ## Abort if new file name already exists
  if (file.exists(new_name)){
      MESSG <- paste("file.backup did nothing: backup file already exists")
      warning(MESSG)
  }
  ret <- file.rename(name, new_name)
  if(!ret) {
      MESSG <- paste("file.rename(", name, ",", new_name, "failed")
      stop(MESSG)
  }
  if(fullpath){
      return(normalizePath(new_name))
  }
  ## else, give back adjusted with same path information it had
  new_name
}

# data  <- "pj4.txt"
# new_file <- f_rename(data)
