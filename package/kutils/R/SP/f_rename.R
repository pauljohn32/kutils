##' rename the file accroding to the creation time of the file 
##' Appends the date of creation to the file name.
##' 
##' @param name A character string for the name of the file with its extension.

f_rename <- function (name){
  library(tools)
  date_cr <- format(file.info(name)$mtime, "%Y%m%d-%H%M")
  ext_name <- file_ext(name)
  noext_name <- file_path_sans_ext(name)
  new_name <- paste0(noext_name,"-",date_cr,".",ext_name)
  return (file.rename(name,new_name))
}

# data  <- "pj4.txt"
# new_file <- f_rename(data)
