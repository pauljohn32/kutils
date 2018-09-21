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
##' @param fullpath Return the full directory path to the
##'     file. Default FALSE, return only the file name.
##' @param keep.old If FALSE (default), rename the file. Otherwise, keep old copy.
##' @param verbose If TRUE, list the files in the output directory
##' @author Shadi Pirhosseinloo <shadi@@ku.edu> Paul Johnson <pauljohn@@ku.edu>
##' @return The name of the newly created file.
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
##' system("touch test.5.txt")
##' ## note: no extension next
##' system("touch test.6")
##' list.files()
##' file.backup("test.1.txt")
##' file.backup("test.2.txt", fullpath=TRUE)
##' list.files()
##' setwd(owd)
##' file.backup(file.path(tdir, "test.3.txt"))
##' ## Next should be same path because input had a full path
##' file.backup(file.path(tdir, "test.4.txt"), fullpath=TRUE)
##' file.backup(file.path(tdir, "test.5.txt"), fullpath = TRUE, verbose = TRUE)
##' file.backup(file.path(tdir, "test.6"))
file.backup <- function(name, fullpath = FALSE, keep.old = FALSE, verbose = FALSE){
    if(!file.exists(name)){
        MESSG <- paste("file", name, "does not exist. No backup created.")
        warning(MESSG)
        return(NULL)
    }
    dir.source <- dirname(normalizePath(name))
    
    date_cr <- base::format(base::file.info(name)$mtime, "%Y%m%d-%H%M")
    ext_name <- tools::file_ext(name)
    noext_name <- tools::file_path_sans_ext(name)
    new_name <- paste0(noext_name, "-", date_cr,
                       if(!ext_name == "") {paste0( ".", ext_name)})
   
    ## Abort if new file name already exists
    if (file.exists(new_name)){
        MESSG <- paste("backup file already exists. No new backup created.")
        warning(MESSG)
        return(new_name)
    }
    ret <- if(keep.old){
               file.copy(name, new_name, recursive = TRUE, overwrite = TRUE,
                         copy.mode = TRUE, copy.date = TRUE)
           }else{
               file.rename(name, new_name)
           }
    if(!ret) {
        MESSG <- paste("file.rename(", name, ",", new_name, "failed")
        stop(MESSG)
    }
    if(verbose){
        cat("Full file list of directory:", dir.source, "\n")
        print(list.files(dir.source))
        cat("End of file list\n")
    }
    if(fullpath){
        new_name_fullpath <- normalizePath(new_name)
        return(new_name_fullpath)
    }
    
    ## else, give back adjusted with same path information it had
    new_name
}

