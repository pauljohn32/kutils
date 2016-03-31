##' Replace column names with new names from a named vector
##'
##' Imagine some beautiful writing here
##' @param dat a data frame
##' @param newname A named vector of the form c(oldname1 = "newname1", oldname2 = "newname")
##' @return a data frame
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu>
colnamesReplace <- function(dat, newname){
    if (is.null(names(newname))){
        stop("The newname vector should be a named vector, using the oldnames as the names")
    }
    oldname <- colnames(dat)
    if(any(newname[oldname] != oldname)){
        cat(paste("colnamesReplace Diagnostic: These names are to be replaced:\n"))
        res <- ifelse (oldname %in% names(newname), newname[oldname], oldname)
        print(c("oldname", "newname"))
        print(cbind(oldname, res))
        colnames(dat) <- tolower(res)
        return(dat)
    } else{
        cat(paste("replaceNames Diagnostic:  No column names were altered"))
        return(dat)
    }
}
