##' delete trailing slash
##'
##' Windows file.exists fails if "/" is on end of file name
##' @param name A path
##' @return Same path with trailing "/" removed.
##' @author Paul Johnson
dts <- function(name) gsub("/$", "", name)



##' return a floored version of a variable
##'
##' Checks that variable exists in data frame
##' @param dat a data frame
##' @param varname a variable name
##' @return The "floored column". sets variable type to integer
##' @author Paul Johnson
floorvar <- function(dat, varname){
    if (!varname %in% colnames(dat)){
        stop("age is not a variable in this dataframe.")
    } else {
        newvar <- floor(dat[ , varname])
    }
    as.integer(newvar)
}
