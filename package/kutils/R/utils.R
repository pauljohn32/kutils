##' Delete trailing slash
##'
##' Windows file.exists fails if "/" is on end of file name
##' @param name A path
##' @return Same path with trailing "/" removed.
##' @export 
##' @author Paul Johnson <pauljohn@@ku.edu>
dts <- function(name) gsub("/$", "", dms(name))

##' Delete multiple slashes, replace with one
##'
##' Sometimes paths end up with "/too//many//slashes".
##' While harmless, this is untidy. Clean it up.
##' @param name A character string to clean
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu>
dms <- function(name) gsub("(/)\\1+", "/", name)


##' return a floored version of a variable
##'
##' Checks that variable exists in data frame
##' @param dat a data frame
##' @param varname a variable name
##' @return The "floored column". sets variable type to integer
##' @export
##' @author Paul Johnson
floorvar <- function(dat, varname){
    if (!varname %in% colnames(dat)){
        stop("age is not a variable in this dataframe.")
    } else {
        newvar <- floor(dat[ , varname])
    }
    as.integer(newvar)
}



##' How many stars would we need for this p value
##'
##' Regression table makers need to know how many stars
##' to attach to parameter estimates. This takes
##' p values and returns a required number of asterixes.
##' Was named "stars" in previous version, but renamed due to
##' conflict with R base function stars
##' @param pval P value
##' @param alpha alpha vector
##' @return a character vector of asterixes, same length as pval 
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @export
##' @examples
##' starsig(0.06)
##' starsig(0.021)
##' starsig(0.001)
starsig <- function(pval, alpha = c(0.05, 0.01, 0.001)) {
    ##xxx <- sum(abs(pval) < alpha)
    ## handle a vector of pvals
    nstars <- sapply(pval, function(x) sum(abs(x) < alpha))
    ## paste0("", rep("*", xxx), collapse = "")
    sapply(nstars, function(y) paste0("", rep("*", y), collapse = ""))
}
 


##' remove elements if they are in a target set
##'
##' If a vector has c("A", "b", "c") and we want
##' to remove "b" and "c", this function can do the work.
##'
##' If elements in y are not members of x, they are silently
##' ignored.
##' 
##' The code for this is not complicated, but it is
##' difficult to remember.  If you don't want to
##' use the function, here's the recipe to remove
##' elements y from x: \code{x <- x[!x \%in\% y[y \%in\% x]]}
##' 
##' @param x vector from which elements are to be removed
##' @param y shorter vector of elements to be removed
##' @param padNA: Default FALSE, Should removed items be replaced with NA values?
##' @return a vector with elements in y removed
##' @author Ben Kite <bakite@@ku.edu> and Paul Johnson <pauljohn@@ku.edu>
##' @export
##' @examples
##' x <- c("a", "b", "c", "d", "e", "f")
##' y <- c("e", "a")
##' removeMatches(x, y)
##' y <- c("q", "r", "s")
##' removeMatches(x, y)
removeMatches <- function(x, y, padNA = FALSE){
    if (padNA) {
        x[x %in% y] <- NA
    } else {
        x <- x[!x %in% y[y %in% x]]
    }
    x
}
