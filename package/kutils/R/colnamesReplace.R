##' Replace column names with new names from a named vector
##'
##' A convenience function to alter column names. Can be called from
##' code cleanup in the variable key system.
##' @param dat a data frame
##' @param newnames Can be a named vector of the form c(oldname1 =
##'     "newname1", oldname2 = "newname") or it may be simply
##'     c("newname1", "newname2") to correspond with the oldname
##'     vector.
##' @param oldnames Optional. If supplied, must be same length as
##'     newnames.
##' @param ... Additional arguments that will be passed to R's
##'     \code{gsub} function, which is used term-by-term inside this
##'     function.
##' @param lowercase Default FALSE. Should all column names be
##'     converted to lower case.
##' @param verbose Default FALSE. Want diagnostic output about column
##'     name changes?
##' @return a data frame
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' set.seed(234234)
##' N <- 200
##' mydf <- data.frame(x5 = rnorm(N), x4 = rnorm(N), x3 = rnorm(N),
##'                    x2 = letters[sample(1:24, 200, replace = TRUE)],
##'                    x1 = factor(sample(c("cindy", "bobby", "marsha",
##'                                         "greg", "chris"), 200, replace = TRUE)),
##'                    stringsAsFactors = FALSE)
##' mydf2 <- colnamesReplace(mydf, newnames = c("x4" = "GLOPPY"))
##' mydf2 <- colnamesReplace(mydf, newnames = c("x4" = "GLOPPY", "USA" = "Interesting"), verbose = TRUE)
##' colnames(mydf)
colnamesReplace <- function(dat, newnames, oldnames = NULL, ...,  lowercase = FALSE, verbose = FALSE){
    if (is.null(oldnames)) {
        if (is.null(names(newnames))){
            stop("The newname vector should be a named vector, using the oldnames as the names")
        }
        oldnames <- names(newnames)
    } else {
        if (length(newnames) != length(oldnames)){
            messg <- paste("newnames has", length(newnames), "elements")
            messg <- c(messg, paste("oldnames has", length(oldnames), "elements"))
            messg <- c(messg, "If oldnames is supplied, must be same length as newnames")
            stop(messg)
        }
        names(newnames) <- oldnames
    }
    namez <- colnames(dat)
    if (any(oldnames %in% namez)){
        newnamez <- mgsub(oldnames, newnames, colnames(dat), ...)
        colnames(dat) <- if (lowercase) tolower(newnamez) else newnamez
        if (verbose){
            cat(paste("colnamesReplace Diagnostic: These names replacements are requested:\n"))
            print(c("oldnames", "newnames"))
            print(cbind(oldnames, newnames ))
            cat(paste("These names were actually replaced", oldnames[oldnames %in% namez]))
        }
        return(dat)
    } else {
        cat(paste("replaceNames Diagnostic:  No column names were altered"))
        return(dat)
    }
}

