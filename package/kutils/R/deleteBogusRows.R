##' Remove rows that are more than 90 percent NA
##'
##' Get rid of the rows that have more than 90 percent empty nonsense. Some
##' imported Excel sheets have "noise" in the last rows, which appear
##' as <NA> or NA in R.
##'
##' @param dframe A data frame
##' @param pm "proportion bogus data" to be tolerated.
##' @return a data frame, invisibly
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu>
deleteBogusRows <- function (dframe, pm = 0.9){
    rowna <- apply(dframe, 1, function(x){sum(is.na(x))})
    badrows <- rowna > pm * dim(dframe)[2]
    if (any(badrows)){
        cat(paste("deleteBogusRows Diagnostic\n"))
        cat(paste("These rows from the data frame: ", deparse(substitute(dframe)), "\n are being purged:"))
        print(rownames(dframe)[badrows])
        cat(paste("The bad content was\n"))
        print(dframe[badrows, ])
        newdframe <- dframe[!badrows,]
        return(newdframe)
    }
    print(paste("No bogus rows were found in: ", deparse(substitute(dframe))))
    invisible(dframe)
}
