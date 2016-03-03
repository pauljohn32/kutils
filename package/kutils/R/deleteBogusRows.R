
##' Remove rows that are more than 90% NA
##'
##' Some imported Excel sheets have "noise" in the last
##' rows, which appear as <NA> or NA in R. Get rid of them.
##' @title
##' @param dframe
##' @return
##' @author Paul Johnson
deleteBogusRows <- function (dframe){
    rowna <- apply(dframe, 1, function(x){sum(is.na(x))})
    badrows <- rowna > .9 * dim(dframe)[2]
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
