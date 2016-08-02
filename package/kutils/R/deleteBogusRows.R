##' Remove rows in which the proportion of missing data exceeds
##' a threshold.
##'
##' If cases are mostly missing, delete them. It often happens that
##' when data is imported from other sources, some noise rows exist at
##' the bottom of the input.  Anything that is missing in more than,
##' say, 90\% of cases is probably useless information.  We invented
##' this to deal with problem that MS Excel users often include a
##' marginal note at the bottom of a spread sheet.
##'
##' @param dframe A data frame or matrix
##' @param pm "proportion missing data" to be tolerated.
##' @param drop Default FALSE: if data frame result is reduced to one
##'     row, should R's default drop behavior "demote" this to a
##'     column vector.
##' @param verbose Default TRUE. Should a report be printed
##'     summarizing information to be delted?
##' @param n Default 25: limit on number of values to print in verbose
##'     diagnostic output. If set to NULL or NA, then all of the
##'     column values will be printed for the bogus rows.
##' @return a data frame, invisibly
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' mymat <- matrix(rnorm(10*100), nrow = 10, ncol = 100,
##'                dimnames = list(1:10, paste0("x", 1:100)))
##' mymat <- rbind(mymat, c(32, rep(NA, 99)))
##' mymat2 <- deleteBogusRows(mymat)
##' mydf <- as.data.frame(mymat)
##' mydf$someFactor <- factor(sample(c("A", "B"), size = NROW(mydf), replace = TRUE))
##' mydf2 <- deleteBogusRows(mydf, n = "all")
deleteBogusRows <- function (dframe, pm = 0.9, drop = FALSE,
                             verbose = TRUE, n = 25){
    if (is.null(n) || is.na(n)) n <- NCOL(dframe)
    if (n > NCOL(dframe)) n <- NCOL(dframe)
    rowna <- apply(dframe, 1, function(x){sum(is.na(x))})
    badrows <- rowna > pm * NCOL(dframe)
    if (any(badrows)){
        if (verbose){
            cat(paste("deleteBogusRows Diagnostic\n"))
            cat(paste("These rows from the data frame: ",
                      deparse(substitute(dframe)), "\n are being purged: "))
            cat(paste(which(badrows), "\n"))
            cat(paste("The bad content was\n"))
            print(dframe[badrows, 1:n, drop = drop])
        }
        dframe <- dframe[!badrows, , drop = drop]
        return(dframe)
    }
    print(paste("No bogus rows were found in: ", deparse(substitute(dframe))))
    invisible(dframe)
}


##' Remove columns in which the proportion of missing data exceeds
##' a threshold.
##'
##' This is a column version of \code{deleteBogusRows}. Use the pm
##' argument to set the proportion of missing required before a column
##' is flagged for deletion
##' 
##' @param dframe A data frame or matrix
##' @param pm "proportion missing data" to be tolerated.
##' @param drop Default FALSE: if data frame result is reduced to one
##'     column, should R's default drop behavior "demote" this to a
##'     column vector.
##' @param verbose Default TRUE. Should a report be printed
##'     summarizing information to be delted?
##' @param n Default 25: limit on number of values to print in
##'     diagnostic output. If set to NULL or NA, then all of the
##'     column values will be printed for the bogus rows.
##' @return a data frame, invisibly
##' @seealso \code{deleteBogusRows}
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu>
deleteBogusColumns <- function (dframe, pm = 0.9, drop = FALSE,
                                verbose = TRUE, n = 25)
{
    if (is.null(n) || is.na(n)) n <- NROW(dframe)
    if (n > NROW(dframe)) n <- NROW(dframe)
    colna <- apply(dframe, 2, function(x){sum(is.na(x))})
    badcols <- colna > pm * NROW(dframe)
    if (any(badcols)){
        if (verbose){
            cat(paste("deleteBogusColumns Diagnostic\n"))
            cat(paste("These columns from the data frame: ", deparse(substitute(dframe)), "\n are being purged: "))
            cat(paste(colnames(dframe)[badcols], "\n"))
            cat(paste("The bad content was:\n"))
            print(dframe[1:n, badcols, drop = drop])
        }
        newdframe <- dframe[, !badcols, drop = drop]
        return(newdframe)
    }
    print(paste("No bogus columns were found in: ", deparse(substitute(dframe))))
    invisible(dframe)
}


## TODO: make deleteBogus a generic, then write a method for data
## frames and a method for data tables.
