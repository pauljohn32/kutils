##' First draft of function to diagnose problems in merges and key variables
##'
##' This is a first effort. It works with 2 data frames and 1 key
##' variable in each. It does not work if the by parameter includes
##' more than one column name (but may work in future). The return is
##' a list which includes full copies of the rows from the data frames
##' in which trouble is observed.
##' @param x data frame
##' @param y data frame
##' @param by Commonly called the "key" variable. A column name to be
##'     used for merging (common to both \code{x} and \code{y})
##' @param by.x Column name in \code{x} to be used for merging. If not
##'     supplied, then \code{by.x} is assumed to be same as \code{by}.
##' @param by.y Column name in \code{y} to be used for merging. If not
##'     supplied, then \code{by.y} is assumed to be same as \code{by}.
##' @param incomparables values in the key (by) variable that are
##'     ignored for matching. We default to include these values as
##'     incomparables: c(NULL, NA, NaN, Inf, "\\s+", ""). Note this is
##'     a larger list of incomparables than assumed by R merge (which
##'     assumes only NULL). 
##' @return A list of data structures that are displayed for keys and
##'     data sets.  The return is \code{list(keysBad, keysDuped,
##'     unmatched)}. \code{unmatched} is a list with 2 elements, the
##'     unmatched cases from \code{x} and \code{y}.
##' @export
##' @author Paul Johnson
##' @examples
##' df1 <- data.frame(id = 1:7, x = rnorm(7))
##' df2 <- data.frame(id = c(2:6, 9:10), x = rnorm(7))
##' mc1 <- mergeCheck(df1, df2, by = "id")
##' ## Use mc1 objects mc1$keysBad, mc1$keysDuped, mc1$unmatched
##' df1 <- data.frame(id = c(1:3, NA, NaN, "", " "), x = rnorm(7))
##' df2 <- data.frame(id = c(2:6, 5:6), x = rnorm(7))
##' mergeCheck(df1, df2, by = "id")
##' df1 <- data.frame(idx = c(1:5, NA, NaN), x = rnorm(7))
##' df2 <- data.frame(idy = c(2:6, 9:10), x = rnorm(7))
##' mergeCheck(df1, df2, by.x = "idx", by.y = "idy")
mergeCheck <- function(x, y, by, by.x = by, by.y = by,
                       incomparables = c(NULL, NA, NaN, Inf, "\\s+", "")) {

    ## copied from R base merge function
    fix.by <- function(by, df) {
        if (is.null(by)) 
            by <- numeric()
        by <- as.vector(by)
        nc <- ncol(df)
        if (is.character(by)) {
            poss <- c("row.names", names(df))
            if (any(bad <- !charmatch(by, poss, 0L))) 
                stop(ngettext(sum(bad), "'by' must specify a uniquely valid column", 
                              "'by' must specify uniquely valid columns"), 
                  domain = NA)
            by <- match(by, poss) - 1L
        }
        else if (is.numeric(by)) {
            if (any(by < 0L) || any(by > nc)) 
                stop("'by' must match numbers of columns")
        }
        else if (is.logical(by)) {
            if (length(by) != nc) 
                stop("'by' must match number of columns")
            by <- seq_along(by)[by]
        }
        else stop("'by' must specify one or more columns as numbers, names or logical")
        if (any(bad <- is.na(by))) 
            stop(ngettext(sum(bad), "'by' must specify a uniquely valid column", 
                          "'by' must specify uniquely valid columns"), 
                 domain = NA)
        by.integer <- unique(by)
        colnames(df)[by.integer]
    }

    if(missing(by)){
        if(missing(by.x) || missing(by.y)) {
            MESSG <- "Must specify a column name in by or both by.x and by.y"
        }
    }
    
    by.x <- fix.by(by.x, x)
    by.y <- fix.by(by.y, y)
    
    #get data frame name
    label1 <- deparse(substitute(x))
    label2 <- deparse(substitute(y))
    
    dlist <- list(x, y)
    names(dlist) <- c(label1, label2)

    bylist <- list(by.x, by.y)
    names(bylist) <- c(label1, label2)
    
    keyEmpty <- function(df, keyName){
        df2 <- df[ df[ , keyName] %in% incomparables, , drop = FALSE]
        if(NROW(df2) > 0) return(df2) else return(NULL)
    }
    
    keyNotUnique <- function(df, keyName){
        dupes <-  df[df[ , keyName] %in% df[duplicated(df[ , keyName]), keyName], , drop = FALSE]
        if(NROW(dupes) > 0) return(dupes) else return(NULL)
    }

    keysBad <- list()
    for(i in names(dlist)) keysBad[[i]] <- keyEmpty(dlist[[i]], keyName = bylist[[i]])
    keysDuped <- list()
    for(i in names(dlist)) keysDuped[[i]] <- keyNotUnique(dlist[[i]], keyName = bylist[[i]])

    unmatched <- list()
    unmatched[[label1]] <- x[is.na(match(x[ , bylist[[label1]]], y[ , bylist[[label2]]])), , drop = FALSE]
    unmatched[[label2]] <- y[is.na(match(y[ , bylist[[label2]]], x[ , bylist[[label1]]])), , drop = FALSE]
   
    #create a list structure as a returned value
    res <- list(keysBad = keysBad, keysDuped = keysDuped, unmatched = unmatched)
    attr(res, "bylist") <- bylist
    class(res) <- "keycheck"
    ## if any results are not NULL, yell at user
    if(any(unlist(lapply(res, function(yyy) lapply(yyy, function(zz) length(zz) > 0))))){
        MESSG <- "Merge difficulties detected\n\n"
        cat(MESSG)
        print(res)
    }
    invisible(res)
}




##' Print out the result of mergeCheck function.
##'
##' This is a placeholder for a more elaborate print method
##' to be prepared in the future. Please advise us what might
##' be most helpful.
##' @param x keycheck output from mergeCheck 
##' @param ... Other arguments
##' @return None, side effect if print to screen
##' @method print keycheck
##' @export
##' @author Paul Johnson
print.keycheck <- function(x, ...){
    if (length(x$keysBad) > 0){
        MESSG <- paste("Unacceptable key values\n")
        cat(MESSG)
        lapply(names(x$keysBad), function(yy){
            cat(paste(yy, "\n"))
            print(x$keysBad[[yy]])})
    }
    if (length(x$keysDuped) > 0){
        MESSG <- paste("Duplicated key values\n")
        cat(MESSG)
        lapply(names(x$keysDuped), function(yy){
            cat(paste(yy, "\n"))
            print(x$keysDuped[[yy]])})
        }
    MESSG <- paste("Unmatched cases from", paste(names(x$unmatched), collapse = " and "), ":\n")
    cat(MESSG)
    lapply(names(x$unmatched), function(yy) {
        cat(paste(yy, "\n"))
        print(x$unmatched[[yy]])
    })
}
