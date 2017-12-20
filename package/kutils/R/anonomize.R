##' Create unique anonymous id values
##'
##' Obscure participant id values by replacing them with "anon-1" and
##' so forth.
##'
##' Caution: the true "confidential" names are used as names in the
##' output vector
##' @param x A column of "confidential" names, possibly with repeats
##' @param prefix Character string to use as prefix in result.
##'     Default is "anon"
##' @return Named character vector of anonymized id names.
##' @author Paul Johnson <pauljohn@@ku.edu>
##' x <- c("bill", "bob", "fred", "bill")
##' (anonomize(x, prefix = "id"))
anonomize <- function(x, prefix = "anon"){
    xunique <- unique(x)
    nameunique <- paste0(prefix, "-", seq_along(xunique))
    names(nameunique) <- xunique
    nameunique[x]
}
