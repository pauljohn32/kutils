##' Delete trailing slash
##'
##' Windows file.exists fails if "/" is on end of file name
##' @param name A path
##' @return Same path with trailing "/" removed.
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu>
dts <- function(name) gsub("/$", "", dms(name))
NULL

##' Delete multiple slashes, replace with one
##'
##' Sometimes paths end up with "/too//many//slashes".
##' While harmless, this is untidy. Clean it up.
##' @param name A character string to clean
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu>
dms <- function(name) gsub("(/)\\1+", "/", name)
NULL


##' How many stars would we need for this p value?
##'
##' Regression table makers need to know how many stars
##' to attach to parameter estimates. This takes
##' p values and a vector which indicates how many stars
##' are deserved.  It returns a required number of asterixes.
##' Was named "stars" in previous version, but renamed due to
##' conflict with R base function \code{stars}
##'
##' Recently, we have requests for different symbols. Some people want
##' a "+" symbol if the p value is smaller than 0.10 but greater than
##' 0.05, while some want tiny smiley faces if p is smaller than
##' 0.001. We accomodate that by allowing a user specified vector of
##' symbols, which defaults to c("*", "**", "***")
##' @param pval P value
##' @param alpha alpha vector, defaults as c(0.05, 0.01, 0.001).
##' @param symbols The default is c("*", "**", "***"), corresponding
##'     to mean that p values smaller than 0.05 receive one star,
##'     values smaller than 0.01 get two stars, and so forth.  Must be
##'     same number of elements as alpha. These need not be asterixes,
##'     could be any character strings that users desire. See example.
##' @return a character vector of asterixes, same length as pval
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @export
##' @examples
##' starsig(0.06)
##' starsig(0.021)
##' starsig(0.001)
##' alpha.ex <- c(0.10, 0.05, 0.01, 0.001)
##' symb.ex <- c("+", "*", "**", ":)!")
##' starsig(0.07, alpha = alpha.ex, symbols = symb.ex)
##' starsig(0.04, alpha = alpha.ex, symbols = symb.ex)
##' starsig(0.009, alpha = alpha.ex, symbols = symb.ex)
##' starsig(0.0009, alpha = alpha.ex, symbols = symb.ex)
##' 
starsig <-
    function(pval, alpha = c(0.05, 0.01, 0.001),
             symbols = c("*", "**", "***"))
{
    if (length(alpha) != length(symbols)) {
        messg <- "alpha vector must have same number of elements as symbols vector"
        stop(messg)
    }
    nstars <- sapply(pval, function(x) sum(abs(x) < alpha))
    sapply(nstars, function(y) symbols[y])
}
NULL 


##' Remove elements if they are in a target vector, possibly replacing with NA
##'
##' If a vector has c("A", "b", "c") and we want to remove "b" and
##' "c", this function can do the work. It can also replace "b" and
##' "c" with the NA symbol.
##'
##' If elements in y are not members of x, they are silently ignored.
##' 
##' The code for this is not complicated, but it is
##' difficult to remember.  Here's the recipe to remove
##' elements y from x: \code{x <- x[!x \%in\% y[y \%in\% x]]}. It is
##' easy to get that wrong when in a hurry, so we use this function
##' instead.  The \code{padNA} was an afterthought, but it helps sometimes.
##' 
##' @param x vector from which elements are to be removed
##' @param y shorter vector of elements to be removed
##' @param padNA Default FALSE, Should removed items be replaced with NA values?
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


##' apply a vector of replacements, one after the other.
##'
##' This is multi-gsub.  Use it when it is necessary to process
##' many patterns and replacements in a given order on a vector.
##' 
##' @param pattern vector of values to be replaced. A vector filled
##'     with patterns as documented in the \code{gsub} pattern
##'     argument
##' @param replacement vector of replacements, otherwise same as
##'     \code{gsub}.  Length of replacement must be either 1 or same
##'     as pattern, otherwise an error results.
##' @param x the vector in which elements are to be replaced, same as
##'     \code{gsub}
##' @param ... Additional arguments to be passed to gsub
##' @return vector with pattern replaced by replacement
##' @author Jared Harpole <jared.harpole@@gmail.com> and Paul Johnson
##'     <pauljohn@@ku.edu>
##' @export
##' @examples
##' x <- c("Tom", "Jerry", "Elmer", "Bugs")
##' pattern <- c("Tom", "Bugs")
##' replacement <- c("Thomas", "Bugs Bunny")
##' (y <- mgsub(pattern, replacement, x))
##' x[1] <- "tom"
##' (y <- mgsub(pattern, replacement, x, ignore.case = TRUE))
##' (y <- mgsub(c("Elmer", "Bugs"), c("Looney Characters"), x, ignore.case = TRUE))
mgsub <- function(pattern, replacement, x, ... ){
    if (length(pattern) != length(replacement)) {
        if (length(replacement) == 1) {
            replacement <- rep(replacement, length(pattern))
        } else {
            messg <- paste("replacement must either be 1 element or the same number of elements as pattern")
            stop(messg)
        }
    }
    for (i in seq_along(pattern)){
        x <- gsub(pattern[i], replacement[i], x, ...)
    }
    x
}
    
