
##' Reverse the levels in a factor
##'
##' Simple literal reversal. Will stop with an error message if x is
##' not a factor (or ordered) variable.
##'
##' Sometimes people want to
##' reverse some levels, excluding others and leaving them at the end
##' of the list. The "eol" argument sets aside some levels and puts
##' them at the end of the list of levels.
##'
##' The use case for the \code{eol} argument is a factor
##' with several missing value labels, as appears in SPSS. With
##' up to 18 different missing codes, we want to leave them
##' at the end. In the case for which this was designed, the
##' researcher did not want to designate those values as
##' missing before inspecting the pattern of observed values.
##' 
##' @param x a factor variable
##' @param eol values to be kept at the end of the list
##' @export
##' @return a new factor variable with reversed values
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' ## Consider alphabetication of upper and lower
##' x <- factor(c("a", "b", "c", "C", "a", "c"))
##' levels(x)
##' xr1 <- reverse(x)
##' xr1
##' ## Keep "C" at end of list, after reverse others
##' xr2 <- reverse(x, eol = "C")
##' xr2
##' y <- ordered(x, levels = c("a", "b", "c", "C"))
##' yr1 <- reverse(y)
##' yr1
##' ## Hmm. end of list amounts to being "maximal".
##' ## Unintended side-effect, but interesting.
##' yr2 <- reverse(y, eol = "C")
##' yr2
reverse <- function(x, eol = c("Skip", "DNP")){
    if (!is.factor(x)) stop("your variable is not a factor")
    rlevels <- rev(levels(x))
    if (length(eol) > 0){
        for (jj in eol){
            if (length(yyy <- grep(jj, rlevels))){
                rlevels <- c(rlevels[-yyy], jj)
            }
        }
    }
    factor(x, levels = rlevels)
}
