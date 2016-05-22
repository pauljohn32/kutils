
##' Reverse the levels in a factor
##'
##' simple literal reversal. No fancy checks on input. If there are
##' valid levels named "Skip" or "DNP", they will be moved back to the
##' end of the list.
##' @param x a factor variable
##' @param eol values to be kept at the end of the list
##' @export
##' @return a new factor variable with reversed values
##' @author Paul Johnson <pauljohn@@ku.edu>
reverse <- function(x, eol = c("Skip", "DNP")){
    rlevels <- rev(levels(x))
    if (length(eol) > 0){
        for (jj in eol){
            if (yyy <- grep(jj, rlevels)){
                rlevels <- c(rlevels[-yyy], jj)
            }
        }
    }

    y <- factor(x, levels = rlevels)
}
