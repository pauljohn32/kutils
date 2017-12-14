
## 
##' Check if values can be safely coerced without introduction of
##' missing values
##'
##' This might be named "coercesSafely" or such.  If values cannot be
##' coerced into class specified, then values must be incorrect.
##' @param value Character vector of values, such as value_new or
##'     value_old for one variable in a key.
##' @param targetclass R class name
##' @param na.strings Values that should be interpreted as R NA.
##'     These are ignored in the coercion check.
##' @return either TRUE, or a vector of values which are not
##'     successfully coerced
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' x1 <- c("TRUE", "FALSE", FALSE, TRUE, NA, ".", "N/A", " ", "")
##' checkCoercion(x1, "logical")
##' x1 <- c(x1, "TRUE.FALSE", "Has a space")
##' ## Should fail:
##' checkCoercion(x1, "logical")
##' x2 <- c(4, 5, 6, 9.2, ".", " ")
##' ## Should fail
##' checkCoercion(x2, "logical")
##' x3 <- factor(c("bob", "emily", "bob", "jane", "N/A", " ", NA, "NA"))
##' checkCoercion(x3, "ordered")
##' checkCoercion(x3, "integer")
##' ## Should fail:
##' checkCoercion(x3, "logical")
##' 
checkCoercion <- function(value, targetclass,
                          na.strings = c("\\.", "", "\\s+",  "N/A") ){
    value.lab <-  deparse(substitute(value))
    ## previous error, which did not use regular expression
    ismissing <- grepl(paste0("^", paste0(na.strings, collapse="$|^"), "$"), value)
    value[ismissing] <- NA
    value <- na.omit(value)
    mytext <- paste0("as.", targetclass, "(value)")
    res <- eval(parse(text = mytext))
    if(any(is.na(res))){
        MESSG <- paste("checkValue: fails coercion of",
                       value.lab, "as", targetclass, "\n")
        cat(MESSG)
        return(value[is.na(res)])
    }
    TRUE
}
