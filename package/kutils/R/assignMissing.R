
##' Scrub a variable's missings away
##'
##' The missings have to be carefully written out in the key file, or
##' else this fails
##' @param dfn A data frame
##' @param varname Which column name is being cleaned?
##' @param missings A vector representing missing values. These are
##' done differently for numeric and factor variables in the key file.
##' @return A (hopefully) cleaned column of data
##' @author Paul Johnson
assignMissing <- function(dfn, varname, missings){
    if (is.factor(dfn[ , varname]) | is.character(dfn[ , varname])){
        mystring <- paste0("dfn[dfn[, ", "\"", varname, "\"] %in% ",  missings, ", \"", varname, "\"", "] <- NA")
        eval(parse(text = mystring))
        avar <- factor(dfn[ , varname])
        return(avar)
    } else {
        avar <- eval(parse(text = paste0("dfn[ , \"",  varname,"\"]")))
        conditional <- paste(quote(avar), missings)
        avarcheck <- eval(parse(text = conditional))
        avar[avarcheck] <- NA
        return(avar)
    }
}
 
