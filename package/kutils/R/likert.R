


##' Percentage tables for Likert Scale variables
##'
##' Creates a table with columns for allowed values and rows for
##' variables.
##' @param data A data frame. Function will try to include all
##'     variables in data, unless vlist is provided.
##' @param vlist A vector of column names in data that should be
##'     displayed
##' @param columnlabels Column labels, optional to beautify variable
##'     names. If not supplied, column names will be used as column
##'     labels. Provide either 1) A named vector that replaces one or
##'     more columns, \code{c(oldname1 = "newlabel1", oldname2 =
##'     "newlabel2")} where oldnames are in colnames(data), or 2) a
##'     vector of same length as vlist (or data if vlist is not
##'     supplied) that will replace them one for one.
##' @param valuelabels A vector of values to beautify existing
##'     levels. If not supplied, factor levels will be used as row
##'     labels
##' @param outdir Output directory
##' @param fn file name for output: not used yet. See example for one
##'     way to save result.
##' @param rows Should output be transposed. This may help if there
##'     are many variables that need to fit on the page.  Percentages
##'     will appear on the rows, rather than columns.
##' @param digits Number of decimals to display in percentages
##' @return character vector
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @importFrom xtable xtable print.xtable
##' @export
##' @examples
##' vvector <- c("Strongly Disagree", "Disagree", "Neutral",
##'               "Agree", "Strongly Agree")
##' set.seed(2342234)
##' N <- 8
##' scales <-
##'     data.frame(Vegas = factor(sample(1:5, N, replace = TRUE), levels = 1:5, labels = vvector),
##'                NewYork = factor(sample(1:5, N, replace = TRUE),levels = 1:5, labels = vvector),
##'                Paris = factor(sample(1:5, N, replace = TRUE), levels = 1:5, labels = vvector),
##'                Berlin = factor(sample(1:5, N, replace = TRUE), levels = 1:5, labels = vvector))
##'
##' likert(scales)
##' 
##' (mySummary1 <- likert(data = scales, vlist = c("Vegas", "NewYork", "Paris")))
##' 
##' (mySummary2 <- likert(scales, vlist = c("Vegas", "NewYork", "Paris"),
##'                     valuelabels = c("SD", "D", "N", "A", "SA")))
##' (mySummary3 <- likert(scales, vlist = c("Vegas", "NewYork", "Paris"),
##'                     valuelabels = c("Strongly Disagree" = "Strong Disagreement")))
##'
##' (mySummary5 <- likert(scales, vlist = c("Vegas", "NewYork", "Paris"),
##'       valuelabels = c("SD", "D", "N", "A", "NA"),
##'       columnlabels = c("Vegas" = "Sin City"), rows = TRUE))
##' 
##'  ## Example of how one might write this in a file. The fn argument is not currently
##'  ## enabled, but the following will work.
##'  ## print(xtable::xtable(mySummary1, digits = 0), type = "html", file = "varCount-1.html")       
##'   
likert <-  function(data, vlist, columnlabels, valuelabels,  outdir,
                    fn, rows = FALSE, digits = 2){
    ## xxx <- lapply(data[ , vlist], table)
    ## t(sapply(data[, vlist], table))
    ## TODO: Insert check that variables have same levels
    
    if (!missing(vlist) && !is.null(vlist)){
        data <- data[ , vlist, drop = FALSE]
    } else {
        vlist <- colnames(data)
    }
    
    ## add error checking
    ## All columns must have same factor levels, else stop
    factorlevels <- unique(lapply(data, levels))
    if(length(factorlevels) > 1) {
        warning("All columns do not have same factor levels, will append")
        factorlevels <- unique(unlist(factorlevels))
    } else {
        factorlevels <- unlist(factorlevels)
    }
    
    if(!missing(valuelabels) && !is.null(valuelabels)){
        if(length(valuelabels) < length(factorlevels)){
            if(is.null(names(valuelabels))){
                MESSG <- "valuelabels is incomplete or unnamed"
                stop(MESSG)
            } else {
                ## valuelabels is named, so replace
                factorlevels <- modifyVector(factorlevels, valuelabels)
            }
        } else if (length(valuelabels) == length(factorlevels)){
            if (is.null(names(valuelabels))) factorlevels <- valuelabels
            else factorlevels <- valuelabels[factorlevels]
        } else if (length(valuelabels) > length(factorlevels)){
            stop("valuelabels vector too long")
        }
        for (i in seq_along(data)) levels(data[ , i]) <- factorlevels
    }
    
    if(!missing(columnlabels) && !is.null(columnlabels)){
        if(length(columnlabels) < length(vlist)){
            if(is.null(names(columnlabels))){
                stop("columnlabels is incomplete or unnamed")
            } else {
                columnlabels <- modifyVector(vlist, columnlabels)
            }
        } else if (length(columnlabels) == length(vlist)){
            if (is.null(names(columnlabels))) names(columnlabels) <- vlist
        } else if (length(columnlabels) > length(vlist)){
            stop("columnlabels vector too long")
        } 
    } else {
        columnlabels <- colnames(data)
    }

    varCount <- matrix(0, nrow = length(factorlevels), ncol = NCOL(data), dimnames = list(factorlevels, colnames(data)))
    for (i in colnames(data)){
        xxx <- table(data[ , i])
        varCount[rownames(xxx), i ] <- xxx 
    }
    varCount <- sapply(data, table)
    colnames(varCount) = columnlabels
    varSums <- colSums(varCount)

    varColPct <- 100.0 * sweep(varCount, 2, varSums, "/")
    varColPct <-  formatC(varColPct, digits = digits, format = "f")
    
    freqTab <- matrix(NA, nrow = NROW(varColPct), ncol = NCOL(varColPct))
    dimnames(freqTab) <- list(rownames(varColPct), colnames(varColPct))
    
    for(i in 1:NROW(varColPct)) {
        freqTab[i, ] <- paste0(varColPct[i, ], "% (", varCount[i, ], ")")
    }
    
    ## freqTab <- rbind( rep(paste0("Pct (Count)")), freqTab)
    ## colnames(freqTab) <- colnames(varColPct)
    freqTab <- rbind(freqTab, varSums)
    rownames(freqTab)[NROW(freqTab)] <- "Total"

    if (!rows) {
        res <- list(table = freqTab, count = varCount, sums = varSums, pcts = varColPct)
    } else {
        res <- list(table = t(freqTab), count = t(varCount), sums = varSums, pcts = t(varColPct))
    }
    class(res) <- c("likert", class(res))
    res
}


##' print method for likert tables
##'
##' cat is called on first item in list
##' @param x likert object, 1st item will be printed
##' @param ... 
##' @return Nothing
##' @method print likert
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu>
print.likert <- function(x, ...){
    print(x[[1]])
    MESSG <- paste("likert is a list, also includes:", paste(names(x)[-1], collapse = " "), "\n")
    cat(MESSG)
}
