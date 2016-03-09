


##' Percentage tables for Likert Scale variables
##'
##' Creates a table with columns for allowed values and rows for
##' variables.
##' @param vlist A vector of variable names
##' @param data A data frame
##' @param labels A vector of values to replace existing levels
##' @param outdir Output directory
##' @param fn file name for output: not used yet
##' @param rows Currently unused indicator for whether output should
##'     be transposed.
##' @return character vector
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @import xtable
##' @export
##' @examples
##'  vvector <- c("Strongly Disagree", "Disagree", "Neutral",
##'               "Agree", "Strongly Agree")
##' set.seed(2342234)
##' N <- 142
##' scales <-
##'     data.frame(Vegas = factor(sample(1:5, N, replace = TRUE), labels = vvector),
##'                NewYork = factor(sample(1:5, N, replace = TRUE), labels = vvector),
##'                Paris = factor(sample(1:5, N, replace = TRUE), labels = vvector),
##'                Berlin = factor(sample(1:5, N, replace = TRUE), labels = vvector))
##' 
##' (mySummary1 <- likert(c("Vegas", "NewYork", "Paris"), scales))
##' 
##' (mySummary2 <- likert(c("Vegas", "NewYork", "Paris"), scales,
##'                     labels = c("SD", "D", "N", "A", "SA")))
##'
likert <-  function(vlist, data, labels, outdir, fn, rows = TRUE){
    ## xxx <- lapply(data[ , vlist], table)
    ## t(sapply(data[, vlist], table))
    ## TODO: Insert check that variables have same levels

    subdat <- data[ , vlist]
    if (!missing(labels))
        for (i in 1:NCOL(subdat)) levels(subdat[ , i]) <- labels
    
    varCount <- sapply(subdat[ , vlist], table)
    varCountSums <- colSums(varCount)
    
    for (i in colnames(varCount)) {
        varCount[ ,i] <- as.integer(varCount[ , i])
    }
    
    varColProp <- varCount
    for (i in colnames(varCount)){
        varColProp[ , i] <- round(100*(varCount[ , i] / varCountSums[i]), 2)
    }
    varColProp
        
    varColPct <- round(100* prop.table(varCount, 2), 2)
    varColPct
    
    freqTab <- matrix(NA, nrow = NROW(varColPct), ncol = NCOL(varColPct))
    dimnames(freqTab) <- list(rownames(varColPct), colnames(varColPct))
    for (i in 1:NROW(varColPct)){
        freqTab[i, ] <- paste0(varColPct[i, ], "% (", varCount[i, ], ")")
    }
    
    freqTab <- rbind( rep(paste0("Pct (Count)")), freqTab)
    colnames(freqTab) <- colnames(varColPct)
    freqTab <- rbind(freqTab, varCountSums)
    rownames(freqTab)[NROW(freqTab)] <- "Sum"
        
##    print(xtable(freqTab, digits = 0), type = "html", file = paste0(outdir, "varCount-1.html"))

    freqTab
}
    
