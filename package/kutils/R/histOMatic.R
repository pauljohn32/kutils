
##' Show variables, one column at a time.
##'
##' This makes it easy to quickly scan through all of the columns in a
##' data frame to spot bad codes (or just gaze at histograms and
##' barplots).  This has an argument that can specify an output file
##' name instead of looking at the histograms on the screen.
##'
##' This draws histograms for numeric variables, draw barplots for factors
##' or character variables. The histograms are standard, upright histograms.
##' The barplots are horizontal, on the grounds that some value labels
##' for factors are too long to fit comfortably under the bars (and
##' I hate writing vertically, but know how to if I want to, but did not).
##' 
##' @param dat An R data frame
##' @param sort Do you want alphabetized columns?
##' @param file Should output go in file rather than to the screen.
##'     Default is NULL, meaning show on screen. If you supply a file
##'     name, we will write PDF output into it.
##' @param textout If TRUE, counts from histogram bins and tables will
##'     appear in the console.
##' @param ... Additional arguments for the histogram, table, or
##'     barplot functions. This function parses the arguments so that
##'     additional arguments are extracted and used for creating the
##'     table. The other dot arguments can be any of the arguments one
##'     would ordinarily intend for a histogram, such as prob, breaks,
##'     xlab, colors, main, etc, or with a barplot. I did not work as
##'     hard to sort out the plot and barplot arguments, but that is
##'     in the TODO list.
##' @param xlabstub A text stub that will appear in the x axis label
##' @param ask Default TRUE, should keyboard interaction advance the
##'     plot.  Setting this to FALSE seems mostly useless, except if
##'     somebody just wants the text output in the console and does not
##'     mind that the graphs whir bye.
##' @export
##' @importFrom utils modifyList
##' @importFrom grDevices dev.off devAskNewPage pdf
##' @import datasets
##' @return A vector of column names that were plotted
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' set.seed(234234)
##' N <- 200
##' mydf <- data.frame(x5 = rnorm(N), x4 = rnorm(N), x3 = rnorm(N),
##'                    x2 = letters[sample(1:24, 200, replace = TRUE)],
##'                    x1 = factor(sample(c("cindy", "bobby", "marsha",
##'                                         "greg", "chris"), 200, replace = TRUE)),
##'                    stringsAsFactors = FALSE)
##' ## Insert 16 missings
##' mydf$x1[sample(1:150, 16,)] <- NA
##' mydf$adate <- as.Date(c("1jan1960", "2jan1960", "31mar1960", "30jul1960"), format = "%d%b%y")
##' look(mydf)
##' look(mydf, sort = FALSE)
##' ## Demonstrate the dot-dot-dot usage to pass in hist params
##' look(mydf, breaks = 30, ylab = "These are Counts, not Densities", prob = FALSE)
##' Not Run: file output
##' ## look(mydf, sort = FALSE, file = "three_histograms.pdf")
##' ## Use some objects from the datasets package
##' library(datasets)
##' look(cars)
##' look(EuStockMarkets)
look <-
    function(dat, sort = TRUE, file = NULL, textout = FALSE, ...,
             xlabstub = "kutils look: ", ask = TRUE)
{
    quickhist <- function(i){
        histargs <- list(prob = TRUE, xlab = paste0(xlabstub, i),
                         main = "")
        histargz <- modifyList(histargs, dots)
        qqq <- modifyList(list(x = dat[ , i]), histargz)
        h1 <- do.call("hist", qqq)
        if (textout){
            df1 <- data.frame("midpoints" = h1$mids, "density" = h1$density)
            cat("\n", i, "\n")
            print(df1)
        }
    }
    
    quickbar <- function(i){
        targs <- list(dat[ , i], exclude = NULL, dnn = "Proportion")
        targz <- modifyList(targs, dotsForTable)

        t1 <- do.call("table", targz)
        t1 <- t1/margin.table(t1)
        names(t1) <- ifelse(is.na(names(t1)), "NA", names(t1))

        par.old <- par(no.readonly = TRUE)
        ## if longest name exceeds guess of space in margin,
        ## then pad the margin left side.
        marinch <- par("mai")
        marrequired <- max(strwidth(names(t1), units = "inches"))
        if (marrequired > marinch[2] + 0.25) {
            marinch[2] <- marrequired + 0.25
            par(xpd = TRUE)
            par("mai" = marinch)
        }
        barplot(t1, horiz = TRUE, las = 1, xlab = paste(xlabstub,  i, "(Proportion)"))
        par(par.old)
        if (textout) {
            cat("\n", i, "\n")
            print(t1)
        }
    }

    varType <- function(x){
        ifelse(is.numeric(x), "numeric",
        ifelse(is.character(x) | is.factor(x), "factor", "noneoftheabove")
        )}

    if (!is.data.frame(dat)) dat <- as.data.frame(dat)
    namez <- colnames(dat)
    namez <- if(sort) sort(namez) else namez
    
    colTypes <- sapply(dat, varType)
    colTypes <- colTypes[namez]

    print(paste("These variables are being omitted because they are",
                "neither numbers nor can they be interpreted as factors"))
    print(colTypes[colTypes == "noneoftheabove"])
    ## remove unrecognized types
    colTypes <- colTypes[colTypes != "noneoftheabove"]
    
    dots <- list(...)
      
    ## get just these arguments out of dots
    tableFormals <- c("exclude", "dnn", "useNA", "deparse.level")
    dotsForTable <- dots[tableFormals[tableFormals %in% names(dots)]]
    dots[names(dotsForTable)] <- NULL
       
    if (!is.null(file)){
        if (!is.character(file)) stop("Sorry, file has to be a character string")
        pdf(file, onefile = TRUE)
    }
    if (is.null(file) || isTRUE(ask)) devAskNewPage(TRUE)
    
    for (i in names(colTypes)){
        if ( colTypes[i] == "numeric" ){
            quickhist(i)
        } else {
            quickbar(i)
        }
    }
    if (!is.null(file)) dev.off()
    namez
}




