
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
##'     barplot functions. These arguments are extracted and used in
##'     the table function: c("exclude", "dnn", "useNA",
##'     "deparse.level"). These arguments are extracted and sent to
##'     the pdf function for usage in file output: c("width",
##'     "height", "onefile", "family", "title", "fonts", "version",
##'     "paper", "encoding", "bg", "fg", "pointsize", "pagecentre",
##'     "colormodel", "useDingbats", "useKerning", "fillOddEven",
##'     "compress") The other arguments should apply to histograms or
##'     barplots.  Histogram arguments like prob or breaks work well.
##'     Arguments like xlab, colors, main, etc, will be used by both
##'     histogram and barplot. That is probably unfortunate; for the
##'     next version I'm collecting suggestions on what to do.
##' @param xlabstub A text stub that will appear in the x axis label
##' @param ask Default TRUE, should keyboard interaction advance the
##'     plot.  Will default to false if the file argument is non-null.
##'     If file is null, setting ask = FALSE will cause graphs to whir
##'     bye without pausing.
##' @export
##' @importFrom utils modifyList
##' @importFrom grDevices dev.off devAskNewPage pdf
##' @importFrom graphics strwidth par
##' @import datasets
##' @return A vector of column names that were plotted
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @aliases histOMatic
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
##' ## Not Run: file output
##' ## look(mydf, sort = FALSE, file = "three_histograms.pdf")
##' ## Use some objects from the datasets package
##' library(datasets)
##' look(cars)
##' look(EuStockMarkets)
##'
##' look(EuStockMarkets, breaks = 50, prob = FALSE)
##' ## Not run
##' ## look(EuStockMarkets, breaks = 50, file = "myeuro.pdf",
##' ##      height = 4, width=3, family = "Times")
##' ## look(EuStockMarkets, breaks = 50, file = "myeuro-%d3.pdf",
##' ##      onefile = FALSE, family = "Times", textout = TRUE)
look <-
    function(dat, sort = TRUE, file = NULL, textout = FALSE, ask, ...,
             xlabstub = "kutils look: ")
{
    quickhist <- function(i){
        histargs <- list(prob = TRUE, xlab = paste0(xlabstub, i),
                         main = i)
        histargz <- modifyList(histargs, dots)
        qqq <- modifyList(list(x = dat[ , i]), histargz)
        h1 <- do.call("hist", qqq)
        if (textout){
            df1 <- data.frame("midpoints" = h1$mids, "density" = h1$density)
            cat(i, "\n")
            print(df1)
        }
    }
    
    quickbar <- function(i){
        targs <- list(dat[ , i], exclude = NULL, dnn = "Proportion")
        targz <- modifyList(targs, dotsForTable)

        t1 <- do.call("table", targz)
        t1 <- t1/margin.table(t1)
        names(t1) <- ifelse(is.na(names(t1)), "NA", names(t1))

        barargs <- list(t1, horiz = TRUE, las = 1, main = i,
                        xlab = paste(xlabstub,  i, "(Proportion)"))
        barargz <- modifyList(barargs, dots)
        
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
        do.call("barplot", barargz)
                
        par(par.old)
        if (textout) {
            cat(i, "\n")
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

    cat(paste("These variables are being omitted because they are",
                "neither numbers nor can they be interpreted as factors: \n"))
    cat(paste(names(colTypes[colTypes == "noneoftheabove"]), "\n"))
    ## remove unrecognized types
    colTypes <- colTypes[colTypes != "noneoftheabove"]
    
    dots <- list(...)
      
    ## copy args for table, remove from dots
    tableFormals <- c("exclude", "dnn", "useNA", "deparse.level")
    dotsForTable <- dots[tableFormals[tableFormals %in% names(dots)]]
    dots[names(dotsForTable)] <- NULL

    ## copy args for pdf, remove from dots
    pdfFormals <- c("width", "height", "onefile", "family", "title", "fonts",
                    "version", "paper", "encoding", "bg", "fg", "pointsize",
                    "pagecentre", "colormodel", "useDingbats", "useKerning",
                    "fillOddEven", "compress")
    dotsForPDF <- dots[pdfFormals[pdfFormals %in% names(dots)]]
    dots[names(dotsForPDF)] <- NULL

    ## Unless user sets ask, we assume TRUE if there is no file argument
    if (missing(ask)) {
        if (is.null(file)) ask <- TRUE else ask <- FALSE
    }
    
    if (!is.null(file)){
        if (!is.character(file)) stop("Sorry, file has to be a character string")
        pdfargs <- list(file = file, onefile = TRUE)
        pdfargz <- modifyList(pdfargs, dotsForPDF)
        do.call("pdf", pdfargz)
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




