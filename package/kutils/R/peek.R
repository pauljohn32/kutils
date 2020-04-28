##' Show variables, one at a time, QUICKLY and EASILY.
##'
##' This makes it easy to quickly scan through all of the columns in a
##' data frame to spot unexpected patterns or data entry errors.  Numeric variables are depicted as
##' histograms, while factor and character variables are summarized by
##' the R table function and then presented as barplots. This is most
##' useful with a large screen graphic device (try running the function
##' provided with this package, \code{dev.create(height=7, width=7)})
##' or any other method you prefer to create a large device. 
##'
##' @section Try the Defaults: Every effort has been made to make this
##'     simple and easy to use. Please run the examples as they are
##'     before becoming too concerned about customization.  This
##'     function is intended for getting a quick look at each
##'     variable, one-by-one, it is not intended to create publication
##'     quality histograms.  For sake of the fastidious users, a lot
##'     of settings can be adjusted. Users can control the parameters
##'     for presentation of histograms (parameters for \code{hist})
##'     and barplots (parameters for \code{barplot}). The function also
##'     can create frequency tables (which users can control by providing
##'     additional named arguments).
##'
##' @section Style: The histograms are standard, upright histograms.
##'     The barplots are horizontal. I chose to make the bars
##'     horizontal because long value labels are more easily
##'     accomodated on the left axis.  The code measures the length
##'     (in inches) for strings and the margin is increased
##'     accordingly.  The examples have a demonstration of that
##'     effect.
##' 
##' @section Dealing with Dots: additional named arguments,
##'     \code{...}, are inspected and sorted into groups intended to
##'     control use of R functions \code{hist}, \code{barplot},
##'     \code{table} and \code{pdf}.  \cr \cr The parameters
##'     c("exclude", "dnn", "useNA", "deparse.level") and will go to
##'     the \code{table} function, which is used to make barplots for
##'     factor and character variables. These named arguments are
##'     extracted and sent to the pdf function: c("width", "height",
##'     "onefile", "family", "title", "fonts", "version", "paper",
##'     "encoding", "bg", "fg", "pointsize", "pagecentre",
##'     "colormodel", "useDingbats", "useKerning", "fillOddEven",
##'     "compress"). Any other arguments that are unique to
##'     \code{hist} or \code{barplot} are sorted out and sent only to
##'     those functions.  \cr \cr Any other arguments, including
##'     graphical parameters will be sent to both the histogram and
##'     barplot functions, so it is a convenient way to obtain uniform
##'     appearance. Additional arguments that are common to
##'     \code{barplot} and \code{hist} will work, and so will any
##'     graphics parameters (named arguments of \code{par}, for
##'     example). However, if one wants to target some arguments to
##'     \code{hist}, but not \code{barplot}, then the \code{histargs}
##'     list argument should be used. Similarly, \code{barargs} should
##'     be used to send argument to the \code{barplot}
##'     function. Warning: the defaults for \code{histargs} and
##'     \code{barargs} include some settings that are needed for the
##'     existing design.  If new lists for \code{histargs} or
##'     \code{barargs} are supplied, the previously specified defaults
##'     are lost.  Hence, users should include the existing members of
##'     those lists, possibly with revised values.  \cr \cr All of
##'     this argument sorting effort is done in order to reduce a
##'     prolific number of warnings that were observed in previous
##'     editions of this function.
##' 
##' @param dat An R data frame or something that can be coerced to a
##'     data frame by \code{as.data.frame}
##' @param sort Default TRUE. Do you want display of the columns in
##'     alphabetical order?
##' @param file Should output go in file rather than to the screen.
##'     Default is NULL, meaning show on screen. If you supply a file
##'     name, we will write PDF output into it.
##' @param textout If TRUE, counts from histogram bins and tables will
##'     appear in the console.
##' @param ask As in the old style R \code{par(ask = TRUE)}: should
##'     keyboard interaction advance to the next plot.  Will default
##'     to false if the file argument is non-null.  If file is null,
##'     setting ask = FALSE will cause graphs to whir bye without
##'     pausing.
##' @param ... Additional arguments for the pdf, histogram, table, or
##'     barplot functions. Please see Details below.
##' @param xlabstub A text stub that will appear in the x axis
##'     label. Currently it includes advertising for this package.
##' @param freq As in the histogram frequency argument. Should graphs
##'     show counts (freq = TRUE) or proportions (AKA densities) (freq
##'     = FALSE)
##' @param histargs A list of arguments to be passed to the
##'     \code{hist} function.
##' @param barargs A list of arguments to be passed to the
##'     \code{barplot} function.
##' @export
##' @importFrom utils modifyList
##' @importFrom grDevices dev.off devAskNewPage pdf
##' @importFrom graphics strwidth par barplot.default hist.default
##' @import datasets
##' @return A vector of column names that were plotted
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @aliases histOMatic
##' @examples
##' \donttest{
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
##' peek(mydf)
##' peek(mydf, sort = FALSE)
##' ## Demonstrate the dot-dot-dot usage to pass in hist params
##' peek(mydf, breaks = 30, ylab = "These are Counts, not Densities", freq = TRUE)
##' ## Not Run: file output
##' ## peek(mydf, sort = FALSE, file = "three_histograms.pdf")
##' ## Use some objects from the datasets package
##' library(datasets)
##' peek(cars, xlabstub = "R cars data: ")
##' peek(EuStockMarkets, xlabstub = "Euro Market Data: ")
##' peek(EuStockMarkets, xlabstub = "Euro Market Data: ", breaks = 50,
##'      freq = TRUE)
##' ## Not run: file output
##' ## peek(EuStockMarkets, breaks = 50, file = "myeuro.pdf",
##' ##      height = 4, width=3, family = "Times")
##' ## peek(EuStockMarkets, breaks = 50, file = "myeuro-%d3.pdf",
##' ##      onefile = FALSE, family = "Times", textout = TRUE)
##' ## xlab goes into "..." and affects both histograms and barplots
##' peek(mydf, breaks = 30, ylab = "These are Counts, not Densities",
##'     freq = TRUE)
##' ## xlab is added in the barargs list.
##' peek(mydf, breaks = 30, ylab = "These are Counts, not Densities",
##'     freq = TRUE, barargs = list(horiz = TRUE, las = 1, xlab = "I'm in barargs"))
##' peek(mydf, breaks = 30, ylab = "These are Counts, not Densities", freq = TRUE,
##'      barargs = list(horiz = TRUE, las = 1, xlim = c(0, 100),
##'      xlab = "I'm in barargs, not in histargs"))
##' levels(mydf$x1) <- c(levels(mydf$x1), "arthur philpot smythe")
##' mydf$x1[4] <- "arthur philpot smythe"
##' mydf$x2[1] <- "I forgot what letter"
##' peek(mydf, breaks = 30,
##'      barargs = list(horiz = TRUE, las = 1))
##' }
  
peek <-
    function(dat, sort = TRUE, file = NULL, textout = FALSE, ask, ...,
             xlabstub = "kutils peek: ", freq = FALSE,
             histargs = list(probability = !freq),
             barargs = list(horiz = TRUE, las = 1))
{
    quickhist <- function(i){
        args <- list(xlab = paste0(xlabstub, i),
                     main = i)
        histargz <- modifyList(args, dots)
        histargz <- modifyList(histargz, histargs)
        qqq <- modifyList(list(x = dat[ , i]), histargz)
        h1 <- do.call("hist", qqq)
        if (textout){
            df1 <- data.frame("midpoints" = h1$mids, "density" = h1$density)
            cat(i, "\n")
            print(df1)
        }
    }
    
    quickbar <- function(i){
        targs <- list(dat[ , i], exclude = NULL,
                      dnn =  if (!freq) "Proportion" else "Frequency")
        targz <- modifyList(targs, dotsForTable)
        ## don't allow hist or barplot arguments to to to table, silences warning
        ## targz[c(names.par, names.barplot.unique, names.hist.unique)] <- NULL
        t1 <- do.call("table", targz)
        if (!freq) t1 <- t1/margin.table(t1)
        names(t1) <- ifelse(is.na(names(t1)), "NA", names(t1))
        ## decision 1: brute force chop
        ## names(t1) <- shorten(names(t1), k = LIMIT, unique = TRUE)
        names(t1) <- sapply(names(t1), stringbreak, k = LIMIT)
        args <- list(t1, main = i,
                     xlab = paste(xlabstub,  i, if (freq)"Frequencies" else "(Proportion)"))
        barargz <- modifyList(args, dots)
        barargz <- modifyList(barargz, barargs)
        ## Remove args that would have gone to hist
        barargz[names.hist.unique] <- NULL
        par.old <- par(no.readonly = TRUE)
        ## if longest name exceeds guess of space in margin,
        ## then pad the margin left side.
        marinch <- par("mai")
        marrequired <- max(strwidth(names(t1), units = "inches"))
        if (marrequired + 0.35 > marinch[2]) {
            marinch[2] <- marrequired + 0.5
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

    ## limit on character strings for barplots
    LIMIT <- 30
    
    ## We get lots of warnings about inappropriate arguments to functions.
    ## Focus on most likely objections by getting names unique to
    ## hist and removing them from the bar portion, and
    ## names unique to bar and removing from the hist portion
    ## This, however, causes a spurious device, so have to hard code parms
    ## names.par <- names(par())

    names.par <- c("xlog", "ylog", "adj", "ann", "ask", "bg", "bty",
                   "cex", "cex.axis", "cex.lab", "cex.main",
                   "cex.sub", "cin", "col", "col.axis", "col.lab",
                   "col.main", "col.sub", "cra", "crt", "csi", "cxy",
                   "din", "err", "family", "fg", "fig", "fin", "font",
                   "font.axis", "font.lab", "font.main", "font.sub",
                   "lab", "las", "lend", "lheight", "ljoin", "lmitre",
                   "lty", "lwd", "mai", "mar", "mex", "mfcol", "mfg",
                   "mfrow", "mgp", "mkh", "new", "oma", "omd", "omi",
                   "page", "pch", "pin", "plt", "ps", "pty", "smo",
                   "srt", "tck", "tcl", "usr", "xaxp", "xaxs", "xaxt",
                   "xpd", "yaxp", "yaxs", "yaxt", "ylbias")
    names.hist <- removeMatches(names(formals(hist.default)), "...")
    names.barplot <- removeMatches(names(formals(barplot.default)), "...")
    names.barplot.unique <- names.barplot[!names.barplot %in% names.hist]
    names.hist.unique <- names.hist[!names.hist %in% names.barplot]
    
    varType <- function(x){
        ifelse(is.numeric(x), "numeric",
        ifelse(is.character(x) | is.factor(x), "factor", "noneoftheabove"))
    }

    if (is.atomic(dat)){
        xclean <- xname <- deparse(substitute(dat))
        dat <- as.data.frame(dat)
        if (length(grep("\\$", xname) > 0)){
            xsplit <- unlist(strsplit(xname, "\\$"))
            xclean <- xsplit[length(xsplit)]
        }
        colnames(dat) <- xclean
    } else {
        if (!is.data.frame(dat)) dat <- as.data.frame(dat)
    }
    
    namez <- colnames(dat)
    namez <- if(sort) sort(namez) else namez
    
    colTypes <- sapply(dat, varType)
    colTypes <- colTypes[namez]

    if (length(colTypes[colTypes == "noneoftheabove"]) > 0) {
        cat(paste("These variables are being omitted because they are",
                  "neither numbers nor can they be interpreted as factors: \n"))
        cat(paste(names(colTypes[colTypes == "noneoftheabove"]), "\n"))
        ## remove unrecognized types
        colTypes <- colTypes[colTypes != "noneoftheabove"]
    }
    
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

    deviceFormals <- c("width", "height", "noRStudioGD")
    dotsForDevice <- dots[deviceFormals[deviceFormals %in% names(dots)]]
    dots[names(dotsForPDF)] <- NULL
     
    
    ## Unless user sets ask, we assume TRUE if there is no file argument
    if (missing(ask)) {
        if (is.null(file)) ask <- TRUE else ask <- FALSE
    }
   
    closeFile <- FALSE 
    if (!is.null(file)){
        if (!is.character(file)) stop("file name should be a character string")
        pdfargs <- list(file = file, onefile = TRUE)
        pdfargz <- modifyList(pdfargs, dotsForPDF)
        do.call("pdf", pdfargz)
        closeFile <- TRUE
    }
    
    for (i in names(colTypes)){
        if (colTypes[i] == "numeric" ){
            quickhist(i)
            if (is.null(file) || isTRUE(ask)) devAskNewPage(TRUE)
        } else {
            quickbar(i)
            if (is.null(file) || isTRUE(ask)) devAskNewPage(TRUE)
        }
    }

    if(isTRUE(closeFile)) {
        dev.off()
    }
    namez
}





##' Create a graphics device
##'
##' This is a way to create a graphic device on screen that can
##' display R plots. It is performing the same purpose as R's dev.new,
##' but it overcomes the limitations of RStudio.  It is needed because
##' RStudio does not implement fully the functionality of
##' dev.new. This is suitable for Windows, Linux, and
##' Macintosh operating systems.
##'
##' The argument in dev.new named noRStudioGD seems to be aimed at same
##' purpose. But it does not do what I want and documentation is too
##' sparse.
##' @param ... Currently, height and width parameters that would be
##'     suitable with dev.new
##' @return NULL
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @export
##' @examples
##' \donttest{
##' if(interactive()) dev.create(height = 7, width = 3)
##' dev.off()
##' }
dev.create <- function(...){
    dots <- list(...)
    systype <- tolower(Sys.info()["sysname"])
    devtype = switch(systype,
                     "darwin"  = "quartz",
                     "linux"   = "x11",
                     "windows" = "windows")
    do.call(devtype, dots)
}
