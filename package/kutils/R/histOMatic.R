
##' Find all numeric columns, draw histograms
##'
##' This makes it easy to quickly scan through all of the numeric
##' columns in a data frame to spot bad codes (or just gaze at histograms).
##' This has an argument that can specify an output file name instead of
##' looking at the histograms on the screen.
##' @param dat An R data frame
##' @param sort. Do you want alphabetized columns?
##' @param file  Should output go in file rather than to the screen.
##' Default is NULL, meaning show on screen. If you supply a file 
##' name, we will write PDF output into it.
##' @param ... Additional arguments for the histogram function. Can be any of hte
##' arguments one would ordinarily intend for a histogram, such as prob, breaks,
##' xlab, colors, main, etc.
##' @export
##' @importFrom stats hist
##' @return A vector of column names that were plotted
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' set.seed(234234)
##' N <- 200
##' mydf <- data.frame(x5 = rnorm(N), x4 = rnorm(N), x3 = rnorm(N),
##'     x2 = letters[sample(1:24, 200, replace = TRUE)],
##'     x1 = gl(40, 5, labels = c("cindy", "bobby", "marsha",
##'             "greg", "chris")), stringsAsFactors = FALSE)
##' histOMatic(mydf)
##' histOMatic(mydf, sort = FALSE)
##' ## Demonstrate the dot-dot-dot usage to pass in hist params
##' histOMatic(mydf, breaks = 30, ylab = "These are Counts, not Densities", prob = FALSE)
##' histOMatic(mydf, sort = FALSE, file = "three_histograms.pdf")
histOMatic <-
    function(dat, sort = TRUE, file = NULL, ...)
{
    namez <- colnames(dat)[sapply(dat, is.numeric)]
    namez <- if(sort) sort(namez) else namez
 
    dots <- list(...)

    if (!is.null(file)){
        if (!is.character(file)) stop("Sorry, file has to be a character string")
        pdf(file, onefile = TRUE)
    }
    if (is.null(file)) devAskNewPage(TRUE)
    for (i in namez){
        histargs <- list(prob = TRUE, xlab = paste0("kutils histOMatic: ", i),
                         main = "")
        argz <- modifyList(histargs, dots)
        qqq <- modifyList(list(x = dat[ , i]), argz)
        do.call("hist", qqq)
    }
    if (!is.null(file)) dev.off()
    namez
}










