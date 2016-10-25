##' Inserts escape characters to protect symbols in LaTeX,
##' HTML, or file names.
##'
##' If type = "tex" then the text like % or > or < or _ will
##' be cleaned up so that it can be included as text in the LaTeX document.
##' If type = "html", we only clean up <, >, and &, with premise
##' that document is in unicode, so we don't need to do the gigantic set anymore.
##' If type = "filename", then symbols that are not allowed in file names, such
##' as "\", "*", are replaced. I don't like spaces in file names either.
##' @param x a string
##' @param type "tex" is default, could be "filename" or "html"
##' @return corrected character vector
##' @author Paul Johnson
##' @examples
##' x1 <- c("_asdf&_&$", "asd adf asd_", "^ % & $asdf_")
##' escape(x1)
##' x2 <- c("a>b", "a<b", "a < c", 'Paul "pj" Johnson')
##' escape(x2, type = "tex")
##' escape(x2, type = "html")
##' escape(x2, type = "filename")
escape <- function (x, type = "tex"){
    if (type == "tex"){
        x <- gsub("\\\\", "SANITIZE.BACKSLASH", x)
        for(i in c("&", "$", "_", "^", "%", "#", "{", "{")) {
            x <- gsub(paste0("\\", i), paste0("\\\\",i), x)
        }
        x <- gsub("\\~", "$\\sim$", x)
        x <- gsub(">", "$>$", x, fixed = TRUE)
        x <- gsub("<", "$<$", x, fixed = TRUE)
        x <- gsub("|", "$|$", x, fixed = TRUE)
        x <- gsub("^", "\\verb|^|", x, fixed = TRUE)
        x <- gsub("SANITIZE.BACKSLASH", "$\\backslash$",
                  x, fixed = TRUE)
        return(x)
    }

    if (type == "filename"){
        ## remove non-alpha numeric characters
        xn <- gsub("[^a-zA-Z0-9\\\\' _()-]", "", x)
        xn <- gsub("\\ *$", "", xn) # Trailing spaces
        xn <- gsub("\\* $", "", xn)
        xn <- gsub("\\", "", x, fixed = TRUE)
        xn <- gsub("\\ ", "_", xn)
        xn <- gsub("_-_", "-", xn)
        xn <- gsub("_-", "-", xn)
        xn <- gsub("-_", "-", xn)
        xn <- gsub("&", "and", xn)
        xn <- gsub("\\\'","", xn)
        xn <- gsub("\\\"","", xn)
        xn <- gsub("#", "_", xn, fixed = TRUE)
        xn <- gsub("c/o", "co", xn)
        xn <- gsub("/", "-", xn)
        xn <- gsub("\\*", "", xn)
        xn <- gsub("\\$", "", xn)
        xn <- gsub("-$", "",  xn)
        xn <- gsub("_$", "", xn)
        xn <- gsub("<", "", xn, fixed = TRUE)
        xn <- gsub(">", "", xn, fixed = TRUE)
        return(xn)
    }

    ## Could have used kutils::mgsub, if it had existed.
    if (type == "html"){
        old <- c( "&", "<", ">", "\"", "\'")
        new <- c("_AMPERSAND_", "&lt;", "&gt;", "&quot;", "&#39;")
        xn <- kutils::mgsub(old, new, x)
        xn <- gsub("_AMPERSAND_", "&amp;", xn)
        return(xn)
    }
    messg(paste("escape function can only handle \"tex\", \"html\" and \"filename"))
    stop(messg)
}
