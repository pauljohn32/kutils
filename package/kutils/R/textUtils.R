##' Text that is to be included as content in documents is cleaned
##' (escaped) to prevent errors
##'
##' This is for fixing up "untrusted text" that is to be passed into a
##' file as content. It protects against "bad" text strings in 3
##' contexts, 1) LaTeX documents, 2) HTML documents, or 3) text in a
##' file name. It converts content text to an improved string that
##' will not cause failures in the eventual document.
##'
##' The special in-document LaTeX symbols like percent sign or dollar sign
##' are "%" and "$". *Warning*: In the R
##' session, these will appear as double-backslashed symbols, while in
##' a saved text file, there will only be the one desired slash.
##'
##' If type = "html", we only clean up <, >, / and &, and quote
##' characters.  If document is in unicode, we don't need to do the
##' gigantic set anymore.
##'
##' If type = "filename", then symbols that are not allowed in file
##' names, such as "\", "*", are replaced. Do not use this on a
##' full path, since it will obliterate path separators.
##'
##' @param x a string, or vector of strings (each of which is
##'     processed separately)
##' @param type "tex" is default, could be "filename" or "html"
##' @return corrected character vector
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' x1 <- c("_asdf&_&$", "asd adf asd_", "^ % & $asdf_")
##' escape(x1)
##' x2 <- c("a>b", "a<b", "a < c", 'Paul "pj" Johnson')
##' escape(x2, type = "tex")
##' escape(x2, type = "html")
##' escape(x2, type = "filename")
escape <- function (x, type = "tex"){
    if (tolower(type) == "tex"){
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

    if (tolower(type) == "filename"){
        ## remove non-alpha numeric characters, any unfamiliar symbols
        xn <- alphaOnly(x)
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

    ## See https://www.owasp.org/index.php/XSS_%28Cross_Site_Scripting%29_Prevention_Cheat_Sheet#RULE_.231_-_HTML_Escape_Before_Inserting_Untrusted_Data_into_HTML_Element_Content
    ## Could have used kutils::mgsub, if it had existed.
    if (tolower(type) == "html"){
        old <- c( "&", "<", ">", "\"", "\'", "/")
        new <- c("_AMPERSAND_", "&lt;", "&gt;", "&quot;", "&#27;", "&#x2F")
        xn <- kutils::mgsub(old, new, x)
        xn <- gsub("_AMPERSAND_", "&amp;", xn)
        return(xn)
    }
    messg <- paste("escape function can only handle \"tex\", \"html\" and \"filename")
    stop(messg)
}




##' Keep only alpha-numeric symbols
##'
##' From a text string, keep ASCII letters, numbers, as well as "'",
##' " ", "_" "(", ")", "-", and "+". For maximum compatability with
##' the cross-platform file-naming standard. Obliterates all
##' characters that migth be mistaken for shell symbols, like "^",
##' "$", "@" and so forth.
##'
##' Removes trailing spaces.
##'
##' This version allows internal spaces in the string, by default. The
##' also argument can be used to eliminate spaces or other hated symbols.
##' @param x text string, or vector of strings (each of which is
##'     processed separately)
##' @param also A named vector of other symbols that the user wants to
##'     remove, along with replacements. For example, c(" " = "_",
##'     "-" = "", "+" = "") to replace space with underscore and minus
##'     and plus signs with nothing.
##' @export
##' @return cleaned text string
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' x <- c("[]kansas(city) Missouri", "percent%slash/",
##'        "\back{squiggle}_under(paren)", "*star-minus+plus")
##' alphaOnly(x)
##' alphaOnly(x, also = c(" " = "_", "+" = "_"))
##' alphaOnly(x, also = c("(" = "[", ")" = "]"))
alphaOnly <- function(x, also){
    y <- gsub("[^a-zA-Z0-9\\\\' _()+-]", "", x)
    y <- gsub("\\ *$", "", y) # Trailing spaces
    if(!missing(also) && !is.null(also)){
        y <-  mgsub(names(also), also, y, fixed = TRUE)
    }
    y
}
