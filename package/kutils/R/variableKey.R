

##' If a numeric variable has only integer values, then
##' make it an integer.
##'
##' Users often accidentally create floating point numeric variables
##' when they really mean integers, such as c(1, 2, 3), when they
##' should have done c(1L, 2L, 3L). Before running \code{as.integer()}
##' to coerce the variable, we'd rather be polite and ask the variable
##' "do you mind being treated as if you are an integer?"  This
##' function checks to see if the variable is "close enough" to being
##' an integer, and then coerces as integer. Otherwise, it returns
##' NULL. And issues a warning.
##'
##' First, calculate absolute value of differences between \code{x}
##' and \code{as.integer(x)}. Second, find out if the sum of those
##' differences is smaller than \code{tol}. If so, then x can
##' reasonably be coerced to an integer.
##'
##' Be careful with the return. The correct return value for variables
##' that should not be coerced as integer is uncertain at this
##' point. We've tested various strategies, sometimes returning FALSE,
##' NULL, or just the original variable.
##' @param x a numeric variable
##' @param tol Tolerance value. Defaults to Machine$double.eps. See
##'     details.
##' @param vmax Maximum value allowed for an integer. Defaults to
##'     Machine$integer.max.
##' @param digits Digits value passed to the zapsmall
##'     function. Defaults to 7.
##' @param verbose Default FALSE: print warnings about x
##' @export
##' @return Either an integer vector or the original variable
##' @author Paul Johnson <pauljohn@@ku.edu> and Ben Kite
##'     <bakite@@ku.edu>
##' @examples
##' x1 <- c(1, 2, 3, 4, 5, 6)
##' is.integer(x1)
##' is.double(x1)
##' is.numeric(x1)
##' (x1int <- safeInteger(x1))
##' is.integer(x1int)
##' is.double(x1int)
##' is.numeric(x1int)
##' x2 <- rnorm(100)
##' x2int <- safeInteger(x2)
##' head(x2int)
##' x3 <- factor(x1, labels = c(LETTERS[1:6]))
##' x3int <- safeInteger(x3)
##'
safeInteger <- function(x, tol = .Machine$double.eps,
                        digits = 7, vmax = .Machine$integer.max,
                        verbose = FALSE)
{
    if(!is.numeric(x)) {
        if (verbose) {
            messg <- paste("safeInteger: x must be numeric or integer.")
            warning(messg)
        }
        return(NULL)
    }

    if(max(x, na.rm = TRUE) > vmax) {
        messg <- paste0("Values in x exceed the maximum integer value of ",
                        vmax, ". ", "safeInteger cannot be used safely ",
                        "for this variable. Original value is returned.")
        warning(messg)
        return(x)
    }

    if(is.integer(x)){
        return(x)
    } else {
        x <- zapsmall(x, digits)
        xnew <- as.integer(x)
        if (sum(abs(x - xnew), na.rm = TRUE) < tol) {
            return(xnew)
        } else {
            if (verbose) {
                messg <- paste("asInteger x:", paste(head(x), collapse=", "),
                               "... is not close enough to an integer")
                warning(messg)
            }
            return(NULL)
        }
    }
    messg <- paste0("safeInteger should not have reached this point")
    stop(messg)
}
NULL

##' Set missing values
##'
##' The missings values have to be carefully written, depending on the
##' type of variable that is being processed.
##'
##' Version 0.95 of kutils introduced a new style for specification of
##' missing values.
##'
##' @param x A variable
##' @param missings A string vector of semi-colon separated values,
##'     ranges, and/or inequalities.  For strings and factors, only an
##'     enumeration of values (or factor levels) to be excluded is
##'     allowed. For numeric variables (integers or floating point
##'     variables), one can specify open and double-sided intervals as
##'     well as particular values to be marked as missing. One can
##'     append particular values and ranges by
##'     "1;2;3;(8,10);[22,24];> 99;< 2". The double-sided interval is
##'     represented in the usual mathematical way, where hard
##'     bracketes indicate "closed" intervals and parentheses indicate
##'     open intervals.\enumerate{
##'
##' \item "(a,b)" means values of x greater than a and smaller than b
##'     will be set as missing.
##'
##' \item "[a,b]" is a closed interval, one which includes the
##'     endpoints, so a <= x <= b will be set as NA
##'
##' \item "(a,b]" and "[a,b)" are acceptable.
##' \item "< a"  indicates all values smaller than a will be missing
##' \item  "<= a" means values smaller than or equal to a will be
##'     excluded
##' \item "> a" and ">= a" have comparable
##'     interpretations.
##' \item "8;9;10" Mark off specific values by
##' an enumeration. Be aware, however, that this is
##' useful only for integer variables.  As demonstrated in the
##' example, for floating point numbers, one must specify intervals.
##' \item For factors and character variables, the argument missings
##' can be written either as "lo;med;hi" or "c('lo','med','hi')" }
##' @param sep A separator symbol, ";" (semicolon) by default
##' @return A cleaned column in which R's NA symbol replaces values
##'     that should be missing
##' @export
##' @importFrom utils head
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' ## 1.  Integers.
##' x <- seq.int(-2L, 22L, by = 2L)
##' ## Exclude scores 8, 10, 18
##' assignMissing(x, "8;10;18")
##' ## Specify range, 4 to 12 inclusive
##' missings <- "[4,12]"
##' assignMissing(x, missings)
##' ## Not inclusive
##' assignMissing(x,  "(4,12)")
##' ## Set missing for any value smaller that 7
##' assignMissing(x, "< 7")
##' assignMissing(x, "<= 8")
##' assignMissing(x, "> 11")
##' assignMissing(x, "< -1;2;4;(7, 9);> 20")
##'
##'
##' ## 2. strings
##' x <- c("low", "low", "med", "high")
##' missings <- "low;high"
##' assignMissing(x, missings)
##' missings <- "med;doesnot exist"
##' assignMissing(x, missings)
##' ## Test alternate separator
##' assignMissing(x, "low|med", sep = "|")
##'
##' ## 3. factors (same as strings, really)
##' x <- factor(c("low", "low", "med", "high"), levels = c("low", "med", "high"))
##' missings <- "low;high"
##' assignMissing(x, missings)
##' ## Previous same as
##' missings <- c("low", "high")
##' assignMissing(x, missings)
##'
##' missings <- c("med", "doesnot exist")
##' assignMissing(x, missings)
##' ## ordered factor:
##' x <- ordered(c("low", "low", "med", "high"), levels = c("low", "med", "high"))
##' missings <- c("low", "high")
##' assignMissing(x, missings)
##'
##' ## 4. Real-valued variable
##' set.seed(234234)
##' x <- rnorm(10)
##' x
##' missings <- "< 0"
##' assignMissing(x, missings)
##' missings <- "> -0.2"
##' assignMissing(x, missings)
##' ## values above 0.1 and below 0.7 are missing
##' missings <- "(0.1,0.7)"
##' assignMissing(x, missings)
##' ## Note that in floating point numbers, it is probably
##' ## futile to specify specific values for missings. Even if we
##' ## type out values to 7 decimals, nothing gets excluded
##' assignMissing(x, "-0.4879708;0.1435791")
##' ## Can mark a range, however
##' assignMissing(x, "(-0.487971,-0.487970);(0.14357, 0.14358)")
##' x
assignMissing <- function(x, missings = NULL, sep = ";"){
    if (is.null(missings)) return(x)
    if (is.character(missings)) missings <- zapspace(missings)
    missings <- unlist(strsplit(missings, split = sep, fixed = TRUE))
    missings <- na.omit(missings)
    missings <- zapspace(missings)
    missings <- gsub("-", " -", missings,  fixed = TRUE)
    ## If no NA  missings to remove, then return
    if (is.null(missings) | length(missings) == 0) return(x)

    if (is.character(x)){
        x[x %in% missings] <- NA
        return(x)
    }

    if (is.logical(x)){
        missings <- as.logical(missings)
        x[x %in% missings] <- NA
        return(x)
    }
    
    if (is.factor(x)){
        levels(x)[which(levels(x) %in% missings)] <- NA
        return(x)
    }

    ## 20170203: TODO following is hard coded to look at first character,
    ## if space first, fails, used zapspace to address above, but that's
    ## stupid bandaid, better regex work would be solid.
    if (is.numeric(x)){
        ## is  numeric includes integer and double and numeric
        ## separate the elements that inequality signs
        hasineq <- missings[substr(missings, 1, 1) %in% c(">", "<", "(", "[")]
        hasnoineq <- setdiff(missings, hasineq)
        hasnoineq <- if (length(hasnoineq) > 0) paste("x == ", hasnoineq)

        ## (9 ->   x > 9
        ## [9 ->   x >= 9
        ins <- c(">", "<" , "(",     "[",     ")",     "]",  ",")
        outs <- c("x >", "x <", "x >", "x >= ", " > x", " >= x", " & ")
        ranges <- mgsub(ins, outs, hasineq, fixed = TRUE)

        for(rr in c(ranges, hasnoineq)){
            eval(parse(text = paste0("x[", rr, "] <- NA")))
        }
        return(x)
    }

    messg <- "Sorry, no missings assigned because variable type was unhandled"
    warning(messg)
    ## return unchanged input, didn't see what to do
    x
}




##' A variable is transformed in an indicated way
##'
##' In the variable key framework, the user might request
##' transformations such as the logarithm, exponential, or square
##' root. This is done by including strings in the recodes column,
##' such as "log(x + 1)" or "3 + 1.1 * x + 0.5 * x ^ 2". This
##' function implements the user's request by parsing the character
##' string and applying the indicated re-calculation.
##'
##' In the variable key framework, this is applied to the raw data,
##' after missings are imposed.
##' @param x A column to be recoded
##' @param recode A character string using placeholder "x". See
##'     examples
##' @return A new column
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' set.seed(234234)
##' x <- rpois(100, lambda = 3)
##' x <- x[order(x)]
##' str1 <- "log(x + 1)"
##' xlog <- assignRecode(x, recode = str1)
##' plot(xlog ~ x, type = "l")
##' mean(xlog, na.rm = TRUE)
##' str2 <- "x^2"
##' xsq <- assignRecode(x, recode = str2)
##' plot(xsq ~ x, type = "l")
##' str3 <- "sqrt(x)"
##' xsrt <- assignRecode(x, recode = str3)
assignRecode <- function(x, recode = NULL){
    y <- eval(parse(text = recode))
}


##' Check and Clean data.frame for usage with variable key functions
##'
##' Checks that the data.frame is made up of simple individual
##' columns. Checks numeric columns to find out if they are acceptable
##' to treat as integers. If they are acceptable to treat as integers,
##' then convert those numeric to integer class variables.
##' @param dframe A data frame
##' @param safeNumericToInteger Default TRUE: Should we treat values
##'     which appear to be integers as integers? If a column is
##'     numeric, it might be safe to treat it as an integer.  In many
##'     csv data sets, the values coded c(1, 2, 3) are really
##'     integers, not floats c(1.0, 2.0, 3.0). See \code{safeInteger}.
##' @param trimws Defaults as "both", in meaning of \code{which}
##'     argument in \code{trimws} function.  Set as NULL if character
##'     variables must not be trimmed to eliminate white
##'     space. Otherwise, value should be one of \code{c("left",
##'     "right", "both")}.
##' @export
##' @return A checked and cleaned data frame
##' @keywords internal
##' @author Paul Johnson <pauljohn@@ku.edu>
cleanDataFrame <- function(dframe, safeNumericToInteger = TRUE, trimws = "both"){
    if(!is.data.frame(dframe)){
        messg <- paste("keyUpdate: The dframe object must be a data frame")
        stop(messg)
    }

    if (!(res <- is.data.frame.simple(dframe))) {
        messg <- paste(paste("cleanDataFrame checked if dframe elements are not single columns.",
                       "This frame has some elements that are not single columns.",
                       "The troublesome elements are:", collapse = ""),
                       paste(attr(res, "not_a_simple_column"), collapse = ", "))
        stop(messg)
    }

    ## If integer-like columns exist, turn them into integers
    if (safeNumericToInteger){
        for(i in colnames(dframe)){
            if(is.numeric(dframe[ , i])
               && !is.null(tmp <- safeInteger(dframe[ , i])))
                dframe[ , i] <- tmp
        }
    }

    ## Clean characters that have leading spaces to delete space
    if (!is.null(trimws)){
        for(i in colnames(dframe)){
            if(is.character(dframe[ , i])){
                dframe[ , i] <- trimws(dframe[ , i], which = trimws)
            } else if (is.factor(dframe[ , i])){
                levels(dframe[ , i]) <- trimws(levels(dframe[ , i]))
            }
        }
    }
    
    dframe
}


##' Check if a data frame is a simple collection of columns (no lists
##' or matrices within)
##'
##' Checks for the existence of dimensions within the data
##' frame. Returns FALSE if any object within dframe has non-null dim
##' value.
##'
##' See: http://stackoverflow.com/questions/38902880/data-frame-in-which-elements-are-not-single-columns
##'
##' @param dframe A data frame
##' @return Boolean, TRUE or FALSE. An attribute "not_a_simple_column"
##'     is created, indicating which of the elements in the dframe
##'     have dimensions
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @export
##' @examples
##' N <- 100
##' mydf <- data.frame(x5 = rnorm(N),
##'                    x4 = rpois(N, lambda = 3),
##'                    x3 = ordered(sample(c("lo", "med", "hi"),
##'                    size = N, replace=TRUE)))
##' is.data.frame.simple(mydf)
##' mydf$amatr <- matrix(0, ncol = 2, nrow = NROW(mydf))
##' is.data.frame.simple(mydf)
##' mydf$amatr <- NULL
##' is.data.frame.simple(mydf)
##' mydf$adf <- mydf
##' is.data.frame.simple(mydf)
is.data.frame.simple <- function(dframe){
    no.dims <- function(x) {!is.null(dim(x))}
    elemdims <- sapply(dframe, no.dims)
    res <-  if(sum(elemdims) > 0) FALSE else TRUE
    attr(res, "not_a_simple_column") <- names(which(elemdims))
    res
}



##' Compare observed values with the values listed
##' (presumably from a variable key).
##'
##' This is purely diagnostic. It prints warnings in
##' either of 2 cases. 1)  observed data has values that
##' are not in the value_old, or 2) that value_old has
##' values that are not in the data.
##'
##' @param x a variable, either character or factor
##' @param value_old a vector of old values for which we are checking
##' @param xname character string to use for x's name when printing output
##' @param diagnostic prints messages about variables if TRUE
##' @keywords internal
##' @return NULL
##' @author Paul Johnson <pauljohn@@ku.edu>
checkValue_old <- function(x, value_old, xname, diagnostic = FALSE){
    if (!is.factor(x) && !is.character(x)) return(NULL)
    ## if value_old is length 1 and is equal to NA, don't bother
    if(all(value_old %in% c("NA", NA))) return(NULL)
    xobs <- unique(x)
    keynotinobs <- value_old[!value_old %in% xobs]
    keynotinobs <- na.omit(keynotinobs)
    if (diagnostic && length(keynotinobs) > 0){
        messg <- paste("Data Check (variable", xname, ":)\n",
                     "Key values were not observed in the input data: ",
                     paste(keynotinobs, collapse = ", "), "\n")
        cat(messg)
    }
    tobsnotinkey <- xobs[!xobs %in% value_old]
    tobsnotinkey <- na.omit(tobsnotinkey)
    if (diagnostic && length(tobsnotinkey) > 0) {
        messg <- paste("Data Check (variable", xname, ":)\n",
                       "These values in the input data were not in value_old: ",
                       paste(tobsnotinkey, collapse = ", "), "\n")
        cat(messg)
    }
    NULL
}



##' Check if values are R NA symbol or any one of the na.strings
##' elements
##'
##' A value vector in the key will generally be a character
##' vector.  This utility is used to check if the characters
##' are either R missing or values in a list of characters that
##' represent missings.
##' 
##' @param x Input data vector
##' @param na.strings Vector of string values to be considered as
##'     missing. Defaults will match values that are equal to ., empty
##'     string "", any number of white space elements, or charcter
##'     string "N/A". We do not include "NA" by default because some
##'     projects use "NA" to mean "not appropriate".
##' @return Logical vector, TRUE if a value is either NA or in
##'     na.strings.
##' @keywords internal
##' ## @examples
##' ## x1 <- c("TRUE", "FALSE", FALSE, TRUE, NA, "NA", ".", "N/A", " ", "")
##' ## x1na <- isNA(x1)
##' ## cbind(x1, x1na)
isNA <- function(x, na.strings = c("\\.", "", "\\s+",  "N/A")){
    ismissing <- grepl(paste0("^", paste0(na.strings, collapse="$|^"), "$"), x)
    ismissing[is.na(x)] <- TRUE
    ismissing
}

##' Create variable key template (in memory or in a file)
##'
##' A variable key is a human readable document that describes the
##' variables in a data set. A key can be revised and re-imported by R
##' to recode data. This might also be referred to as a
##' "programmable codebook."  This function inspects a data frame,
##' takes notice of its variable names, their classes, and legal
##' values, and then it creates a table summarizing that
##' information. The aim is to create a document that principal
##' investigators and research assistants can use to keep a project
##' well organized.  Please see the vignette in this package.
##'
##' The variable key can be created in two formats, wide and long.
##' The original style of the variable key, wide, has one row per
##' variable. It has a style for compact notation about current values
##' and required recodes.  That is more compact, probably easier for
##' experts to read, but perhaps more difficult to edit. The long
##' style variable key has one row per value per variable.  Thus, in a
##' larger project, the long key can have many rows. However, in a
##' larger project, the long style key is easier to edit with a spread
##' sheet program.
##'
##' After a key is created, it should be re-imported into R with the
##' \code{kutils::keyImport} function.  Then the key structure can
##' guide the importation and recoding of the data set.
##'
##' Concerning the varlab attribute. Run \code{attr(key, "varlab"} to
##' review existing labels, if any.
##'
##' Storing the variable labels in files requires some care because
##' the \code{rds}, \code{xlsx}, and \code{csv} formats have different
##' capabilities.  The \code{rds} storage format saves all attributes without
##' difficulty. In contrast, because \code{csv} and \code{xlsx} do not save
##' attributes, the varlabs are stored as separate character
##' matrices. For \code{xlsx} files, the varlab object is saved as a second
##' sheet in \code{xlsx} file, while in \code{csv} a second file suffixed
##' "-varlab.csv" is created. 
##'
##' @param dframe A data frame
##' @param long Default FALSE.
##' @param sort Default FALSE. Should the rows representing the
##'     variables be sorted alphabetically? Otherwise, they appear in
##'     the order in which they were included in the original dataset.
##' @param file DEFAULT NULL, meaning no file is produced. Choose a
##'     file name ending in either "csv" (for comma separated
##'     variables), "xlsx" (compatible with Microsoft Excel), or "rds"
##'     (R serialization data). The file name will be used to select
##'     among the 3 storage formats. XLSX output requires the openxlsx
##'     package.
##' @param max.levels How high is the limit on the number of values
##'     for discrete (integer, character, and Date) variables?
##'     Default = 15. If observed number exceeds max.levels, we
##'     conclude the author should not assign new values in the key
##'     and only the missing value will be included in the key as a
##'     "placeholder". This does not affect variables declared as
##'     factor or ordered variables, for which all levels are included
##'     in all cases.
##' @param missings Values in exising data which should be treated as
##'     missing in the new key. Character string in format acceptable
##'     to the \code{assignMissing} function. Can be a string with
##'     several missing indicators"1;2;3;(8,10);[22,24];> 99;< 2".
##' @param missSymbol Default ".".  A character string used to
##'     represent missing values in the key that is created.  Relevant
##'     (mostly) for the key's \code{value_new} column. Default is the
##'     period, ".". Because R's symbol \code{NA} can be mistaken for
##'     the character string \code{"NA"}, we use a different
##'     (hopefully unmistakable) symbol in the key.
##' @param varlab A key can have a companion data structure for
##'     variable labels. Default is FALSE, but the value may also be
##'     TRUE or a named vector of variable labels, such as
##'     \code{c("x1" = "happiness", "x2" = "wealth")}. The labels
##'     become an attribute of the key object. See Details for
##'     information on storage of varlabs in saved key files.
##' @param safeNumericToInteger Default TRUE: Should we treat values
##'     which appear to be integers as integers? If a column is
##'     numeric, it might be safe to treat it as an integer.  In many
##'     csv data sets, the values coded c(1, 2, 3) are really
##'     integers, not floats c(1.0, 2.0, 3.0). See \code{safeInteger}.
##' @param trimws Default is "both", user can change to "left", "right", or
##'     set as NULL to avoid any trimming.
##' @return A key in the form of a data frame. May also be saved on
##'     disk if the file argument is supplied. The key may have an
##'     attribute "varlab", variable labels.
##' @export
##' @importFrom utils write.csv
##' @importFrom methods as
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' set.seed(234234)
##' N <- 200
##' mydf <- data.frame(x5 = rnorm(N),
##'                    x4 = rpois(N, lambda = 3),
##'                    x3 = ordered(sample(c("lo", "med", "hi"),
##'                    size = N, replace=TRUE),
##'                    levels = c("med", "lo", "hi")),
##'                    x2 = letters[sample(c(1:4,6), N, replace = TRUE)],
##'                    x1 = factor(sample(c("cindy", "bobby", "marcia",
##'                                         "greg", "peter"), N,
##'                    replace = TRUE)),
##'                    x7 = ordered(letters[sample(c(1:4,6), N, replace = TRUE)]),
##'                    x6 = sample(c(1:5), N, replace = TRUE),
##'                    stringsAsFactors = FALSE)
##' mydf$x4[sample(1:N, 10)] <- 999
##' mydf$x5[sample(1:N, 10)] <- -999
##'
##' ## Note: If we change this example data, we need to save a copy in
##' ## "../inst/extdata" for packacing
##' dn <- tempdir()
##' write.csv(mydf, file = file.path(dn, "mydf.csv"), row.names = FALSE)
##' mydf.templ <- keyTemplate(mydf, file = file.path(dn, "mydf.templ.csv"),
##'                           varlab = TRUE)
##' mydf.templ_long <- keyTemplate(mydf, long = TRUE,
##'                             file = file.path(dn, "mydf.templlong.csv"),
##'                             varlab = TRUE)
##'
##' mydf.templx <- keyTemplate(mydf, file = file.path(dn, "mydf.templ.xlsx"),
##'                             varlab = TRUE)
##' mydf.templ_longx <- keyTemplate(mydf, long = TRUE,
##'                              file = file.path(dn, "mydf.templ_long.xlsx"),
##'                              varlab = TRUE)
##' ## Check the varlab attribute
##' attr(mydf.templ, "varlab")
##' mydf.tmpl2 <- keyTemplate(mydf,
##'                          varlab = c(x5 = "height", x4 = "age",
##'                          x3 = "intelligence", x1 = "Name"))
##' ## Check the varlab attribute
##' attr(mydf.tmpl2, "varlab")
##' 
##' ## Try with the national longitudinal study data
##' data(natlongsurv)
##' natlong.templ <- keyTemplate(natlongsurv,
##'                           file = file.path(dn, "natlongsurv.templ.csv"),
##'                           max.levels = 15, varlab = TRUE, sort = TRUE)
##'
##' natlong.templlong <- keyTemplate(natlongsurv, long = TRUE,
##'                    file = file.path(dn, "natlongsurv.templ_long.csv"), sort = TRUE)
##' if(interactive()) View(natlong.templlong)
##' natlong.templlong2 <- keyTemplate(natlongsurv, long = TRUE,
##'                       missings = "<0", max.levels = 50, sort = TRUE,
##'                       varlab = TRUE)
##' if(interactive()) View(natlong.templlong2)
##' 
##' natlong.templwide2 <- keyTemplate(natlongsurv, long = FALSE,
##'                       missings = "<0", max.levels = 50, sort = TRUE)
##' if(interactive()) View(natlong.templwide2)
##'
##' all.equal(wide2long(natlong.templwide2), natlong.templlong2)
##'
##' head(keyTemplate(natlongsurv, file = file.path(dn, "natlongsurv.templ.xlsx"),
##'              max.levels = 15, varlab = TRUE, sort = TRUE), 10)
##' head(keyTemplate(natlongsurv, file = file.path(dn, "natlongsurv.templ.xlsx"),
##'              long = TRUE, max.levels = 15, varlab = TRUE, sort = TRUE), 10)
##'
##' list.files(dn)
##'
keyTemplate <-
    function(dframe, long = FALSE, sort = FALSE,
             file = NULL, max.levels = 15, missings = NULL, missSymbol = ".",
             safeNumericToInteger = TRUE, trimws = "both", 
             varlab = FALSE)
{
    if (class(dframe)[1] != "data.frame"){
        MESSG <- paste("Warning: keyTemplate is intended for an R *data.frame*,",
                       "not subclasses like tibbles or data.tables.",
                       "keyTemplate coerced your input",
                       "with as.data.frame().\n")
        if (inherits(dframe, "data.frame")){
            ## Coerce without warning, but print message
            cat(MESSG)
        } else {
            ## Elevate the message to a warning
            warning(MESSG)
        }
        dframe <- as.data.frame(dframe)
    }
            
    dframe <- cleanDataFrame(dframe, safeNumericToInteger = safeNumericToInteger,
                             trimws = trimws)

    df.class <- sapply(dframe, function(x)class(x)[1])
    cn <- colnames(dframe)

    ## Keep at most max.levels elements, but always add an NA symbol
    ## if there is none.  Also, always keep value that are missing
    ## according to missings
    shortenValues <- function(x, max.levels, missings){
        xmiss <- assignMissing(x, missings)
        xnotmissing <- if (length(temp <- x[-which(is.na(xmiss))]) > 0) temp else NULL
        xismissing <- unique(x[which(is.na(xmiss))])
        if (!any(vapply(xismissing, is.na, logical(1)))) xismissing <- c(xismissing, NA)
        if (length(xnotmissing) <= max.levels) {
            xnotmissing <- xnotmissing[1:min(max.levels, length(xnotmissing))]
            df <- data.frame(value_old = c(xnotmissing, xismissing),
                             value_new = c(xnotmissing, rep(NA, length(xismissing))),
                             stringsAsFactors = FALSE)
            return(df)
        } else {
            return(data.frame(value_old = NA, value_new = NA,
                              stringsAsFactors = FALSE))
        }
    }

    ## Returns all unique values, inserts NA at end if not present
    getUnique <- function(xname){
        ## For discrete variables integer, character, logical:
        if (df.class[[xname]] %in% c("integer", "character", "logical")){
            val <- unique(dframe[ , xname, drop = TRUE])
            ##pj 20170926: new sort method keeps missing on end, sets NA as missSymbol
            ##pj 20180502: tibble fails on following order function.
            val.sort <- val[order(val)]
            if(!NA %in% val.sort) val.sort <- c(val.sort, NA)
            return(val.sort)
        }
        ## For discrete variables factor, ordered:
        if (df.class[[xname]] %in% c("factor", "ordered")){
            return(c(levels(dframe[ , xname]), NA))
        }
        ## if coercion check passes, then use "as" to coerce the missing
        if (checkCoercion(c(NA), df.class[[xname]])) {
            value <- NA
            mytext <- paste0("as.",  df.class[[xname]], "(value)")
            res <- eval(parse(text = mytext))
            return(res)
        } else{
            ## Give up trying to cast the NA type
            return(c(NA))
        }
    }

    ## First, make a long key
    ## Generate a small key for one variable
    smallTemplate <- function(xname, missings = NULL){
        value_old <- getUnique(xname)
        valoldnew <- shortenValues(value_old,
                                   max.levels = max.levels,
                                   missings = missings)
        valoldnew[is.na(valoldnew)] <- missSymbol
        keysmall <- data.frame(name_old = xname, name_new = xname, 
                               class_old = df.class[[xname]],
                               class_new = df.class[[xname]],
                               value_old = as.character(valoldnew[ , "value_old"]),
                               value_new = as.character(valoldnew[ , "value_new"]),
                               missings = "",
                               recodes = "", 
                               stringsAsFactors = FALSE)
        keysmall
    }
        
    keyList <- lapply(cn, smallTemplate, missings = missings)

    key <- do.call("rbind", keyList)

    if(isTRUE(long)){
        if (sort) key <- key[order(key$name_old, key$name_new), ]
        class(key) <- c("keylong", class(key))
     } else {
        ## else !long, so make a wide key
        key <- long2wide(key)
        rownames(key) <- key$name_old
        if (sort) key <- key[order(key$name_old), ]
        ## put in order of original data columns
        else key <- key[cn, ]
    }

    attr(key, "missSymbol") <- missSymbol
    if (!missing(varlab) && !identical(varlab, FALSE)) {
        attr(key, "varlab") <- varlabTemplate(key, varlab)
        varlab <- TRUE
    }
   
    if (!missing(file) && !is.null(file)){
        keySave(key, file, na_ = missSymbol, varlab = varlab)
    }
    key
}
NULL

##' Create Variable Label Template
##'
##' Receive a key, create a varlab object, with columns
##'     \code{name_old} \code{name_new}, and \code{varlab}.
##'
##' If not specified, a matrix is created with empty variable labels.
##' @param obj A variable key
##' @param varlab Default NULL, function will start from clean slate,
##'     a set of column labels that match \code{name_new}. User can
##'     specify values by providing a named vector of labels, e.g.,
##'     \code{c("x1" = "happiness", "x2" = "wealth")}, where the names
##'     are values to be matched against "name_new" in key.
##' @return Character matrix with columns \code{name_new} and \code{varlab}.
##' @export
##' @author Paul Johnson
##' @examples
##' mydf.path <- system.file("extdata", "mydf.csv", package = "kutils")
##' mydf <- read.csv(mydf.path, stringsAsFactors=FALSE)
##' mydf.keywide1 <- keyTemplate(mydf, long = FALSE, sort = FALSE,
##'                     varlab = TRUE)
##' attr(mydf.keywide1, "varlab")
##' mydf.keywide2 <- keyTemplate(mydf, long = FALSE, sort = FALSE,
##'                     varlab = c("x3" = "fun"))
##' attr(mydf.keywide2, "varlab")
##' attr(mydf.keywide2, "varlab") <- varlabTemplate(mydf.keywide2,
##'                   varlab = c("x5" = "wealth", "x10" = "happy"))
##' attr(mydf.keywide2, "varlab")
##' attr(mydf.keywide2, "varlab") <- varlabTemplate(mydf.keywide2,
##'                   varlab = TRUE)
##' attr(mydf.keywide2, "varlab")
##' ## Target we are trying to match:
##' mydf.keylong <- keyTemplate(mydf, long = TRUE, sort = FALSE, varlab = TRUE)
##' attr(mydf.keylong, "varlab")
##' attr(mydf.keylong, "varlab") <- NULL
##' varlabTemplate(mydf.keylong)
##' attr(mydf.keylong, "varlab") <- varlabTemplate(mydf.keylong,
##'                    varlab = c("x3" = "wealth", "x10" = "happy"))
##' attr(mydf.keylong, "varlab")
##' attr(mydf.keylong, "varlab") <- varlabTemplate(mydf.keylong, varlab = TRUE)
##' attr(mydf.keylong, "varlab")
varlabTemplate <- function(obj, varlab = TRUE){
    if (identical(varlab, FALSE)){
        return(NULL)
    }
    varlabs.orig <- attr(obj, "varlab")
    if(is.null(varlabs.orig)){
        if (isTRUE(varlab)) {
            varlabs <- unique(obj[ , "name_new", drop = TRUE])
            names(varlabs) <- varlabs
            return(varlabs)
        } else if (is.vector(varlab)){
            return(varlab)
        } else {
            MESSG <- paste("varlabTemplate input:",
                           paste(varlab, collapse = " "),
                           "is not understandable")
            stop(MESSG)
        }
    }

    varlabs.all <- unique(obj[ , "name_new"])
    names(varlabs.all) <- varlabs.all
    if (isTRUE(varlab)){
        ## don't change existing varlabs, only new ones
        ## replace from varlabs.orig into varlabs.all
        varlabs <- varlabs.all
        varlabs.addtokey <- varlabs.all[!names(varlabs.all) %in% names(varlabs.orig)]
        return(c(varlabs.orig, varlabs.addtokey))
    } else if (is.vector(varlab)){
        ## keep if in original and not in varlab
        keepinkey <- varlabs.orig[!names(varlabs.orig) %in% names(varlab)]
        ## add from varlab to replace
        replaceinkey <- varlab[names(varlab) %in% names(varlabs.orig)]
        addtokey <- varlab[!names(varlab) %in% names(varlabs.orig)]
        res <- c(keepinkey, replaceinkey, addtokey)
        if(any(duplicated(names(res)))) stop("varlabTemplate fail1")
        return(c(keepinkey, replaceinkey, addtokey))
    } else {
            MESSG <- paste("varlabTemplate input:",
                           paste(varlab, collapse = " "),
                           "is not understandable")
            stop(MESSG)
    }
}
NULL

##' Save key as file after deducing type from suffix
##'
##' This is specialized to saving of key objects, it is not a
##' general purpose function for saving things.  It scans the
##' suffix of the file name and then does the right thing.
##'
##' In updates 2017-09, a varlab element was introduced.  The varlab
##' attribute of the object is saved.  The files created incorporate
##' the variable labels object in different ways. 1) XLSX: variable
##' labels a worksheet named "varlab" 2) CSV: variable labels saved in
##' a separate file suffixed "-varlab.csv". 3) RDS: varlab is an
##' attribute of the key object.
##' @param obj a variable key object
##' @param file file name. must end in "csv", "xlsx" or "rds"
##' @param na_ Value to insert to represent a missing score. Default
##'     ".".
##' @param varlab FALSE or TRUE. Default is FALSE, no new labels will
##'     be created. If a key object has a varlab already, it is saved
##'     with the key, always. This parameter controls whether a new
##'     varlab template should be created when the object is saved.
##'     If TRUE and obj has no varlab attribute, a new varlab template
##'     is created by the \code{varlabTemplate} function. If TRUE and
##'     a varlab attribute currently exists, but some variables are
##'     missing labels, then \code{varlabTemplate} is called to fill
##'     in new variable labels.
##' @return NULL if no file is created. Otherwise, a key object with
##'     an attribute varlab is returned.
##' @importFrom openxlsx addWorksheet writeDataTable saveWorkbook
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu>
keySave <- function(obj, file, na_ = ".", varlab){
    obj[is.na(obj)] <- "."
    if (!missing(varlab) && !varlab %in% c(TRUE, FALSE)){
        MESSG <- "keySave varlab argument must be TRUE or FALSE"
        stop(MESSG)
    }
    
    if (!is.null(attr(obj, "varlab"))){
        varlab <- TRUE
    }  else if (!missing(varlab) && !identical(varlab, FALSE)
                && is.null(attr(obj, "varlab"))) {
     ## varlab neither provided with key nor varlab == FALSE, so create
        attr(obj, "varlab") <- varlabTemplate(obj, varlab = TRUE)
        varlab <- TRUE
    } else {
        varlab <- FALSE
    }
      
    if (length(grep("csv$", tolower(file))) > 0){
        write.csv(obj, file = file, na = na_, row.names = FALSE)
        if(!identical(varlab, FALSE) && !is.null(attr(obj, "varlab"))){
            varlab.orig <- attr(obj, "varlab")
            varlab.mat <- cbind("name_new" = names(varlab.orig),
                                "varlab" = varlab.orig)
            write.csv(varlab.mat,
                      file = gsub(".csv$", "-varlab.csv", file),
                      row.names = FALSE, na = na_)
        }
    } else if (length(grep("xlsx$", tolower(file)))){
        wb <- openxlsx::createWorkbook()
        addWorksheet(wb, "key")
        writeDataTable(wb, sheet = "key", x = obj)
        if(!identical(varlab, FALSE) && !is.null(attr(obj, "varlab"))){
            varlab.orig <- attr(obj, "varlab")
            varlab.mat <- data.frame("name_new" = names(varlab.orig),
                                "varlab" = varlab.orig)
            addWorksheet(wb, "varlab")
            writeDataTable(wb, sheet = "varlab", x = varlab.mat)
        }
        saveWorkbook(wb, file, overwrite = TRUE)
    } else if (length(grep("rds$", tolower(file)))){
        saveRDS(obj, file = file)
    } else {
        warning("keySave: unrecognized suffix. No file created")
        NULL
    }
    invisible(obj)
}
NULL

##' Read file after deducing file type from suffix.
##'
##' If the input is XLSX, sheets named "key" and "varlab" are
##' imported if the exist. If input is CSV, then the key
##' CSV file is imported and another file suffixed with "-varlab" is
##' imported if it exists.
##'
##' The variable lables are a named vector saved as an attribute of
##' the key object.
##' @param file name of file to be imported, including path to
##'     file. file name must end in "csv", "xlsx" or "rds"
##' @param ... additional arguments for read.csv or read.xlsx.
##' @param na.strings Values to be converted to R missing symbol
##'     NA. Default is white space, "\\s+".
##' @return A data frame or matrix.
##' @importFrom utils read.csv
##' @importFrom openxlsx read.xlsx getSheetNames readWorkbook
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu>
keyRead <- function(file, ..., na.strings = c("\\s+")){
    ## TODO: implement code to sort out the dots arguments, find
    ## which are aimed at read.xlsx or read.csv, and divide them. See
    ## peek() function example.
    dots <- list(...)
    readxlsxFormals <- names(formals(openxlsx::read.xlsx))
    readcsvFormals <- names(formals(read.csv))
    dotsforxlsx <- dots[readxlsxFormals[readxlsxFormals %in% names(dots)]]
    dotsforcsv <- dots[readcsvFormals[readcsvFormals %in% names(dots)]]
    if (!is.character(file) || !file.exists(file)){
        messg <- paste("keyRead: 'file' not found")
        stop(messg)
    } else {
        ## key is file name, so scan for suffix
        if (length(grep("xlsx$", tolower(file))) > 0){
            sheetNames <- getSheetNames(file)
            xlsxargs <- list(xlsxFile = file, sheet = "key", colNames = TRUE,
                             check.names = FALSE, na.strings = na.strings)
            xlsxargz <- modifyList(xlsxargs, dotsforxlsx)
            key <- do.call("readWorkbook", xlsxargz)
            ## Force columns to be of type "character"
            ## replace NAs with empty strings
            for(i in colnames(key)){
                if (class(key[ , i]) != "character") key[ , i] <- as.character(key[ , i])
                key[which(is.na(key[ ,i])), i] <- "."
            }
            if ("varlab" %in% sheetNames){
                xlsxargz[["sheet"]] <- "varlab"
                varlab.mat <- tryCatch(do.call("readWorkbook", xlsxargz),
                                       finally = NULL)
                varlab <- varlab.mat[ , "varlab"]
                names(varlab) <- varlab.mat[ , "name_new"]
                attr(key, "varlab") <- varlab
            }
        } else if (length(grep("csv$", tolower(file))) > 0){
            csvargs <- list(file = file, stringsAsFactors = FALSE,
                            colClasses = "character", na.strings = na.strings)
            csvargz <- modifyList(csvargs, dotsforcsv)
            key <- do.call("read.csv", csvargz)
            filevarlab <- gsub("csv$", "-varlab.csv", file)
            if (file.exists(filevarlab)){
                csvargs[["file"]] <- filevarlab
                varlab.mat <- do.call("read.csv", csvargz)
                varlab <- varlab.mat[ , "varlab"]
                names(varlab) <- varlab.mat[ , "name_new"]
                attr(key, "varlab") <- varlab
            }
        } else if (length(grep("rds$", tolower(file))) > 0){
            key <- readRDS(file)
        }
    }
    invisible(key)
}
NULL

##' Convert nothing to R missing(NA).
##'
##' By "nothing", we mean white space or other indications of
##' nothingness.  Goal is to find character strings that users
##' might insert in a key to indicate missing values. Those things,
##' which are given default values in the argument nothings, will be
##' changed to NA.
##'
##' Using regular expression matching, any value that has nothing
##' except for the indicated "nothing" values is converted to NA.  The
##' "nothing" values included by default are a period by itself (A SAS
##' missing value), an empty string, or white space, meaning " ", or
##' any number of spaces, or a tab.
##' @param x A character vector. If x is not a character vector, it is
##'     returned unaltered without warning.
##' @param nothings A vector of values to be matched by regular
##'     expressions as missing.  The default vector is c("\\.",
##'     "\\s"), where "\\." means a literal period (backslashes needed
##'     to escape the symbol which would otherwise match anything in a
##'     regular expression).
##' @param zapspace Should leading and trailing white space be
##'     ignored, so that, for example " . " and "." are both treated
##'     as missing.
##' @return A vector with "nothing" values replaced by R's NA symbol.
##'     Does not alter other values in the vector. Previous version
##'     had applied zapspace to non-missing values, but it no longer
##'     does so.
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @export
##' @examples
##' gg <- c("", " ", "   ", "\t", "\t some", "some\t", " space first", ".",
##'        " . ")
##' n2NA(x = gg)
##' n2NA(x = gg, zapspace = FALSE)
##' n2NA(x = gg, nothings = c("\\s"), zapspace = FALSE)
##' n2NA(x = gg, nothings = c("\\."), zapspace = TRUE)
##' n2NA(x = gg, nothings = c("\\."), zapspace = FALSE)
n2NA <- function(x, nothings = c("\\.", "\\s"), zapspace = TRUE){
    if (!is.character(x)) {
        return(x)
    }
    if (!zapspace){
        for(j in seq_along(nothings)){
            x[grep(paste0("^", nothings[j], "*$"), x)] <- NA
        }
    } else {
        for(j in seq_along(nothings)){
            x[grep(paste0("^\\s*", nothings[j], "*\\s*$"), x)] <- NA
        }
    }
    x
}
NULL

##' Convert leading or trailing white space and tab characters to nothing.
##'
##' This eliminates any characters matched by the regular expression
##' `\\s` if they appear at the beginning of a string or at its
##' end. It does not alter spaces in the interior of a string. This
##' was created when I was not aware of R's \code{trimws} and the purpose
##' is the same. On our TODO list, we intend to eliminate this function
##' and replace its use with \code{trimws}
##' @param x A character vector
##' @return If x is a character vector, return is a character vector
##'     with leading and trailing white space values removed. If x is
##'     not a character vector, an unaltered x is returned.
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @export
##' @examples
##' gg <- c("", " ", "   ", "\t", "\t some", "some\t", " space first")
##' (zapspace(gg))
zapspace <- function(x){
    if (!is.character(x)){
        return(x)
    }
    y <- gsub("^\\s*", "", x)
    y <- gsub("\\s*$", "", y)
    y
}
NULL


##' Import/validate a key object or import/validate a key from a file.
##'
##' After the researcher has updated the key by filling in new names
##' and values, we import that key file. This function can import the
##' file by its name, after deducing the file type from the suffix, or
##' it can receive a key object from memory.
##'
##' This can be either a wide or long format key file.
##'
##' This cleans up variables in following ways.  1) \code{name_old}
##' and \code{name_new} have leading and trailing spaces removed 2)
##' \code{value_old} and \code{value_new} have leading and trailing
##' spaces removed, and if they are empty or blank spaces, then new
##' values are set as NA.
##'
##' Policy change concerning empty "value_new" cells in input keys
##' (20170929).
##' 
##' There is confusion about what ought to happen in a wide key when
##' the user leaves value_new as empty or missing. Literally, this
##' means all values are converted to missing, which does not seem
##' reasonable. Hence, when a key is wide, and value_new is one of the
##' na.strings elements, we assume the value_new is to be copied
##' from value_old. That is to say, if value_new is not supplied,
##' the values remain same as in old data.
##'
##' In a long key, the behavior is different.  Since the user can
##' specify each value for a variable in a separate row, the na.strings
##' appearing in value_new are treated as missing scores in the new
##' data set to be created.
##' 
##' @param key A key object (class key or keylong) or a file name
##'     character string (ending in csv, xlsx or rds).
##' @param ignoreCase In the use of this key, should we ignore
##'     differences in capitalization of the "name_old" variable?
##'     Sometimes there are inadvertent misspellings due to changes in
##'     capitalization. Columns named "var01" and "Var01" and "VAR01"
##'     probably should receive the same treatment, even if the key
##'     has name_old equal to "Var01".
##' @param sep Character separator in \code{value_old} and
##'     \code{value_new} strings in a wide key. Default is are "|".
##'     It is also allowed to use "<" for ordered variables. Use
##'     regular expressions in supplying separator values.
##' @param na.strings Values that should be converted to missing data.
##'     This is relevant in \code{name_new} as well as
##'     \code{value_new}. In spreadsheet cells, we treat "empty" cells
##'     (the string ""), or values like "." or "N/A", as missing with
##'     defaults ".", "", "\\s" (white space), and "N/A". Change that
##'     if those are not to be treated as missings.
##' @param ... additional arguments for read.csv or read.xlsx.
##' @param keynames Don't use this unless you are very careful. In our
##'     current scheme, the column names in a key should be
##'     c("name_old", "name_new", "class_old", "class_new",
##'     "value_old", "value_new", "missings", "recodes"). If your key
##'     does not use those column names, it is necessary to provide
##'     keynames in a format "our_name"="your_name". For example,
##'     keynames = c(name_old = "oldvar", name_new = "newname",
##'     class_old = "vartype", class_new = "class", value_old =
##'     "score", value_new = "val").
##' @param missSymbol Defaults to period "." as missing value
##'     indicator.
##' @export
##' @return key object, should be same "wide" or "long" as the input
##'     Missing symbols in value_old and value_new converted to ".".
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' mydf.key.path <- system.file("extdata", "mydf.key.csv", package = "kutils")
##' mydf.key <-  keyImport(mydf.key.path)
##' ## Create some dupes
##' mydf.key <- rbind(mydf.key, mydf.key[c(1,7), ])
##' mydf.key2 <- keyImport(mydf.key)
##' mydf.key2
##' ## create some empty value_new cells
##' mydf.key[c(3, 5, 7) , "value_new"] <- ""
##' mydf.key3 <- keyImport(mydf.key)
##' mydf.key3
##' mydf.keylong.path <- system.file("extdata", "mydf.key_long.csv", package = "kutils")
##' mydf.keylong <- keyImport(mydf.keylong.path)
##'
##' ## testDF is a slightly more elaborate version created for unit testing:
##' testdf.path <- system.file("extdata", "testDF.csv", package = "kutils")
##' testdf <- read.csv(testdf.path, header = TRUE)
##' keytemp <- keyTemplate(testdf, long = TRUE)
##' ## A "hand edited key file"
##' keyPath <- system.file("extdata", "testDF-key.csv", package="kutils")
##' key <- keyImport(keyPath)
##' keydiff <- keyDiff(keytemp, key)
##' key2 <- rbind(key, keydiff$neworaltered)
##' key2 <- unique(key)
##' if(interactive())View(key2)
##' 
keyImport <- function(key, ignoreCase = TRUE,
                      sep = c(character = "\\|", logical = "\\|",
                              integer = "\\|", factor = "\\|",
                              ordered = "[\\|<]", numeric = "\\|")
                      , na.strings = c("\\.", "", "\\s+",  "N/A")
                      , missSymbol = "."
                      , ...
                      , keynames = NULL)
{
    if (is.character(key)) key <- keyRead(key)

    legalClasses = c("integer", "numeric", "double", "factor",
                     "ordered", "character", "logical")
    keynames.std <- c("name_old",
                      "name_new",
                      "class_old",
                      "class_new",
                      "value_old",
                      "value_new",
                      "missings",
                      "recodes")

    if(!is.null(keynames)){
        ## User supplied keynames, so reverse their keynames
        ## to put corrected names onto the  key
        colnames(key) <- mapvalues(colnames(key), from = keynames,
                                   to = names(keynames), warn_missing=FALSE)
    }
    ## Use partial matching to try to fix column names that editor
    ## may have altered accidentally. 
    ## If key does not include all of the expected
    ## names, this uses partial matching to replace the existing
    ## unrecognized names.
    if (any(!keynames.std %in% colnames(key))) { 
        key.oldnames <- colnames(key)
        ## Filter out oldnames that don't pmatch, rm the NAs
        key.oldnames.pmatches <- na.omit(key.oldnames[pmatch(key.oldnames, keynames.std)])
        keynames.std.pmatches <- keynames.std[pmatch(key.oldnames.pmatches, keynames.std)]
        colnames(key) <- mapvalues(colnames(key),
                                   from = key.oldnames.pmatches,
                                   to = keynames.std.pmatches)
    }
    ## Now, what to do if columns are missing?
    ## if no "name_old" "class_old", "value_old", then quit
    ## if "name_new", "class_new", "value_new" missing, then copy from old.
    ## if "missings" and "recodes" are missing, create new full of ""
    ## TODO: omit blank rows
    cols.required <- c("name_old", "class_old", "value_old")
    cols.copyable <- c("name_new", "class_new", "value_new")
    cols.blankable <- c("missings", "recodes")
    if(any(!cols.required %in% colnames(key))){
        MESSG <- paste0("Key is missing columns: ",
                 paste0(cols.required[!cols.required %in% colnames(key)], collapse = " "))
        stop(MESSG)
    }
    if(any(!cols.copyable %in% colnames(key))){
        cols.missing <- cols.copyable[!cols.copyable %in% colnames(key)]
        for(i in cols.missing) {
            j <- gsub("new$", "old", i)
            key[ , i] <- key[ ,j]
        }
    }
    if(any(!cols.blankable %in% colnames(key))){
        cols.missing <- cols.blankable[!cols.blankable %in% colnames(key)]
        for(i in cols.missing) {
            key[ , i] <- ""
        }
    }

    
   
    uniquifyNameNew <- function(key, long = FALSE){
        ## wide key easy!
        if(!long){
            name_new_orig <- key[ , "name_new"]
            key[ , "name_new"] <- make.names(key[ , "name_new"], unique = TRUE)
            if (sumdupes <- sum(key[ , "name_new"] != name_new_orig)){
                MESSG <- paste("keyImport: Wide key with duplicated name_new values.",
                               sumdupes, " name_new values were altered:\n")
                cat(MESSG)
                print(key[name_new_orig != key[ , "name_new"], c("name_old", "name_new")])
            }
            return(key[ , "name_new"])
        } else {
            ## long key, TODO 20171003: Difficult to get right for
            ## sure, so lets return an error now, think on it later.
            newindex <- paste0(key[ , "name_old"], "|",  key[ , "name_new"], "|",
                               key[ , "value_old"])
            dupes <- duplicated(newindex)
            if(any(dupes)){
                cat("keyImport: These are duplicated rows in the key:\n")
                print(key[dupes, c("name_old", "name_new", "value_old")])
                MESSG <- paste("keyImport terminated: long key format is incorrect.")
                stop(MESSG)
            }
            return(key[ , "name_new"])
        }
     }

    key.orig <- key

    ## Omit key rows in which name_new is a missing or na.string
    key <- key[!isNA(key$name_new), ]
    
    ## Deduce if this is a long key. If separators | < assume it is wide
    if (any(grepl("[\\|<]", key$value_old))) {
        ## There are separators, this is almost surely a wide key
        long <- FALSE
    } else {
        long <- TRUE
    }
    
    if (!long){
        MESSG <- paste("keyImport guessed that is a wide format key.\n")
        cat(MESSG)
        ## Use the unique-ified key now
        key[ , "name_new"] <- uniquifyNameNew(key, long = FALSE)
        ## If wide key and value_new is empty/missing, make it a copy of value_old.
        ## Otherwise, the missing treatment sets all values as missing, which is pointless.
        ismissing <- isNA( key[ , "value_new"])
        key[ismissing, "value_new"] <- key[ismissing , "value_old"]
        key <- wide2long(key, sep)
        keysplit <- split(key, key[ , "name_new"])
        valcheck <- vapply(keysplit, function(keyvar){
            keyclass <- unique(keyvar$class_new)
            isTRUE(checkCoercion(keyvar$value_new,  unique(keyclass),
                                 na.strings = na.strings))
        }, logical(1))
        if(any(!valcheck)){
            MESSG <- "value_new in key cannot be coerced to class_new\n"
            cat(MESSG)
            print(keysplit[!valcheck])
            MESSG <- 
            stop(MESSG)
        }
        
        rownames(key) <- NULL
    } else {
        key[ , "name_new"] <- uniquifyNameNew(key, long = TRUE)
        keyw <- long2wide(key)
        ## Use same trick as in import of wide key to replace empty value_new
        ## with value_old
        ismissing <- grepl(paste0("^", paste0(na.strings, collapse="$|^"), "$"), keyw[ , "value_new"])
        keyw[ismissing, "value_new"] <- keyw[ismissing , "value_old"]
        key <- wide2long(keyw)
    }
    
    ## For Logicals: If value_new is 1 or 0, convert to TRUE/FALSE symbols
    key[key$class_new == "logical" & !is.na(key$value_new) & key$value_new == 1, "value_new"] <- TRUE
    key[key$class_new == "logical" & !is.na(key$value_new) & key$value_new == 0, "value_new"] <- FALSE
    key[key$class_old == "logical" & !is.na(key$value_old) & key$value_old == 1, "value_old"] <- TRUE
    key[key$class_old == "logical" & !is.na(key$value_old) & key$value_old == 0, "value_old"] <- FALSE
    
    key$missings <- gsub("<-", "< -", key$missings, fixed = TRUE)
    
    ## protect against user-inserted spaces (leading or trailing)
    key$name_old <- zapspace(key$name_old)
    key$name_new <- zapspace(key$name_new)
    
    ## if recode or missings are not supplied, we get logicals.
    ## handle empty missing/recode columns
    if (all(is.na(key$missings))) key$missings <- character(length(key$missings))
    if (all(is.na(key$recodes))) key$recodes <- character(length(key$recodes))
    
    ## If name_new is any missing symbol, remove from key
    remove <- key$name_old[key$name_new %in% na.strings]
    key <- key[!key$name_old %in% remove,]

    if (length(unique(remove)) >= 1){
        MESSG <- paste0("keyImport: Delete entries for ",
                        length(remove),
                        "variables because name_new is an na.string.\n") 
        warning(MESSG)
    }

    ## if this is long key, following is safe. How about wide key?
    key$value_old <- trimws(key$value_old)
    key$value_new <- trimws(key$value_new)

    ## pj 20171002: following appears redundant
    ## key$value_old <- ifelse(is.na(key$value_old), missSymbol, key$value_old)
    ## key$value_new <- ifelse(is.na(key$value_new), missSymbol, key$value_new)
    key[key$value_new %in% na.strings, "value_new"] <- missSymbol
    key[key$value_old %in% na.strings, "value_old"] <- missSymbol
    
    ## Delete repeated rows:
    key <- key[!duplicated(key), ]

    if (any(!unique(key$class_new) %in% legalClasses)){
        messg <- paste("Unfamiliar class_new value\n",
                       paste(unique(key$class_new)[!unique(key$class_new) %in% legalClasses], collapse = ", ")
                       )
        warning(messg)
    }

    if (long){
        class(key) <- c("keylong", "data.frame")
        attr(key, "ignoreCase") <- ignoreCase
        attr(key, "na.strings") <- na.strings
        return(key)
    } else {
        keywide <- long2wide(key)
        class(keywide) <- c("key", "data.frame")
        attr(keywide, "ignoreCase") <- ignoreCase
        attr(keywide, "na.strings") <- na.strings
        return(keywide)
    }
    stop("keyImport should not reach this point")
}



##' Convert the variable key into a list of long keys, one for each variable.
##'
##' A keylist element is a long key for one new variable. The new
##' variables are named by "name_old.name_new" for uniqueness.
##'
##' If the key has one old variable being recoded 6 ways, that begets
##' 6 elements in the resulting list. Attributes including the classes
##' of the old and new variables are included.
##'
##' @param key A key object or a file name, csv, xlsx or rds.
##' @param sep Separator regular expressions
##' @param na.strings Strings that will be treated as NA. This will be
##'     used only if the key object does not have an na.strings
##'     attribute.
##' @keywords internal
##' @return A list with one element per variable name, along with some
##'     attributes like class_old and class_new. The class is set as
##'     well, "keylist"
##' @author Paul Johnson <pauljohn@@ku.edu>
## ##' @examples
## ##' mydf.key.path <- system.file("extdata", "mydf.key.csv", package = "kutils")
## ##' mydf.key <-  keyImport(mydf.key.path)
## ##' mydf.key.keylist <- makeKeylist(mydf.key)
## ##' mydf.keylong.path <- system.file("extdata", "mydf.key_long.csv", package = "kutils")
## ##' mydf.keylong <- keyImport(mydf.keylong.path)
## ##' mydf.keylong.keylist
makeKeylist <- function(key,
                        sep = c(character = "\\|", logical = "\\|",
                              integer = "\\|", factor = "\\|",
                              ordered = "[\\|<]", numeric = "\\|"),
                        na.strings = c("\\.", "", "\\s+",  "N/A"))
{
    ## if x is in na.strings, return NA
    ## if "split" is NA or blank space or TAB, return unchanged x
    strsplit2 <- function(x, split, fixed = FALSE, perl = FALSE, useBytes = FALSE){
        if (is.na(n2NA(zapspace(split)))) return(x)
        if (is.na(n2NA(zapspace(x)))) return(NA)
        strsplit(x, split, fixed = fixed, perl = perl, useBytes = useBytes)
    }

    ## if it is already a key list, return with no changes
    if (inherits(key,"keylist")) return(key)
    if (is.character(key)) key <- keyImport(key)
    if (inherits(key, "key")){
        long <- FALSE
    } else if (inherits(key, "keylong")){
        long <- TRUE
    }
    ## Allow arguments to override na.strings from key?
    if (missing(na.strings) && !is.null(attr.na.strings <- attr(key, "na.strings"))){
        na.strings <- attr.na.strings
    }
    if (!long){
        key <- wide2long(key)
    }
    
    ## like unique, but it throws away white space, NA, and
    ## fails if there is more than one unique nonmissing value
    unique.one <- function(x){
        y <- unique(na.omit(n2NA(zapspace(x))))
        if (length(y) > 1){
            messg <- paste("Value of", deparse(substitute(x)), "not unique")
            stop (messg)
        }
        y
    }

    ## Clean up the "name_new" column. Danger that 2 name_new
    ## values are same for different variables, but they must be
    ## unique named columns.
    name_new.unique <- unique(key$name_new)
    name_new.clean <- make.names(name_new.unique, unique = TRUE)
    names(name_new.clean) <- name_new.unique
    key[ , "name_new"] <- name_new.clean[key[, "name_new"]]
    
    ## Create a keylist member for a given key chunk
    makeOneVar <- function(keyds){
        name_old <- unique.one(keyds$name_old)
        name_new <- unique.one(keyds$name_new)
        class_old <- unique.one(keyds$class_old)
        class_new <- unique.one(keyds$class_new)
        recodes <- unique(na.omit(n2NA(zapspace(keyds$recodes))))
        recodes <- if(length(recodes) > 0) unlist(strsplit(recodes, split=";", fixed = TRUE))
        missings <- paste(na.omit(n2NA(zapspace(keyds$missings))), collapse = ";")
        value_new <- keyds$value_new
        value_old <- keyds$value_old
        if (class_old == "logical") {
            value_old[value_old == "TRUE"] <- TRUE
            value_old[value_old == "FALSE"] <- FALSE
        }
        if (class_new == "logical") {
            value_new[value_new == "TRUE"] <- TRUE
            value_new[value_new == "FALSE"] <- FALSE
        }
        value_new[isNA(value_new, na.strings)] <- NA
        value_old[isNA(value_old, na.strings)] <- NA
        ## If not a factor, cast new values with class_new. Don't do this to
        ## factors, though, leave as text
        if (!class_new %in% c("factor", "ordered") && class(value_new) != class_new){
            mytext <- paste0("value_new <- as.", class_new, "(value_new)")
            value_new <- eval(parse(text = mytext))
        }
        values <- data.frame("value_old" = value_old,
                             "value_new" = value_new, stringsAsFactors = FALSE)
        list(name_old = name_old, name_new = name_new,
             class_old = class_old, class_new = class_new,
             values = values,  missings = missings, recodes = recodes)
    }
    
    ## Make this a factor and control the ordering of the levels. Otherwise,
    ## split applies factor() and re-alphabetizes it.
    name_old.new <- paste0(key[ , "name_old"], ".", key[ , "name_new"])
    name_old.new <- factor(name_old.new, levels = unique(name_old.new))
    keysplit <- split(key, name_old.new, drop = FALSE)
    keylist <- lapply(keysplit, makeOneVar)

    attr(keylist, "na.strings") <- na.strings
    attr(keylist, "class_old") <- sapply(keylist, function(keyds) keyds$class_old)
    attr(keylist, "class_new") <- sapply(keylist, function(keyds) keyds$class_new)
    class(keylist) <- "keylist"
    keylist
}
NULL




##' Apply variable key to data frame (generate recoded data frame)
##'
##' This is the main objective of the variable key system.
##' @param dframe An R data frame
##' @param key A variable key object, of class either "key" or
##'     "keylong"
##' @param diagnostic Default TRUE: Compare the old and new data
##'     frames carefully with the keyDiagnostic function.
##' @param drop Default TRUE. True implies drop = c("vars",
##'     "vals"). TRUE applies to both variables ("vars") and values
##'     ("vals"). "vars" means that a column will be omitted from data
##'     if it is not in the key "name_old". Similarly, if anything
##'     except "." appears in value_old, then setting drop="vals"
##'     means omission of a value from key "value_old" causes
##'     observations with those values to become NA.  This is the
##'     original variable key behavior.  The drop argument allows
##'     "partial keys", beginning with kutils version 1.12. drop =
##'     FALSE means that neither values nor variables are omitted.
##'     Rather than TRUE, one can specify either drop = "vars", or
##'     drop = "vals".
##' @param safeNumericToInteger Default TRUE: Should we treat values
##'     which appear to be integers as integers? If a column is
##'     numeric, it might be safe to treat it as an integer.  In many
##'     csv data sets, the values coded c(1, 2, 3) are really
##'     integers, not floats c(1.0, 2.0, 3.0). See \code{safeInteger}.
##' @param trimws Default is "both", can change to "left", "right", or
##'     set as NULL to avoid any trimming.
##' @param ignoreCase Default TRUE. If column name is capitalized
##'     differently than name_old in the key, but the two are
##'     otherwise identical, then the difference in capitalization
##'     will be ignored.
##' @param debug Default FALSE. If TRUE, emit some warnings.
##' @return A new data.frame object, with renamed and recoded variables
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @export
##' @importFrom plyr mapvalues
##' @examples
##' mydf.key.path <- system.file("extdata", "mydf.key.csv", package = "kutils")
##' mydf.key <-  keyImport(mydf.key.path)
##' mydf.path <- system.file("extdata", "mydf.csv", package = "kutils")
##'
##' mydf <- read.csv(mydf.path, stringsAsFactors = FALSE)
##' mydf2 <- keyApply(mydf, mydf.key)
##'
##' nls.keylong.path <- system.file("extdata", "natlongsurv.key_long.csv", package = "kutils")
##' nls.keylong <- keyImport(nls.keylong.path, long = TRUE)
##' data(natlongsurv)
##' nls.dat <- keyApply(natlongsurv, nls.keylong)
##'
keyApply <- function(dframe, key, diagnostic = TRUE,
                     safeNumericToInteger = TRUE, trimws = "both",
                     ignoreCase = TRUE,
                     drop = TRUE, debug = FALSE)
{
    legalClasses <- c("integer", "numeric", "double", "factor",
                      "ordered", "character", "logical")
    
    if(is.na(drop)|| is.null(drop)) stop("keyApply: drop argument is NA.")
    if (is.character(drop)){
        stopifnot(drop %in%  c("vars","vals"))
    }
    if (is.logical(drop)){
        if(drop) drop <- c("vars", "vals")
    }

    if (class(dframe)[1] != "data.frame"){
        MESSG <- paste("Warning: keyApply is intended for an R *data.frame*,",
                       "not subclasses like tibbles or data.tables.",
                       "keyApply coerced your input",
                       "with as.data.frame().\n")
        if (inherits(dframe, "data.frame")){
            ## Coerce without warning, but print message
            cat(MESSG)
        } else {
            ## Elevate the message to a warning
            warning(MESSG)
        }
        dframe <- as.data.frame(dframe)
    }
    dframe <- cleanDataFrame(dframe, safeNumericToInteger = safeNumericToInteger,
                             trimws = trimws)
    if (diagnostic) dforig <- dframe

    ## Need to snapshot class of input variables
    class_old.dframe <- sapply(dframe, function(x) class(x)[1])

    ## implement ignoreCase by keeping vector dfname_old.orig that we
    ## can use later to put old names back onto data frame.
    ## If key has multiple entries that are identcal after tolower(), will use
    ## first one.
    ## Keep vector of original names. If ignoreCase=FALSE, this changes nothing.
    dfname_old.orig <- colnames(dframe)
    if (ignoreCase){
        colnames(dframe) <- tolower(colnames(dframe))
    }
    names(dfname_old.orig) <- colnames(dframe)
    ## Create vector to use for re-assignment later
    names(class_old.dframe) <- colnames(dframe)

    na.strings <- attr(key, "na.strings")
    ## TODO: figure out what to do if class_old does not match input data
    ## coerce existing column to type requested in data frame?

    ## keylist: a list of long keys, one per variable
    keylist <- makeKeylist(key)
    ## list for collecting new variables.
    xlist <- list()

    names_in_key <- unique(key$name_old)
    if (ignoreCase) names_in_key <- tolower(names_in_key)
    names_not_in_key <- setdiff(colnames(dframe), names_in_key)

    ## B/c "vars" in drop, copy over the variables that are
    ## not in the key.
    if(length(names_not_in_key) && (!"vars" %in% drop)){
        for(jj in names_not_in_key){
            mytext <- paste0("xlist[[\"", jj, "\"]] <- ", "dframe[ , jj]")
            eval(parse(text = mytext))
        }
    }

    ## Process variables in keylist, put into "xlist"
    for (v in keylist) {
        if(debug){
            print(paste("\n debug"))
            print(v)
        }
        ## Extract values for convenience
        values <- v$values
        oldVals <- v$values$value_old
        newVals <- v$values$value_new
        ## keep spare copy of original name
        v$name_old.orig <- v$name_old
        if(ignoreCase) v$name_old <- tolower(v$name_old)

        ## TODO: what if class_old does not match class of imported
        ## data?  Need to think through implications of doing something like
        ## xnew <- as(xnew, class_old)

        ## If variable name from key is not in the data frame, go to next variable.
        if (!v$name_old %in% colnames(dframe)){
            messg <- paste("keyApply: ", v$name_old.orig, "not in data.\n")
            cat(messg)
            next()
        }

        ## DELETE VARIABLES from data frame
        ## If name_new is empty, then ignore that variable
        if (length(v$name_new) == 0 || isTRUE(isNA(unique(v$name_new)))){
            messg <- paste("keyApply: ", v$name_old.orig, "dropped.\n")
            cat(messg)
            next()
        }
        ## Extract candidate variable to column, will recode xnew.
        xnew <- dframe[ , v$name_old]

        ## Apply missing codes
        if (length(v$missings) > 0){
            xnew <- assignMissing(xnew, v$missings)
        }

        ## Be simple. If they have "recodes" in key, apply them.
        ## Ignore value_new. next().
        if (length(v$recodes) > 0 && !all(is.na(v$recodes))) {
            for (cmd in v$recodes) xnew <- assignRecode(xnew, cmd)
            if(!inherits(xnew, v$class_new)) {
                messg <- paste("keyApply: the key's recode function for the variable",
                               v$name_old, " did generate correct output class")
                print(v)
                stop(messg)
            }
            mytext <- paste0("xlist[[\"", v$name_new, "\"]] <- ", "xnew")
            eval(parse(text = mytext))
            next()
        }

        ## value_old and value_new are full of only NA, so
        ## only perform direct class conversions
        if (NROW(na.omit(values)) == 0 || length(na.omit(newVals)) == 0) {
            ## if coercion not safe, stop
            if(!isTRUE(checkCoercion(xnew, unique(v$class_new), na.strings))){
                print(v)
                stop("keyApply: coercion failed, ", unique(v$name_old.orig))
            }
            if (v$class_new %in% c("factor", "ordered")){
                # no direct conversion to factors
                xnew1 <- factor(xnew, ordered=(v$class_new == "ordered"))  
            } else {
                eval(parse(text=paste0("xnew1 <- ", "as.", v$class_new, "(xnew)")))
            }
            mytext <- paste0("xlist[[\"", v$name_new, "\"]] <- ", "xnew1")
            eval(parse(text = mytext))
            next()
        }

        ## if vals is drop, following SHOULD set as missing scores in xnew that
        ## are not in the key 
        if("vals" %in% drop){
            xnew[!xnew %in% values$value_old] <- NA
            ## pj 20171006 the factor gotca killed this:
            ## xnew <- ifelse(xnew %in% values$value_old, xnew,  NA)
        }

        ## If output is ordered or factor
        if(length(v$class_new) > 0 && v$class_new %in% c("ordered", "factor")){
            xnew2 <- plyr::mapvalues(xnew, from = values$value_old,
                                     to = values$value_new, warn_missing = FALSE)
            xnew2 <- factor(xnew2, levels = unique(values$value_new), 
                            ordered=(v$class_new == "ordered"))
            mytext <- paste0("xlist[[\"", v$name_new, "\"]] <- xnew2")
            eval(parse(text = mytext))
            next()
        }

        ## if class from data frame is not same as class_old, then MUST cast
        ## as correct type. Could cast as character.
        if ((class(xnew)[1] != v$class_old) ||
           (v$class_old %in% c("ordered", "factor"))) {
            if(!isTRUE(checkCoercion(unique(xnew), unique(v$class_old), na.strings))){
                MESSG <- paste("keyApply:", unique(v$name_old.orig),
                               "input data cannot be coerced to original class.")
                print(v)
                stop(MESSG)
            }
            xnew.orig <- xnew
            if(v$class_old %in% c("ordered", "factor")){
                ## creates factor with levels in value_old
                mytext1 <- paste0("xnew <- ", v$class_old, "(xnew, values$value_old)")
            } else {
                ## coerce to class_old
                mytext1 <- paste0("xnew <- as.", v$class_old, "(xnew)")
            }
            eval(parse(text = mytext1))
        }
        
        ##Class stays same, use mapvalues, only on values that differ:
        if (classsame <- v$class_new == v$class_old)
        {
            ## keep only rows where value_old and value_new differ
            values <- values[!mapply(identical, values$value_old,
                                     values$value_new), ]
            xnew <- plyr::mapvalues(xnew, values[ , "value_old"],
                                    values[ ,"value_new"],
                                    warn_missing = FALSE)
            mytext <- paste0("xlist[[\"", v$name_new, "\"]] <- xnew")
            eval(parse(text = mytext))
            next()
        }
        
        ## Special treatment to convert level values to integer/numeric values
        if (v$class_old %in% c("factor", "ordered") &&
            v$class_new %in% c("integer", "numeric", "double"))
        {
            if(!isTRUE(checkCoercion(values$value_new, unique(v$class_new), na.strings))){
                ## Value new cannot be coerced numeric, should stop
                MESSG <- paste("key error:", v$name_old.orig, "value_new not consistent with class_new:\n")
                print(v)
                stop(MESSG)
            }
            if(!isTRUE(checkCoercion(xnew, unique(v$class_new), na.strings))){
                ## Value new cannot be coerced numeric, should stop
                MESSG <- paste("key error: values of", v$name_old.orig,
                               "don't match class_new:\n")
                print(v)
                stop(MESSG)
            }
            
            ## otherwise continue
            xnew2 <- plyr::mapvalues(xnew, values[ , "value_old"],
                                     values[ , "value_new"],
                                     warn_missing = FALSE)
            if(!isTRUE(checkCoercion(levels(xnew2), unique(v$class_new), na.strings))){
                MESSG <- paste("keyApply value error:", v$name_old, "levels don't match class_new.")
                print(v)
                warning(MESSG)
            }
            xnew3 <- as(levels(xnew2)[xnew2], v$class_new)
            mytext <- paste0("xlist[[\"", v$name_new, "\"]] <- xnew3")
            eval(parse(text = mytext))
            next()
        }
        
        ## convert for variables with new mapping of value_old to value_new
        ## need to run this if values differ or one is NA but other not
        valueDiff <- v$values$value_old != v$values$value_new
        if (any(valueDiff, na.rm = TRUE) || any(is.na(valueDiff))) {
            xnew <- plyr::mapvalues(xnew, values$value_old, values$value_new, warn_missing = FALSE)
            mytext1 <- paste0("xnew2 <- as.", v$class_new, "(xnew)")
            eval(parse(text = mytext1))
            mytext <- paste0("xlist[[\"", v$name_new, "\"]] <- xnew2")
            eval(parse(text = mytext))
            next()
        }
        print(v)
        MESSG <- paste("keyApply:", v$name_old, "Logic error in value mapping.")
        stop(MESSG)
    }
    
    ## How to pass stringsAsFactors=FALSE as argument? Only way is
    ## run through environment?
    stringsAsFactors.orig <-  unname(unlist(options("stringsAsFactors")))
    options(stringsAsFactors = FALSE)
    dframe <- do.call(data.frame, xlist)
    options(stringsAsFactors = stringsAsFactors.orig )
    if(diagnostic) keyDiagnostic(dforig, dframe, keylist)
    dframe
}
NULL

##' Diagnose accuracy of result from applying variable key to data
##'
##' Compare the old and new data frames, checking for accuracy of
##' calculations in various ways.
##' 
##' CAUTION: This can print WIDE matrices.  Because the on-screen
##' output will be WIDE, make the display window WIDE!
##'
##' Crosstabulate new variable versus old variable to see the coding
##' mismatches.  For tables of up to 10 values or so, that will be
##' satisfactory.
##'
##' For numeric variables, it appears there is no good thing to do
##' except possibly to re-apply any transformations.
##' 
##' @param dfold Original data frame
##' @param dfnew The new recoded data frame
##' @param keylist The imported variable key that was used to
##'     transform dfold into dfnew.
##' @param max.values Show up to this number of values for the old
##'     variable
##' @param nametrunc Truncate column and row names. Needed if there
##'     are long factor labels and we want to fit more information on
##'     table. Default = 18 for new name, old name is 10 more
##'     characters (18 + 10 = 28).
##' @param wide Number of characters per row in printed
##'     output. Suggest very wide screen, default = 200.
##' @param confidential Should numbers in table be rounded to nearest
##'     "10" to comply with security standard enforced by some
##'     American research departments.
##' @return NULL
##' @author Paul Johnson <pauljohn@@ku.edu>
keyDiagnostic <- function(dfold, dfnew, keylist, max.values = 20,
                          nametrunc = 18, wide = 200, confidential = FALSE)
{

    ## TODO if class in dfnew does not match keylist specification, fail

    ## TODO think more deeply on warning signs of bad recoding.
    ## other summary of match and mismatch.

    width.orig <- options("width")
    options(width = wide)
    if (confidential) {
        roundAt <- -1
    } else {
        roundAt <- 2
    }
    for (v in keylist){
        if (is.na(v$name_new) ||(length(v$name_new) == 0) || (!v$name_new %in% colnames(dfnew))){
            messg <- paste("Variable", v$name_new, "is not included in the new data frame")
            next()
        }
        if (length(unique(dfnew[ , v$name_new])) <= max.values){
            name_new.trunc <- substr(v$name_new, 1, min(nchar(v$name_new), nametrunc))
            name_old.trunc <- paste0(substr(v$name_old, 1, min(nchar(v$name_old), nametrunc + 10)), " (old var)")
            print(round(table(dfnew[ , v$name_new], dfold[ , v$name_old],
                              exclude = NULL, dnn = c(name_new.trunc, name_old.trunc)), roundAt))
        } else {
            messg <- paste("Variable", v$name_new, "has", max.values,
                           "unique values.", "Too large for a table.")
            print(messg)
        }
    }
    options(width = unlist(width.orig))
    NULL
}


##' Convert a key object from wide to long format
##'
##' This is not flexible, assumes columns are named in our canonical
##' style, which means the columns are named c("name_old", "name_new",
##' "class_old", "class_new", "value_old", "value_new").
##' @param key A variable key in the wide format
##' @param sep Default separator is the pipe, "\\|" for most
##'     variables, while ordered accepts pipe or less than, "\\|<". If
##'     the key did not follow those customs, other sep values may be
##'     specified for each variable class.
##' @return A long format variable key
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' mydf.path <- system.file("extdata", "mydf.csv", package = "kutils")
##' mydf <- read.csv(mydf.path, stringsAsFactors=FALSE)
##' ## Target we are trying to match:
##' mydf.keylong <- keyTemplate(mydf, long = TRUE, sort = FALSE)
##' 
##' mydf.key <- keyTemplate(mydf)
##' mydf.keywide2long <- wide2long(mydf.key)
##'
##' ## rownames not meaningful in long key, so remove in both versions
##' row.names(mydf.keywide2long) <- NULL
##' row.names(mydf.keylong) <- NULL
##' all.equal(mydf.keylong, mydf.keywide2long)
wide2long <- function(key, sep = c(character = "\\|", logical = "\\|",
                              integer = "\\|", factor = "\\|",
                              ordered = "[\\|<]", numeric = "\\|"))
{
    makeOneVar <- function(x){
        value_old <- unlist(strsplit(x$value_old, sep[x$class_old]))
        value_new <- unlist(strsplit(x$value_new, sep[x$class_new]))
        if (length(value_old) == 0) value_old <- as.character("")
        if (length(value_new) == 0) value_new <- as.character("")
        values <- cbind(value_old, value_new)
        values <- unique(values)
        ## missings <- na.omit(unlist(strsplit(x$missings, ";")))
        missings <- c(x$missings, rep("", NROW(values) - length(x$missings)))
        ## recodes <- na.omit(unlist(strsplit(x$recodes, ";")))
        recodes <- c(x$recodes, rep("", NROW(values) - length(x$recodes)))

        zz <- data.frame(name_old = x$name_old,
                         name_new = x$name_new,
                         class_old = x$class_old,
                         class_new = x$class_new,
                         value_old = values[ , "value_old"],
                         value_new = values[ , "value_new"],
                         missings = missings,
                         recodes = recodes, stringsAsFactors = FALSE)
        zz <- sortStanza(zz)
        zz
    }

    ## Unique-ify name_new
    key[ , "name_new"] <- make.names(key[ , "name_new"], unique = TRUE)
    
    ## keysplit
    name_old.new <- paste0(key[ , "name_old"], ".", key[ , "name_new"])
    name_old.new <- factor(name_old.new, levels = unique(name_old.new))
    ks <- split(key, name_old.new, drop = TRUE)
    ## build a "long stanza" for each variable
    ksl <- lapply(ks, makeOneVar)

    keylong <- do.call(rbind, lapply(ksl, as.data.frame, stringsAsFactors = FALSE))
    attr(keylong, "na.strings") <- attr(key, "na.strings")
    attr(keylong, "varlab") <- attr(key, "varlab")
    class(keylong) <- c("keylong", "data.frame")
    keylong
}



##' convert a key object from long to wide format
##'
##' ##' This is not flexible, assumes columns are named in our canonical
##' style, which means the columns are named c("name_old", "name_new",
##' "class_old", "class_new", "value_old", "value_new").
##' @param keylong A variable key in the long format
##' @param na.strings Strings to be treated as missings in value_new
##' @param missSymbol Default is ".", character to insert in value when R NA is found.
##' @return A wide format variable key
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' mydf.path <- system.file("extdata", "mydf.csv", package = "kutils")
##' mydf <- read.csv(mydf.path, stringsAsFactors=FALSE)
##' ## A wide key we are trying to match:
##' mydf.key <- keyTemplate(mydf, long = FALSE, sort = TRUE)
##' mydf.key["x4", "missings"] <- "999"
##' ## A long key we will convert next
##' mydf.keylong <- keyTemplate(mydf, long = TRUE, sort = TRUE)
##' mydf.keylong[mydf.keylong[ , "name_old"] == "x4" &
##'     mydf.keylong[ , "value_old"] == "999", "missings"] <- "999"
##' mydf.long2wide <- long2wide(mydf.keylong)
##' all.equal(mydf.key, mydf.long2wide)
##' 
##' mydf.keylong.path <- system.file("extdata", "mydf.key_long.csv", package = "kutils")
##' mydf.keylong <- keyImport(mydf.keylong.path)
##' mydf.keywide <- long2wide(mydf.keylong)
##' mydf.keylong2 <- wide2long(mydf.keywide)
##' ## Is error if following not TRUE
##' all.equal(mydf.keylong2, mydf.keylong)
##' 
long2wide <- function(keylong, na.strings = c("\\.", "", "\\s+",  "N/A"),
                      missSymbol = "."){
    name_old.new <- paste0(keylong[ , "name_old"], ".", keylong[ , "name_new"])
    name_old.new <- factor(name_old.new, levels = unique(name_old.new))
    ##kls = keylong split.
    ## 20161215: why didn't I use keylist maker here?
    keylong[is.na(keylong[ , "value_new"]), "value_new"] <- missSymbol
    keylong[is.na(keylong[ , "value_old"]), "value_old"] <- missSymbol

    kls <- split(keylong, name_old.new, drop = TRUE)

    makeOneWide <- function(x){
        sep_old <- if(unique(x$class_old) == "ordered") "<" else "|"
        sep_new <- if(unique(x$class_new) == "ordered") "<" else "|"
        ## Replace "" with NA, then get rid of NAs
        missings <- n2NA(unique(x$missings))
        missings <- na.omit(missings)
        recodes <- n2NA(unique(x$recodes))
        recodes <- na.omit(recodes)
        values <- cbind(value_old = x$value_old, value_new = x$value_new)
        values <- unique(values)
        values <- sortStanza(values)
        value_new.ismissing <- isNA(values[ , "value_new"])
                   
        if(any(!na.omit(value_new.ismissing))){
            newvalues <- paste(values[ , "value_new"], collapse = sep_new)
        } else {
            newvalues <- "."
        }
        list(name_old = unique(x$name_old),
             name_new = unique(x$name_new),
             class_old = unique(x$class_old),
             class_new = unique(x$class_new),
             value_old = paste(values[ , "value_old"], collapse = sep_old),
             value_new = newvalues,
             missings = paste(missings, collapse = ";"),
             recodes =  paste(recodes, collapse = ";"))
    }

    keywide <- lapply(kls, makeOneWide)

    key <- do.call("rbind", lapply(keywide, data.frame, stringsAsFactors = FALSE))
    class(key) <- c("key", "data.frame")
    attr(key, "na.strings") <- attr(keylong, "na.strings")
    attr(key, "varlab") <- attr(keylong, "varlab")
    key
}

##' An all.equal method for variable wide keys
##'
##' Disregards attributes by defaults. Before comparing the two keys,
##' the values are sorted by \code{"name_new")}.
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @method all.equal key
##' @param target A wide variable key
##' @param current A wide variable key
##' @param ... Other arguments that are ignored
##' @param check.attributes Default FALSE
##' @export
all.equal.key <- function(target, current, ..., check.attributes = FALSE){
    target <- target[order(target[ , "name_new"]), ]
    current <- current[order(current[ , "name_new"]), ]
    reslt <- base::all.equal.list(target, current, check.attributes = FALSE)
    reslt
}


##' An all.equal method for variable long keys
##'
##' Disregards attributes by defaults. Before comparing the two keys,
##' the values are sorted by \code{"name_new")}.
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @method all.equal keylong
##' @export
##' @param target A long variable key
##' @param current A long variable key
##' @param ... Other arguments that are ignored
##' @param check.attributes Default FALSE
all.equal.keylong <- function(target, current, ..., check.attributes = FALSE){
    target <- target[order(target[ , "name_new"],
                           target[ , "value_old"],
                           target[ , "value_new"]), ]
    current <- current[order(current[ , "name_new"],
                             current[ , "value_old"],
                             current[ , "value_new"]), ]
    reslt <- base::all.equal.list(target, current, check.attributes = FALSE)
    reslt
}


##' Update a key in light of a new data frame (add variables and
##' values)
##'
##' The following chores must be handled.
##' 1. If the data.frame has variables which are not currently
##' listed in the variable key's "name_old" variable, then new
##' variables are added to the key.
##' 2. If the data.frame has new values for the previously
##' existing variables, then those values must be added to the
##' keys.
##' 3. If the old key has "name_new" or "class_new" designated
##' for variables, those MUST be preserved in the new key
##' for all new values of those variables.
##'
##' This function will not alter key values for "class_old",
##' "value_old" or "value_new" for variables that have no new
##' information.
##'
##' This function deduces if the key provided is in the wide or long
##' format from the class of the object.
##' @param key A variable key
##' @param dframe A data.frame object.
##' @param append If long key, should new rows be added to the end of
##'     the updated key? Default is TRUE. If FALSE, new rows will be
##'     sorted with the original values.
##' @param safeNumericToInteger Default TRUE: Should we treat
##'     variables which appear to be integers as integers? In many csv
##'     data sets, the values coded \code{c(1, 2, 3)} are really
##'     integers, not floats \code{c(1.0, 2.0, 3.0)}. See
##'     \code{safeInteger}.
##' ## Need to consider implementing this:
##' ## @param ignoreCase
##' @export
##' @return Updated variable key.
##' @importFrom plyr rbind.fill
##' @author Ben Kite <bakite@ku.edu>
##' @examples
##' ## Original data frame has 2 variables
##' dat1 <- data.frame("Score" = c(1, 2, 3, 42, 4, 2),
##'                    "Gender" = c("M", "M", "M", "F", "F", "F"))
##' ## New data has all of original dat1, plus a new variable "Weight"
##' #and has new values for "Gender" and "Score"
##' dat2 <- plyr::rbind.fill(dat1, data.frame("Score" = 7,
##'            "Gender" = "other", "Weight" = rnorm(3)))
##' ## Create a long key for the original data, specify some
##' ## recodes for Score and Gender in value_new
##' key1.long <- keyTemplate(dat1, long = TRUE, varlab = TRUE)
##' 
##' key1.long$value_new <- gsub("42", "10", key1.long$value_new)
##' key1.long$value_new[key1.long$name_new == "Gender"] <-
##'        mgsub(c("F", "M"), c("female", "male"),
##'        key1.long$value_new[key1.long$name_new == "Gender"])  
##' key1.long[key1.long$name_old == "Score", "name_new"] <- "NewScore"
##' keyUpdate(key1.long, dat2, append = TRUE)
##' ## Throw away one row, make sure key still has Score values
##' dat2 <- dat2[-1,]
##' (key1.long.u <- keyUpdate(key1.long, dat2, append = FALSE))
##' ## Key change Score to character variable
##' key1.longc <- key1.long
##' key1.longc[key1.longc$name_old == "Score", "class_new"] <- "character"
##' keyUpdate(key1.longc, dat2, append = TRUE)
##' str(dat3 <- keyApply(dat2, key1.longc))
##' ## Now try a wide key
##' key1.wide <- keyTemplate(dat1)
##' ## Put in new values, same as in key1.long
##' key1.wide[key1.wide$name_old == "Score", c("name_new", "value_new")] <-
##'                             c("NewScore", "1|2|3|4|10|.")
##' key1.wide[key1.wide$name_old == "Gender", "value_new"] <- "female|male|."
##' ## Make sure key1.wide equivalent to key1.long:
##' ## If this is not true, it is a fail
##' all.equal(long2wide(key1.long), key1.wide, check.attributes = FALSE)
##' (key1.wide.u <- keyUpdate(key1.wide, dat2))
##' key1.long.to.wide <- long2wide(key1.long.u)
##' all.equal(key1.long.to.wide, key1.wide.u, check.attributes = FALSE)
##' str(keyApply(dat2, key1.wide.u))
##' 
##' mydf.key.path <- system.file("extdata", "mydf.key.csv", package = "kutils")
##' mydf.key <-  keyImport(mydf.key.path)
##' ##'
##' set.seed(112233)
##' N <- 20
##' ## The new Jan data arrived!
##' mydf2 <- data.frame(x5 = rnorm(N),
##'                     x4 = rpois(N, lambda = 3),
##'                     x3 = ordered(sample(c("lo", "med", "hi"),
##'                                        size = N, replace=TRUE),
##'                                 levels = c("med", "lo", "hi")),
##'                     x2 = letters[sample(c(1:4,6), N, replace = TRUE)],
##'                     x1 = factor(sample(c("jan"), N, replace = TRUE)),
##'                     x7 = ordered(letters[sample(c(1:4,6), N, replace = TRUE)]),
##'                     x6 = sample(c(1:5), N, replace = TRUE),
##'                     stringsAsFactors = FALSE)
##' mydf.key2 <- keyUpdate(mydf.key, mydf2)
##' mydf.key2
##' mydf.key2["x1", "value_old"] <- "cindy|bobby|jan|peter|marcia|greg|."
##' mydf.key2["x1", "value_new"] <- "Cindy<Bobby<Jan<Peter<Marcia<Greg<."
##' ##'
##' mydf.key.path <- system.file("extdata", "mydf.key.csv", package = "kutils")
##' mydf.path <- system.file("extdata", "mydf.csv", package = "kutils")
##' mydf <- read.csv(mydf.path, stringsAsFactors=FALSE)
##' mydf3 <- rbind(mydf, mydf2)
##' ## Now recode with revised key
##' mydf4 <- keyApply(mydf3, mydf.key2)
##' rockchalk::summarize(mydf4)
keyUpdate <- function(key, dframe, append = TRUE,
                      safeNumericToInteger = TRUE)
{
    ## it is a long key, or convert it into one
    long <- TRUE
    if (class(key)[1] == "key") {
        key <- wide2long(key)
        long <- FALSE ## key in was wide
    }
    if (class(key)[1] != "keylong") {
        messg <- paste("The key object is from the wrong class.")
        stop(messg)
    }

    dframe <- cleanDataFrame(dframe, safeNumericToInteger = safeNumericToInteger)
    keynew <- keyTemplate(dframe, long = TRUE)
    if (isTRUE(all.equal(keynew, key))){
        return(key)
    }
   
    ## pj 20170929 BIG QUESTION. If keyDiff is correct (still dont know),
    ## can't we use it, and then just take the neworaltered object and
    ## append to old key?  We never want to delete rows from key, just
    ## add new ones.  Maybe then need check for contradictions.

    ## CHECK: what does "long2wide" do when rows in a long key are
    ## "shuffled" or if the new values all exist at end of long key.

    nameval.old <- paste0(key$name_old, key$value_old)
    nameval.new <- paste0(keynew$name_old, keynew$value_old)
    ## Throw away previously observed name-value combinations
    ## Creates new rows to insert in original key long form
    keynew2 <- keynew[!nameval.new %in% nameval.old, ]

    ## if name_new and class_new are re-defined in old key,
    ## copy  those into new key
    name.old.new <- unique(key[ , c("name_old", "name_new")])
    rownames(name.old.new) <- name.old.new[ , "name_old"]

    ## Tricky if new variable arrived with data, can't just copy
    ## name_new without checking
    keynew2$name_new <- ifelse(keynew2$name_old %in% name.old.new[ , "name_old"],
                               name.old.new[keynew2$name_old, "name_new"],
                               keynew2$name_new)

    class.old.new <- unique(key[ , c("name_old", "class_old", "class_new")])
    rownames(class.old.new)<- class.old.new[ , "name_old"]
    ## for same-name_old cases, copy in class
    keynew2$class_new <- ifelse(keynew2$name_old %in% name.old.new[ , "name_old"],
                                class.old.new[keynew2$name_old, "class_new"],
                                keynew2$class_new)

    output <- rbind(key, keynew2)
    ## User expects key returned in same format, keylong or key
    if(!long) {
        output <- long2wide(output)
        row.names(output) <- output$name_old
        return(output)
    } else {
        if (!append) {
            output <- output[order(output$name_old),]
            output <- naLast(output)
        }
        row.names(output) <- seq(1, nrow(output), 1)
        return(output)
    }
    attr(output, "na.strings") <- attr(key, "na.strings")
    output
}

##' Sort key so that non missing values are first in the
##' value vector.
##'
##' @keywords internal
##' @param key key, long or wide
##' @param byvar Default is "name_new", the column for sorting into blocks.
##' @param valvar The value variable on which sorting is to be done, "value_new".
##' @param na.strings vector of characters to be treated as NA
##' @return sorted key, with blocks that have the missings last
naLast <- function(key, byvar = "name_new", valvar = "value_new", 
                   na.strings = c("\\.", "", "\\s+",  "N/A"))
{
    ##keep attributes not equal to "names" and "row.names"
    attrs <- attributes(key)[!names(attributes(key)) %in% c("names", "row.names")]
    long <- if(inherits(key, "keylong")) TRUE else FALSE
    if(!long) key <- wide2long(key)
    keysplit <- split(key, key[ , byvar])
    for(jj in names(keysplit)) {
        keysplit[[jj]] <- sortStanza(keysplit[[jj]],
                                     na.strings = na.strings)
    }
    key <- do.call(rbind, keysplit)
    if(!long) return(long2wide(key))
    ## else long return
    key
}


##' Move missing values to last row in long key block
##' 
##' Receive key stanza and sort the rows so that the missing
##' values are last in the list. Leaves ordering of other rows
##' unchanged otherwise.
##' 
##' @param keyblock a variable key, or section of rows
##' @param valvar Default is "value_new", the column for sorting.
##' @param na.strings Stings to be treated as missing, along with R's
##'     NA symbol
##' @return keyblock, row re-arranged with missings last
##' @keywords internal
sortStanza <- function(keyblock, valvar = "value_new",
                       na.strings = c("\\.", "", "\\s+",  "N/A"))
{
    ordr <- seq_along(keyblock[ , valvar])
    ismissing <- isNA(keyblock[ , valvar])
    neworder <- c(ordr[!ismissing], ordr[ismissing])
    keyblock[neworder, , drop = FALSE]
}
    

##' Show difference between 2 keys
##'
##' @param oldkey key, original 
##' @param newkey key, possibly created by keyUpdate or by user edits
##' @return NULL, or list with as many as 2 key difference data.frames,
##'  named "deleted" and "neworaltered"
##' @author Ben Kite <bakite@@ku.edu> and Paul Johnson <pauljohn@@ku.edu>
##' @export
##' @examples
##' 
##' dat1 <- data.frame("Score" = c(1, 2, 3, 42, 4, 2),
##'                    "Gender" = c("M", "M", "M", "F", "F", "F"))
##' ## First try with a long key
##' key1 <- keyTemplate(dat1, long = TRUE)
##' key1$value_new <- gsub("42", "10", key1$value_new)
##' key1$value_new[key1$name_new == "Gender"] <-
##'        mgsub(c("F", "M"), c("female", "male"),
##'        key1$value_new[key1$name_new == "Gender"])  
##' key1[key1$name_old == "Score", "name_new"] <- "NewScore"
##' dat2 <- data.frame("Score" = 7, "Gender" = "other", "Weight" = rnorm(3))
##' dat2 <- plyr::rbind.fill(dat1, dat2)
##' dat2 <- dat2[-1,]
##' key2 <- keyUpdate(key1, dat2, append = TRUE)
##' (kdiff <- keyDiff(key1, key2))
keyDiff <- function(oldkey, newkey){
    oldkey.name <- deparse(substitute(oldkey))
    newkey.name <- deparse(substitute(newkey))
    rownames(oldkey) <- paste0(rownames(oldkey), ".old")
    rownames(newkey) <- paste0(rownames(newkey), ".new")
    oldkey$key <- "old"
    newkey$key <- "new"
    xx <- rbind(oldkey, newkey)
    ## xx$top: is a duplicate of one that went before
    xx$top <- duplicated(xx[, -match("key", colnames(xx))])
    ## xx$bot: will be duplicated below
    xx$bot <- duplicated(xx[, -match(c("key", "top"), colnames(xx))], fromLast = TRUE)
    ## lines that are in old key but not in new key:
    deleted <- xx[xx$key == "old" & !xx$bot, -match(c("key", "top", "bot"), colnames(xx)) , drop = FALSE]
    ## in newkey but not old key
    neworaltered <- xx[xx$key == "new" & !xx$top, -match(c("key", "top", "bot"), colnames(xx)), drop = FALSE]

    if(NROW(deleted) == 0  && NROW(neworaltered)  == 0){
        print("There are no differences between these keys!")
        return(NULL)
    }

    if(NROW(deleted) > 0)  {
        messg1 <- paste0("keyDiff: ", NROW(deleted),
                         " rows in ", oldkey.name,
                         " are not in ", newkey.name,
                         "\n")
        attr(deleted, "message") <- messg1
        cat(messg1)
    } else {
        deleted <- NULL
    }
     if(NROW(neworaltered) > 0){
         messg2 <- paste0("keyDiff: ", NROW(neworaltered),
                          " rows in ", newkey.name,
                          " are not in ", oldkey.name, "\n")
         attr(neworaltered, "message") <- messg2
         cat(messg2)
     }
      
    output <- list(deleted = deleted,
                   neworaltered = neworaltered)
    class(output) <- "keyDiff"
    invisible(output)
}

##' Print a keyDiff object
##'
##' @method print keyDiff
##' @export
##' @param x A keyDiff object
##' @param ... Other arguments passed through to print
##' @author Ben Kite <bakite@@ku.edu>
print.keyDiff <- function(x, ...){
    if(!is.null(x[["deleted"]])){
        dat <- x[["deleted"]]
        cat(attr(dat, "message")) 
        print(dat, ...)
    }
    if(!is.null(x[["neworaltered"]])){
        dat <- x[["neworaltered"]]
        cat(attr(dat, "message"))
        print(x[["neworaltered"]], ...)
    }
}





##' Compares keys from different data sets; finds differences classes of variables.

##' This used to check for similarity of keys from various data sets,
##' one precursor to either combining the keys or merging the data
##' sets themselves.
##'
##' When several supposedly "equivalent" data sets are used
##' to generate variable keys, there may be trouble. If variables
##' with same name have different classes, keyApply might fail
##' when applied to one of the data sets.
##'
##' This reports on differences in classes among keys. By default, it
##' looks for differences in "class_old", because that's where we
##' usually see trouble.
##'
##' The output here is diagnostic. The keys can be fixed manually, or the
##' function keysPool can implement an automatic correction.
##' @param keys A list with variable keys.
##' @param col Name of key column to check for equivalence. Default is "class_old", but
##' "class_new" can be checked as well.
##' @param excludere Exclude variables matching a regular expression
##'     (re). Default example shows exclusion of variables that end in
##'     the symbol "TEXT".
##' @return Data.frame summarizing class differences among keys
##' @author Paul Johnson
##' @export
##' @examples
##' set.seed(234)
##' dat1 <- data.frame(x1 = rnorm(100),
##'                    x2 = sample(c("Male", "Female"), 100, replace = TRUE),
##'                    x3_TEXT = "A", x4 = sample(1:10000, 100))
##' dat2 <- data.frame(x1 = rnorm(100), x2 = sample(c("Male", "Female"),
##'                    100, replace = TRUE),
##'                    x3_TEXT = sample(1:100, 100),
##'                    stringsAsFactors = FALSE)
##' key1 <- keyTemplate(dat1)
##' key2 <- keyTemplate(dat2)
##' keys <- list(key1, key2)
##' keysPoolCheck(keys)
##' ## See problem in class_old
##' keysPoolCheck(keys, col = "class_old")
##' ## problems in class_new
##' keysPoolCheck(keys, col = "class_new")
##' keysPoolCheck(keys, excludere = "TEXT$")
keysPoolCheck <- function(keys, col = "class_old", excludere = "TEXT$"){
    ## How spot trouble? class_old changes among keys?
    classnameold <- lapply(keys, function(x) {
        fst <- x[!duplicated(x$name_old), ]
        res <- fst[ , c("name_old", col)]
        rownames(res) <- NULL
        res
    })

    for(i in 2:length(keys)){
        if (i == 2){
            classmerge <- merge(classnameold[[1]], classnameold[[2]],
                                by = "name_old", suffixes = c("1", "2"))
        } else {
            classmerge <- merge(classmerge, classnameold[[i]],
                                by = "name_old", suffixes = c("", i))
        }
    }
    ## ## Suppose all detected logicals should be integer (long story why)
    ## classmerge[classmerge == "logical"] <- "integer"
    ## ## Promote all integers to numeric
    ## classmerge[classmerge == "integer"] <- "numeric"

    classmerge$troublevar <- apply(classmerge, 1, function(x){length(unique(x[grep(col, names(x))])) > 1})
    classProblems <- classmerge[classmerge$troublevar, ]
   
    classProblems <- classProblems[!grepl(excludere, classProblems$name_old), ]
    classProblems
}



##' Check a key for consistency of names, values with classes.
##'
##' Split the key into blocks of rows defined by "name_new". Within
##' these blocks, Perform these checks: 1. name_old must be
##' homogeneous (identical) within a block of rows. class_old and
##' class_new must also be identical.
##' 2. elements in "value_new" must be consistent with "class_new".
##' If values cannot be coerced to match the class specified by
##' class_new, there must be user error.
##' Same for "value_old" and "class_old".
##' @param key A variable key object.
##' @param colname Leave as default to check consistency between classes, values, and names.
##' One can specify a check only on "class_old" or "class_new", for example.  But now that
##' all work correctly, I suggest you leave the default.
##' @param na.strings A regular expression of allowed text strings that represent missings.
##' Now it amounts to any of these: ".", "NA", "N/A", or any white space or tab as signified by \\s+.
##' @return Profuse warnings and a list of failed key blocks.
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu> and Ben Kite <bakite@@ku.edu>
keyCheck <- function(key,
                     colname = c("name_new", "class_old", "class_new"),
                     na.strings = c("\\.", "", "\\s+",  "N/A")){
    if (is.character(key)){
        key <- keyRead(key)
    }
    if (prod(c("name_old", "name_new", "class_old", "class_new",
               "value_old", "value_new") %in% names(key)) != 1L){
        stop ("At a minimum a variable key needs to have the following columns: name_old, name_new, class_old, class_new, value_old, value_new")
    }
    ## Deduce if this is a long key

    if(inherits(key, "keylong")) long = TRUE else long = FALSE

    if (!long){
        xx <- strsplit(key$value_old, split = "[|<]", fixed = FALSE)
        xx.l <- sapply(xx, length)
        yy <- strsplit(key$value_new, "[|<]", fixed = FALSE)
        yy.l <- sapply(yy, length)
        inconsistent <- xx.l != yy.l
        issues <- key[inconsistent, "name_old"]
        if (length(issues) > 0){
            stop (paste0("Value error, columns: ", issues))
        }
    }
    ## Transition from Ben's work to PJ's
    if(!inherits(key, "keylong")) keylong <- kutils::wide2long(key) else keylong <- key

    keysplit <- split(keylong, keylong[ , "name_new"])

    keyfails <- list()
    for(ii in intersect(c("name_old", "class_old", "class_new"), colname)){
            ## check same-value for all of "class_old", or "class_new"
            for(jj in names(keysplit)) {
                keyblock <- keysplit[[jj]]
                ## Stanza 1: check homogeneous colname = class_old(or new) values"
                if (length(unique(keyblock[ , ii])) > 1) {
                    warning(paste("Key Violation:", jj, ii,  "\n"), immediate. = TRUE)
                    keyfails[[jj]] <- keysplit[[jj]]
                }
            }
    }
    for(ii in intersect(c("class_old", "class_new"), colname)){
        ## compare value_old(new) against
        ## class_old(new). If all non-missing cannot be coerced to
        ## indicated class, key should fail.
        for (jj in names(keysplit)) {
            keyblock <- keysplit[[jj]]
            value_col <- paste0("value_", gsub("class_(.*)", "\\1", ii))
            value <- keyblock[ , value_col]
            ## exclude any that have missing marker from na.strings
            value <- na.omit(value[!value %in% na.strings])
            testcol <- NA
            mytext <- paste0("testcol <- as.", keyblock[1, ii], "(value)")
            eval(parse(text = mytext))
            if (sum(is.na(testcol)) > 0L){
                warning(paste("Key value violation:", ii, jj, "\n"), immediate. = TRUE)
                keyfails[[jj]] <- keysplit[[jj]]
            }
            ## did not fail yet, so return NULL for fails
            ##NULL
        }
    }


    if (long){
        if (!identical(key, wide2long(long2wide(key)))){
            stop ("Key error: wide2long(long2wide(key)) fails.")
        }
    } else {
        if (!identical(key, long2wide(wide2long(key)))){
            stop ("Key error: long2wide(wide2long(key)) fails.")
        }
    }
    if (length(keyfails) > 0){
        return(keyfails)
    }else{
        message("No errors were detected")
    }
}


##' Homogenize class values and create a long key by pooling variable
##' keys.
##'
##' For long-format keys, this is one way to correct for errors in
##' "class_old" or "class_new" for common variables. For a long key
##' created by stacking together several long keys, or for a list of
##' long keys, this will try to homogenize the classes by using a
##' "highest common denominator" approach.  If one key has x1 as a
##' floating point, but another block of rows in the key has x1 as
##' integer, then class must be changed to floating point
##' (numeric). If another section of a key has x1 as a character, then
##' character becomes the class.
##'
##' Users might run keyTemplate on several data sets, arriving
##' at keys that need to be combined.  The long versions of the
##' keys can be stacked together by a function like \code{rbind}.
##' If the values class_old and class_new for a single variable are
##' inconsistent, then the "key stack" will fail the tests in keyCheck.
##' This function automates the process of fixing the class variables by
##' "promoting" classes where possible.
##'
##' Begin with a simple example.  In one data set, the value of x is
##' drawn from integers 1L, 2L, 3L, while in another set it is
##' floating values like 1.1, 2.2. After creating long format keys,
##' and stacking them together, the values of class_old will clash.
##' For x, we will observe both "integer" and "numeric" in the
##' class_old column.  In that situation, the class_old for all of the
##' rows under consideration should be set as "numeric".
##'
##' The promotion schemes are described by the variable classes, where
##' we have the most conservative changes first. The most destructive
##' change is when variables are converted from integer to character,
##' for example. The conservative conversion strategies are specified
##' in the classes variable, in which the last element in a vector
##' will be used to replace the preceeding classes.  For example,
##' c("ordered", "factor", "character") means that the class_old
##' values of "ordered" and "factor" will be replaced by "character".
##'
##' The conversions specified by classes are tried, in order.
##' 1. logical -> integer
##' 2. integer -> numeric
##' 3. ordered -> factor
##'
##' If their application fails to homogenize a vector, then class is
##' changed to "character". For example, when the value of class_old
##' observed is c("ordered", "numeric", "character"). In that case,
##' the class is promoted to "character", it is the least common
##' denominator.
##' @param keylong A list of long keys, or just one long key,
##'     presumably a result of rbinding several long keys.
##' @param keysplit Not often needed for user-level code. A list of
##'     key blocks, each of which is to be inspected and
##'     homogenized. Not used if a keylong argument is provided.
##' @param classes A list of vectors specifying legal promotions.
##' @param colnames Either c("class_old","class_new), ""class_old", or
##'     "class_new".  The former is the default.
##' @param textre A regular expression matching a column name to be
##'     treated as character. Default matches any variable name ending
##'     in "TEXT"
##' @return A class-corrected version of the same format as the input,
##'     either a long key or a list of key elements.
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @export
##' @examples
##' dat1 <- data.frame(x1 = as.integer(rnorm(100)), x2 = sample(c("Apple", "Orange"),
##'                    100, replace = TRUE), x3 = ifelse(rnorm(100) < 0, TRUE, FALSE))
##' dat2 <- data.frame(x1 = rnorm(100), x2 = ordered(sample(c("Apple", "Orange"),
##'                    100, replace = TRUE)), x3 = rbinom(100, 1, .5),
##'                    stringsAsFactors = FALSE)
##' key1 <- keyTemplate(dat1, long = TRUE)
##' key2 <- keyTemplate(dat2, long = TRUE)
##' keys2stack <- rbind(key1, key2)
##' keys2stack.fix <- keysPool(keys2stack)
##' keys2stack.fix2 <- keysPool(keys2stack.fix, colname = "class_new")
##' ## Sometimes this will not be able to homogenize
##' dat1 <- data.frame(x1 = as.integer(rnorm(100)),
##'                    x2 = sample(c("Apple", "Orange"), 100, replace = TRUE))
##' dat2 <- data.frame(x1 = rnorm(100),
##'                    x2 = sample(c("Apple", "Orange"), 100, replace = TRUE),
##'                    stringsAsFactors = FALSE)
##' key1 <- keyTemplate(dat1, long = TRUE)
##' key2 <- keyTemplate(dat2, long = TRUE)
##' ## Create a stack of keys for yourself
##' keys2stack <- rbind(key1, key2)
##' keys.fix <- keysPool(keys2stack)
##' ## We will create stack of keys for you
##' keys.fix2 <- keysPool(list(key1, key2)) 
##' ## View(keys.fix)
##' ## View(keys.fix2)
##'
##' 
##' ## If you have wide keys, convert them with wide2long, either by
##' key1 <- keyTemplate(dat1)
##' key2 <- keyTemplate(dat2)
##' keysstack.wide <- rbind(wide2long(key1), wide2long(key2))
##' keys.fix <- keysPool(keysstack.wide)
##' ## or
##' keysPool(list(wide2long(key1), wide2long(key2)))
keysPool <- function(keylong = NULL, keysplit = NULL,
                        classes = list(c("logical", "integer"),
                                       c("integer", "numeric"),
                                       c("ordered", "factor")),
                        colnames = c("class_old","class_new"),
                        textre = "TEXT$")
{

    if (is.list(keylong) && !is.data.frame(keylong)){
        islongkey <- vapply(keylong, function(x){
            inherits(x, "keylong")
        }, logical(1))
        if (any(!islongkey)){
            MESSG <- "All elements in keylong list must be long keys"
            stop(MESSG)
        }
        keylong <- do.call(rbind, keylong)
    }
        
    if (!is.null(keylong)){
        if (!is.null(keysplit)){
            warning("keysplit is ignored because keylong was not null")
        }
        if (!inherits(keylong, "keylong")) {
            stop("keylong is not recognized as a long key")
        } else {
            NULL
        }
        if (missing(keysplit)) keysplit <- split(keylong, keylong[ , "name_new"])
    } else {
        if (is.null(keysplit)) {
            stop("A key or keysplit argument must be provided")
        }
    }

    ## Change the values of colname (say, class_old) to equal the last value of classes.
    ## keyblock: a row block from a variable key
    ## classes: vector of classes, the last of which is the acceptable one, to replace
    ## the others.
    ## colname: either "class_old" or "class_new"
    classClean <- function (keyblock, colnames = colnames, classes)
    {
        for (col in colnames) {
            keyblock[keyblock[ , col] %in% classes[-length(classes)], col] <- classes[length(classes)]
        }
        keyblock
    }

    ## if mixed, promote all to last named class
    for(i in seq_along(keysplit)){
        keyblock <- keysplit[[i]]
        if (length(unique(keyblock[, colnames[1]])) == 1) next() # use first col if two
        ## Special case. Any variable matching textre
        if (any(grepl(textre, names(keysplit)[i], ignore.case = TRUE))) {
            for (col in colnames) {
                keyblock[ , col] <- "character"
            }
            keysplit[[i]] <- unique(keyblock)
            next()
        }

        for (j in classes) {
            keyblock <- classClean(keyblock, colnames = colnames, classes = j)
            keysplit[[i]] <- unique(keyblock)
            if (length(unique(keyblock[, colnames[1]])) == 1) {
                next()
            }
        }

        if (length(unique(keyblock[, colnames[1]])) > 1) {
            MESSG <- paste("Cannot painlessly reduce key classes to homogeneous class.",
                           names(keysplit)[i], "changing class to character")
            warning(paste0(names(keysplit)[i], " ", paste(unique(keyblock[ , colnames[1]]),
                                                    collapse = " + "), ". ", MESSG), immediate. = TRUE)
            for (col in colnames) {
                keyblock[ , col] <- "character"
            }
            keysplit[[i]] <- unique(keyblock)
            ##warning(paste(names(keysplit)[i], "changing class to character"), immediate. = TRUE)
        }
    }

    ## If a key came in, give back a key
    if(!is.null(keylong)){
        keystack <- do.call(rbind, keysplit)
        class(keystack) <- c("keylong", "data.frame")
        return(keystack)
    }
    ## If a keysplit was passed in, give that back
    return(keysplit)
}


##' Checks a key for dangerous matches of old and new values in a key
##' for different levels.
##'
##' Positions in a long key are referred to as levels. If a value is
##' mismatched at levels 1 and 3, this means that issues are in rows 1
##' and 3 of the section of the given variable in a long key.
##'
##' @title keyCrossRef
##' @param key A variable key, ideally a long key. If a wide key is
##'     provided it is converted to long.
##' @param ignoreClass Classes that should be excluded from
##'     check. Useful when many integer variables are being reverse-
##'     coded. Takes a string or vector.
##' @param verbose Should a statement about the number of issues
##'     detected be returned? Defaults to FALSE.
##' @param lowercase Should old and new values be passed through
##'     tolower function? Defaults to FALSE.
##' @return Presents a warning for potentially problematic key
##'     sections. Return is dependent on verbose argument.
##' @author Ben Kite <bakite@@ku.edu>
##' @examples
##' dat <- data.frame(x1 = sample(c("a", "b", "c", "d"), 100, replace = TRUE),
##'                   x2 = sample(c("Apple", "Orange"), 100, replace = TRUE),
##'                   x3 = ordered(sample(c("low", "medium", "high"), 100, replace = TRUE),
##'                   levels = c("low", "medium", "high")),
##'                   stringsAsFactors = FALSE)
##' key <- keyTemplate(dat, long = TRUE)
##' ## No errors with a fresh key.
##' kutils:::keyCrossRef(key, verbose = TRUE)
##' key[1:2, "value_new"] <- c("b", "a")
##' key[5, "value_new"]
##' key[7:9, "value_new"] <- c("high", "medium", "low")
##' kutils:::keyCrossRef(key)
##' kutils:::keyCrossRef(key, ignoreClass = c("ordered", "character"), verbose = TRUE)
keyCrossRef <- function(key, ignoreClass = NULL, verbose = FALSE, lowercase = FALSE){
    if(!inherits(key, "keylong")){
        key <- kutils::wide2long(key)
    }
    keysplit <- split(key, key[ , "name_new"])
    problems <- 0
    for (k in keysplit){
        if (k[1, "class_new"] %in% ignoreClass){
            next()
        }
        N <- nrow(k)
        for (r in 1:length(k[,"value_new"])){
            i <- k[r, "value_new"]
            others <- k[seq(1, N)[!seq(1,N) %in% r],"value_old"]
            if (isTRUE(lowercase)){
                i <- tolower(i)
                others <- tolower(others)
            }
            if (i %in% others){
                problems <- problems + 1
                warning(k[1, "name_new"], ": The value of \"", i, "\" is at level ",
                        r, " in value_new, but \"", i,
                        "\" is also the value at level ",
                        which(k[,"value_old"] == i), " in value_old.")
            }
        }
    }
    if (isTRUE(verbose)){
        if (problems == 0){
            return("No potentially problematic value matches across levels detected.")
        } else {
            return(paste0("There are ", problems,
                          " potential issues with this key that need to be considered"))
        }
    }

}


##' Look for old (or new) names in variable key
##'
##' Use the key to find the original name of a variable that has been
##' renamed, or find the new name of an original variable.  The
##' \code{get} argument indicates if the \code{name_old} or
##' \code{name_new} is desired.
##'
##' If \code{get = "name_old"}, the return is a character vector, with
##' one element per value of \code{x}.  If there is no match for a
##' value of \code{x}, the value NA is returned for that
##' value. However, if \code{get = "name_new"}, the return might be
##' either a vector (one element per value of \code{x}) or a list with
##' one element for each value of \code{x}.  The list is returned when
##' a value of \code{x} corresponds to more than one element in
##' \code{name_old}.
##' @param x A variable name. If \code{get = "name_old"}, then
##'     \code{x} is a value for \code{name_new}. If \code{get =
##'     "name_new"}, \code{x} should be a value for \code{name_old}.
##' @param key Which key should be used
##' @param get Either "name_old" (to retrieve the original name) or
##'     "name_new" (to get the new name)
##' @return A vector or list of matches between x and either name_new
##'     or name_old elements in the key.
##' @author Paul Johnson
##' @export
##' @examples
##' mydf.key.path <- system.file("extdata", "mydf.key.csv", package = "kutils")
##' mydf.key <-  keyImport(mydf.key.path)
##' mydf.key$name_new <- paste0("new_", mydf.key$name_new)
##' keyLookup("new_x5", mydf.key, get = "name_old")
##' keyLookup(c("new_x6", "new_x1"), mydf.key, get = "name_old")
##' keyLookup(c("x6", "x1"), mydf.key, get = "name_new")
##' keyLookup(c("asdf", "new_x1"), mydf.key, get = "name_old")
##'
##' mydf.key <- rbind(mydf.key,
##'                  c("x3", "x3f",  "ordered", "factor", "","","",""))
##' keyLookup(c("x3"), mydf.key, get = "name_new")
##' keyLookup(c("x1", "x3", "x5"), mydf.key, get = "name_new")
keyLookup <- function(x, key, get = "name_old"){
    if(!length(match.arg(get, c("name_old", "name_new")))){
        MESSG <- "keyLookup: get must be 'name_old' or 'name_new'"
        stop(MESSG)
    }
    
    if(class(key)[1] == "keylong"){
        key <- long2wide(key)
    }
    
    if(get == "name_old"){
        if (any(duplicated(key$name_new))){
            MESSG <- paste0("keyLookup finds duplicates in 'name_new'")
            stop(MESSG)
        }
        target <- key$name_new[match(x, key$name_new, nomatch = NA)]
        return(target)
    }
    ## else get == "name_new"
    target <- sapply(x, function(jj){
        fits <- key[which(key$name_old %in% jj), "name_new"]
        fits
    })

    for(jj in names(target)){
        if(!length(target[[jj]])) {
            MESSG <- paste("No value in name_old matches:", jj)
            warning(MESSG)
            target[jj] <- NULL
        } else if (length(target[[jj]]) > 1){
            MESSG <- paste0("Note: name_old '", jj,
                            "' matches several values in name_new: ",
                            paste(target[[jj]], collapse = ", "))
            print(MESSG)
        }
    }
    if(length(target) == 0 || is.null(target)){
        return(NULL)
    }
    target
}



##' Import an SPSS file, create a key representing the numeric ->
##' factor transition
##'
##' This is a way to keep track of the scores that are used in the SPSS file.
##' It is also an easy way to start a new variable key that makes it convenient
##' to work on the value_new column with R text functions.
##' @param dat A character string path to the SPSS file
##' @param long TRUE returns a long key, otherwise a wide key
##' @return A variable key (long or wide)
##' @importFrom foreign read.spss
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu>
keyTemplateSPSS <- function(dat, long = TRUE){
    datf <- read.spss(dat, max.value.labels = 15, to.data.frame = TRUE,
                      use.value.labels = TRUE)
    datn <- read.spss(dat, to.data.frame = TRUE, use.value.labels = FALSE)
    key <- statdatKey(datf, datn, long)
    key
}



##' Import a Stata (version 12 or lower) file, create a key
##' representing the numeric -> factor transition
##'
##' This is a way to keep track of the scores that are used in the Stata file.
##' It is also an easy way to start a new variable key that makes it convenient
##' to work on the value_new column with R text functions.
##' @param dat A character string path to the Stata file
##' @param long TRUE returns a long key, otherwise a wide key
##' @return A variable key (long or wide)
##' @importFrom foreign read.dta
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu>
keyTemplateStata <- function(dat, long = TRUE){
    datf <- read.dta(dat, convert.factors = TRUE)
    datn <- read.dta(dat, convert.factors = FALSE)
    key <- statdatKey(datf, datn, long)
    key
}

##' keyFactors: private function that does work for keyTemplateSPSS and
##' key template Stata
##' @param datf Data frame with factors
##' @param datn Numeric data frame
##' @param long Should the result be a long or wide key
statdatKey <- function(datf, datn, long = TRUE){
    numericTF <- vapply(datf, is.numeric, logical(1)) 
    notnumeric <- colnames(datf)[!numericTF]
    isnumeric <- colnames(datf)[numericTF]
    
    partA <- lapply(isnumeric, function(i) {
        data.frame(name_old = i, name_new = i,
                   class_old = class(datf[ , i])[1], class_new = class(datf[ , i])[1],
                   value_old = ".", value_new = ".", stringsAsFactors = FALSE)
    })
    partB <- lapply(notnumeric, function(i) {
        val <- unique(datn[ , i])
        val <- val[order(val)]
        val.level <- datf[match(val, datn[ , i]),  i]
        data.frame(name_old = i, name_new = paste0(i, "f"),
                                   class_old = class(datn[ , i])[1], class_new = class(datf[ , i])[1],
                                   value_old = val, value_new = val.level, stringsAsFactors = FALSE)
        ## match(datn[match(levels(datf$Q76), datf$Q76),"Q76"])
    })
    keylong <- rbind(do.call(rbind, partA), do.call(rbind, partB))
    ## 20180418: variables did not come out in same order as SPSS.
    keywide <- long2wide(keylong)
    rownames(keywide) <- keywide$name_old
    keywide <- keywide[colnames(datf), ]
    if (!long) return(keywide)
    ## else turn the wide key long
    wide2long(keywide)
}
