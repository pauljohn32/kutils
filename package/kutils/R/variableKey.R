
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
##' First, calculate absolute value of differences between
##' \code{x} and \code{as.integer(x)}. Second, compare the sum of those
##' differences is smaller than \code{tol}, then x can reasonably
##' be coerced to an integer.
##'
##' Be careful with the return. The correct return value for variables that
##' should not be coerced as integer is uncertain at this point. We've tested
##' various strategies, sometimes returning FALSE, NULL, or just the original
##' variable.
##' @param x a numeric variable
##' @param tol Tolerance value. Defaults to Machine$double.eps. See
##'     details.
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
safeInteger <- function(x, tol = .Machine$double.eps, digits = 7, verbose = FALSE)
{
    if(!is.numeric(x)) {
        if (verbose) {
            messg <- paste("asInteger is intended only for numeric x. Doing nothing.")
            warning(messg)
        }
        return(NULL)
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
                messg <- paste("asInteger x:", paste(head(x), collapse=", "), "... is not close enough to an integer")
                warning(messg)
            }
            return(NULL)
        }
    }
    messg <- paste0("safeInteger should not have reached this point")
    stop(messg)
}
NULL


##' Scrub a variable's missings away
##'
##' The missings values have to be carefully written, depending on the
##' type of variable that is being processed.
##' @param x A variable
##' @param missings A string with a vector of values or R expressions.
##'     These are done differently for integer, numeric, factor, and
##'     character variables.  \enumerate{ \item For integer variables,
##'     use a character string representing part of an R expression
##'     such "> 8", ">= 8", "< 7", or "<= 7", or a character string
##'     enclosing a range, a two valued vector, as in "c(8,9)". Any
##'     strings that do not begin with ">", "<", or "c" will be
##'     ignored. To reset particular values as missing one-by-one, use
##'     the variable key.
##'
##' \item For numerics, use an inequality such as "> 99". The only
##'    other alternative we have allowed is a character string that
##'    represents a range such as "c(99, 101)", to mean that values
##'    greater than OR equal to 99 and less than OR equal to 101 will
##'    be set as missing.
##'
##' \item For factors, include a vector of levels to be
##'         marked as missing and removed from the list of levels.
##'
##' \item For character variables, a character vector of
##'         values to be marked as missing.
##' }
##'
##' One of the concerns is that comparison of real-valued numerics is
##' not dependable.  Exact comparisons with == are
##' unreliable, so don't ask for them.
##'
##' @return A cleaned column in which R's NA symbol replaces values
##'     that should be missing
##' @export
##' @importFrom utils head
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' ## 1.  Integers.
##' ## must be very sure these are truly integers, or else fails
##' x <- seq.int(2L, 22L, by = 2L)
##' ## Specify range, 4 to 12 inclusive
##' missings <- "c(4, 12)"
##' assignMissing(x, missings)
##'
##' missings <- " < 7"
##' assignMissing(x, missings)
##'
##' missings <- " > 11"
##' assignMissing(x, missings)
##'
##' ## 2. strings
##' x <- c("low", "low", "med", "high")
##' missings <- "c(\"low\", \"high\")"
##' assignMissing(x, missings)
##' missings <- c("med", "doesnot exist")
##' assignMissing(x, missings)
##'
##' ## 3. factors (same as strings inside assignMissing)
##' x <- factor(c("low", "low", "med", "high"), levels = c("low", "med", "high"))
##' missings <- c("low", "high")
##' assignMissing(x, missings)
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
##' missings <- "< 0"
##' assignMissing(x, missings)
##' missings <- "> -0.2"
##' assignMissing(x, missings)
##' missings <- "c(0.1, 0.7)"
##' assignMissing(x, missings)
assignMissing <- function(x, missings = NULL){

    if (is.character(missings)) missings <- zapspace(missings)
    missings <-  na.omit(missings)
    ## If no NA  missings to remove, then return
    if (is.null(missings) | length(missings) == 0) return(x)

    if (is.character(x)){
        x[x %in% missings] <- NA
        return(x)
    }
    if (is.factor(x)){
        levels(x)[which(levels(x) %in% missings)] <- NA
        return(x)
    }
    if (is.integer(x)){
        if(substr(missings, 1, 1) %in% c(">", "<")){
            conditional <- paste(quote(x), missings)
            xcheck <- eval(parse(text = conditional))
            x[xcheck] <- NA
        } else if (substr(missings, 1, 1) == "c"){
            misvec <- as.integer(eval(parse(text = missings)))
            if (length(misvec) != 2) stop("Missings interval should have 2 numeric values")
            if (any(is.na(misvec))) stop("Missings interval should not have any NA values")
            misvec <- sort(misvec)
            x[x >= misvec[1] & x <= misvec[2]] <- NA
        } else {
            messg("Error in assignMissings")
            print(messg)
            messg <- "Here are the first 20 values of the variable being recoded"
            print(messg)
            print(head(x, 20))
            messg <- "Here is the missing value string that was supplied"
            print(messg)
            print(missings)
            messg <- paste0("The missing string was not understandable")
            stop(messg)
        }
        return(x)
    }
    if (is.double(x)) {
        if(substr(missings, 1, 1) %in% c(">", "<")){
            conditional <- paste(quote(x), missings)
            xcheck <- eval(parse(text = conditional))
            x[xcheck] <- NA
        } else if (substr(missings, 1, 1) == "c"){
            misvec <- eval(parse(text = missings))
            if (length(misvec) != 2) stop("Missings interval should have 2 numeric values")
            if (!is.numeric(misvec)) stop("Missings vector must be numeric")
            if (any(is.na(misvec))) stop("Missings interval should not have any NA values")
            misvec <- sort(misvec)
            x[x >= misvec[1] & x <= misvec[2]] <- NA
        } else {
            messg <- paste0("missings for variable ", deparse(substitute(x)), " not understandable")
            stop(messg)
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
##' columns. Checks numeric columns to find out if they are
##' acceptable to treat as integers. If they are acceptable to
##' treat as integers, then convert those numeric to integer class
##' variables.
##' @param dframe A data frame
##' @param safeNumericToInteger Default TRUE: Should we treat values
##'     which appear to be integers as integers? If a column is
##'     numeric, it might be safe to treat it as an integer.  In many
##'     csv data sets, the values coded c(1, 2, 3) are really
##'     integers, not floats c(1.0, 2.0, 3.0). See
##'     \code{safeInteger}.
##' @export
##' @return A checked and cleaned data frame
##' @keywords internal
##' @author Paul Johnson <pauljohn@@ku.edu>
cleanDataFrame <- function(dframe, safeNumericToInteger = TRUE){
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
##' @keywords internal
##' @return NULL
##' @author Paul Johnson <pauljohn@@ku.edu>
checkValues <- function(x, value_old, xname){
    if (!is.factor(x) && !is.character(x)) return(NULL)
    ## if value_old is length 1 and is equal to NA, don't bother
    if(all(value_old %in% c("NA", NA))) return(NULL)
    xobs <- unique(x)
    keynotinobs <- value_old[!value_old %in% xobs]
    keynotinobs <- na.omit(keynotinobs)
    if (length(keynotinobs) > 0){
        messg <- paste("Data Check (variable", xname, ":)\n",
                     "Key values were not observed in the input data: ",
                     paste(keynotinobs, collapse = ", "), "\n")
        cat(messg)
    }
    tobsnotinkey <- xobs[!xobs %in% value_old]
    tobsnotinkey <- na.omit(tobsnotinkey)
    if (length(tobsnotinkey) > 0) {
        messg <- paste("Data Check (variable", xname, ":)\n",
                       "These values in the input data were not in value_old: ",
                       paste(tobsnotinkey, collapse = ", "), "\n")
        cat(messg)
    }
    NULL
}




##' Create variable key template
##'
##' A variable key is a human readable document that describes the
##' variables in a data set. A key can be revised and re-imported by R
##' so as to guide the re-importation and recoding of data. This might
##' also be referred to as a "programmable codebook."  This function
##' inspects a data frame, takes notice of its variable names, their
##' classes, and legal values, and then it creates a table summarizing
##' that information. The aim is to create a document that principal
##' investigators and research assistants can use to keep a project
##' well organize.  Please see the vignette in this package.
##'
##' The variable key can be created in two formats.  The original
##' style of the variable key has one row per variable. It has a style
##' for compact notation about current values and required recodes.
##' That is more compact, probably easier for experts to use, but
##' perhaps more complicated for non-programmers. The long style
##' variable key has one row per value per variable.  Thus, in a
##' larger project, the long style key can be quite voluminous. However,
##' in a larger project, the long style key seems to be more likely to
##' generate the intended result.
##'
##' After a key is created, it should be re-imported into R with the
##' \code{kutils::keyImport} function.  Then the key structure can
##' guide the importation and recoding of the data set.
##'
##' @param dframe A data frame
##' @param long Default FALSE.
##' @param sort Default FALSE. Should the rows representing the
##'     variables be sorted alphabetically? Otherwise, they appear in
##'     the order in which they were included in the original dataset.
##' @param file DEFAULT NULL, meaning no file is produced. The primary
##'     purpose of this function is to create the output file, so
##'     users should write a text string for the output file's base
##'     name.  We planned for 3 storage formats, comma separate
##'     variables, MS Excel XLSX format, or R's RDS.  To designate
##'     which format will be used, the user should specify a file name
##'     that ends in a suffix ".csv", ".rds", or ".xlsx". If
##'     spreadsheet output in an XLSX file is requested, user should
##'     make sure the openxlsx package is installed.
##' @param outdir The output directory for the new variable key files.
##'     Default is current working directory.
##' @param max.levels This is to regulate the enumeration of values
##'     for integer, character, and Date variables. When enumerating
##'     existing values for a variable, what is the maximum number of
##'     levels that should be included in the key? Default = 15. This
##'     does not affect variables declared as factor or ordered
##'     variables, for which all levels are included in all cases.
##' @param safeNumericToInteger Default TRUE: Should we treat values
##'     which appear to be integers as integers? If a column is
##'     numeric, it might be safe to treat it as an integer.  In many
##'     csv data sets, the values coded c(1, 2, 3) are really
##'     integers, not floats c(1.0, 2.0, 3.0). See
##'     \code{safeInteger}.
##' @return A key in the form of a data frame. May also be saved on
##'     disk.
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
##' ## Should be same as content of
##' ## write.csv(mydf, file = "../inst/extdata/mydf.csv", row.names = FALSE)
##' mydf.templ <- keyTemplate(mydf, file = "mydf.templ.csv")
##' mydf.templ_long <- keyTemplate(mydf, long = TRUE, file = "mydf.templlong.csv")
##' ## write.csv(mydf.templ, file = "../inst/extdata/mydf.templ.csv", row.names = FALSE)
##' ## write.csv(mydf.templ_long, file = "../inst/extdata/mydf.templ_long.csv", row.names = FALSE)
##' ## smartSave(mydf.templ, file = "mydf.templ.xlsx", outdir = ".")
##' ## smartSave(mydf.templ_long, file = "mydf.templ_long.xlsx", outdir = ".")
##'
##' ## Try with the national longitudinal study data
##' data(natlongsurv)
##' natlong.templ <- keyTemplate(natlongsurv, file = "natlongsurv.templ.csv",
##'                            max.levels = 15, sort = TRUE)
##'
##' natlong.templlong <- keyTemplate(natlongsurv, long = TRUE,
##'                    file = "natlongsurv.templ_long.csv", sort = TRUE)
##'
##' \donttest{
##' if (require(openxlsx)){
##'    openxlsx::write.xlsx(natlong.templ, file = "natlongsurv.templ.xlsx")
##'    openxlsx::write.xlsx(natlong.templlong, file = "natlongsurv.templ_long.xlsx")
##' }
##' }
##'
keyTemplate <- function(dframe, long = FALSE, sort = FALSE,
                        file = NULL, outdir = getwd(),
                        max.levels = 15, safeNumericToInteger = TRUE)
{

    dframe <- cleanDataFrame(dframe, safeNumericToInteger = safeNumericToInteger)

    df.class <- sapply(dframe, function(x)class(x)[1])
    cn <- colnames(dframe)
    ## Make a long key
    if(isTRUE(long)){

        getUnique <- function(xname){
            if (df.class[[xname]] == "numeric") return ("")
            if (df.class[[xname]] %in% c("integer", "character")){
                xunique <- sort(unique(dframe[ , xname]))
                if(length(xunique) <= max.levels) {
                    return (xunique[1:min(max.levels, length(xunique))])
                } else {
                    return ("")
                }

            }
            if (df.class[[xname]] %in% c("factor", "ordered")){
                return(levels(dframe[ , xname]))
            }
        }

        key <- do.call("rbind", lapply(cn, function(x){
            expand.grid(name = x,
                        class = df.class[[x]],
                        value = getUnique(x), stringsAsFactors = FALSE)}))

        key <- data.frame(name_old = key$name,
                          name_new = key$name,
                          class_old = key$class,
                          class_new = key$class,
                          value_old = key$value,
                          value_new = key$value,
                          missings = "",
                          recodes = "",
                          stringsAsFactors = FALSE)

        if (sort) key <- key[order(key$name_old), ]
        key$missings <- ""
        key$recodes <- ""

        class(key) <- c("keylong", class(key))
    } else {

        ## else !long, so make a wide key
        key <- data.frame(name_old = colnames(dframe), name_new = colnames(dframe),
                          class_old = df.class, class_new = df.class,
                          value_old = as.character(""), value_new = "",
                          missings = "",  recodes = "", stringsAsFactors = FALSE)


        for (i in cn){
            getValues <- function(x){
                y <- dframe[ , x]
                z <- ""
                if (df.class[x] %in% c("integer", "logical", "character", "Date")) {
                    if (length(unique(y)) <= max.levels){
                        z <- paste0(sort(unique(y)[1:min(max.levels, length(unique(y)))]), collapse = "|")
                    } else {
                        z <- ""
                    }
                    return(z)
                }
                if (df.class[x] == "ordered"){
                    z <- paste0(levels(y), collapse = "<")
                    return(z)
                }
                if (df.class[x] == "factor"){
                    z <- paste0(levels(y), collapse = "|")
                    return(z)
                }
                z
            }
            key[i, "value_old"] <- getValues(i)
        }

        key[ , "value_new"] <- key[ , "value_old"]

        if (sort) key <- key[order(key$name_old), ]
        class(key) <- c("key", class(key))
    }

    if (!missing(file) && !is.null(file)){
        smartSave(key, file, outdir)
    }
    key
}

##' save file after deducing type from suffix
##'
##' just a convenience for this work, not general purpose
##' @param obj an object
##' @param file file name. must end in "csv", "xlsx" or "rds"
##' @param outdir directory path
##' @return NULL
##' @keywords internal
##' @author Paul Johnson <pauljohn@@ku.edu>
smartSave <- function(obj, file, outdir){
    fp <- file
    if (!is.null(outdir) && missing(outdir))
        fp <- paste0(outdir, "/", file)
    if (length(grep("csv$", tolower(file))) > 0){
        write.csv(obj, file = fp, row.names = FALSE)
    } else if ((length(grep("xlsx$", tolower(file))))){
        openxlsx::write.xlsx(obj, file = fp)
    } else if (length(grep("rds$", tolower(file)))){
        saveRDS(obj, file = fp)
    } else {
        warning("Proposed key name did not end in csv, xlsx, or rds, so no file created")
    }
}


##' read file after deducing file type from suffix.
##'
##' @param file name of file to be imported, including path to
##'     file. file name must end in "csv", "xlsx" or "rds"
##' @param ... additional arguments for read.csv or read.xlsx.
##' @return A data frame or matrix.
##' @importFrom utils read.csv
##' @importFrom openxlsx read.xlsx write.xlsx
##' @keywords internal
##' @author Paul Johnson <pauljohn@@ku.edu>
smartRead <- function(file, ...){
    ## TODO: implement code to sort out the dots arguments, find
    ## which are aimed at read.xlsx or read.csv, and divide them. See
    ## peek() function example.
    dots <- list(...)
    readxlsxFormals <- names(formals(openxlsx::read.xlsx))
    readcsvFormals <- names(formals(read.csv))
    dotsforxlsx <- dots[readxlsxFormals[readxlsxFormals %in% names(dots)]]
    dotsforcsv <- dots[readcsvFormals[readcsvFormals %in% names(dots)]]
    if (!is.character(file) || !file.exists(file)){
        messg <- paste("smartRead: argument 'file' should be a character string",
                       "that gives the full path and file name to be used")
        stop(messg)
    } else {

        ## key is file name, so scan for suffix
        if (length(grep("xlsx$", tolower(file))) > 0){
            xlsxargs <- list(file = file, colNames = TRUE, check.names = FALSE)
            xlsxargz <- modifyList(xlsxargs, dotsforxlsx)
            names(xlsxargz)[which(names(xlsxargz) == "file")] <- "xlsxFile"
            key <- do.call("read.xlsx", xlsxargz)
            ## Force columns to be of type "character"
            for(i in colnames(key)){
                if (class(key[ , i]) != "character") key[ , i] <- as.character(key[ , i])
            }
        } else if (length(grep("csv$", tolower(file))) > 0){
            csvargs <- list(file = file, stringsAsFactors = FALSE, colClasses = "character")
            csvargz <- modifyList(csvargs, dotsforcsv)
            key <- do.call("read.csv", csvargz)
        } else if (length(grep("rds$", tolower(file))) > 0){
            key <- readRDS(file)
        }
    }
    key
}


##' Convert nothing to R missing(NA).
##'
##' By "nothing", we mean white space or other indications of
##' nothingness.  Character strings that are used for missing
##' values. Those things, which are given default values in the
##' argument nothings, will be changed to NA.
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
##' gg <- c("", " ", "   ", "\t", "\t some", "some\t", " space first", ".", " . ")
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
##' end. It does not alter spaces in the interior of a string
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


## 20161108 TODO: apply logic to cause error when numeric or integer class
## have character values.  Caution about "NA" no "N/A" is needed
## before rejecting value_new vectors.

##' Import a file and clean up for use as variable key
##'
##' After the researcher has updated the key by filling in new names
##' and values, we import that key file. This function imports the
##' file by its name, after deducing the file type from the suffix.
##'
##' This can be either a wide or long format key file.
##'
##' This cleans up variables in following ways.  1) \code{name_old}
##' and \code{name_new} have leading and trailing spaces removed 2)
##' \code{value_old} and \code{value_new} have leading and trailing
##' spaces removed, and if they are empty or blank spaces, then new
##' values are set as NA.  3) if \code{value_old} and \code{value_new}
##' are identical, the values are removed from the key.
##' @param file A file name, ending in csv, xlsx or rds.
##' @param ignoreCase In the use of this key, should we ignore
##'     differences in capitalization of the "name_old" variable?
##'     Sometimes there are inadvertent misspellings due to changes in
##'     capitalization. Columns named "var01" and "Var01" and "VAR01"
##'     probably should receive the same treatment, even if the key
##'     has name_old equal to "Var01".
##' @param sep Defaults are specified, don't change this unless you
##'     know what you are doing. In wide keys, what separators are
##'     used between values?  This should be a named vector which
##'     declares separators that are used in the key. In our defaults,
##'     the separator for classes character, logical, integer, and
##'     numeric is the pipe, "|", while for factor and ordered
##'     variables, the separator may be either pipe or less than.  Use
##'     regular expressions in supplying separator values.
##' @param na.strings Values in the value_new column which will be
##'     treated as NA in the key. The defaults are ".", "", "\\s"
##'     (white space), "NA", and "N/A".  These will prevent a new
##'     value like "" or " " from being created, so if one intends to
##'     insert white space, the na.strings vector must be specified.
##' @param ... additional arguments for read.csv or read.xlsx.
##' @param keynames Don't use this unless you are very careful. In
##'     our current scheme, the column names in a key should be
##'     c("name_old", "name_new", "class_old", "class_new",
##'     "value_old", "value_new", "missings", "recodes"). If your key
##'     does not use those column names, it is necessary to provide
##'     keynames in a format "our_name"="your_name". For example,
##'     keynames = c(name_old = "oldvar", name_new = "newname", class_old =
##'     "vartype", class_new = "class", value_old = "score", value_new
##'     = "val")
##' .
##' @export
##' @return key object
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' mydf.key.path <- system.file("extdata", "mydf.key.csv", package = "kutils")
##' mydf.key <-  keyImport(mydf.key.path)
##'
##' mydf.keylong.path <- system.file("extdata", "mydf.key_long.csv", package = "kutils")
##' mydf.keylong <- keyImport(mydf.keylong.path)
keyImport <- function(file, ignoreCase = TRUE,
                      sep = c(character = "\\|", logical = "\\|",
                              integer = "\\|", factor = "[\\|<]",
                              ordered = "[\\|<]", numeric = "\\|")
                     ,
                      na.strings = c(".", "", "\\s",  "NA", "N/A")
                     ,
                      ...
                     ,
                      keynames = NULL)
{
    key <- smartRead(file)

    legalClasses = c("integer", "numeric", "double", "factor",
                     "ordered", "character", "logical")
    if(!is.null(keynames)){
        keynames.std <- c(name_old = "name_old",
                          name_new = "name_new",
                          class_old = "class_old",
                          class_new = "class_new",
                          value_old = "value_old",
                          value_new = "value_new",
                          missings = "missings",
                          recodes = "recodes")
        keynames.std[names(keynames)] <- keynames
        key <- colnamesReplace(key, newnames = names(keynames.std), oldnames = keynames.std)
    }
    ## TODO: omit blank rows

    ## Deduce if this is a long key
    name_old.new <- paste0(key[ , "name_old"], ".", key[ , "name_new"])
    if (max(table(name_old.new)) > 1){
        long <- TRUE
    } else {
        long <- FALSE
    }

    key.orig <- key
    if (!long) key <- wide2long(key, sep)
    ## protect against user-inserted spaces (leading or trailing)
    key$name_old <- zapspace(key$name_old)
    key$name_new <- zapspace(key$name_new)

    ## if this is long key, following is safe. How about wide key?
    key$value_old <- n2NA(zapspace(key$value_old))
    key$value_new <- n2NA(zapspace(key$value_new))
    key$value_new[key$value_new %in% na.strings] <- NA

    ## Delete repeated rows:
    dups <- duplicated(key)
    if (any(dups, na.rm = TRUE))key <- key[!(dups), ]

    if (any(!unique(key$class_new) %in% legalClasses)){
        messg <- paste("Unfamiliar class_new values observed!\n",
                       "No pre-set converters are available for the classes: ",
                       paste(unique(key$class_new)[!unique(key$class_new) %in% legalClasses], collapse = ", "),
                       "Suitable recode statements in the key should generate variables from those classes."
                       )
        warning(messg)
    }

    attr(key, "ignoreCase") <- ignoreCase
    attr(key, "na.strings") <- na.strings
    if (long){
        class(key) <- c("keylong", "data.frame")
        return(key)
    } else {
        keywide <- long2wide(key)
        class(keywide) <- c("key", "data.frame")
        return(keywide)
    }
    stop("keyImport should not reach this point")
}



##' Convert the variable key into a keylist structure for use in
##' keyApply
##'
##' The return value is a list, with one element per "name_old" to
##' "name_new" variable combination.  If the key has one old variable
##' being recoded 6 ways, that begets 6 elements in the resulting
##' list. Attributes including the classes of the old and new
##' variables are included.
##'
##' @param key A key object or a file name, csv, xlsx or rds.
##' @param sep Separator regular expressions
##' @keywords internal
##' @return A list with one element per variable name, along with some
##'     attributes like class_old and class_new. The class is set as
##'     well, "keylist".
##' @author Paul Johnson <pauljohn@@ku.edu>
makeKeylist <- function(key,
                        sep = c(character = "\\|", logical = "\\|",
                              integer = "\\|", factor = "[\\|<]",
                              ordered = "[\\|<]", numeric = "\\|")
                        )
{
    ## if x is "", return NA
    ## if "split" is NA or blank space or TAB, return unchanged x
    strsplit2 <- function(x, split, fixed = FALSE, perl = FALSE, useBytes = FALSE){
        if (is.na(n2NA(zapspace(split)))) return(x)
        if (is.na(n2NA(zapspace(x)))) return(NA)
        strsplit(x, split, fixed = fixed, perl = perl, useBytes = useBytes)
    }

    ## if it is already a key list, return with no changes
    if (inherits(key,"keylist")) return(key)
    if (is.character(key)) key <- keyImport(key)
    if (inherits(key, "key")) {
        long <- FALSE
    } else if (inherits(key, "keylong")){
        long <- TRUE
    }

    ## TODO: if name_new is missing or empty, remove that from key
    name_old.new <- paste0(key[ , "name_old"], ".", key[ , "name_new"])

    if (!long){
        ## It is a wide/short key
        ## Lets protect ourselves from the user's new names
        key$name_new <- make.names(key$name_new, unique = TRUE)
        ## Split by old.new combination, allows multiple line per name_old
        name_old.new <- factor(name_old.new, levels = unique(name_old.new))
        keysplit <- split(key, name_old.new, drop = FALSE)

        makeOneVar <- function(keyds){
            keyds$value_old.orig <- keyds$value_old
            keyds$value_new.orig <- keyds$value_new
            val_old <- unlist(strsplit2(keyds$value_old.orig, sep[keyds$class_old]))
            val_new <- unlist(strsplit2(keyds$value_new.orig, sep[keyds$class_new]))
            recodes <- unlist(strsplit2(keyds$recodes, ";", fixed = TRUE))
            missings <- unlist(strsplit2(keyds$missings, ";", fixed = TRUE))
            list(name_old = keyds$name_old, name_new = keyds$name_new,
                 class_old = keyds$class_old, class_new = keyds$class_new,
                 value_old = val_old, value_new = val_new,
                 recodes = recodes, missings = missings)
        }
        keylist <- lapply(keysplit, makeOneVar)
    } else {
        ## It was a long key
        ## like unique, but it throws away white space, NA
        unique2 <- function(x){
            y <- unique(na.omit(n2NA(zapspace(x))))
            if (length(y) > 1){
                messg <- paste("Value of", deparse(substitute(x)), "not unique")
                stop (messg)
            }
            y
        }

        ## Make best effort to clean up the "name_new" column.
        ## There's an unsolved problem protecting from user error in name_old
        ## name_new combinations in long key. If user has forgotten to
        ## correctly assign name_new, a host of bad things can
        ## happen. I've tested various heuristics to detect that error, all
        ## fail on easy test cases. So stop that mission and just validate
        ## the name_new values
        name_new.unique <- unique(key$name_new)
        name_new.clean <- make.names(name_new.unique, unique = TRUE)
        names(name_new.clean) <- name_new.unique
        ## TODO: abstract for alternative names inkey, not "name_new"
        key[ , "name_new"] <- name_new.clean[key[, "name_new"]]

        ## Create a keylist member for a given data frame
        makeOneVar <- function(keyds){
            name_old <- unique2(keyds$name_old)
            name_new <- unique2(keyds$name_new)
            class_old <- unique2(keyds$class_old)
            class_new <- unique2(keyds$class_new)
            recodes <- unique(na.omit(n2NA(zapspace(keyds$recodes))))
            recodes <- if(length(recodes) > 0) unlist(strsplit(recodes, split=";", fixed = TRUE))
            missings <- unlist(na.omit(n2NA(zapspace(keyds$missings))))
            missings <- if(length(missings) > 0) unlist(strsplit(missings, split= ";", fixed = TRUE))
            value_new <- keyds$value_new
            ## value_new[value_new %in% na.strings] <- NA
            list(name_old = name_old, name_new = name_new,
                 class_old = class_old, class_new = class_new,
                 value_old = keyds$value_old, value_new = value_new,
                 missings = missings, recodes = recodes)
        }
        ## Make this a factor and control the ordering of the levels. Otherwise,
        ## split applies factor() and re-alphabetizes it.
        name_old.new <- factor(name_old.new, levels = unique(name_old.new))
        keysplit <- split(key, name_old.new, drop = FALSE)
        keylist <- lapply(keysplit, makeOneVar)
    }

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
##' @param key A variable key object, of class either "key" or "keylong"
##' @param diagnostic Default TRUE: Compare the old and new data
##'     frames carefully with the keyDiagnostic function.
##' @param safeNumericToInteger Default TRUE: Should we treat values
##'     which appear to be integers as integers? If a column is
##'     numeric, it might be safe to treat it as an integer.  In many
##'     csv data sets, the values coded c(1, 2, 3) are really
##'     integers, not floats c(1.0, 2.0, 3.0). See \code{safeInteger}.
##' @param ignoreCase Default TRUE. If column name is capitalized
##'     differently than name_old in the key, but the two are
##'     otherwise identical, then the difference in capitalization
##'     will be ignored.
##' @param debug Default FALSE. If TRUE, emit some warnings.
##' @return A recoded version of dframe
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
                     safeNumericToInteger = TRUE, ignoreCase = TRUE,
                     debug = FALSE){
    legalClasses = c("integer", "numeric", "double", "factor",
                     "ordered", "character", "logical")

    dframe <- cleanDataFrame(dframe, safeNumericToInteger = safeNumericToInteger)
    if (diagnostic) dforig <- dframe

    ## implement ignoreCase by keeping vector dfname_old.orig that we
    ## can use later to put old names back onto data frame.
    ## If key has multiple entries that are identcal after tolower(), will use
    ## first one.
    ## Keep vector of original names. If ignoreCase=FALSE, this changes nothing.
    dfname_old.orig <- colnames(dframe)
    if (ignoreCase){
        ## lowercase the names
        colnames(dframe) <- tolower(colnames(dframe))
    }
    names(dfname_old.orig) <- colnames(dframe)

    ## Hmm. Need to snapshot class of input variables before changing anything
    class_old.dframe <- sapply(dframe, function(x) class(x)[1])
    names(class_old.dframe) <- colnames(dframe)

    ## TODO: figure out what to do if class_old does not match input data
    ## coerce existing column to type requested in data frame
    ## Wait! Should this happen last or later?

    keylist <- makeKeylist(key)
    ## a list for collecting new variables.
    xlist <- list()

    for (v in keylist) {
        if(debug){
            print(paste("\n debug"))
            print(v)
        }
        class_new.key <- v$class_new
        class_old.key <- v$class_old
        class_old.data <- class_old.dframe[v$name_old]
        ## keep spare copy of original name, in case it gets lowercased next
        v$name_old.orig <- v$name_old
        if(ignoreCase) v$name_old <- tolower(v$name_old)

        ## TODO: what if class_old does not match class of imported
        ## data.  Need to think through implications of doing something like
        ## xnew <- as(xnew, class_old)

        ## If variable name from key is not in the data frame, go to next variable.
        if (!v$name_old %in% colnames(dframe)){
            messg <- paste("Notice: The data.frame does not include ", v$name_old.orig,
                           "which is in the key. That key element has no effect.")
            print(messg)
            next()
        }

        ## Extract candidate variable to column.
        xnew <- dframe[ , v$name_old]

        if (length(v$missings) > 0){
            for(m in v$missings){
                xnew <- assignMissing(xnew, m)
            }
        }

        ## Be simple. If they have "recodes" in key, apply them.
        if (length(v$recodes) > 0 && !all(is.na(v$recodes))) {
            for (cmd in v$recodes) xnew <- assignRecode(xnew, cmd)
            if(class(xnew) != class_new.key){
                messg <- paste("The return from the recode function was not of the correct class.",
                               "The returned object should be from 'class_new' by the key.")
                print(v)
                stop(messg)
            }
            mytext <- paste0("xlist[[\"", v$name_new, "\"]] <- ", "xnew")
            eval(parse(text = mytext))
        } else if(class_new.key %in% c("ordered", "factor")) {
            ## There was no recode, so apply special fixup for ordered and factor variables.
            ## TODO: If $v$value_old is "" (character empty), what to do?
            if (sum(!is.na(v$value_old) > 0)){
                if (length(v$value_old) == length(v$value_old)){
                    ## TODO: keep only differences between value_old and value_new?
                    ## Following to Work around the "deprecated duplicated levels" and "unused levels problem"
                    mytext <- paste0("xnew <- ", class_new.key, "(xnew, levels = v$value_old)")
                    eval(parse(text = mytext))
                    newlevels <- v$value_new
                    checkValues(xnew, v$value_old, dfname_old.orig[v$name_old])
                    names(newlevels) <- v$value_old
                    levels(xnew) <- newlevels[levels(xnew)]
                    mytext <- paste0("xlist[[\"", v$name_new, "\"]] <- xnew")
                    eval(parse(text = mytext))
                } else {
                    messg <- "keyApply: value_old and value_new are not equal in length"
                    stop(messg)
                }
            }
        } else if(class_new.key %in% c("integer", "numeric") && class_old.key %in% c("ordered", "factor")){
            mytext <- paste0("xvals <- as.", class_new.key, "(v$value_new)")
            eval(parse(text = mytext))
            xnew <- as.numeric(levels(xnew))[xnew]
            mytext <- paste0("xlist[[\"", v$name_new, "\"]] <- xnew")
            eval(parse(text = mytext))
        } else {
            ## There was no recode function, and this is not a factor or an ordered variable.
            ## Then process value_old to value_new.  Relying heavily on plyr::mapvalues
            xnew <- zapspace(xnew)
            if (length(v$value_old) == length(v$value_new)){
                checkValues(xnew, v$value_old, dfname_old.orig[v$name_old])
                ## Only change xnew if there are value_old and differences with value_new
                ## 20161107: could eliminate same-value old, new pairs, to be efficient,
                ## but this applies them all.
                if (any(!is.na(v$value_old)) && (!identical(v$value_old, v$value_new))){
                    xnew <- plyr::mapvalues(xnew, v$value_old, v$value_new, warn_missing = FALSE)
                }
                ## 20161107: Plan implemented now:
                ## Arrive here with no guidance except class_new value. Use
                ## R coercion and hope there's no error, but only for legalClasses.
                ## If class_new is some other, and xnew is not one of those, STOP!
                if ((class(xnew) != class_new.key)){
                    if (class_new.key %in% legalClasses) {
                        ## class is not correct, so try coercion
                        xnew <- as(xnew, class_new.key)
                    } else {
                        messg <- paste("The class of the variable did not match",
                                       "the class specified in the variable key.")
                        print(v)
                        stop(messg)
                    }
                }
                mytext <- paste0("xlist[[\"", v$name_new, "\"]] <- ", "xnew")
                eval(parse(text = mytext))
            } else {
                messg <- paste(dfname_old.orig[v$name_old], "is neither factor not ordered.",
                               "Why are new and old value vectors not same in length?")
                stop(messg)
            }
        }
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
##' The simplest thing is to crosstabulate new variable versus old variable to
##' see the mismatch.  For tables of up to 10 values or so, that will be satisfactory.
##'
##' For numeric variables, it appears there is no good thing to do
##' except possibly to re-apply any transformations. Maybe a simple
##' scatterplot will suffice.
##' @param dfold Original data frame
##' @param dfnew The new recoded data frame
##' @param keylist The imported variable key that was used to
##'     transform dfold into dfnew.
##' @param max.values Show up to this number of values for the old
##'     variable
##' @param nametrunc Truncate column and row names. Needed if there
##'     are long factor labels and we want to fit more information on
##'     table. Default = 6.
##' @param wide Number of characters per row in printed
##'     output. Suggest very wide screen, default = 200.
##' @param confidential Should numbers in table be rounded to nearest "10"
##'     to comply with security standard enforced by some American research
##'     departments.
##' @return NULL
##' @author Paul Johnson <pauljohn@@ku.edu>
keyDiagnostic <- function(dfold, dfnew, keylist, max.values = 20,
                          nametrunc = 8, wide = 200, confidential = FALSE)
{

    ## TODO if class in dfnew does not match keylist specification, fail

    ## TODO think more deeply on warning signs of bad recoding.
    ## other summary of match and mismatch.
    print(paste("We are going to run \"options(width=", wide, ")\"",
          "Please make your output display window wide", "so you can see the tables"))
    width.orig <- options("width")
    options(width = wide)
    if (confidential) {
        roundAt <- -1
    } else {
        roundAt <- 2
    }
    for (v in keylist){
        if (!v$name_new %in% colnames(dfnew)){
            messg <- paste("Variable", v$name_new, "is not included in the new data frame")
            next()
        }
        if (length(unique(dfold[ , v$name_old])) <= max.values){
            name_new.trunc <- substr(v$name_new, 1, min(nchar(v$name_new), nametrunc))
            name_old.trunc <- paste0(substr(v$name_old, 1, min(nchar(v$name_old), nametrunc)), " (old var)")
            print(round(table(dfnew[ , v$name_new], dfold[ , v$name_old],
                              exclude = NULL, dnn = c(name_new.trunc, name_old.trunc)), roundAt))
        } else {
            messg <- paste("Variable", v$name_new, "has more than", max.values,
                           "unique values.", "That is too large for a diagnostic table.")
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
##' ## View(mydf.keylong)
##'
##' mydf.key <- keyTemplate(mydf)
##' mydf.keywide2long <- wide2long(mydf.key)
##'
##' ## rownames not meaningful in long key, so remove in both versions
##' row.names(mydf.keywide2long) <- NULL
##' row.names(mydf.keylong) <- NULL
##' all.equal(mydf.keylong, mydf.keywide2long)
wide2long <- function(key, sep = c(character = "\\|", logical = "\\|",
                              integer = "\\|", factor = "[\\|<]",
                              ordered = "[\\|<]", numeric = "\\|"))
{
    makeOneVar <- function(x){
        value_old <- unlist(strsplit(x$value_old, sep[x$class_old]))
        value_new <- unlist(strsplit(x$value_new, sep[x$class_new]))
        if (length(value_old) == 0) value_old <- as.character("")
        if (length(value_new) == 0) value_new <- as.character("")
        values <- cbind(value_old, value_new)
        values <- unique(values)
        missings <- na.omit(unlist(strsplit(x$missings, ";")))
        missings <- c(missings, rep("", NROW(values) - length(missings)))
        recodes <- na.omit(unlist(strsplit(x$recodes, ";")))
        recodes <- c(recodes, rep("", NROW(values) - length(recodes)))

        zz <- data.frame(name_old = x$name_old,
                         name_new = x$name_new,
                         class_old = x$class_old,
                         class_new = x$class_new,
                         value_old = values[ , "value_old"],
                         value_new = values[ , "value_new"],
                         missings = missings,
                         recodes = recodes, stringsAsFactors = FALSE)
        ##zz <- lapply(zz, function(obj) if (length(obj) == 0) "" else x)
        zz
    }


    ## keysplit
    name_old.new <- paste0(key[ , "name_old"], ".", key[ , "name_new"])
    name_old.new <- factor(name_old.new, levels = unique(name_old.new))
    ks <- split(key, name_old.new, drop = TRUE)

    ## build a "long stanza" for each variable
    ksl <- lapply(ks, makeOneVar)

    keylong <- do.call(rbind, lapply(ksl, as.data.frame, stringsAsFactors = FALSE))

    class(keylong) <- c("keylong", "data.frame")
    keylong
}



##' convert a key object from long to wide format
##'
##' ##' This is not flexible, assumes columns are named in our canonical
##' style, which means the columns are named c("name_old", "name_new",
##' "class_old", "class_new", "value_old", "value_new").
##' @param keylong A variable key in the long format
##' @return A wide format variable key
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' mydf.path <- system.file("extdata", "mydf.csv", package = "kutils")
##' mydf <- read.csv(mydf.path, stringsAsFactors=FALSE)
##' ## A wide key we are trying to match:
##' mydf.key <- keyTemplate(mydf, long = FALSE, sort = TRUE)
##' mydf.key["x4", "missings"] <- "c(999)"
##' ## A long ke we will convert next
##' mydf.keylong <- keyTemplate(mydf, long = TRUE, sort = TRUE)
##' mydf.keylong["11", "missings"] <- "c(999)"
##' mydf.long2wide <- long2wide(mydf.keylong)
##' ## Tune the rownames to match style of long key
##' rownames(mydf.key) <- paste0(mydf.key$name_old, ".", mydf.key$name_new)
##' all.equal(mydf.key, mydf.long2wide)
long2wide <- function(keylong){
    name_old.new <- paste0(keylong[ , "name_old"], ".", keylong[ , "name_new"])
    name_old.new <- factor(name_old.new, levels = unique(name_old.new))
    ##kls = keylong split.
    ## 20161215: why didn't I use keylist maker here?
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

        ## 20170130: Was worried that NA and "NA" become indistinguisable
        ## The following caused errors
        ## printvals <- function(x, collapse){
        ##     x <- ifelse(is.na(x), "__MISING__", x)
        ##     paste(x, collapse = collapse)
        ## }
        ## Lets step back from that, see if a key can "round trip"
        printvals <- function(x, collapse){
            paste(x, collapse = collapse)
        }
        
        
        list(name_old = unique(x$name_old),
             name_new = unique(x$name_new),
             class_old = unique(x$class_old),
             class_new = unique(x$class_new),
             value_old = printvals(values[ , "value_old"], collapse = sep_old),
             value_new = printvals(values[ , "value_new"], collapse = sep_new),
             missings = paste(missings, collapse = ";"),
             recodes =  paste(recodes, collapse = ";"))
    }

    keywide <- lapply(kls, makeOneWide)

    key <- do.call("rbind", lapply(keywide, data.frame, stringsAsFactors = FALSE))
    class(key) <- c("key", "data.frame")
    key
}

## PJ 20161006: this is not ready, project where we were planning to
## us it discontinued.
## ##' Stack together several separate key templates, make a "jumbo key".
## ##'
## ##' Try to homogenize classes and name_old
## ##'
## ##' Try to tolerate inconsistent capitalization in name_old
## ##' @param aList list of key template objects
## ##' @param file file name intended for output
## ##' @param outdir directory name intended for output
## ##' @return A stacked variable key in the long format
## ##' @importFrom plyr rbind.fill
## ##' @author Paul Johnson
## keyStacker <- function(aList, file, outdir = "."){
##     ## TODO: Must convert keys to long form for stacking

##     key_jumbo <- rbind.fill(aList)
##     key_jumbo <- key_jumbo[order(key_jumbo$name_old), ]
##     key_jumbo <- unique(key_jumbo)

##     ## check for capitalization differences, remove equivalent rows.

##     keytest <- key_jumbo
##     keytest$name_old <- tolower(keytest$name_old)
##     keytest$name_new <- tolower(keytest$name_new)
##     key_jumbo[!duplicated(keytest), ]

##     if (!missing(file) && !is.null(file)){
##         smartSave(key_jumbo, file, outdir)
##     }
##     key_jumbo
##  }


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
##' This function deduces if the key provided is in the wide
##' or long format from the class of the object.
##' @param key A variable key
##' @param dframe A data.frame object.
##' @param append If long key, should new rows be added to the
##'     end of the updated key? Default is TRUE. If FALSE,
##'     new rows will be sorted with the original values.
##' @param safeNumericToInteger Default TRUE: Should we treat variables
##'     which appear to be integers as integers? In many
##'     csv data sets, the values coded c(1, 2, 3) are really
##'     integers, not floats c(1.0, 2.0, 3.0). See
##'     \code{safeInteger}.
##' ## Need to consider implementing this:
##' ## @param ignoreCase
##' @export
##' @return Updated variable key.
##' @importFrom plyr rbind.fill
##' @author Ben Kite <bakite@@ku.edu>
##' @examples
##' dat1 <- data.frame("Score" = c(1, 2, 3, 42, 4, 2),
##'                    "Gender" = c("M", "M", "M", "F", "F", "F"))
##' ## First try with a long key
##' key1 <- keyTemplate(dat1, long = TRUE)
##' key1[5, "value_new"] <- 10
##' key1[6, "value_new"] <- "female"
##' key1[7, "value_new"] <- "male"
##' key1[key1$name_old == "Score", "name_new"] <- "NewScore"
##' dat2 <- data.frame("Score" = 7, "Gender" = "other", "Weight" = rnorm(3))
##' dat2 <- plyr::rbind.fill(dat1, dat2)
##' dat2 <- dat2[-1,]
##' keyUpdate(key1, dat2, append = TRUE)
##' keyUpdate(key1, dat2, append = FALSE)
##' key1c <- key1
##' key1c[key1c$name_old == "Score", "class_new"] <- "character"
##' keyUpdate(key1c, dat2, append = TRUE)
##' str(dat3 <- keyApply(dat2, key1c))
##' ## Now try a wide key
##' key1 <- keyTemplate(dat1)
##' (key1.u <- keyUpdate(key1, dat2))
##' str(keyApply(dat2, key1.u))
##' str(keyApply(dat2, key1c))
##'
##' mydf.key.path <- system.file("extdata", "mydf.key.csv", package = "kutils")
##' mydf.key <-  keyImport(mydf.key.path)
##'
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
##' mydf.key2["x1", "value_old"] <- "cindy|bobby|jan|peter|marcia|greg"
##' mydf.key2["x1", "value_new"] <- "Cindy<Bobby<Jan<Peter<Marcia<Greg"
##'
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

    if(class(key)[1] == "keylong") {
        long <- TRUE
    } else if (class(key)[1] == "key") {
        long <- FALSE
    } else {
        messg <- paste("The key object is neither a longkey or key object")
        stop(messg)
    }

    dframe <- cleanDataFrame(dframe, safeNumericToInteger = safeNumericToInteger)
    if(!long){
        key <- wide2long(key)
    }
    keynew <- keyTemplate(dframe, long = TRUE)
    if (identical(keynew, key)){
        return(keynew)
    }

    ## Work to do
    ## 1. All new "variable-value" combinations must be added
    ## to the new key, whether they are for variables currently listed
    ## in "name_old" or not.
    ## 2. New values for previous variables must be added to key
    ## 3. name_new and class_new have to be copied over to key
    ## rows that are added.

    ## CHECK: what does "long2wide" do when rows in a long key are
    ## "shuffled" or if the new values all exist at end of long key.

    ## TODO: Some danger here that key column names are not abstracted,
    ## but I stopped abstracting them in other functions as well
    ## because it is too much work.

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
    ## Tried hard to find an index way to do this, but frustrating
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
        }
        row.names(output) <- seq(1, nrow(output), 1)
        return(output)
    }
    output
}


##' Compares a key provided to keyUpdate with the return of keyUpdate
##'
##' @param oldkey key that was provided to keyUpdate function
##' @param newkey updated key returned by keyUpdate function
##' @return summary of differences between the two keys
##' @author Ben Kite <bakite@@ku.edu>
##' @export
##' @examples
##' library(rockchalk)
##' dat1 <- data.frame("Score" = c(1, 2, 3, 42, 4, 2),
##'                    "Gender" = c("M", "M", "M", "F", "F", "F"))
##' ## First try with a long key
##' key1 <- keyTemplate(dat1, long = TRUE)
##' key1[5, "value_new"] <- 10
##' key1[6, "value_new"] <- "female"
##' key1[7, "value_new"] <- "male"
##' key1[key1$name_old == "Score", "name_new"] <- "NewScore"
##' dat2 <- data.frame("Score" = 7, "Gender" = "other", "Weight" = rnorm(3))
##' dat2 <- plyr::rbind.fill(dat1, dat2)
##' dat2 <- dat2[-1,]
##' key2 <- keyUpdate(key1, dat2, append = TRUE)
##' keyDiff(key1, key2)
keyDiff <- function(oldkey, newkey){
    rownames(oldkey) <- paste0(rownames(oldkey), ".old")
    rownames(newkey) <- paste0(rownames(newkey), ".new")
    xx <- rbind(newkey, oldkey)
    top <- duplicated(xx[,-ncol(xx)])
    bot <- duplicated(xx[,-ncol(xx)], fromLast = TRUE)
    yy <- ifelse(top + bot == 0, TRUE, FALSE)
    changes <- xx[yy,]
    if(nrow(changes) == 0) return("There are no differences between these keys!")
    changes <- changes[order(changes[,"name_old"]),]
    updated <- changes[,"name_old"][duplicated(changes[,"name_old"])]
    new <- names(table(changes[,"name_old"]))[which(table(changes[,"name_old"]) == 1)]
    messag <- paste0("The following variables are updated/added in the new key: ", paste0(paste0(updated, collapse = ", "), paste0(new, collapse = ", ")), "")
    output <- list("changes" = messag, "updatedRows" = data.frame(changes))
    class(output) <- "keyDiagnostic"
    output
}

##' Print the "changes" component of a keyDiagnostic object
##'
##' @method print keyDiagnostic
##' @export
##' @param x A keyDiagnostic object
##' @param ... Other arguments passed through to print
##' @author Ben Kite <bakite@@ku.edu>
print.keyDiagnostic <- function(x, ...) print(x[["changes"]], ...)
