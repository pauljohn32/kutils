
##' Scrub a variable's missings away
##'
##' The missings values have to be carefully written, depending on
##' the type of variable that is being processed.
##' @param x A variable
##' @param missings A string with a vector of values or R expressions.
##'     These are done differently for integer, numeric, factor, and
##'     character variables.
##'\enumerate{
##' \item For integer variables, use a vector of
##'     missings, as in c(8,9,10) or part of an R expression such
##'     "> 8", ">= 8", "< 7", or "<= 7". Only expressions beginning with > or < are allowed.
##'
##' \item For numerics, use an inequality such as "> 99". The only
##'    other alternative we have allowed is a pair such as c(99, 101),
##'    to mean that values greater than OR equal to 99 and less than
##'    OR equal to 101 will be set as missing.
##' 
##' \item For factors, include a vector of levels to be
##'         marked as missing and removed from the list of levels.
##'
##' \item For character variables, a character vector of
##'         values to be marked as missing.
##' }
##' One of the concerns is that comparison of real-valued numerics is
##' not entirely dependable.  Exact comparisons with == are
##' unreliable, so don't ask for them.
##'
##' For factors, integers, and characters, particular values can
##' be listed. If a particular variable does not have observations
##' with the indicated values, the request will be ignored.
##' 
##' @return A (hopefully) cleaned column of data
##' @export
##' @author Paul Johnson
##' @examples
##' ## 1.  Integers.
##' ## must be very sure these are truly integers, or else fails
##' x <- seq.int(2L, 22L, by = 2L)
##' missings <- c(2)
##' assignMissing(x, missings)
##' 
##' missings <- c(10, 18, 22)
##' assignMissing(x, missings)
##' 
##' missings <- " < 7"
##' assignMissing(x, missings)
##' 
##' missings <- " > 11"
##' assignMissing(x, missings)
##' ## 2. strings
##' x <- c("low", "low", "med", "high")
##' missings <- c("low", "high")
##' assignMissing(x, missings)
##' missings <- c("med", "doesnot exist")
##' assignMissing(x, missings)
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
## 4. Real-valued variable
##' set.seed(234234)
##' x <- rnorm(10)
##' missings <- "< 0"
##' assignMissing(x, missings)
##' missings <- "> -0.2"
##' assignMissing(x, missings)
##' missings <- "c(0.1, 0.7)"
##' assignMissing(x, missings)
assignMissing <- function(x, missings){
    if (is.character(missings)) missings <- zapspace(missings)
    if (is.character(x)){
        x[x %in% missings] <- NA
        return(x)
    }
    if (is.factor(x)){
        levels(x)[which(levels(x) %in% missings)] <- NA
        return(x)
    }
    if (is.integer(x)){
        if (!is.character(missings)) mysep = "==" else mysep = ""
        conditional <- paste(paste(quote(x), mysep, missings), collapse = " | ")
        xcheck <- eval(parse(text = conditional))
        x[xcheck] <- NA
        return(x)
    }
    if (is.double(x)) {
        if(substr(missings, 1, 1) %in% c(">", "<")){
            conditional <- paste(quote(x), missings)
            xcheck <- eval(parse(text = conditional))
            x[xcheck] <- NA
        } else if (substr(missings, 1, 1) == "c"){
            misvec <- eval(parse(text = missings))
            if (length(misvec) > 2) stop("Missings interval has more than 2 values")
            if (!is.numeric(misvec)) stop("Missings vector must be numeric")
            if (any(is.na(misvec))) stop("Missings interval should not have any NA values")
            misvec <- sort(misvec)
            x[x >= misvec[1] & x <= misvec[2]] <- NA
        }
        return(x)
    }
    
    messg <- "Sorry, no missings assigned because variable type was unhandled"
    warning(messg)
    ## return unchanged input, didn't see what to do
    x 
}
 

##' Create a new, cleaned data frame
##'
##' This depends on the assignMissing function. It looks at the key
##' file to figure out which variables need cleaning, then it loops
##' through them.
##' @param dframe The data frame to be cleaned
##' @param key The key, a data frame in which columns named \code{name_old},
##'    \code{name_new}, and \code{missings} must exist.
##' @return A new data frame
##' @author Paul Johnson <pauljohn@@ku.edu>
cleanDF <- function(dframe, key){
    if (any(!isTRUE(c("name_old", "name_new", "missings") %in% colnames(key)))){
        messg <- "missings variable is not a column in the key"
        stop(messg)
    }

    ## Names of variables that have something under "missing"
    hasmissings <- names(na.omit(apply(key, 1, function(arow) {
        hasmiss <- nzchar(arow["missings"], keepNA = TRUE)
    })))
    
    key <- key[hasmissings, ]

    for (i in key[ , "name_old"]){
        mvals <- key[key[ , "name_old" == i], "missings"]
        print(mvals)
        exprs <- unlist(strsplit(mvals, ";"))
        for(j in exprs){
            dframe[ , key[i, "name_new"]] <- assignMissing(dframe[, i], j)
        }
    }
    dframe
}


##' Create a 'wide format' variable key table
##'
##' This is the original style of the variable key. It is more
##' compact, probably easier for experts to use, but perhaps more
##' complicated for non-programmers.
##' @param dframe A data frame
##' @param sort Default FALSE. Should the rows representing the
##'     variables be sorted alphabetically? Otherwise, they appear in
##'     the order in which they were included in the original dataset.
##' @param file A text string for the output file's base name.  Defaut
##'     is "key.csv"
##' @param outdir The output directory for the new variable key files.
##'     Default is current working directory.
##' @param max.levels Default = 15. When enumerating existing values
##'     for a variable, what is the maximum number of valuses that
##'     should be included in the variable key?
##' @return A key in the form of a data frame. The output formats will
##'     be described in the Details section.
##' @export
##' @author Paul Johnson
##' @examples
##' set.seed(234234)
##' N <- 200
##' mydf <- data.frame(x5 = rnorm(N), x4 = rnorm(N),
##'                    x3 = ordered(sample(c("lo", "med", "hi"),
##'                    size = N, replace=TRUE),
##'                    levels = c("lo", "med", "hi")),
##'                    x2 = letters[sample(1:24, 200, replace = TRUE)],
##'                    x1 = factor(sample(c("cindy", "bobby", "marsha",
##'                                         "greg", "chris"), 200, replace = TRUE)))
##' key <- keyTemplate(mydf, file = "mydf.key.csv")
##'
##' data(natlongsurv)
##' key2 <- keyTemplate(natlongsurv, file = "natlongsurv.key.csv", max.levels = 15,
##'     sort = TRUE)
##' 
keyTemplate <- function(dframe, sort = FALSE,  file = "key.csv",
                        outdir = getwd(), max.levels = 15)
{
    df.class <- sapply(dframe, function(x)class(x)[1])
    key <- data.frame(name_old = colnames(dframe), name_new = colnames(dframe),
                      class_old = df.class, class_new = df.class,
                      value_old = as.character(""), value_new = "", 
                      recodes = "", missings = "", stringsAsFactors = FALSE)
    

    for (i in names(df.class)){
        getValues <- function(x){
            y <- dframe[ , x]
            z <- ""
            if (df.class[x] %in% c("integer", "logical", "character", "Date")) {
                z <- paste0(unique(y)[1:min(max.levels, length(unique(y)))], collapse = "|")
                return(z)
            }
            if (df.class[x] == "ordered"){
                z <- paste0(levels(y), collapse = "<")
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

    if (!is.null(file) | !is.na(file)){
        write.csv(key, paste0(outdir, "/", file), row.names = FALSE)
    }

    key
}

    
##' Create a variable key template in the long form
##'
##' A variable key is a human readable document that can be
##' interpreted by R to import and recode data. This function
##' generates the long form variable key, in which the rows represent
##' possible values that a variable might take on.
##'
##' This function creates a table in an output file that researchers, even
##' ones who do not use R, can enter new variable names, new values, and so forth.
##' 
##' @param dframe A data frame
##' @param sort Default FALSE. Should the rows representing the
##'     variables be sorted alphabetically? Otherwise, they appear in
##'     the order in which they were included in the original dataset.
##' @param file A text string for the output file's base name.  Defaut
##'     is "key.csv"
##' @param outdir The output directory for the new variable key files.
##'     Default is current working directory.
##' @param max.levels Default = 10. When enumerating existing values
##'     for a variable, what is the maximum number of valuses that should be included in the variable
##'     key?  There will be one row per value.
##' @export
##' @importFrom utils write.csv
##' @return A data frame including the variable key. Creates a file
##'     named "key.csv".
##' @author Paul Johnson <pauljohn@@ku.edu> and Ben Kite
##'     <bakite@@ku.edu>
##' @examples
##' set.seed(234234)
##' N <- 200
##' mydf <- data.frame(x5 = rnorm(N), x4 = rnorm(N),
##'                    x3 = ordered(sample(c("lo", "med", "hi"),
##'                    size = N, replace=TRUE),
##'                    levels = c("lo", "med", "hi")),
##'                    x2 = letters[sample(1:24, 200, replace = TRUE)],
##'                    x1 = factor(sample(c("cindy", "bobby", "marsha",
##'                                         "greg", "chris"), 200,
##'                    replace = TRUE)),
##'                    stringsAsFactors = FALSE)
##' keylong <- keyTemplateLong(mydf, file = "mydfkey.long.csv")
##'
##' data(natlongsurv)
##' key2 <- keyTemplateLong(natlongsurv, file = "natlongsurv.longkey.csv", max.levels = 15,
##'     sort = TRUE)
##' 
##' \donttest{
##' if (require(openxlsx)){
##'    write.xlsx(key2, file = "natlongsurv.longkey.xlsx")
##' }
##' }
##' 
keyTemplateLong <- function(dframe, file = "key.long.csv", outdir = getwd(),
                            max.levels = 10, sort = FALSE)
{
    cn <- colnames(dframe)
    df.class <- sapply(dframe, function(x)class(x)[1])
    getUnique <- function(xname){
        if (df.class[[xname]] == "numeric") return ("")
        if (df.class[[xname]] %in% c("integer", "character")){
            xunique <- sort(unique(dframe[ , xname]))
            return (xunique[1:min(max.levels, length(xunique))])
        }
        if (df.class[[xname]] %in% c("factor", "ordered")){
            return(levels(dframe[ , xname]))
        }
    }

    key <- do.call("rbind", lapply(cn, function(x){
        expand.grid(name = x,
                    class = df.class[[x]],
                    value = getUnique(x), stringsAsFactors = FALSE) } ) )
    

    key <- data.frame(name_old = key$name,
                      name_new = key$name,
                      class_old = key$class,
                      class_new = key$class,
                      value_old = key$value,
                      value_new = key$value,
                      stringsAsFactors = FALSE)
    
    if (sort) key <- key[order(key$name_old), ]
    key$missings <- ""
    key$recodes <- ""
    
    if (!is.null(file) | !is.na(file)){
        write.csv(key, paste0(outdir, "/", file), row.names = FALSE)
    }
    key
}


##' Convert nothing to R NA (nothing = white space or other indications of missing value)
##'
##' Using regular expression matching, any value that has nothing
##' except for the indicated "nothing" values is converted to NA.  The
##' "nothing" values included by default are a period by itself (A SAS
##' missing value), an empty string, or white space, meaning " ", or
##' any number of spaces, or a tab.
##' @param x A character vector
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
    if (!is.character(x)) stop("n2NA requires a character variable")
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
##' This eliminates any characters matched by the regular expression `\\s` if
##' they appear at the beginning of a string or at its end. It does not alter
##' spaces in the interior of a string
##' @param x A character vector
##' @return A character vector with leading and trailing white space values removed.
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @export
##' @examples
##' gg <- c("", " ", "   ", "\t", "\t some", "some\t", " space first")
##' (zapspace(gg))
zapspace <- function(x){
    if (!is.character(x)){
        warning("zapspace does nothing to non-character variables")
        return(x)
    }
    y <- gsub("^\\s*", "", x)
    y <- gsub("\\s*$", "", y)
    y
}



##' Import a variable key
##'
##' After researcher has updated the key by filling in
##' new names and values, we import that key file.
##' @param file file name, csv or xlsx
##' @param long Is this a long format key file? 
##' @param ... additional arguments for read.csv or read.xlsx.
##' @param keynames If key column names differ from
##'     what we expect, declare the new names in a named vector.
##' @param sepold The separators we use are "|" and "<", but
##'     if for whatever reason those were altered, say so here.
##' @param sepnew Ditto sepold
##' @param missingare Values in the value_new column which
##'     will be treated as NA in the key.
##' @importFrom utils read.csv
##' @export
##' @return A list with one element per variable name
##' @author Paul Johnson
keyimport <- function(file, long = FALSE, ...,
                      keynames = c(name_old = "name_old",
                                   name_new = "name_new",
                                   class_old = "class_old",
                                   class_new = "class_new",
                                   value_old = "value_old",
                                   value_new = "value_new",
                                   missings = "missings"),
                      sepold = c(character = "|", logical = "|",
                              integer = "|", factor = "|",
                              ordered = "<", numeric = "|")
                     ,
                      sepnew = sepold
                     ,
                      missingare = c(".", "\\s",  NA)
                      )
{
    ## if x is "", return NA
    ## if "split" is NA or blank space or TAB, return unchanged x
    strsplit2 <- function(x, split, fixed = TRUE, perl = FALSE, useBytes = FALSE){
        if (is.na(n2NA(zapspace(split)))) return(x)
        if (is.na(n2NA(zapspace(x)))) return(NA)
        strsplit(x, split, fixed = TRUE, perl = FALSE, useBytes = FALSE)
    }
   
    key <- read.csv(file, stringsAsFactors = FALSE, ...)
    ## get rid of leading and trailing spaces that users sometimes insert in
    ## key when using a spreadsheet
    for (j in colnames(key)[sapply(key, is.character)]) {
        key[ , j] <- zapspace(key[ , j])
    }

    ## if ("\\s" %in% missingare){
    ##     for (j in colnames(key)[sapply(key, is.character)]) {
    ##         key[ , j] <- n2NA(key[ , j])
    ##     }
    ## }
    
    if (!long){
        ## It is a short key
        keysplit <- split(key, key$name_old, drop = FALSE)

        keylist <- lapply(keysplit, function(keyds) {
          
            keyds$value_old.orig <- keyds$value_old
            keyds$value_new.orig <- keyds$value_new
            keyds$value_old <- NA
            keyds$value_new <- NA
            
            val_old <- mapply(strsplit2, keyds$value_old.orig, sepold[keyds$class_old], fixed = TRUE)
            val_new <- mapply(strsplit2, keyds$value_new.orig, sepnew[keyds$class_new], fixed = TRUE)
            recodes <- unlist(strsplit2(keyds$recodes, ";", fixed = TRUE))
            missings <- unlist(strsplit2(keyds$missings, ";", fixed = TRUE))
            ## key3 <- expand.grid(name_old = keyds$name_old, name_new = keyds$name_new,
            ##                    class_old = keyds$class_old, class_new = keyds$class_new,
            ##                    value_old = unlist(val_old))
            ## key3[ , "value_new"] <- unlist(val_new)
            list(name_old = keyds$name_old, name_new = keyds$name_new,
                 class_old = keyds$class_old, class_new = keyds$class_new,
                 value_old = val_old, value_new = val_new,
                 recodes = recodes, missings = missings)
        })  
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
        keysplit <- split(key, key$name_old, drop = FALSE)

        keylist <- lapply(keysplit, function(keyds){
            name_old <- unique2(keyds$name_old)
            name_new <- unique2(keyds$name_new)
            class_old <- unique2(keyds$class_old)
            class_new <- unique2(keyds$class_new)
            recodes <- unique(na.omit(n2NA(zapspace(keyds$recodes))))
            missings <- unlist(na.omit(n2NA(zapspace(keyds$missings))))
            list(name_old = name_old, name_new = name_new,
                 class_old = class_old, class_new = class_new,
                 value_old = keyds$value_old, value_new = keyds$value_new,
                 recodes = recodes, missings = missings)
        }
        )
    }
    class(keylist) <- "keylist"
    keylist
}



## applyKey <- function(key, dframe, keynames = c(name_old = "name_old",
##                                                name_new = "name_new",
##                                                class_old = "class_old",
##                                                class_new = "class_new",
##                                                value_old = "value_old",
##                                                value_new = "value_new",
##                                                missings = "missings"))
## {
##     key <- colnamesReplace(key, keynames)

##     ## if(!is.null(key[ , keynames["missings"]])){
##     ##     keymissing <- key[ , c(name_old, missings)]
##     ##     keymissing[!is.na(keymissing[ , "missings"])]
        
##     ##     for (i in keymissing$name_old) {
##     ##         dframe[ ,i] <- assignMissing(dframe[ ,i], keymissing[keymissing[ , "name_old"] == i, "missings"])
##     ##     }
##     ## }



##     ## go down the rows applying the changes
##     for (i in 1:NROW(key)){
##         if (all(is.na(key$value_old2[[i]]))) next()
##         old_value <- key$value_old2[[i]]
##         new_value <- key$value_new2[[i]]
##         if(length(old_value) != length(new_value)) {
##             messg <- paste("Variable ", key$name_old[i], " had different lengths of value old and new.\n", "We are going to ignore rather than fail.")
##             warning(messg)
##             next()
##         }
       
##         dframe[ , key$name_old[i]] <-  mapvalues(dframe[ , key$name_old[i]], key$value_old2[[i]], key$value_new2[[i]])
##     }
##     dframe
## }





    
## applyKeyLong <- function(key, dframe, keynames = c(name_old = "name_old",
##                                                name_new = "name_new",
##                                                class_old = "class_old",
##                                                class_new = "class_new",
##                                                value_old = "value_old",
##                                                value_new = "value_new",
##                                                missings = "missings"))
## {
##     key <- colnamesReplace(key, keynames)
    
##     if(!is.null(key[ , keynames["missings"]])){
        
##         keymissing <- key[ , c(keynames["name_old"], keynames["missings"])]
##         keymissing[!is.na(keymissing[ , "missings"])]
        
##         for (i in key$name_old){
##             dframe[ ,i] <- assignMissing(dframe[ ,i], key[key$name_old == i, "missings"])
##         }
##     }
    

## }
