
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
##' @return A (hopefully) cleaned column of data
##' @export
##' @author Paul Johnson
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
            print(x)
            print(missings)
            messg <- paste0("missings for variable ", deparse(substitute(x)), " not understandable")
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
 


##' Create variable key
##'
##' A variable key is a human readable document that can be
##' interpreted by R to import and recode data. This might also be
##' referred to as a "programmable codebook."  This function inspects
##' a data frame, takes notice of its variable names, their classes,
##' and legal values, and then it creates a table summarizing that
##' information. The aim is to create a document that principal
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
##' @return A key in the form of a data frame. May also be saved on
##'     disk.
##' @export
##' @importFrom utils write.csv
##' @importFrom methods as
##' @author Paul Johnson
##' @examples
##' set.seed(234234)
##' N <- 200
##' mydf <- data.frame(x5 = rnorm(N),
##'                    x4 = rpois(N, lambda = 3),
##'                    x3 = ordered(sample(c("lo", "med", "hi"),
##'                    size = N, replace=TRUE),
##'                    levels = c("med", "lo", "hi")),
##'                    x2 = letters[sample(c(1:4,6), 200, replace = TRUE)],
##'                    x1 = factor(sample(c("cindy", "bobby", "marcia",
##'                                         "greg", "peter"), 200,
##'                    replace = TRUE)),
##'                    x7 = ordered(letters[sample(c(1:4,6), 200, replace = TRUE)]),
##'                    x6 = sample(c(1:5), 200, replace = TRUE),
##'                    stringsAsFactors = FALSE)
##' mydf$x4[sample(1:N, 10)] <- 999
##' mydf$x5[sample(1:N, 10)] <- -999
##' 
##' ## Should be same as content of
##' ## write.csv(mydf, file = "../inst/extdata/mydf.csv")
##' 
##' mydf.key <- keyTemplate(mydf, file = "mydf.key.csv")
##' mydf.keylong <- keyTemplate(mydf, long = TRUE, file = "mydfkey.long.csv")
##'
##' ## Try with the national longitudinal study data
##' data(natlongsurv)
##' natlong.key <- keyTemplate(natlongsurv, file = "natlongsurv.key.csv",
##'                            max.levels = 15, sort = TRUE)
##'
##' natlong.keylong <- keyTemplate(natlongsurv, long = TRUE,
##'                    file = "natlongsurv.keylong.csv", sort = TRUE)
##' 
##' \donttest{
##' if (require(openxlsx)){
##'    openxlsx::write.xlsx(natlong.key, file = "natlongsurv.key.xlsx")
##'    openxlsx::write.xlsx(natlong.keylong, file = "natlongsurv.keylong.xlsx")
##' }
##' }
##'  
keyTemplate <- function(dframe, long = FALSE, sort = FALSE, file = NULL,
                        outdir = getwd(), max.levels = 15)
{
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
   
    if (!missing(file) & !is.null(file)){
        fp <- paste0(outdir, "/", file)
        if (length(grep("csv$", tolower(file))) > 0){
            write.csv(key, file = fp, row.names = FALSE)
        } else if ((length(grep("xlsx$", tolower(file))))){
            openxlsx::write.xlsx(key, file = fp)
        } else if (length(grep("rds$", tolower(file)))){
            saveRDS(key, file = fp)
        } else {
            warning("Proposed key name did not end in csv, xlsx, or rds, so no file created")
        }
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


##' Import a variable key
##'
##' After researcher has updated the key by filling in new names and
##' values, we import that key file.
##'
##' The return value is a list, with one element per "name_old" to "name_new" variable combination.
##' If the key has one old variable being recoded 6 ways, that begets 6 elements
##' in the resulting list. Attributes including the classes of the old
##' and new variables are included. 
##' 
##' @param key A key object or a file name, csv, xlsx or rds.
##' @param long Is this a long format key file? If \code{key} is an
##'     object of class key or keylong, the value of long will be
##'     noticed and automatically set.
##' @param ... additional arguments for read.csv or read.xlsx.
##' @param keynames If key column names differ from what we expect,
##'     declare the new names in a named vector.
##' @param sepold The separators we currently use are "|" and "<", but
##'     if for whatever reason those were altered, say so here. Use
##'     regular expressions.
##' @param sepnew Ditto sepold
##' @param missingare Values in the value_new column which will be
##'     treated as NA in the key. The defaults will prevent a new
##'     value like "" or " ", so if one intends to insert white space,
##'     change the missingare vector.
##' @importFrom utils read.csv
##' @importFrom openxlsx read.xlsx write.xlsx
##' @export
##' @return A list with one element per variable name, along with some
##'     attributes like class_old and class_new. The class is set as
##'     well, "keylist". 
##' @author Paul Johnson
##' @examples
##' mydf.key.path <- system.file("extdata", "mydf.key_new.csv", package = "kutils")
##' mydf.keylist <-  keyImport(mydf.key.path)
##' mydf.key <- read.csv(mydf.key.path, stringsAsFactor = FALSE)
##' mydf.keylist2 <- keyImport(mydf.key, long = FALSE)
##' identical(mydf.keylist, mydf.keylist2)
##' mydf.keylong.path <- system.file("extdata", "mydfkey.long_new.csv", package = "kutils")
##' mydf.keylong.keylist <- keyImport(mydf.keylong.path, long = TRUE)
keyImport <- function(key, long = FALSE, ...,
                      keynames = c(name_old = "name_old",
                                   name_new = "name_new",
                                   class_old = "class_old",
                                   class_new = "class_new",
                                   value_old = "value_old",
                                   value_new = "value_new",
                                   missings = "missings")
                     ,
                      sepold = c(character = "\\|", logical = "\\|",
                              integer = "\\|", factor = "[\\|<]",
                              ordered = "[\\|<]", numeric = "\\|")
                     ,
                      sepnew = sepold
                     ,
                      missingare = c(".", "\\s",  "NA")
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
    if (inherits(key,"keylist")) return(keylist)
    if (inherits(key, "key")) {
        long <- FALSE
    } else if (inherits(key, "keylong")){
        long <- TRUE
    } else if (is.character(key)){
        ## key is file name, so scan for suffix
        if (length(grep("xlsx$", tolower(key))) > 0){
            studat <- openxlsx::read.xlsx(key, colNames = TRUE, check.names = FALSE)
        } else if (length(grep("csv$", tolower(key))) > 0){
            key <- read.csv(key, stringsAsFactors = FALSE, ...)
        } else if (length(grep("rds$", tolower(key))) > 0){
            key <- readRDS(key)
            if (inherits(key, "key")) {
                long <- FALSE
            } else if (inherits(key, "keylong")){
                long <- TRUE
            } else {
                stop("RDS object was neither key nor keylong")
            }
        }
    }

    
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

    ## TODO: if name_new is missing or empty, remove that from key
    
    ## TODO: abstract for "name_old", "name_new"
    name_old.new <- paste0(key[ , "name_old"], ".", key[ , "name_new"])


    if (!long){
        ## It is a wide/short key
        ## Lets protect ourselves from the user's new names
        key$name_new <- make.names(key$name_new, unique = TRUE)
        ## Split by old.new combination, allows multiple line per name_old
        keysplit <- split(key, name_old.new, drop = FALSE)

        keylist <- lapply(keysplit, function(keyds) {
            keyds$value_old.orig <- keyds$value_old
            keyds$value_new.orig <- keyds$value_new
          
            val_old <- unlist(strsplit2(keyds$value_old.orig, sepold[keyds$class_old]))
            val_new <- unlist(strsplit2(keyds$value_new.orig, sepnew[keyds$class_new]))
            val_new[val_new %in% missingare] <- NA
            recodes <- unlist(strsplit2(keyds$recodes, ";", fixed = TRUE))
            missings <- unlist(strsplit2(keyds$missings, ";", fixed = TRUE))
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
            value_new[value_new %in% missingare] <- NA
            list(name_old = name_old, name_new = name_new,
                 class_old = class_old, class_new = class_new,
                 value_old = keyds$value_old, value_new = value_new,
                 missings = missings, recodes = recodes)
        }
        
        keysplit <- split(key, name_old.new, drop = FALSE)
        keylist <- lapply(keysplit, makeOneVar)
    }

    attr(keylist, "class_old") <- sapply(keylist, function(keyds) keyds$class_old)
    attr(keylist, "class_new") <- sapply(keylist, function(keyds) keyds$class_new)
    class(keylist) <- "keylist"
    keylist
}
NULL




##' Apply variable key to data frame to recode data
##'
##' This is the main objective of the variable key system.
##' @param dframe An R data frame
##' @param keylist A keylist object
##' @param diagnostic Default TRUE: Compare the old and new
##'        data frames carefully with the keyDiagnostic function.
##' @return A recoded version of dframe
##' @export
##' @importFrom plyr mapvalues
##' @examples
##' mydf.key.path <- system.file("extdata", "mydf.key_new.csv", package = "kutils")
##' mydf.keylist <-  keyImport(mydf.key.path)
##' ## The column class "ordered" is not allowed in read.csv, but "factor" is. 
##' fixclasses <- attr(mydf.keylist, "class_old")
##' fixclasses <- gsub("ordered", "factor", fixclasses)
##' mydf.path <- system.file("extdata", "mydf.csv", package = "kutils")
##' mydf <- read.csv(mydf.path, colClasses = fixclasses, stringsAsFactors = FALSE)
##' mydf2 <- keyApply(mydf, mydf.keylist)
keyApply <- function(dframe, keylist, diagnostic = TRUE){
    
    if (diagnostic) dforig <- dframe

    ## Hmm. Need to snapshot class of input variables before changing anything
    class_old.dframe <- sapply(dframe, function(x) class(x)[1])
    names(class_old.dframe) <- colnames(dframe)

    ## TODO: figure out what to do if class_old does not match input data
    ## coerce existing column to type requested in data frame
    ## Wait! Should this happen last or later?

    ## a list for collecting new variables.
    xlist <- list()
    
    for (v in keylist) {
        name_old <- v$name_old
        name_new <- v$name_new
        class_new.key <- v$class_new
        class_old.key <- v$class_old
        class_old.data <- class_old.dframe[name_old]
        
        ## First apply missings to column. With new design,
        ## these might differ among rows (same name_old has various missings).
        xnew <- dframe[ , name_old]
        ## may be several missings for each variable
        ## xnew <- assignMissing(dframe[ , name_old], v$missings)
        if (length(v$missings) > 0){
            for(m in v$missings){
                xnew <- assignMissing(xnew, m)
            }
        }
       
        
        if(class_new.key %in% c("ordered", "factor")) {
            ## If $v$value_old is empty, what to do?
            ## Too risky to assing value_new in that case, cutting out code which
            ## tried to guess and do so.
            if (length(v$value_old) == length(v$value_old)){
                ## Work around the "deprecated duplicated levels" and "unused levels problem"
                mytext <- paste0("xnew <- ", class_new.key, "(xnew, levels = v$value_old)")
                eval(parse(text = mytext))
                newlevels <- v$value_new
                names(newlevels) <- v$value_old
                levels(xnew) <- newlevels[levels(xnew)]
                mytext <- paste0("xlist[[\"", name_new, "\"]] <- xnew")
                eval(parse(text = mytext))
            } else {
                stop("Can't understand why value_old and value_new are not equal in length")
            }
        } else {
            if (length(v$value_old) == length(v$value_new)){
                ## got error on numerics or integers where no values were set,
                ## so only do re-assignment if any value_old are not NA.
                ## TODO: check mapvalues works with NA on output value
                if (any(!is.na(v$value_old))){
                    xnew <- plyr::mapvalues(xnew, v$value_old, v$value_new, warn_missing = FALSE)
                }
                ## coerce data to class type in key if that differs
                ## from data. Key may have changed "numeric" to "integer", for example.
                if (class(xnew) != class_new.key){
                    xnew <- as(xnew, class_new.key)
                }
                
                mytext <- paste0("xlist[[\"", name_new, "\"]] <- ", "xnew")
                eval(parse(text = mytext))
            } else {
                messg <- paste(v$name_old, "is neither factor not ordered.",
                               "Why are new and old value vectors not same in length?")
                stop(messg)
            }
        }
    }
    
    dframe <- do.call(data.frame, xlist)
    keyDiagnostic(dforig, dframe, keylist)
    dframe
}
NULL

##' Diagnose accuracy of variable key application
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
##' @return NULL
##' @author Paul Johnson <pauljohn@@ku.edu>
keyDiagnostic <-
    function(dfold, dfnew, keylist, max.values = 20,
             nametrunc = 6, wide = 200)
{

    ## TODO if class in dfnew does not match keylist specification, fail

    ## TODO think more deeply on warning signs of bad recoding.
    ## other summary of match and mismatch.
    print("Make your display wide")
    width.orig <- options("width")
    options(width = wide)
    for (v in keylist){
        if (length(unique(dfold[ , v$name_old])) <= max.values){
            name_new.trunc <- substr(v$name_new, 1, min(nchar(v$name_new), nametrunc))
            name_old.trunc <- paste0(substr(v$name_old, 1, min(nchar(v$name_old), nametrunc)), ".old")
            print(round(table(dfnew[ , v$name_new], dfold[ , v$name_old], exclude = NULL, dnn = c(name_new.trunc, name_old.trunc)), -1))
        } else {
            print("need to write some code to deal with case where number of values is high")
            print("suggest that for discrete, we take table output and most numerous values.  For numeric variables, how about a missing value table and a scatter plot?")
        }
    }
    options(width = unlist(width.orig))
    NULL
}
