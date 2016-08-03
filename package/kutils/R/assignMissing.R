
##' Scrub a variable's missings away
##'
##' The missings have to be carefully written out in the key file, or
##' else this fails
##' @param avar A variable
##' @param missings A string with a vector of values or R expressions.
##'     These are done differently for integer, numeric, factor, and
##'     character variables. For integer variables, use a vector of
##'     missings, as in c(8,9,10) or part of an R expression such
##'     "> 8". For numerics, use a range such as "> 99".  If a
##'     variable is real-valued, exact comparisons with == are
##'     unreliable, so don't ask for them.  use a two value vector to
##'     indicate a range that should be set to missing. For example,
##'     c("> 99", "< 101") will set values between 99 and 101 as
##'     NA. For factors and character variables, it should be a set of
##'     levels to be marked as NA, as in c("low", "high") or a vector
##'     of numbers referring to existing levels as returned by
##'     \code{levels()}.
##' @return A (hopefully) cleaned column of data
##' @export
##' @author Paul Johnson
##' @examples
## 1.  Integers.
## must be very sure these are truly integers, or else fails
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
## ## 2. strings
## x <- c("low", "low", "med", "high")
## missings <- c("low", "high")
## assignMissing(x, missings)
## missings <- c("med", "doesnot exist")
## assignMissing(x, missings)
## ## 3. factors (same as strings inside assignMissing)
## x <- factor(c("low", "low", "med", "high"))
## missings <- c("low", "high")
## assignMissing(x, missings)
## missings <- c("med", "doesnot exist")
## assignMissing(x, missings)
## ## 4. Real-valued variable
## set.seed(234234)
## x <- rnorm(10)
## missings <- "< 0"
## assignMissing(x, missings)
## missings <- c("> 0.1", "< 0.7")
## assignMissing(x, missings)
## ## Inclusive 
## x <- round(x, 2)
## missings <- c(">= 0.1", "<= 0.14")
## assignMissing(x, missings)
## ## If you need to remove the low and the high,
## ## necessary to set missings as a string with a semi-colon separator.
## missings <- c("< 0 ; > 2")
## assignMissing(x2, missings[2])
assignMissing <- function(avar, missings){
    if (is.factor(avar) | is.character(avar)){
        avar[avar %in% missings] <- NA
        avar <- factor(avar)
        return(avar)
    } else if (is.integer(avar)) {
        if (is.integer(avar) && !is.character(missings)) mysep = "==" else mysep = ""
        conditional <- paste(paste(quote(avar), mysep, missings), collapse = " | ")
        avarcheck <- eval(parse(text = conditional))
        avar[avarcheck] <- NA
        return(avar)
    } else if (is.double(avar)) { 
        conditional <- paste(paste(quote(avar), missings), collapse = " & ")
        avarcheck <- eval(parse(text = conditional))
        avar[avarcheck] <- NA
        return(avar)
    } else {
        messg <- "Sorry, no missings assigned because variable type was unhandled"
        warning(messg)
    }
    ## return unchanced input, didn't see what to do
    avar 
}
 

##' Create a new, cleaned data frame
##'
##' This depends on the assignMissing function. It looks at the key
##' file to figure out which variables need cleaning, then it loops
##' through them.
##' @param dframe The data frame to be cleaned
##' @param key The key, a data frame in which columns named \code{oldname},
##'    \code{newname}, and \code{missings} must exist.
##' @return A new data frame
##' @author Paul Johnson <pauljohn@@ku.edu>
cleanDF <- function(dframe, key){
    if (any(!isTRUE(c("oldname", "newname", "missings") %in% colnames(key)))){
        messg <- "missings variable is not a column in the key"
        stop(messg)
    }

    ## Names of variables that have something under "missing"
    hasmissings <- names(na.omit(apply(key, 1, function(arow) {
        hasmiss <- nzchar(arow["missings"], keepNA = TRUE)
    })))
    
    key <- key[hasmissings, ]

    for (i in key[ , "oldname"]){
        mvals <- key[key[ , "oldname" == i], "missings"]
        print(mvals)
        exprs <- unlist(strsplit(mvals, ";"))
        for(j in exprs){
            dframe[ , key[i, "newname"]] <- assignMissing(dframe[, i], j)
        }
    }
    dframe
}


##' Create a 'wide format' variable key table
##'
##' This is the original style of the variable key. It is more compact,
##' probably easier for experts to use, but perhaps more (too) complicated
##' for non-programmers.
##' @param dframe A data frame
##' @param cnames The column names to be created in the new variable key
##' @param file A text string for the output file's base name. 
##'     Defaut is "key.csv"    
##' @param outdir The output directory for the new variable key files. 
##'     Default is current working directory. 
##' @return A key in the form of a data frame
##' @author Paul Johnson
##' 
keyTemplate <- function(dframe, cnames = c(oldname = "oldname",
                                           newname = "newname",
                                           oldclass = "oldclass",
                                           newclass = "newclass",
                                           recodes = "recodes",
                                           missings = "missings"),
                        file = "key.csv", outdir = getwd())
{
    df.class <- sapply(dframe, function(x)class(x)[1])
    key <- data.frame(oldname = colnames(dframe), newname = "", oldclass = df.class,
                      newclass = "", recodes = "", missings = "")
    
    write.csv(key, paste0(outdir, "/", file), row.names = FALSE)
    key
}

    
##' Create a variable key template in the long form
##'
##' A variable key is a human readable document that can be
##' interpreted by R to import and recode data. This function generates
##' the long form variable key, in which the rows represent possible values that a variable might take on.
##'
##' This function creates a table in an output file that researchers, even
##' ones who do not use R, can enter new variable names, new values, and so forth.
##' 
##' @param dframe A data frame
##' @param cnames Vector of column names desired in the output
##'     file. It will hardly ever be necessary to change these, but it
##'     is allowed.
##' @param sort Default FALSE. Should the rows representing the
##'     variables be sorted alphabetically? Otherwise, they appear in
##'     the order in which they were included in the original dataset.
##' @param file A text string for the output file's base name. 
##'     Defaut is "key.csv"    
##' @param outdir The output directory for the new variable key files. 
##'     Default is current working directory.
##' @param maxvalues Default = 10.
##' @export
##' @importFrom utils write.csv
##' @return A data frame including the variable key. Creates a file named "key.csv".
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
##'                                         "greg", "chris"), 200, replace = TRUE)),
##'                    stringsAsFactors = FALSE)
##' keyTemplateLong(mydf)
##' \donttest{
##' if (require(openxlsx)){
##'    write.xlsx(key, file = "natlongsurv.key.csv")
##' }
##' }
##' 
keyTemplateLong <- function(dframe, cnames = c(oldname = "oldname",
                                               newname = "newname",
                                               oldclass = "oldclass",
                                               newclass = "newclass",
                                               oldvalue = "oldvalue",
                                               newvalue = "newvalue",
                                               missings = "missings"),
                            file = "key.csv", outdir = getwd(),
                            maxvalues = 10, sort = FALSE)
{
    cn <- colnames(dframe)
    df.class <- sapply(dframe, function(x)class(x)[1])

    df.unique <- lapply(dframe, unique)
    
    df.unique <- lapply(df.unique, sort, na.last = TRUE)
    df.length <- sapply(df.unique, length)

    df.length2 <- ifelse(df.class %in% c("factor", "ordered"), df.length,
                  ifelse(df.class %in% c("character", "integer") & df.length <= maxvalues, df.length,
                  ifelse(df.class %in% c("character", "integer"), maxvalues,
                  ifelse(df.class %in% c("numeric"), 1, 1))))

    names(df.length2) <- names(df.class)
    df.unique2 <- lapply(names(df.unique), function(x) df.unique[[x]][seq.int(df.length2[[x]])])

    ppp <- sapply(names(df.class), function(x){
        sss <- df.length2[[x]] ## This line and the one below deal with variables with all missing!
        ssss <- ifelse(sss == 0, 1, sss)
        vname <- rep(x, each = ssss)
        vname
    })
   

    key <- data.frame(oldname = unlist(ppp), newname = "",  stringsAsFactors = FALSE)
    key$oldclass <- df.class[key$oldname]
    key$newclass <- rep(" ", nrow(key))
    key$seq <- unlist(tapply(key$oldname, key$oldname, seq_along))
    
    xxx <- lapply(df.unique2, function(x) as.character(x[seq_along(x)]))
    key$oldvalue <- unlist(xxx) ## This wasn't working properly.  There were 8 too many values, but the addition to the function for the ppp object fixed it.
    key$seq <- NULL
    key$oldvalue <- ifelse(key$oldclass == "numeric", ".", key$oldvalue)
    key$oldvalue <- ifelse(key$oldclass == "integer", ifelse(nchar(key$oldvalue) >= 4, ".", key$oldvalue), key$oldvalue)
    key[is.na(key$oldvalue), "oldvalue"] <- "NA"
    key[key$oldvalue %in% c("", " ", "  "), "oldvalue"] <- "NA"
    key$newvalue <- ifelse(is.na(key$oldvalue), "NA", "")
    key$newvalue <- ifelse(key$oldvalue  == ".", ".", "")
    if (sort) key <- key[order(key$oldnames), ]
    
    colnamesReplace(key, newnames = cnames)
     
    write.csv(key, paste0(outdir, "/", file), row.names = FALSE)
    key
}




## importWithKeyLong <- (dframe, key){

##     ## First, apply missings
##     ## Then apply numeric recodes with the newvalue parsed
##     ## Then apply factors with the newvalues as they are


##     ## clean up key column names, then
##     ## colnamesReplace
    
## }


## importWithKeyShort <- function(dframe, key){

##     for (i in colnames(dframe)){
##         dframe[ , key[key$oldname == i, "newname"]] <- cleanVar("nlsdat", i, key[key$var2 == i , "missings"])
## }



## }
