


##' Create a new, cleaned data frame
##'
##' This depends on the assignMissing function. It looks at the
##' key file to figure out which variables need cleaning, then
##' it loops through them.
##' @param dfn The data frame to be cleaned
##' @param key The key file
##' @return A new data frame
##' @author Paul Johnson
cleanDF <- function(dfn, key){
    varsToClean <- key$VarName[nzchar(key$missings)]
    for (i in varsToClean){
        print(key[key$VarName == i, "missings"])
        dfn[ , i] <- assignMissing(dfn, i, key[key$VarName == i , "missings"])
    }
    dfn
}




##' Creates data objects in workspace, using the filekey and key to find files & variables
##'
##' .. content for \details{} ..
##' @title
##' @param datadir
##' @param filekey
##' @param key
##' @return Returns a data frame with dfnames and original file names, along with other info
##' @author Paul Johnson
retrieveData <- function(datadir, filekey, key){
    dfnames <- apply(filekey, 1, function(x){
        filename <- x[["file"]]
        basename <- x[["basename"]]
        sheets <- as.numeric(x[["sheets"]])
        skips <- as.numeric(x[["skip"]])

        for (i in 1:sheets){
            dat <- try(read.xlsx(paste0(datadir, filename),  startRow = skips + 1, sheet = i, colNames = TRUE, detectDates = TRUE ))
            keysub <- key[key$dsname == filename & key$sheet == i, ]
            newdfname <- paste0(basename, "_", i)
            cat(paste("retrieveData Diagnostic\n Processing file: ", filename, "Sheet: ", i, "Creating dataframe:", newdfname, "\n"))

            if (NROW(keysub) > 0){
                newnames <- keysub$newname
                ## force them to lower case if was error in key

                newnames <- tolower(newnames)
                names(newnames) <- keysub$oldname
                dat <- replaceNames(dat, newnames)
            } else {
                colnames(dat) <- tolower(colnames(dat))
            }
            dat <- deleteBogusRows(dat)
            assign( newdfname, dat, envir = .GlobalEnv)
        }
        #paste0(basename, "_", seq(1,sheets))
        data.frame(dfname = paste0(basename, "_", seq(1,sheets)), filename = filename, sheet = seq(1, sheets), stringsAsFactors = FALSE)
    })

    do.call("rbind", dfnames)
}




##'  Check the legal values by using the key
##'
##' .. content for \details{} ..
##' @param dat A data frame
##' @param key A variable key which includes the legal_values column
##' @param dfname text string identifier for data frame name. If not supplied, function will
##' get the name of the object passed in as dat using deparse(substitute...)
##' @return No return, this is run for a side effect of reporting errors
##' @author
checkVar2 <- function(dat, key, dfname, debug = FALSE){
    if (missing(dfname) || is.null(dfname))  dfname <- deparse(substitute(dat))
    ## key subet for dfname
    keysub  <- key[key$dfname == dfname,]
    keysub  <- keysub[is.na(keysub$legal_values) == FALSE, ] ## kick out the variables that don't need check, ie., no legal value constrained

    if(!NROW(keysub) > 0) {
        if(debug) print(paste("The key specifies no legal values for the data frame", dfname))
        return()
    }

    for ( i in 1:dim(keysub)[1]){
        if(debug) print(paste("In ", dfname, "varname",  keysub$newname[i]))

        mystring1 <- paste0("as.", keysub$type[i], "(", keysub$dfname[i], "$", keysub$newname[i], ")")
        if(debug) print(paste("mystring1 is", mystring1))
        dat[ , keysub$newname[i]]  <- eval(parse(text = mystring1))

        mystring <- paste0("dat[,", "\"", keysub$newname[i], "\"]"," %not.in% ", keysub$legal_values[i])
        illegal.data <- eval(parse(text = mystring ))

        if (any(illegal.data)) {
            cat(paste0("The data frame ", xlsnames[dfname], " has illegal values in the variable ", keysub$newname[i], "\n"))
            cnames <- c(colnames(dat)[1:2], keysub$newname[i])
            print(dat[illegal.data, cnames])
        }
    }
}




##' creates a simplified variable by collapsing values
##'
##'
##' @param dat A data frame
##' @param vname Name of drug variable, default "primsub"
##' @param drugkey A key structure from which recodes are obtained
##' @return A recoded variable.
##' @author Ben Kite Paul Johnson
collapseValues <- function(dat, vname = "primsub",  drugkey){
    if (!vname %in% names(dat)){
        stop("You failed to supply the correct data frame")
    } else{
        name <- tolower(drugkey$name)
        newname <- tolower(drugkey$newcategory)
        names(newname) <- name
        data <- tolower(dat$primsub)
        primsub.c <- newname[data]
    }
    primsub.c
}

##' recode the variables according to operationlized definitions offered in Code books
##' @param the key file that contains dfnames (the key file that merge key.rds and dfnames)
##' @return NA
##' @author Po-Yi Chen

recodeDat <- function(key){
    for ( j in unique(key$dfname)){


        mystring <- paste0("oldDat <- dat <- ", j)
        eval(parse(text = mystring))
        dfnames <- j
        workingKey <- key[key$dfname == dfnames,]

        ## create a varialbe name needtoRecode to categorize the types of recoding
        ## if needtoRcode == 0, it means we dont have to alter the variable at all
        ## if needtoRcode == 1, it means we want to change the variable's type but not the values within it
        ## if needtoRcode == 2, it means we want to change the variable's value but not its type
        ## if needtoRcpde == 3, it means we want to change the variable's type and value
        workingKey$needtoRecode <- rep(0, dim(workingKey)[1])
        for ( i in 1:dim(workingKey)[1]){
            if (is.na(workingKey$new_type[i]) == FALSE && is.na(workingKey$new_values[i]) == TRUE){
                workingKey$needtoRecode[i] <- 1
            }
            if (is.na(workingKey$new_type[i]) == TRUE && is.na(workingKey$new_values[i]) == FALSE){
                workingKey$needtoRecode[i] <- 2
            }
            if (is.na(workingKey$new_type[i]) == FALSE && is.na(workingKey$new_values[i]) == FALSE){
                workingKey$needtoRecode[i] <- 3
            }
        }


        workingKey3 <- workingKey[workingKey$needtoRecode == 3,]
        workingKey1 <- workingKey[workingKey$needtoRecode == 1,]

        for (i in workingKey3$newname){

            ##i <- "arrmonth"
            mystring <- workingKey3[workingKey3$newname == i,]$legal_values
            oldValues <- eval(parse(text = mystring))
            mystring <- workingKey3[workingKey3$newname == i,]$new_values
            newValues <- eval(parse(text = mystring))

            dat[,i] <- mapvalues(dat[,i], from = oldValues, to = newValues)
            mystring <- paste0("as.", workingKey3[workingKey3$newname == i,]$new_type, "(", "dat[,i]",")")
            dat[,i] <- eval(parse(text = mystring))
        }

        for ( i in workingKey1$newname){


            mystring <- paste0("as.", workingKey1[workingKey1$newname == i,]$new_type, "(", "dat[,i]",")")
            dat[,i] <- eval(parse(text = mystring))

        }

    ##assign( newdfname, dat, envir = .GlobalEnv)
    ##mystring <- paste0("assign(", j, ", dat, envir = .GlobalEnv)")
    mystring <- paste0(j, "<<- dat")
    eval(parse(text = mystring))

    }

}
