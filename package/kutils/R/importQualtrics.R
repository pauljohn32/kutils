##' Import Qualtrics survey files, apply clean column names
##'
##' Defaults are based on most common format received from Qualtrics
##' downloads to CSV or XLSX (MS Excel) formats.  We assume that the
##' file has the column names in row 1 and that 3 rows are skipped
##' before the real data begins. If the parameter questrow is used, it
##' designates a row that is interpreted as the survey questions
##' themselves.  Often, this is in row 2.
##' @param file file name (including path if in another directory) of
##'     a CSV or XLSX file from Qualtrics.
##' @param namerow Row number for variable names.  Default 1, the
##'     information to be used as column names (same as HEADER row in
##'     R's read.table function)
##' @param questionrow Row number to be treated as the questions in the
##'     survey. Default is 2. If questions do not seem to be present
##'     in this row, there will be a warning.
##' @param importidrow Row number to be treated as Qualtrics meta
##'     data. Default is 3. Many CSV created by Qualtrics will have
##'     row 3 with a character string such as
##'     \code{"{""ImportId"":""QID1303_4""}"}.  If importids are not
##'     present in this row, there will be a warning.
##' @param skip Number of rows that are meta data. Current Qualtrics
##'     CSV files will usually have 3 metadata rows, 1 = name, 2 =
##'     question, 3 = ImportId. This function will try to guess how
##'     many rows of metadata are present. skip should be at least
##'     as large as \code{max(namerow, questions, and importids)}
##' @param dropTEXT Default TRUE, columns ending in "_TEXT" are
##'     omitted.
##' @param stringsAsFactors Default FALSE, same meaning as R's
##'     read.csv. Does not affect importation of Excel files.
##' @importFrom openxlsx read.xlsx
##' @importFrom utils type.convert
##' @export
##' @return Data frame that has attribute "meta"
##' @author Paul Johnson <pauljohn@@ku.edu>
importQualtrics <- function (file, namerow = 1, questionrow = 2,
                             importidrow = 3, skip = 3, dropTEXT = TRUE,
                             stringsAsFactors = FALSE){
    if (length(grep("csv$", tolower(file))) > 0) {
        dat1 <- read.csv(file, allowEscapes=TRUE, fill=TRUE,
                         header = FALSE, stringsAsFactors = stringsAsFactors)
 
    } else {
       dat1 <- read.xlsx(file, skipEmptyCols = FALSE,
                          colNames = FALSE)
    }

    importrow <- grep("\\{\"ImportId", dat1[ , 1])
    if((length(importrow) > 0)){
        if (importrow != importidrow){
            MESSG <- paste("ImportId information appears to be in row", importrow, ".\n")
            warning(MESSG)
            importidrow <- importrow
        } else {
            if (skip < importrow) skip <- importrow
        }
    } else {
        MESSG <- paste("Data does not include an ImportId metadata line.\n",
                       "We are omitting", skip, "rows, but this may throw\n",
                       "away some data that you want. Check your input,\n",
                       "re-specify the skip  parameter")
        warning(MESSG)
    }

    if (dropTEXT){
        ## Remove variable names ending in "_TEXT"
        dat1[ , grep("TEXT$", colnames(dat1), value = TRUE)] <- NULL
    } 
    
    meta <- as.data.frame(t(dat1[1:skip, ]), stringsAsFactors = FALSE)
    colnames(meta) <- paste0("row", 1:skip)
    colnames(meta)[namerow] <- "name"
    colnames(meta)[questionrow] <- "question"
    colnames(meta)[importidrow] <- "ImportId.orig"
    meta$name.clean <- make.names(meta[ , "name"], unique = TRUE)
    colnames(dat1) <- meta$name.clean
    meta$col.number <- 1:NROW(meta)
    if((length(importrow) > 0)){
    ## could not find 1 regex to handle whole problem
        meta$importid <- gsub("\\{\"(.*)\":\"(.*)\".*\\}", "\\2",
                              gsub("(.*),(.*)", "\\1}", meta[ , "ImportId.orig"]))
    }
    
    d2.1 <- dat1[ -(1:skip), ]
    dat2 <- as.data.frame(lapply(d2.1, type.convert, as.is = TRUE))
                
    if(NROW(dat2) != NROW(dat1) - skip){
        MESSG2 <- paste("\nNOTE:\n",
                        "Please verify number of rows in imported data.\n")
        warning(MESSG2)
    } else {
        MESSG3 <- paste("\n NOTE:\n",
                        "Number of imported rows appears correct.\n",
                        "Nevertheless, please verify against original data.\n")
        warning(MESSG3)
    }


    if(!is.null(meta)){
        attr(dat2, "meta") <- meta   
    }
    dat2
}


##' Create meta data frame to align identical questions
##'
##' Qualtrics returns a data frame that has vertical "blocks", one for
##' each "treatment condition" in an experimental condition.
##' Researchers often want to align the questions from the blocks
##' vertically, essentially converting the Qualtrics "wide" format to
##' a "long" format. This is a helper function that identifies
##' questions that may need to be stacked together.  The input is a
##' meta data structure (can be retrieved as an attribute from
##' importQualtrics). It will find out which questions are identical
##' and prepare to re-align ("stack") the columns.
##' @param meta A meta data structure retrieved from importQualtrics
##' @param questionname Character string for name of column in meta data that holds the questions
##' @return A new meta data table that horizontally aligns equivalent questions.
##' @export
##' @importFrom stats reshape
##' @author Paul Johnson
qualtricsBlockStack <- function(meta, questionname = "question"){
    meta$first.match <- match(meta[ , questionname], meta[ , questionname])
    
    ## data.table method was causing errors, so forget about it
    metasplit <- split(meta, f = meta$first.match)
    meta <- do.call("rbind", lapply(metasplit, function(x) {x$sect <- 1:NROW(x); x}))
      
    reslt <- reshape(meta, direction="wide",
            v.names = c("col.number", "name", "name.clean", "importid"),
            timevar = "sect", idvar = "first.match", drop = c("ImportId.orig"))
    reslt[ , "first.match"] <- NULL
    reslt
}
