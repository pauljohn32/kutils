##' Import Qualtrics survey files, apply clean column names
##'
##' Defaults are based on most common format received from Qualtrics
##' downloads to CSV or XLSX (MS Excel) formats.  We assume that the
##' file has the column names in row 1 and that 3 rows are skipped
##' before the real data begins. If the parameter questrow is used, it
##' designates a row that is interpreted as the survey questions
##' themselves.  Often, this is in row 2.
##' @param file Character string with file name of a CSV or XLSX file
##'     from Qualtrics.
##' @param namerow Default 1, the information to be used as column
##'     names (the HEADER information in R's read.table function)
##' @param skip Number of rows to omit because they are not data
##' @param questrow Row number to be treated as the questions in the
##'     survey. Usually 2. Default is NULL, meaning questions are not
##'     imported.
##' @param dropTEXT Default TRUE, columns ending in "_TEXT" are
##'     omitted.
##' @param stringsAsFactors Default FALSE, same meaning as R's
##'     read.csv. Does not affect importation of Excel files.
##' @importFrom openxlsx read.xlsx
##' @export
##' @return Data frame that has attribute "questions" if questrow is
##'     specified.
##' @author Paul Johnson <pauljohn@@ku.edu>
importQualtrics <- function (file, namerow = 1, skip = 3, questrow = NULL,
                             dropTEXT = TRUE, stringsAsFactors = FALSE){
    if (length(grep("csv$", tolower(file))) > 0) {
        dat1 <- read.csv(file,
                         stringsAsFactors = stringsAsFactors,
                         header = FALSE)
        dat2 <- read.csv(file,
                         stringsAsFactors = stringsAsFactors,
                         skip = skip, header = FALSE)
    } else {
        startRow <- skip + 1
        dat1 <- read.xlsx(file, skipEmptyCols = FALSE,
                          colNames = FALSE)
        dat2 <- read.xlsx(file, startRow = startRow,
                          skipEmptyCols = FALSE, colNames = FALSE)
    }
        
    dat1.colnames <- make.names(dat1[namerow, ], unique = TRUE)
    if(dim(dat2)[2] == length(dat1.colnames)){
        colnames(dat2) <- dat1.colnames
    } else {
        MESSG <- "Unexpected columns in data"
        stop(MESSG)
    }
    if (dropText){
        ## Remove variable names ending in "_TEXT"
        dat2[ , grep("TEXT$", colnames(dat2), value = TRUE)] <- NULL
    }
    if(!is.null(questrow)){
        questions <- as.vector(t(dat1[questrow, ]))
        names(questions) <- dat1.colnames
        attr(dat2, "questions") <- questions[colnames(dat2)]
    }
    dat2
}
