##' Import Qualtrics survey files, apply clean variable names
##'
##' Defaults to assume that the file has the column names in row 1 and
##' that 3 rows are skipped before the real data begins.
##' @param fn a CSV file name. Will generalize later to Excel, etc
##' @param namerow Default 1, which row has the column names?
##' @param questrow Does one row have the question? if Yes, designate
##'     it and the questions will be returned as an attribue of the
##'     data.
##' @param skip Number of rows to omit because they are not data
##' @param dropTEXT Default TRUE, columns ending in "_TEXT" are
##'     omitted
##' @param stringsAsFactors Default FALSE, same meaning as R's
##'     read.csv
##' @return Data frame with correct column names and questions as an
##'     attribute if questrow parameter is designated.
##' @author Paul Johnson <pauljohn@@ku.edu>
importQualtrics <- function (fn, namerow = 1, skip = 3, questrow = NULL,  dropTEXT = TRUE,
                            stringsAsFactors = FALSE){
    dat1 <- read.csv(file.path(ddir, fn[1]), stringsAsFactors = stringsAsFactors,
                     header = FALSE)
    dat2 <- read.csv(file.path(ddir, fn[1]), stringsAsFactors = stringsAsFactors,
                     skip = skip, header = FALSE)
    
    dat1.colnames <- make.names(dat1[namerow, ], unique = TRUE)

    if(dim(dat2)[2] == length(dat1.colnames)){
        colnames(dat2) <- dat1.colnames
    }
    ## Remove variable names ending in "_TEXT"
    dat2[ , grep("TEXT$", colnames(dat2), value = TRUE)] <- NULL
    if(!is.null(questrow)){
        questions <- as.vector(t(dat1[questrow, ]))
        names(questions) <- dat1.colnames
        attr(dat2, "questions") <- questions[colnames(dat2)]
    }
    dat2
}
