## unit tests for variable key
## Ben Kite
## Charlie Redmon
## 20170801

## load packages
library(RUnit)
library(kutils)

## set data file paths
dfPath <- system.file("extdata", "mydf.csv", package = "kutils")
widekeyPath <- system.file("extdata", "mydf.key.csv", package = "kutils")
longkeyPath <- system.file("extdata", "mydf.key_long.csv", package = "kutils")

## can set "tolerance" argument for handling floating point issues
test.safeInteger <- function() {
    checkEquals(kutils::safeInteger(1.001), NULL)
    checkEquals(kutils::safeInteger(1.0000000000001), 1)
}

test.keyTemplate <- function(){
    dat <- data.frame("Score" = c(1, 2, 3, 42, 4, 2),
                      "Gender" = c("M", "M", "M", "F", "F", "F"),
                      "x" = rnorm(6))
    kt <- keyTemplate(dat)
    checkEquals(kt$class_old, c("integer", "factor", "numeric"))
    checkEquals(kt$value_new, c("1|2|3|4|42", "F|M", ""))
}

## from inst/examples/ directory:
## test keyImport function
##   ERROR: checkEquals not working in runTestFile, though it works
##          when called from the command line (possible namespace
##          issue?)
test.keyImport <- function() {
    ## check wide keys...
    widekey1 <- keyImport(widekeyPath)
    widekeyDF <- read.csv(widekeyPath, stringsAsFactors=FALSE)
    widekey2 <- keyImport(widekeyDF, long=FALSE)
    checkEquals(widekey1, widekey2)

    ## check long keys
    longkey1 <- keyImport(longkeyPath, long=TRUE)
    longkeyDF <- read.csv(longkeyPath, stringsAsFactors=FALSE)
    longkey2 <- keyImport(longkeyDF, long=TRUE)
    checkEquals(longkey1, longkey2)

}

## test keyApply function
## test.keyApply <- function() {
##     df <- read.csv(dfPath, stringsAsFactors = FALSE)

## keyApply(dframe, mydf.keylist1)

## }

## add test.keyApply
## must handle conversions between logical, integer, double, factor, ordinal,
##   and character types

