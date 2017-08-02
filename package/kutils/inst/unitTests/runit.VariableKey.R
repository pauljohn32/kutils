## unit tests for variable key
## Ben Kite
## Charlie Redmon
## 20170801

## load packages
library(RUnit)
library(kutils)

## set data file paths
dfPath <- "../extdata/testDF.csv"
keyPath <- "../extdata/testDFkey.csv"
widekeyPath <- "../extdata/mydf.key.csv"
longkeyPath <- "../extdata/mydf.key_long.csv"

## dfPath <- system.file("extdata", "mydf.csv", package = "kutils")
## widekeyPath <- system.file("extdata", "mydf.key.csv", package = "kutils")
## longkeyPath <- system.file("extdata", "mydf.key_long.csv", package = "kutils")

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
## test keyImport function (currently fails but this is due to an upstream
##   problem with wide2long)
test.keyImport <- function() {

    ## check wide key direct import and import from read-in data
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
test.keyApply <- function() {

    df0 <- read.csv(dfPath, stringsAsFactors=TRUE)
    key <- keyImport(keyPath)
    df1 <- keyApply(df0, key)

    ## TEST CONVERSIONS FROM LOGICAL
    l <- df0[,"varL"]
    
    ## logical --> logical
    ll0 <- ifelse(l == FALSE, TRUE, FALSE)  #code version of recode
    ll1 <- df1[,"varLL"]  #VarKey version of recode
    checkEquals(ll0, ll1)

    ## logical --> integer
    li0 <- ifelse(l == FALSE, 0, 1)
    li1 <- df1[,"varLI"]
    checkEquals(li0, li1)

    ## logical --> numeric
    ln0 <- ifelse(l == FALSE, -0.5, 0.5)
    ln1 <- df1[,"varLN"]
    checkEquals(ln0, ln1)

    ## logical --> factor
    lf0 <- factor(ifelse(l == FALSE, "no", "yes"), levels=c("yes","no"))
    lf1 <- df1[,"varLF"]
    checkEquals(lf0, lf1)

    ## logical --> ordered (FAIL DUE TO KEYAPPLY HANDLING OF ORDERED CLASS)
    lo0 <- ordered(ifelse(l == FALSE, "fail", "pass"))
    lo1 <- df1[,"varLO"]
    checkEquals(lo0, lo1)

    ## logical --> character
    lc0 <- as.character(ifelse(l == FALSE, "A", "B"))
    lc1 <- df1[,"varLC"]
    checkEquals(lc0, lc1)

    ## TEST CONVERSIONS FROM INTEGER
    i1 <- df0[,"varI1"]
    i2 <- df0[,"varI2"]

    ## integer --> logical
    il0 <- as.logical(ifelse(i2 == 1, TRUE, FALSE))
    il1 <- df1[,"varIL"]
    checkEquals(il0, il1)

    ## integer --> integer
    ii0 <- ifelse(i1 >= 999, NA, i1)
    ii1 <- df1[,"varII"]
    checkEquals(ii0, ii1)

    ## integer --> numeric
    in0 <- as.numeric(plyr::mapvalues(i2, from=1:5, to=seq(1, 3, by=0.5)))
    in1 <- df1[,"varIN"]
    checkEquals(in0, in1)

    ## integer --> factor
    if0 <- factor(ifelse(i2 < 3, "nonHOV", "HOV"), levels=c("nonHOV","HOV"))
    if1 <- df1[,"varIF"]
    checkEquals(if0, if1)

    ## integer --> ordered (FAIL DUE TO KEYAPPLY HANDLING OF ORDERED CLASS)
    io0 <- ordered(plyr::mapvalues(i2, from=1:5,
                   to=c("unfavorable", "slightlyUnfavorable", "neutral",
                        "slightlyFavorable", "favorable")),
                   levels=c("unfavorable", "slightlyUnfavorable", "neutral",
                            "slightlyFavorable", "favorable"))
    io1 <- df1[,"varIO"]
    checkEquals(io0, io1)

    ## integer --> character
    ic0 <- as.character(plyr::mapvalues(i2, from=1:5,
                                        to=c("a", "b", "c", "d", "f")))
    ic1 <- df1[,"varIC"]
    checkEquals(ic0, ic1)
    
    ## TEST CONVERSIONS FROM NUMERIC
    ## n0 <- df0[,"varN"]
    ## n1 <- ifelse(n0 <= -999, NA, n0)
    
    ## ## numeric --> logical (all non-zero = TRUE?)
    ## nl0 <- df1[,"varNL"]
    ## nl1 <- ifelse(n1 != 0, TRUE, FALSE)
    ## checkEquals(nl0, nl1)
    
    ## ## numeric --> integer (round down)
    ## ni0 <- df0[,"varN"]
    
    ## ## numeric --> numeric (check equality after handling missings)
    ## nn <- df1[,"varNN"]
    ## checkEquals(n1, nn)  #consider adding tolerance argument

    ## numeric --> factor (?)
    
    ## numeric --> ordinal (?)

    ## numeric --> character (?)
    
}

