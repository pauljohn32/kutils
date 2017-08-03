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

## define precision level for float comparisons
floatPrecision <- 1e-6

## test keyApply function
test.keyApply <- function() {

    df0 <- read.csv(dfPath, stringsAsFactors=TRUE)
    df0$varO1 <- ordered(df0$varO1)
    df0$varC1 <- as.character(df0$varN2)
    key <- keyImport(keyPath)
    df1 <- keyApply(df0, key)

    ## TEST CONVERSIONS FROM LOGICAL
    l <- df0[,"varL"]
    
    ## logical --> logical
    ll0 <- ifelse(l == FALSE, TRUE, FALSE)
    checkEquals(ll0, df1[,"varLL"])  #compare code version with VarKey version

    ## logical --> integer
    li0 <- ifelse(l == FALSE, 0, 1)
    checkEquals(li0, df1[,"varLI"])

    ## logical --> numeric
    ln0 <- ifelse(l == FALSE, -0.5, 0.5)
    checkEqualsNumeric(ln0, df1[,"varLN"], tolerance=floatPrecision)

    ## logical --> factor
    lf0 <- factor(ifelse(l == FALSE, "no", "yes"), levels=c("yes","no"))
    checkEquals(lf0, df1[,"varLF"])

    ## logical --> ordinal (FAIL DUE TO KEYAPPLY HANDLING OF ORDERED CLASS)
    lo0 <- ordered(ifelse(l == FALSE, "fail", "pass"))
    lo1 <- df1[,"varLO"]
    checkEquals(lo0, lo1)

    ## logical --> character
    lc0 <- as.character(ifelse(l == FALSE, "A", "B"))
    checkEquals(lc0, df1[,"varLC"])

    ## TEST CONVERSIONS FROM INTEGER
    i1 <- df0[,"varI1"]
    i2 <- df0[,"varI2"]

    ## integer --> logical
    il0 <- as.logical(ifelse(i2 == 1, TRUE, FALSE))
    checkEquals(il0, df1[,"varIL"])

    ## integer --> integer
    ii0 <- ifelse(i1 >= 999, NA, i1)
    checkEquals(ii0, df1[,"varII"])

    ## integer --> numeric
    in0 <- as.numeric(plyr::mapvalues(i2, from=1:5, to=seq(1, 3, by=0.5)))
    checkEquals(in0, df1[,"varIN"])

    ## integer --> factor
    if0 <- factor(ifelse(i2 < 3, "nonHOV", "HOV"), levels=c("nonHOV","HOV"))
    checkEquals(if0, df1[,"varIF"])

    ## integer --> ordinal (FAIL DUE TO KEYAPPLY HANDLING OF ORDERED CLASS)
    io0 <- ordered(plyr::mapvalues(i2, from=1:5,
                   to=c("unfavorable", "slightlyUnfavorable", "neutral",
                        "slightlyFavorable", "favorable")),
                   levels=c("unfavorable", "slightlyUnfavorable", "neutral",
                            "slightlyFavorable", "favorable"))
    checkEquals(io0, df1[,"varIO"])

    ## integer --> character
    ic0 <- as.character(plyr::mapvalues(i2, from=1:5,
                                        to=c("a", "b", "c", "d", "f")))
    checkEquals(ic0, df1[,"varIC"])
    
    ## TEST CONVERSIONS FROM NUMERIC
    n1 <- df0[,"varN1"]
    n2 <- df0[,"varN2"]
    
    ## numeric --> logical
    nl0 <- ifelse(n2 < 0.5, FALSE, TRUE)
    checkEquals(nl0, df1[,"varNL"])

    ## numeric --> integer (FAIL: RETAINS NUMERIC CLASS)
    ni0 <- as.integer(ifelse(n1 <= -999, NA, n1))
    checkEquals(ni0, df1[,"varNI"])

    ## numeric --> numeric
    nn0 <- ifelse(n1 <= -999, NA, n1)
    checkEqualsNumeric(nn0, df1[,"varNN"], tolerance=floatPrecision)

    ## numeric --> factor
    nf0 <- factor(plyr::mapvalues(as.character(n2),
                  from=as.character(seq(0, 1, 0.1)),
                  to=c(rep("Q1", 3), rep(paste0("Q",2:5), each=2))))
    checkEquals(nf0, df1[,"varNF"])
    
    ## numeric --> ordinal (FAIL DUE TO KEYAPPLY HANDLING OF ORDERED CLASS)
    no1 <- ordered(plyr::mapvalues(as.character(n2),
                   from=as.character(seq(0, 1, 0.1)),
                   to=c(rep("lower",5), rep("middle",4), rep("upper",2))))
    checkEquals(no1, df1[,"varNO"])
    
    ## numeric --> character
    nc1 <- plyr::mapvalues(as.character(n2), from=as.character(seq(0, 1, 0.1)),
                           to=c("A","B","C","D","E","F","G","H","I","J","K"))
    checkEquals(nc1, df1[,"varNC"])

    ## TEST CONVERSIONS FROM FACTOR
    f1 <- df0[,"varF1"]
    f2 <- df0[,"varF2"]
    f3 <- df0[,"varF3"]
    f4 <- df0[,"varF4"]
    
    ## factor --> logical
    fl0 <- ifelse(f1 == "yes", TRUE, FALSE)
    checkEquals(fl0, df1[,"varFL"])

    ## factor --> integer
    fi0 <- ifelse(f1 == "yes", 1, 0)
    checkEquals(fi0, df1[,"varFI"])

    ## factor --> numeric
    fn0 <- as.numeric(as.character(plyr::mapvalues(f2,
                                                   from=c("lo", "med", "hi"),
                                                   to=c(-0.5, 0, 0.5))))
    checkEquals(fn0, df1[,"varFN"])

    ## factor --> factor
    ff0 <- plyr::mapvalues(f3, from=levels(f3), to=c(rep("P", 4), "F"))
    checkEquals(ff0, df1[,"varFF"])

    ## factor --> ordinal (FAIL DUE TO KEYAPPLY HANDLING OF ORDERED CLASS)
    ff0 <- ordered(f2, levels=c("lo","med","hi"))
    checkEquals(ff0, df1[,"varFO"])

    ## factor --> character
    fc0 <- as.character(plyr::mapvalues(f4, from=levels(f4),
                        to=c("Bobby","Cindy","Greg","Marcia","Peter")))
    checkEquals(fc0, df1[,"varFC"])

    ## TEST CONVERSIONS FROM ORDINAL
    o <- df0[,"varO1"]

    ## ordinal --> logical
    ol0 <- ifelse(o == "1", FALSE, TRUE)
    checkEquals(ol0, df1[,"varOL"])

    ## ordinal --> integer (NOTE: < must be changed to | in value_new col)
    oi0 <- as.integer(as.character(o))
    checkEquals(oi0, df1[,"varOI"])

    ## ordinal --> numeric
    on0 <- as.numeric(as.character(o))
    checkEqualsNumeric(on0, df1[,"varON"], tolerance=floatPrecision)

    ## ordinal --> factor (FAIL: CONVERSION RETAINS ORDERED CLASS)
    of0 <- factor(plyr::mapvalues(o, c(1,3,5), c("A","B","C")), ordered=FALSE)
    checkEquals(of0, df1[,"varOF"])

    ## ordinal --> ordinal
    of0 <- plyr::mapvalues(o, from=c(1,3,5), to=c("low","mid","high"))
    checkEquals(of0, df1[,"varOO"])

    ## ordinal --> character (NOTE: < must be changed to | in value_new col)
    oc0 <- as.character(o)
    checkEquals(oc0, df1[,"varOC"])

    ## TEST CONVERSIONS FROM CHARACTER
    c1 <- df0[,"varC1"]

    ## character --> logical
    cl0 <- ifelse(c1 %in% c("0","0.1","0.2","0.3","0.4"), FALSE, TRUE)
    checkEquals(cl0, df1[,"varCL"])

    ## character --> integer
    ci0 <- ifelse(c1 %in% c("0","0.1","0.2","0.3","0.4"), 0, 1)
    checkEquals(ci0, df1[,"varCI"])

    ## character --> numeric (FAIL: RETAINS CHARACTER CLASS)
    ## NOTE: value_new must be empty to prevent "logic error" in keyApply
    cn0 <- as.numeric(c1)
    checkEqualsNumeric(cn0, df1[,"varCN"], tolerance=floatPrecision)

    ## character --> factor
    cf0 <- factor(plyr::mapvalues(c1, from=as.character(seq(0,1,0.1)),
                  to=c("X","X","X","X","Y","Y","Y","Y","Z","Z","Z")))
    checkEquals(cf0, df1[,"varCF"])

    ## character --> ordinal (FAIL DUE TO KEYAPPLY HANDLING OF ORDERED CLASS)
    co0 <- ordered(plyr::mapvalues(c1, from=as.character(seq(0,1,0.1)),
                   to=c("F","F","F","F","F","F","D","C","B","A","A")),
                   levels=c("F","D","C","B","A"))
    checkEquals(co0, df1[,"varCO"])

    ## character --> character
    cc0 <- plyr::mapvalues(c1, from=as.character(seq(0,1,0.1)),
                           to=letters[seq(1, 11)])
    checkEquals(cc0, df1[,"varCC"])
    
}

