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
    checkIdentical(ll0, df1[,"varLL"])  #compare code version with VarKey version

    ## logical --> integer (1)
    li0 <- as.integer(l)
    checkIdentical(li0, df1[,"varLI1"])

    ## logical --> integer (2)
    li0 <- as.integer(ifelse(l == FALSE, 0, 1))
    checkIdentical(li0, df1[,"varLI2"])

    ## logical --> numeric (1)
    ln0 <- as.numeric(l)
    checkEqualsNumeric(ln0, df1[,"varLN1"], tolerance=floatPrecision)
    
    ## logical --> numeric (2)
    ln0 <- ifelse(l == FALSE, -0.5, 0.5)
    checkEqualsNumeric(ln0, df1[,"varLN2"], tolerance=floatPrecision)

    ## logical --> factor (1)
    lf0 <- factor(l)
    checkIdentical(lf0, df1[,"varLF1"])
    
    ## logical --> factor (2)
    lf0 <- factor(ifelse(l == FALSE, "no", "yes"), levels=c("yes","no"))
    checkIdentical(lf0, df1[,"varLF2"])

    ## logical --> ordinal (1)
    lo0 <- ordered(l)
    checkIdentical(lo0, df1[,"varLO1"])
    
    ## logical --> ordinal (2)
    lo0 <- ordered(ifelse(l == FALSE, "fail", "pass"))
    checkIdentical(lo0, df1[,"varLO2"])

    ## logical --> character (1)
    lc0 <- as.character(l)
    checkIdentical(lc0, df1[,"varLC1"])

    ## logical --> character (2)
    lc0 <- as.character(ifelse(l == FALSE, "A", "B"))
    checkIdentical(lc0, df1[,"varLC2"])

    ## TEST CONVERSIONS FROM INTEGER
    i1 <- df0[,"varI1"]
    i2 <- df0[,"varI2"]
    
    ## integer --> logical (1)
    il0 <- as.logical(ifelse(i1 >= 999, NA, i1))
    checkIdentical(il0, df1[,"varIL1"])

    ## integer --> logical (2)
    il0 <- as.logical(ifelse(i2 == 1, TRUE, FALSE))
    checkIdentical(il0, df1[,"varIL2"])
    
    ## integer --> integer
    ii0 <- ifelse(i1 >= 999, NA, i1)
    checkIdentical(ii0, df1[,"varII"])

    ## integer --> numeric (1)
    in0 <- as.numeric(ifelse(i1 >= 999, NA, i1))
    checkEqualsNumeric(in0, df1[,"varIN1"], tolerance=floatPrecision)
 
    ## integer --> numeric (2)
    in0 <- as.numeric(plyr::mapvalues(i2, from=1:5, to=seq(1, 3, by=0.5)))
    checkEqualsNumeric(in0, df1[,"varIN2"], tolerance=floatPrecision)

    ## integer --> factor (1)
    if0 <- factor(i2)
    checkIdentical(if0, df1[,"varIF1"])
    
    ## integer --> factor (2)
    if0 <- factor(ifelse(i2 < 3, "nonHOV", "HOV"), levels=c("nonHOV","HOV"))
    checkIdentical(if0, df1[,"varIF2"])

    ## integer --> ordinal (1)
    io0 <- ordered(i2)
    checkIdentical(io0, df1[,"varIO1"])
    
    ## integer --> ordinal (2)
    io0 <- ordered(plyr::mapvalues(i2, from=1:5,
                   to=c("unfavorable", "slightlyUnfavorable", "neutral",
                        "slightlyFavorable", "favorable")),
                   levels=c("unfavorable", "slightlyUnfavorable", "neutral",
                            "slightlyFavorable", "favorable"))
    checkIdentical(io0, df1[,"varIO2"])

    ## integer --> character (1)
    ic0 <- as.character(ifelse(i1 >= 999, NA, i1))
    checkIdentical(ic0, df1[,"varIC1"])
    
    ## integer --> character (2)
    ic0 <- as.character(plyr::mapvalues(i2, from=1:5,
                                        to=c("a", "b", "c", "d", "f")))
    checkIdentical(ic0, df1[,"varIC2"])
    
    ## TEST CONVERSIONS FROM NUMERIC
    n1 <- df0[,"varN1"]
    n2 <- df0[,"varN2"]

    ## numeric --> logical (2)
    nl0 <- as.logical(n2)
    checkIdentical(nl0, df1[,"varNL1"])
    
    ## numeric --> logical (2)
    nl0 <- ifelse(n2 < 0.5, FALSE, TRUE)
    checkIdentical(nl0, df1[,"varNL2"])

    ## numeric --> integer (1)
    ni0 <- as.integer(n2)
    checkEquals(ni0, df1[,"varNI1"])
    
    ## numeric --> integer (2)
    ni0 <- as.integer(ifelse(n2 < 0.5, 0, 1))
    checkEquals(ni0, df1[,"varNI2"])

    ## numeric --> numeric
    nn0 <- ifelse(n1 <= -999, NA, n1)
    checkEqualsNumeric(nn0, df1[,"varNN"], tolerance=floatPrecision)

    ## numeric --> factor (1)
    nf0 <- factor(n2)
    checkIdentical(nf0, df1[,"varNF1"])
    
    ## numeric --> factor (2)
    nf0 <- factor(plyr::mapvalues(as.character(n2),
                  from=as.character(seq(0, 1, 0.1)),
                  to=c(rep("Q1", 3), rep(paste0("Q",2:5), each=2))))
    checkIdentical(nf0, df1[,"varNF2"])
    
    ## numeric --> ordinal (1)
    no1 <- ordered(n2)
    checkIdentical(no1, df1[,"varNO1"])

    ## numeric --> ordinal (2)
    no1 <- ordered(plyr::mapvalues(as.character(n2),
                   from=as.character(seq(0, 1, 0.1)),
                   to=c(rep("lower",5), rep("middle",4), rep("upper",2))))
    checkIdentical(no1, df1[,"varNO2"])
    
    ## numeric --> character (1)
    nc0 <- as.character(ifelse(n1 <= -999, NA, n1))
    checkIdentical(nc0, df1[,"varNC1"])

    ## numeric --> character (2)
    nc0 <- plyr::mapvalues(as.character(n2), from=as.character(seq(0, 1, 0.1)),
                           to=c("A","B","C","D","E","F","G","H","I","J","K"))
    checkIdentical(nc0, df1[,"varNC2"])

    ## TEST CONVERSIONS FROM FACTOR
    f1 <- df0[,"varF1"]
    f2 <- df0[,"varF2"]
    f3 <- df0[,"varF3"]
    f4 <- df0[,"varF4"]

    ## factor --> logical (1)
    fl0 <- as.logical(f1)
    checkIdentical(fl0, df1[,"varFL1"])
    
    ## factor --> logical (2)
    fl0 <- ifelse(f1 == "yes", TRUE, FALSE)
    checkIdentical(fl0, df1[,"varFL2"])

    ## factor --> integer (1)
    fi0 <- as.integer(f1)
    checkIdentical(fi0, df1[,"varFI1"])
    
    ## factor --> integer (2)
    fi0 <- as.integer(ifelse(f1 == "yes", 1, 0))
    checkIdentical(fi0, df1[,"varFI2"])

    ## factor --> numeric (1)
    fn0 <- as.numeric(f2)
    checkEqualsNumeric(fn0, df1[,"varFN1"], tolerance=floatPrecision)
    
    ## factor --> numeric (2)
    fn0 <- as.numeric(as.character(plyr::mapvalues(f2,
                                                   from=c("lo", "med", "hi"),
                                                   to=c(-0.5, 0, 0.5))))
    checkEqualsNumeric(fn0, df1[,"varFN2"], tolerance=floatPrecision)

    ## factor --> factor
    ff0 <- plyr::mapvalues(f3, from=levels(f3), to=c(rep("P", 4), "F"))
    checkEquals(ff0, df1[,"varFF"])

    ## factor --> ordinal (2)
    fo0 <- ordered(f2)
    checkIdentical(fo0, df1[,"varFO1"])

    ## factor --> ordinal (2)
    fo0 <- ordered(plyr::mapvalues(f2,
                   from=levels(f2), to=c("high", "low", "mid")),
                   levels=c("low","mid","high"))
    checkIdentical(fo0, df1[,"varFO2"])

    ## factor --> character (1)
    fc0 <- as.character(f4)
    checkIdentical(fc0, df1[,"varFC1"])

    
    ## factor --> character (2)
    fc0 <- as.character(plyr::mapvalues(f4, from=levels(f4),
                        to=c("Bobby","Cindy","Greg","Marcia","Peter")))
    checkIdentical(fc0, df1[,"varFC2"])

    ## TEST CONVERSIONS FROM ORDINAL
    o <- df0[,"varO1"]

    ## ordinal --> logical (1)
    ol0 <- as.logical(o)
    checkIdentical(ol0, df1[,"varOL1"])
    
    ## ordinal --> logical (2)
    ol0 <- ifelse(o == "1", FALSE, TRUE)
    checkIdentical(ol0, df1[,"varOL2"])

    ## ordinal --> integer (1)
    oi0 <- as.integer(o)
    checkIdentical(oi0, df1[,"varOI1"])
    
    ## ordinal --> integer (2)
    oi0 <- as.integer(as.character(o))
    checkIdentical(oi0, df1[,"varOI2"])

    ## ordinal --> numeric (1)
    on0 <- as.numeric(o)
    checkEqualsNumeric(on0, df1[,"varON1"], tolerance=floatPrecision)
    
    ## ordinal --> numeric (2)
    on0 <- as.numeric(as.character(o))
    checkEqualsNumeric(on0, df1[,"varON2"], tolerance=floatPrecision)

    ## ordinal --> factor (1)
    of0 <- factor(o, ordered=FALSE)
    checkIdentical(of0, df1[,"varOF1"])
    
    ## ordinal --> factor (2)
    of0 <- factor(plyr::mapvalues(o, c(1,3,5), c("A","B","C")), ordered=FALSE)
    checkIdentical(of0, df1[,"varOF2"])

    ## ordinal --> ordinal
    of0 <- plyr::mapvalues(o, from=c(1,3,5), to=c("low","mid","high"))
    checkIdentical(of0, df1[,"varOO"])

    ## ordinal --> character (1)
    oc0 <- as.character(o)
    checkIdentical(oc0, df1[,"varOC1"])
    
    ## ordinal --> character (2)
    oc0 <- as.character(plyr::mapvalues(o, from=c("1","3","5"),
                                        to=c("1","5","7")))
    checkIdentical(oc0, df1[,"varOC2"])

    ## TEST CONVERSIONS FROM CHARACTER
    c1 <- df0[,"varC1"]

    ## character --> logical (1)
    cl0 <- as.logical(c1)
    checkIdentical(cl0, df1[,"varCL1"])
    
    ## character --> logical (2)
    cl0 <- ifelse(c1 %in% c("0","0.1","0.2","0.3","0.4"), FALSE, TRUE)
    checkIdentical(cl0, df1[,"varCL2"])

    ## character --> integer (1)
    ci0 <- as.integer(c1)
    checkIdentical(ci0, df1[,"varCI1"])
    
    ## character --> integer (2)
    ci0 <- as.integer(ifelse(c1 %in% c("0","0.1","0.2","0.3","0.4"), 0, 1))
    checkIdentical(ci0, df1[,"varCI2"])

    ## character --> numeric (1)
    cn0 <- as.numeric(c1)
    checkEqualsNumeric(cn0, df1[,"varCN1"], tolerance=floatPrecision)
    
    ## character --> numeric (2)
    cn0 <- as.numeric(plyr::mapvalues(c1, from=seq(0, 1, 0.1),
                                      to=seq(-0.5, 0.5, 0.1)))
    checkEqualsNumeric(cn0, df1[,"varCN2"], tolerance=floatPrecision)

    ## character --> factor (1)
    cf0 <- factor(c1)
    checkIdentical(cf0, df1[,"varCF1"])
    
    ## character --> factor (2)
    cf0 <- factor(plyr::mapvalues(c1, from=as.character(seq(0,1,0.1)),
                  to=c("X","X","X","X","Y","Y","Y","Y","Z","Z","Z")))
    checkIdentical(cf0, df1[,"varCF2"])

    ## character --> ordinal (1)
    co0 <- ordered(c1)
    checkIdentical(co0, df1[,"varCO1"])
        
    ## character --> ordinal (2) 
    co0 <- ordered(plyr::mapvalues(c1, from=as.character(seq(0,1,0.1)),
                   to=c("F","F","F","F","F","F","D","C","B","A","A")),
                   levels=c("F","D","C","B","A"))
    checkIdentical(co0, df1[,"varCO2"])

    ## character --> character
    cc0 <- plyr::mapvalues(c1, from=as.character(seq(0,1,0.1)),
                           to=letters[seq(1, 11)])
    checkIdentical(cc0, df1[,"varCC"])
    
}

