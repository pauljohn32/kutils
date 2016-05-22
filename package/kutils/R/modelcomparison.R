##' Generate a "storage" data.frame
##'
##' Generating "storage" (data.frame) to store global fit indices of models,
##' while user don't have specify the arguments here, they are all set up
##' through the CFAModComptab function
##' @param est, different estimates will generate storages with different column
##' @param namesOfRow the name of each row in storage
##' @return a data.frame that can store the results (fittness info) for models
##' @author Po-Yi Chen <poyichen@@ku.edu>
storageGen <- function(est, namesOfRow){

    numOFrow <- length(namesOfRow)

    ## tell lavaan what kinds of fit indices it should extract for different estimators and establish the corresponding data.frame storage
    ## currently only allow three kinds of estimators, ML, MLR, WLSMV
    if(est == "ML"){
        fit.measures <-  c("chisq", "df", "deltaChisq", "pvalue", "deltaDf", "nestPvalue",
                           "rmsea", "cfi", "tli", "srmr", "aic", "bic")
        resultStorage <- as.data.frame(matrix(999, numOFrow, length(fit.measures)))
        colnames(resultStorage) <- fit.measures
        rownames(resultStorage) <- namesOfRow

    } else if (est == "MLR"){

        ## note: fit.measures here mean the colnames of the final outPutStorage
        fit.measures <- c("chisq.scaled", "df.scaled", "pvalue",
                          "deltaChisq", "deltaDf", "nestPvalue",
                          "cfi.scaled", "rmsea.scaled", "tli.scaled",
                          "srmr", "aic", "bic")
        resultStorage <- as.data.frame(matrix(999, numOFrow, length(fit.measures)))
        colnames(resultStorage) <- fit.measures
        rownames(resultStorage) <- namesOfRow

    } else if (est == "WLSMV"){

        fit.measures <- c("chisq.scaled", "df.scaled", "pvalue", "deltaChisq",
                          "deltaDf",  "nestPvalue", "cfi.scaled", "rmsea.scaled",
                          "tli.scaled", "srmr_mplus", "aic", "bic")
        resultStorage <- as.data.frame(matrix(999, numOFrow, length(fit.measures)))
        colnames(resultStorage) <- fit.measures
        rownames(resultStorage) <- namesOfRow

    }

    resultStorage
}

##' Checking whether the models put in nesModPairs & nonNestMod are legal or not
##'
##' Checking whether the models put in nesModPairs & nonNestMod are
##' legal or not Specifically, models must be lavaan, cfa objects; the
##' df.nested should > df.baseline; when conducting nested model
##' comparison, estimator of two models must be identical
##' @param nestModPairs list of objects representing nested models
##' @param nonNestMod list of objects to be compared as non-nested models
##' @return No return
##' @author Po-Yi Chen <poyichen@@ku.edu>
modelcheck <- function(nestModPairs, nonNestMod){

    classCheck <- function(x){
        result <- FALSE
        if(class(x)[1] != "lavaan" || x@Options$model.type != "cfa"){result <- TRUE}
        result
    }

    if(is.null(nestModPairs) != TRUE){
        result <- sapply(nestModPairs[!sapply(nestModPairs, is.na)], classCheck)
        if(sum(result) > 0){stop("some models are not lavaan CFA objects")}
    }

    if(is.null(nonNestMod) != TRUE){
        result <- sapply(nonNestMod, classCheck)
        if(sum(result) > 0){stop("some models are not lavaan CFA objects")}
    }

    if(!is.null(nestModPairs)){
        basewMod <- nestModPairs[seq(from = 1, to = length(nestModPairs),2)]
        compMod <- nestModPairs[seq(from = 2, to = length(nestModPairs),2)]

        for(i in 1:length(basewMod)){

            if(is.na(basewMod[i]) !=TRUE && is.na(compMod[i]) != TRUE){

                if(inspect(compMod[[i]],"fit")["df"] >=  inspect(basewMod[[i]],"fit")["df"]){stop("some models are not nested")}
                if(basewMod[[i]]@Options$estimator != compMod[[i]]@Options$estimator){stop("nested models must be estimated through the same estimator")}
            }
        }
    }
    NULL
}


##' Add stars
##'
##' Adding stars to the chisq and deltal chisq statistic according to their level of significance.
##' @param store the storage of fit indices of all models fit indices
##'     inside
##' @param globStar if globStar = TRUE, three stars will be added
##'     aside to the global chisq if p < 0.001, two stars for p < 0.01,
##'     one star for p < 0.05
##' @param nestStar if nestStar = TRUE, three stars will be added
##'     aside to the delta chisq if p < 0.001, two stars for p < 0.01, one
##'     star for p < 0.05
##' @return storage data frame
##' @author Po-Yi Chen
pValRePresent <- function(store, globStar = TRUE, nestStar = TRUE){
    for (i in 1:length(store[,"pvalue"])){
        if(is.numeric(store[i,"pvalue"])){
            if(store[i,"pvalue"] < 0.001 && globStar == TRUE){

                store[i,"chisq"] <- paste0(as.character(store[i,"chisq"]), "***")

            }else if(store[i,"pvalue"] < 0.01 && globStar == TRUE){

                store[i,"chisq"] <- paste0(as.character(store[i,"chisq"]), "**")

            }else if (store[i,"pvalue"] < 0.05 && globStar == TRUE){

                store[i,"chisq"] <- paste0(as.character(store[i,"chisq"]), "*")
            }
        }
    }

    for (i in 1:length(store[,"nestPvalue"])){

        if(store[i,"nestPvalue"] != "-"){

            if(as.numeric(store[i,"nestPvalue"]) < 0.001 &&  nestStar == TRUE){

                store[i,"deltaChisq"] <- paste0(as.character(store[i,"deltaChisq"]), "***")

            }else if(as.numeric(store[i,"nestPvalue"]) < 0.01 &&  nestStar == TRUE){

                store[i,"deltaChisq"] <- paste0(as.character(store[i,"deltaChisq"]), "**")

            }else if (as.numeric(store[i,"nestPvalue"]) < 0.05 &&  nestStar == TRUE){

                store[i,"deltaChisq"] <- paste0(as.character(store[i,"deltaChisq"]), "*")
            }
        }
    }
    store
}






##' CFA Model table
##'
##' comparing the nested and non-nested model. the lavaan object be
##' listed in the arugment :nestModPairs" will be compared as a pair.
##' e.g., if nestModPairs = c(mod1, mod2, mod3, mod4, mod5, NA),  mod1 will
##' be nestly compared to mod2 (df mod1 < df mod2); mod3 will be nestly compared to mod3; mod5
##' will not be nestly compared to any models. On the other hand, as for the lavaan objects
##' be listed in the argument "nonNestMod", their fit indices will directly be
##' presented in rows above the results of nested model comparison
##'
##' @param nestModPairs a list contains lavaan objects that will benestedly compared.
##' @param nestRowName a vector of the names of the row
##' @param nonNestMod a vector contains lavaan CFA objects that will
##'     be nonNestly comapred
##' @param nonNestRowName row name of the non-nested model in the final output table
##' @param est so far only allow MLR, WLSMVor ML
##' @param fitDrop the fit indices you want to drop, the default only
##' @param footNote contents that you want to put as the footnote of the table
##' @param tabTitle tile of the name
##' @param texFilename the name of the out .tex file
##' @import lavaan
##' @export
##' @return a .tex file that can be open with lyx and gnerate the table
##' @author Po-Yi Chen <poyichen@@ku.edu>
CFAModComptab <-
    function(nestModPairs = NULL, nestRowName = NULL,
             nonNestMod = NULL, nonNestRowName = NULL, est = "ML",
             fitDrop = c("pvalue", "nestPvalue"), footNote = "",
             tabTitle, texFilename)
{

    suppressWarnings(modelcheck(nestModPairs, nonNestMod))

    if(est == "ML"){

        fitMeasureGrep <-  c("chisq","df",  "pvalue", "rmsea", "cfi", "tli", "srmr", "aic", "bic")
        fitMeasureStore <- c("chisq", "df", "pvalue", "rmsea", "cfi", "tli", "srmr", "aic", "bic")

    } else if(est == "MLR"){

        fitMeasureGrep <-  c("chisq.scaled", "df.scaled", "pvalue","cfi.scaled",
                             "rmsea.scaled", "tli.scaled", "srmr", "aic", "bic")
        fitMeasureStore <- c("chisq.scaled", "df.scaled", "pvalue","cfi.scaled",
                             "rmsea.scaled", "tli.scaled", "srmr", "aic", "bic")

    } else if(est == "WLSMV"){

        fitMeasureGrep <-  c("chisq.scaled", "df.scaled", "pvalue", "cfi.scaled",
                             "rmsea.scaled", "tli.scaled", "srmr_mplus")
        fitMeasureStore <- c("chisq.scaled", "df.scaled", "pvalue", "cfi.scaled",
                             "rmsea.scaled", "tli.scaled", "srmr_mplus")

    } else {stop("please set est = ML, MLR, or WLSMV)")}

    if(is.null(nestModPairs) && is.null(nonNestMod)){
        stop(paste("please put at least one",
                   "lavaan object in arguments"))
    }
    if(length(nestModPairs) %% 2 != 0){stop("the length of nestModPairs has to be a even number")}
    if(length(nestModPairs) != 2*length(nestRowName)) {stop("each pair of nested model needs to have its own name")}
    if(length(nonNestMod) != length(nonNestRowName))  {stop("each non- nested model needs to have its own name")}

    if(sum(fitDrop %in% c("chisq", "df", "deltaChisq", "pvalue", "deltaDf",
                          "nestPvalue", "rmsea", "cfi", "tli", "srmr", "aic", "bic") == FALSE) != 0){
        stop(paste0("fit indices you can drop are chisq,df,deltaChisq, pvalue, deltaDf, nestPvalue,",
                    "rmsea, cfi, tli, srmr, aic,or bic"))
    }

    if(!is.null(nestModPairs)){
        NestResStor <- storageGen(est, nestRowName)
    } else {NestResStor <- NULL}

    if(!is.null(nonNestMod)){
        nonNestResStor <- storageGen(est, nonNestRowName)
    } else {nonNestResStor <- NULL}

    if(!is.null(NestResStor)){

        basewMod <- nestModPairs[seq(from = 1, to = length(nestModPairs),2)]
        compMod <- nestModPairs[seq(from = 2, to = length(nestModPairs),2)]

        for (i in 1:length(basewMod)){

            NestResStor[i,fitMeasureStore] <- inspect(basewMod[[i]],'fit')[fitMeasureGrep]

            if (is.na(compMod[i])){

                NestResStor[,"deltaDf"][i] <- "-"
                NestResStor[,"deltaChisq"][i] <- "-"
                NestResStor[,"nestPvalue"][i] <- "-"

            } else if (est == "ML"){

                NestResStor[,"deltaDf"][i] <- -(inspect(compMod[[i]], 'fit')["df"] - inspect(basewMod[[i]], 'fit')["df"])
                NestResStor[,"deltaChisq"][i] <- -(round(inspect(compMod[[i]], 'fit')["chisq"] - inspect(basewMod[[i]], 'fit')["chisq"], 3))
                NestResStor[, "nestPvalue"][i] <- anova(compMod[[i]], basewMod[[i]])$Pr[2]

            } else if (est == "MLR" || est == "WLSMV"){

                NestResStor[,"deltaDf"][i] <- -(inspect(compMod[[i]], 'fit')["df.scaled"] - inspect(basewMod[[i]], 'fit')["df.scaled"])
                NestResStor[,"deltaChisq"][i] <- -(round(inspect(compMod[[i]], 'fit')["chisq.scaled"] - inspect(basewMod[[i]], 'fit')["chisq.scaled"], 3))
                NestResStor[,"nestPvalue"][i] <- anova(compMod[[i]], basewMod[[i]])$Pr[2]
            }
        }
    }

    if (!is.null(nonNestResStor)){

        for (i in 1:length(nonNestMod)){

            nonNestResStor[i,fitMeasureStore] <- round(inspect(nonNestMod[[i]],'fit')[fitMeasureGrep],2)
            nonNestResStor[,"deltaDf"][i] <- "-"
            nonNestResStor[,"deltaChisq"][i] <- "-"
            nonNestResStor[,"nestPvalue"][i] <- "-"
        }
    }

    if(is.null(nonNestResStor) != TRUE && is.null(NestResStor) != TRUE){

        resultStorage <- rbind(nonNestResStor, NestResStor)

    } else if(is.null(nonNestResStor) && is.null(NestResStor) != TRUE){

        resultStorage <-  NestResStor

    } else if(is.null(nonNestResStor) != TRUE && is.null(NestResStor)){

       resultStorage <-  nonNestResStor

    }

    if (est == "MLR" || est == "WLSMV"){colnames(resultStorage) <- gsub(".scaled", "", colnames(resultStorage))}
    resultStorage[,"chisq"] <- round(resultStorage[,"chisq"], 2)
    resultStorage <- pValRePresent(resultStorage)

    colnames(resultStorage)[colnames(resultStorage) == "srmr_mplus"] <- "wrmr"

    if (is.null(fitDrop) != TRUE){
        for (i in fitDrop){resultStorage <-  resultStorage[colnames(resultStorage) != i]}
    }

    if(est == "WLSMV"){resultStorage <-  resultStorage[colnames(resultStorage) != "aic"]}
    if(est == "WLSMV"){resultStorage <-  resultStorage[colnames(resultStorage) != "bic"]}

    comment <- list()
    comment$pos <- list()
    comment$pos[[1]] <- c(nrow(resultStorage))
    comment$command  <- c(paste("\\hline \n", footNote, sep = ""))


    mystring <- print(xtable(resultStorage, caption = tabTitle), add.to.row = comment, hline.after = c(-1, 0),
                      caption.placement = 'top')
    mystring <- gsub("chisq", "$\\\\chi^{2}$", mystring)
    mystring <- gsub("deltaChisq","$\\\\chi_{diff}^{2}$", mystring)
    mystring <- gsub("deltaDf", "$\\\\Delta df$", mystring)
    mystring <- gsub("ptarget", "\\\\textit{p}", mystring)
    mystring <- gsub("pvalue", "\\\\textit{p}-value", mystring)

    mystring2 <- paste0(texFilename, ".tex")
    write(file = mystring2, print(mystring))

    if(est == "WLSMV"){
        print("WARNING!!!!!")

        print(paste0("The pvalues of nested model comparisons are obtained from anova(lavaanObje1, lavaanObje2),",
                     "rather than the widely-used DIFFTEST function in Mplus"))
    }
}


###################
### testing area ##
###################
## library(lavaan)
## library(xtable)


## dat <- read.table("data.txt")
## dat <- dat[,-c(8)]
## colnames(dat) <- c("group", "v1", "v2", "v3", "v4", "v5", "v6")
## dat[dat == 999] <- 2


## congModel <- '
##               f1 =~ 1*v1 + v2 + v3 + v4 + v5 + v6
## 			  f1 ~~ f1
## 			  f1 ~0*1
## 			 '

## weakModel <- '
##               f1 =~ 1*v1 + c(L2,L2)*v2 + c(L3,L3)*v3 + c(L4,L4)*v4 + c(L5,L5)*v5 + c(L6,L6)*v6
## 			  f1 ~~ f1
## 			  f1 ~0*1
## 			'

## partialweakModel <- '
##               f1 =~ 1*v1 + c(L2,L2)*v2 + c(L3,L3)*v3 + c(L4,L4)*v4 + c(L5,L5)*v5 + v6
## 			  f1 ~~ f1
## 			  f1 ~0*1
## 			'
## partialweakModel2 <- '
##               f1 =~ 1*v1 + c(L2,L2)*v2 + c(L3,L3)*v3 + c(L4,L4)*v4 + v5 + v6
## 			  f1 ~~ f1
## 			  f1 ~0*1
## 			'
## strongModel <- '
##               f1 =~ 1*v1 + c(L2,L2)*v2 + c(L3,L3)*v3 + c(L4,L4)*v4 + c(L5,L5)*v5 + c(L6,L6)*v6
## 			  f1 ~~ f1
## 			  f1 ~ c(0,NA)*1
## 			  v1 ~ c(I1,I1)*1
## 			  v2 ~ c(I2,I2)*1
## 			  v3 ~ c(I3,I3)*1
## 			  v4 ~ c(I4,I4)*1
## 			  v5 ~ c(I5,I5)*1
## 			  v6 ~ c(I6,I6)*1
## 			'

## partialstrongModel1 <- '
##               f1 =~ 1*v1 + c(L2,L2)*v2 + c(L3,L3)*v3 + c(L4,L4)*v4 + c(L5,L5)*v5 + v6
## 			  f1 ~~ f1
## 			  f1 ~ c(0,NA)*1
## 			  v1 ~ c(I1,I1)*1
## 			  v2 ~ c(I2,I2)*1
## 			  v3 ~ c(I3,I3)*1
## 			  v4 ~ c(I4,I4)*1
## 			  v5 ~ c(I5,I5)*1
## 			  v6 ~ c(I6,I6)*1
## 			'

## partialstrictModel2 <- '
##               f1 =~ 1*v1 + c(L2,L2)*v2 + c(L3,L3)*v3 + c(L4,L4)*v4 + v5 + v6
## 			  f1 ~~ c(1,NA)*f1
## 			  f1 ~ c(0,NA)*1

## 			  v1 ~ c(I1,I1)*1
## 			  v2 ~ c(I2,I2)*1
## 			  v3 ~ c(I3,I3)*1
## 			  v4 ~ c(I4,I4)*1
## 			  v5 ~ c(I5,I5)*1
## 			  v6 ~ c(I6,I6)*1

## 			  v1 ~~ c(R1,R1)*v1
## 			  v2 ~~ c(R2,R2)*v2
## 			  v3 ~~ c(R3,R3)*v3
## 			  v4 ~~ c(R4,R4)*v4
## 			  v5 ~~ c(R5,R5)*v5
## 			  v6 ~~ c(R6,R6)*v6
## '

## cc1 <- cfa(congModel, data=dat, group="group", meanstructure=TRUE, estimator = "MLR")
## cc2 <- cfa(weakModel, data=dat, group="group", meanstructure=TRUE, estimator = "MLR")
## cc21 <- cfa(partialweakModel, data=dat, group="group", meanstructure=TRUE, estimator = "MLR")
## ## cc22 <- cfa(partialweakModel2, data=dat, group="group", meanstructure=TRUE, estimator = "ML")
## cc3 <- cfa(partialstrongModel1, data=dat, group="group", meanstructure=TRUE, estimator = "MLR")

## ## cc32 <- cfa(partialstrongModel2, data=dat, group="group", meanstructure=TRUE, estimator = "MLR")
## ## cc42 <- cfa(partialstrictModel2, data=dat, group="group", meanstructure=TRUE, estimator = "MLR")

## datFemale <- dat[dat$group == 0,]
## datFemale <- datFemale[colnames(datFemale) != "group"]

## ccFemale <- cfa(congModel, data = datFemale , meanstructure=TRUE, estimator = "MLR")

## datMale <- dat[dat$group == 1,]
## datMale <- datMale[colnames(datMale) != "group"]

## ccMale <- cfa(congModel, data = datMale , meanstructure=TRUE, estimator = "MLR")

## ## cc4 <- cfa(strictModel, data=dat, group="group", meanstructure=TRUE, estimator = "WLSMV")


## ## inspect(cc1,'fit')[fit.measures]

## ### end of testing area
## ## CFAModComptex(modList1 = c(ccFemale, ccMale, cc1, cc2, cc21, cc3), modList2 = c(NA, NA, NA, cc1, cc1, cc21), est = "WLSMV",
## ##               namesOfRow = c("male", "female", "configural", "metric", "patial metric1", "partial metric2"),
## ##               texFilename = "WLSMV", mgNum = 2)

## CFAModComptab(nestModPairs = c(cc1, NA, cc2, cc1, cc21, cc1, cc3, cc21), nestRowName = c("configural", "weak", "partial weak", "partial strong"), nonNestMod = c(ccFemale, ccMale),
##                nonNestRowName = c("female", "male"), est = "MLR", fitDrop = c("pvalue", "nestPvalue", "bic"), footNote = "Note: hello!", tabTitle = "model comparison", texFilename = "table03")

## CFAModComptab(nonNestMod = c(ccFemale, ccMale, cc1, cc2, cc3),
##                nonNestRowName = c("female", "male", "congf", "metric", "scaler"),est = "MLR", fitDrop = c("pvalue", "nestPvalue", "bic"),
##               footNote = "", tabTitle = "model comparison", texFilename = "table03")
## undebug(CFAModComptab)


