##' Creates Structural Equation Modeling Tables
##'
##' Creates LaTeX markup for structural equation modeling output
##' tables in the style of the American Psychological
##' Association(APA). Input objects should be created by the
##' "\code{lavaan}" package.
##'
##' The argument paramSets determines the inclusion of estimate sections.
##' \itemize{
##' \item "loadings" are the factor loadings in the model.
##' \item "slopes" are the regression slopes in the model.
##' \item "intercepts" are
##' the observed variable intercepts.
##' \item "means" are the observed
##' variable means.
##' \item "residuals" are the observed
##' variable residual variances.
##' \item "covariances" are the observed
##' variable covariances.
##' \item "latentvariances" are the latent
##' variable variances.
##' \item "latentcovariances" are the latent covariances 
##' \item "latentmeans" are the latent variable
##' means (or intercepts).
##' \item "thresholds" arise in latent
##' response variates (non-numeric indicator data).
##' }
##'
##' The standardized parameter regulates the number of columns to be
##' included. If standardized=TRUE, columns will be inserted for
##' the standardized parameter estimate and its standard error.
##'
##' The default columns are:
##' \enumerate{
##' \item the parameter estimates,
##' \item the standard errors,
##' \item standardized parameter estimates, and
##' \item standardized standard errors.
##' }
##'
##' The colNames parameter is used to specify different columns,
##' or to alter the displayed labels for them.
##'
##' @param object A lavaan object returned by cfa() or sem(), or a
##'     named list of lavaan objects.
##' @param colNames Column names for main table. This is used to both
##'     *select* which columns are include and *how* their labels will
##'     appear in output. The allowed names are "est", "se", "z", "p"
##'     or "est(se)". The default is for a 4 column table, \code{c(est
##'     = "Estimate", se = "SE", z = "z", p = "p")}. User can instead
##'     request any columns, such as c(est = "Estimate", se = "SE") or
##'     c("est(se)" = "Estimates(Std.Err.)". If one vector of columns
##'     is supplied, it is used for all model objects. Different
##'     columns for different models can be requested by
##'     supplying a list of named vectors. Names must be same names
##'     supplied in the \code{object} list of models.
##' @param paramSets Parameter sets to be included. Valid values are
##'     \code{"all"} or any of the following: \code{c("loadings",
##'     "slopes", "intercepts", "residuals", "covariances",
##'     "latentmeans", "latentvariances", "latentcovariances",
##'     "thresholds"}. Default is "all", which means that any of these
##'     parameters present in the fitted model will be included in the
##'     output. If \code{object} is a list of fitted models, and
##'     paramSets is "all" or a vector, then same parameter sets are
##'     reported for all models. Different parameter sets can be
##'     requested by supplying a list of named vectors. Names must be
##'     same names supplied in the \code{object} list of models.
##' @param fit Summary indicators to be included. Can be "chi-square",
##'     a specially formatted element, or fit indices provided by
##'     \code{lavaan::fitMeasures(object)}. Currently, they are
##'     "npar", "fmin", "chisq", "df", "pvalue", "baseline.chisq",
##'     "baseline.df", "baseline.pvalue", "cfi", "tli", "nnfi", "rfi",
##'     "nfi", "pnfi", "ifi", "rni", "logl", "unrestricted.logl",
##'     "aic", "bic", "ntotal", "bic2", "rmsea", "rmsea.ci.lower",
##'     "rmsea.ci.upper", "rmsea.pvalue", "rmr", "rmr_nomean", "srmr",
##'     "srmr_bentler", "srmr_bentler_nomean", "srmr_bollen",
##'     "srmr_bollen_nomean", "srmr_mplus", "srmr_mplus_nomean",
##'     "cn_05", "cn_01", "gfi", "agfi", "pgfi", "mfi", "ecvi".
##' @param names_fit Names for the fit measures requested by the fit
##'     parameter.  Must have same number of elements as fit.  For
##'     example, fit = c("cfi.scaled", "tli.scaled"), names_fit =
##'     c("CFI", "TLI").
##' @param names_upper Should the names of the model fit parameters be
##'     forced to be uppercase.  The default is TRUE.  This will also
##'     affect whatever is specified in names_fit.
##' @param type Choose "latex", "html", "csv", or any combination. If
##'     user supplies c("latex", "html", "csv"), all 3 sets of markup
##'     will be returned.
##' @param includeGroup Should group membership be reported for
##'     parameters in a column on the right? Defaults to FALSE.
##' @param group Group for which parameters should be reported. Only
##'     relevant for multiple group models. If not supplied,
##'     parameters for all groups will be reported, along with a
##'     column indicating the group (if includeGroup = TRUE).
##' @param file Base name for output file.
##' @param longtable If TRUE, use longtable for LaTeX
##'     documents. Default is FALSE.
##' @importFrom stats pnorm
##' @return Markup for SEM table, or a list of markup character
##'     strings, one for each value of \code{type}.
##' @export
##' @author Ben Kite <bakite@@ku.edu> Paul Johnson <pauljohn@@ku.edu>
##' @examples \donttest{
##' ## These run longer than 5 seconds
##' ## CFA model
##' require(lavaan)
##' ## The example from lavaan's docs
##' HS.model <- ' visual  =~ x1 + x2 + x3
##'               textual =~ x4 + x5 + x6
##'               speed   =~ x7 + x8 + x9'
##' fit1 <- cfa(HS.model, data = HolzingerSwineford1939, std.lv = TRUE)
##' fit1.t1 <- semTable(fit1, fit = c("chi-square", "rmsea"))
##' fit1.t1 <- semTable(fit1, fit = c("chi-square", "rmsea"),
##'            colNames = c("est" = "Est",  se = "Std. Err."))
##' fit1.t1 <- semTable(fit1, fit = c("chi-square", "rmsea"),
##'            colNames = c("estse" = "Estimate(Std.Err.)"))
##'
##' ## Fit same model with standardization
##' fit1.std <- update(fit1, std.lv = TRUE, std.ov = TRUE) 
##' ## include 2 models in table request
##' semTable(list("Ordinary" = fit1, "Standardized" = fit1.std))
##'
##' semTable(list("Ordinary" = fit1, "Standardized" = fit1.std),
##'     colNames = list("Ordinary" = c("est", "se"), "Standardized" = c("est")))
##' 
##' cat(fit1.t1)
##' fit1.t2 <- semTable(fit1, fit = c("chisq", "rmsea"), standardized = TRUE)
##' cat(fit1.t2)
##' fit1.t3 <- semTable(fit1, fit = c("chisq", "rmsea", "tli"),
##'                     colNames = c("est" = "Estimates", "se" = "Std.Err."),
##'                     standardized = TRUE)
##' cat(fit1.t3)
##' 
##' ## Can create file if desired
##' ## cat(fit1.t2, file = "table1.t2.tex")
##' ## Basic SEM
##' regmodel <- 'visual  =~ x1 + x2 + x3
##'              textual =~ x4 + x5 + x6
##'              speed   =~ x7 + x8 + x9
##'              visual ~ textual + speed
##' '
##'
##' fit2 <- sem(regmodel, data = HolzingerSwineford1939, std.lv = TRUE)
##' fit2.t <- semTable(fit2, fit = "rmsea", type = "html")
##' cat(fit2.t)
##' #### Example with file output
##' ##semTable(output1, file = "exampleTable", fit = "rmsea",
##' ##         standardized = TRUE, paramSets = c("loadings", "latentvariances"),
##' ##         type = "html")
##' fit3 <- sem(regmodel, data = HolzingerSwineford1939, group = "school")
##' fit3.t1 <- semTable(fit3)
##' cat(fit3.t1)
##' fit3.t2 <- semTable(fit3, colNames = c("est" = "Est (MLE)", "se" = "Std.Err."))
##' cat(fit3.t2)
##' fit3.t2 <- semTable(fit3, fit = c("chisq", "rmsea", "cli"))
##' cat(fit3.t2)
##' 
##' model <- "factor =~ .7*y1 + .7*y2 + .7*y3 + .7*y4
##'                  y1 | -1*t1 + 1*t2
##'                  y2 | -.5*t1 + 1*t2
##'                  y3 | -.2*t1 + 1*t2
##'                  y4 | -1*t1 + 1*t2"
##' dat <- simulateData(model, sample.nobs = 300)
##' testmodel <- "ExampleFactor =~ y1 + y2 + y3 + y4"
##' fit4 <- cfa(testmodel, data = dat, ordered = colnames(dat),
##'     std.lv = FALSE)
##' fit4.t1 <- semTable(fit4, paramSets = c("loadings", "thresholds", "residuals"),
##'     fit = c("tli", "chi-square"),
##'     names_fit = c("TLI", "chi-square"), type = "html")
##' fit4.t2 <- semTable(fit4, fit = c("rmsea", "tli", "chi-square"),
##'     names_fit = c("RMSEA", "TLI", "chi-square"), type = "latex")
##' 
##' 
##' ## Example with file output requested in the command
##' ## semTable(output, file = "catTable.tex",
##' ##    paramSets = c("loadings", "thresholds", "residuals"),
##' ##    fit = c("tli", "chi-square"),
##' ##    names_fit = c("TLI", "chi-square"), type = "latex")
##' }

semTable <-
    function(object, file = NULL, paramSets = "all",
             fit = c("chi-square", "cfi", "tli", "rmsea"),
             names_fit = fit,
             colNames = c("est" = "Estimates", "se" = "SE", "z" = "z", "p" = "p"),
             standardized = FALSE, 
             names_upper = TRUE, type = "latex",
             includeGroup = FALSE,
             group = NULL, longtable = FALSE)
{

    ## do.call(rbind, alist) unexpectedly converts characters to factors.
    ## it does not accept stringsAsFactors=FALSE,
    ## So set globally to avoid hassle
    options.orig <- options()
    options(stringsAsFactors = FALSE)
    on.exit(options(options.orig))
    
    ## local shorthand to replace the verbose
    ## formatC(round(chimeas$stat, 3), format = 'f', digits = 2)
    ## with frnd(chimeas$stat)
    ## For reasons I don't understand, most prevalent example here
    ## is round to 3, then show 2 digits
    frnd <- function(x, rnd = 3, digits = 2) {
        formatC(round(x, rnd), format = 'f', digits = digits)
    }

    ## 20171021
    ## The Maker functions all had copies of a standard stanza
    ## that used formatC and round in this way. Aggregate those
    ## actions
    ## TODO: safety check for operations on columns that don't exist
    ## trows must be a row subset of the parameters table
    roundSubtable <- function(trows){
        colNames <- intersect(c("est", "se", "stdest", "stdse", "z"), colnames(trows))
        for (i in colNames) trows[ , i] <- frnd(trows[ , i])
        trows$p <- frnd(trows$p, 3, 3)
        trows$p <- gsub("0\\.", "\\.", trows$p)
        trows$est <- ifelse(trows$free == 0, paste0(trows$est, "_FIXED_"), trows$est)
        trows[trows$free == 0, intersect(colnames(trows), c("se", "z", "p", "stdse"))] <- ""
        trows
    }


    ## If paramSets != "all", follow user request to select paramSets for table
    ## If paramSets == "all", then
    ## 1. remove "variances" and insert "residuals" "covariances" "latentvariances"
    ## 2. remove "means" and insert "intercepts" and "latentmeans"
    cleanParamSets <- function(paramSets, parameters) {
        if (paramSets != "all") {
            return(unique(paramSets))
        }
        
        variables <- attr(parameters, "variables")
        latents <- attr(parameters, "latents")
        
        paramops <- c("=~" = "loadings", "~" = "slopes", "~1" = "means",
                      "~~" = "variances", "|" = "thresholds")
        params <- paramops[unique(parameters$op)]
        names(params) <- NULL
        if ("variances" %in% params){
            params <- setdiff(params, "variances")
            if (any(parameters$lhs %in% variables &
                    parameters$rhs %in% variables &
                    parameters$lhs == parameters$rhs &
                    parameters$op == "~~")) {
                params <- c(params, "residuals")
            }
            if (any(parameters$lhs %in% variables &
                             parameters$rhs %in% variables &
                             parameters$lhs != parameters$rhs &
                             parameters$op == "~~")) {
                params <- c(params, "covariances")
            }
            if (any(parameters$rhs %in% latents &
                    parameters$lhs %in% latents &
                    parameters$lhs == parameters$rhs &
                    parameters$op == "~~")) {
                params <- c(params, "latentvariances")
            }
            if (any(parameters$rhs %in% latents &
                             parameters$lhs %in% latents &
                             parameters$lhs != parameters$rhs &
                             parameters$op == "~~")) {
                params <- c(params, "latentcovariances")
            }
        
        }
        if ("means" %in% params){
            params <- setdiff(params, "means")
            if(any(parameters$lhs %in% variables &
                            parameters$op == "~1")){
                params <- c(params, "intercepts")
            }
            if(any(parameters$lhs %in% latents &
                            parameters$op == "~1")){
                params <- c(params, "latentmeans")
            }
        }
        params
    }

    
    getParamTable <- function(object){
        
        createEstSE <- function(dframe){
            dframe <- roundSubtable(dframe)
            ifelse(dframe[ , "free"] != 0,
                   paste0(dframe[ , "est"], "(", dframe[ , "se"], ")", dframe[ , "starsig"]),
                   paste0(dframe[ , "est"], "_FIXED_"))
        }

        if(class(object)[1] != "lavaan"){
            stop("object is not a lavaan output object.")
        }

        parList <- object@ParTable[
                              intersect(names(object@ParTable),
                                        c("lhs", "op", "rhs", "free", "group", "est", "se"))]
        parameters <- as.data.frame(parList, stringsAsFactors=FALSE)
        parameters$z <- ifelse(parameters$free != 0 & parameters$se != 0, parameters$est/parameters$se, NA)
        parameters$p <- 2*pnorm(abs(parameters$z), lower.tail = FALSE)
        parameters$starsig <- starsig(parameters$p)
        parameters$estse <- createEstSE(parameters)

        ## items previously global are specialized to this model
        attr(parameters, "variables") <- unique(unlist(object@Data@ov.names))
        attr(parameters, "latents") <- unique(unlist(object@pta$vnames$lv))
        attr(parameters, "params") <- cleanParamSets(paramSets, parameters)

        parameters <- roundSubtable(parameters)
        parameters
    }
    
  
    loadingMaker <- function(lvname,  parameters, report = c("est", "se", "z", "p")){
        variables <- attr(parameters, "variables")
        trows <- parameters[which(parameters$rhs %in% variables &
                                  parameters$lhs %in% lvname &
                                  parameters$op == "=~"), ,
                            drop = FALSE]
        rownames(trows) <- paste("loadings", lvname, trows[ , "rhs"], sep = ".")
        trows <- data.frame(col1 = trows$rhs, trows[ , report, drop = FALSE])
        ## don't put "_BOC_" at beginning if in colnum 1
        title <- list(title = lvname,
                      markup = "_UL__CONTENT__EOUL__EOC_",
                      colnum = 1)
        attr(trows, "title") <- title
        class(trows) <- c("trows", class(trows))
        trows
    }

    interceptMaker <- function(variables, parameters, report = c("est", "se", "z", "p")){
        ints <- parameters[which(parameters$op == "~1" & parameters$lhs %in% variables), "lhs"]
        trows <- parameters[which(parameters$lhs %in% ints & parameters$op == "~1"),, drop = FALSE]
        if(dim(trows)[1] == 0)
            stop("Intercept estimates are requested in the table, but I can't find them in the output!")
        rownames(trows) <- paste("intercepts.", trows[ , "lhs"])
        trows <- data.frame(col1 = trows$lhs, trows[, report, drop = FALSE])
        title <- list(title = "Intercepts",
                      markup = paste0("_BOMC", length(report), "__UL__CONTENT__EOUL__EOMC_"),
                      colnum = 2)
        attr(trows, "title") <- title
        class(trows) <- c("trows", class(trows))
        trows
    }

    observedMeanMaker <- function(variables, parameters, regs, report = c("est", "se", "z", "p")){
        ivs <- unique(regs$rhs)
        trows <- parameters[which(parameters$lhs %in% ivs & parameters$op == "~1"),, drop = FALSE]
        if(dim(trows)[1] == 0)
            stop("Predictor variable mean estimates missing in output!")
        rownames(trows) <- paste("means.", trows[ , "lhs"])
        trows <- data.frame(col1 = trows$lhs, trows[, report, drop = FALSE])
        title <- list(title = "Means",
                      markup = paste0("_BOMC", length(report), "__UL__CONTENT__EOUL__EOMC_"),
                      colnum = 2)
        attr(trows, "title") <- title
        class(trows) <- c("trows", class(trows))
        trows
    }

    slopeMaker <- function(dvname, parameters, regs, report = c("est", "se", "z", "p")){
        trows <- regs[which(regs$lhs == dvname), , drop = FALSE]
        rownames(trows) <- paste("slopes", dvname, trows[ , "rhs"], sep = ".")
        trows <- data.frame(col1 = trows$rhs, trows[ , report, drop = FALSE])
        title <- list(title = dvname,
                      markup = "_UL__CONTENT__EOUL__EOC_",
                      colnum = 1)
        attr(trows, "title") <- title
        class(trows) <- c("trows", class(trows))
        trows
    }

    thresholdMaker <- function(variables, parameters, report = c("est", "se", "z", "p")){
        trows <- parameters[which(parameters$op == "|" & parameters$lhs %in% variables),,
                            drop = FALSE]
        if(dim(trows)[1] == 0)
            stop("Threshold estimates are missing in output!")
        thresnum <- substring(trows$rhs, 2, nchar(trows$rhs))
        trows$lhs <- paste0(trows$lhs, "(", thresnum, ")")
        rownames(trows) <- paste("thresholds", trows[ , "lhs"], thresnum, sep = ".")
        trows <- data.frame(col1 = trows$lhs, trows[ , report, drop = FALSE])
        title <- list(title = "Thresholds",
                      markup = paste0("_BOMC", length(report), "__UL__CONTENT__EOUL__EOMC_"),
                      colnum = 2)
        attr(trows, "title") <- title
        class(trows) <- c("trows", class(trows))
        trows
    }
    

    residualMaker <- function(variables, parameters, covariance = FALSE,
                              report = c("est", "se", "z", "p")){
        trows <- parameters[which(parameters$rhs %in% variables &
                                  parameters$lhs %in% variables &
                                  parameters$op == "~~"),,
                            drop = FALSE ]
        if(dim(trows)[1] == 0) stop("residualMaker failure")
        if (isTRUE(covariance)){
            trows <- trows[which(trows$rhs != trows$lhs), ]
            rownames(trows) <- paste("covariances", trows[ , "lhs"], sep = ".")
            trows <- data.frame(col1 = trows$lhs, trows[ , report, drop = FALSE])
            title <- list(title = "Covariances",
                          markup = paste0("_BOMC", length(report), "__UL__CONTENT__EOUL__EOMC_"),
                          colnum = 2)
            attr(trows, "title") <- title
            class(trows) <- c("trows", class(trows))
            return(trows)
        } else {
            trows <- trows[which(trows$rhs == trows$lhs),, drop = FALSE]
            rownames(trows) <- paste("variances", trows[ , "lhs"], sep = ".")
            trows <- data.frame(col1 = trows$lhs, trows[ , report, drop = FALSE])
            title <- list(title = "Variances",
                          markup = paste0("_BOMC", length(report), "__UL__CONTENT__EOUL__EOMC_"),
                          colnum = 2)
            attr(trows, "title") <- title
            class(trows) <- c("trows", class(trows))
            return(trows)
        }
    }

    latentMaker <- function(latents, parameters, report = c("est", "se", "z", "p")){
        trows <- parameters[which(parameters$rhs %in% latents &
                                  parameters$lhs %in% latents &
                                  parameters$op == "~~"),,
                            drop = FALSE]
        if(dim(trows)[1] == 0){
            stop("Latent variance/covariance estimates missing in output!")
        }
        rownames(trows) <- paste0("latentvariances", ".", trows[ , "lhs"],
                                 ".", trows[ , "rhs"])
        trows[ , "lhs"] <- paste0(trows[ , "lhs"], " w/", trows[ , "rhs"])
        trows <- data.frame(col1 = trows$lhs, trows[ , report, drop = FALSE])
        title <- list(title = "Latent Var/Covariances",
                      markup = paste0("_BOMC", length(report), "__UL__CONTENT__EOUL__EOMC_"),
                      colnum = 2)
        attr(trows, "title") <- title
        class(trows) <- c("trows", class(trows))
        trows
     }

    latentMeanMaker <- function(latents, parameters, report = c("est", "se", "z", "p")){
        trows <- parameters[which(parameters$lhs %in% latents & parameters$op == "~1"),,
                            drop = FALSE]
        if(dim(trows)[1] == 0)
            stop("Latent mean estimates missing in output!")
        rownames(trows) <- paste("latentmeans", trows[ , "lhs"], sep = ".")
        trows <- data.frame(col1 = trows$lhs, trows[ , report, drop = FALSE])
        title <- list(title = "Latent Means/Intercept",
                      markup = paste0("_BOMC", length(report), "__UL__CONTENT__EOUL__EOMC_"),
                      colnum = 2)
        attr(trows, "title") <- title
        class(trows) <- c("trows", class(trows))
        trows
     }

    ## Create elaborate markup for Chi-Square value
    getChiSq <- function(object){
        ## build response for "chi-square"
        chimeas <- object@Fit@test[[1]]
        chimeas$stat <- frnd(chimeas$stat)
        chimeas$pvalue <- frnd(chimeas$pvalue, 3,  3)
        chimeas$pvalue <- gsub("0\\.", "\\.", chimeas$pvalue)
        chisq <- paste0("_CHI2_", "(", chimeas$df, ") = ",
                        chimeas$stat , "(p=", chimeas$pvalue,")")
        chisq
    }

    ## Gather summary fit indicators
    fitMaker <- function(object) {
        if(names_upper == TRUE){
            names_fit <- toupper(names_fit)
            names(fit) <- names_fit
        }else{
            names(fit) <- names_fit
        }
        
        fitmeas <- lavaan::fitMeasures(object)[]
        fitmeas <- sapply(fitmeas, frnd)
        fitidx <- fitmeas[fit]
        names(fitidx) <- names(fit)
        fitidx <- paste(names(fitidx), "=", fitidx)
        names(fitidx) <- names(fit) # reassign names
        chisq <- grep("chi-square", names(fit), ignore.case = TRUE,
                      value = TRUE)
        fitidx[chisq] <- getChiSq(object)
        paste(fitidx, collapse = "; ")
    }


    ## The trows objects have an attribute "title" and this
    ## retrieves and formats that
    getTitleMarkup <- function(trows){
        title <- attr(trows, "title")
        header  <- "_BR_"
        ## Tricky business b/c row markup does not call for _BOR_ in column 1,
        ## that's provided by "_BR_". 
        if(title$colnum == 1){
            header <- paste0(header,
                             gsub("_CONTENT_", title$title, title$markup),
                             "_EOR_")
        } else if (title$colnum == 2){
            header <- paste0(header, "_EOC_")
            header <- paste0(header, gsub("_CONTENT_", title$title, title$markup))
            header <- paste0(header, "_EOR_")
        } else {
            MESSG <- "getTitleMarkup: colnum > 2 was not planned for"
            stop(MESSG)
        }
        header
    }
       
    ## A trows object is a matrix, this inserts formatting markup.
    ## trows also has attribute "title",
    markupTable <- function(trows) {
        #trowsf <- trows[ , c("col1", report)]
        trowsf <- trows
        for(i in 1:(NCOL(trowsf) -1)){
            trowsf[ , i] <- paste0(trowsf[ , i], "_EOC__BOC_")
        }
        trowsf[ , 1] <- paste0("_BR_", trowsf[, 1])
        trowsf[ , NCOL(trowsf)] <- paste0(trowsf[ , NCOL(trowsf)], "_EOC__EOR_")
        res <- paste(apply(trowsf, 1, paste, collapse = " "), collapse = "\n")
        ## TODO 20171028 CAUTION: workaround here b/c title info sometimes missing, must
        ## go back find why it is missing
        if (!is.null(attr(trows, "title"))){
            header <- getTitleMarkup(trows)
            res <- paste(header, "\n", res, "\n")
        }
        return(res)
    } 


    ## For each fitted SEM model, cycle through process of
    ## 1. Retrieve parameters
    ## Extract parameters into a data.frame
    extractParameters <- function(object, colNames){    
        paramTable <- getParamTable(object)
        params <- attr(paramTable, "params")

        ## report was name used for varnames of columns for
        ## keeing, ex: c("est" "se" "z" "p")
        report <- names(colNames)
  
        reslt <- list()
        if("loadings" %in% params){
            latents <- attr(paramTable, "latents")
            loadingInfo <- lapply(latents, loadingMaker,
                                  parameters = paramTable, report = report)
            names(loadingInfo) <- latents
            ## a list of trows objects
            ## this is a title for the collection of lists
            title <- list(title = c("Factor Loadings"),
                     markup = paste0("_BOMC", length(report), "__UL__CONTENT__EOUL__EOMC_"),
                     colnum = 2)
            attr(loadingInfo, "title") <- title
            class(loadingInfo) <- c("trowsList", class(loadingInfo))
            reslt[["loadings"]] <- loadingInfo
        }
        
        if("slopes" %in% params){
            regs <- paramTable[which(paramTable$op == "~"), ]
            dvs <- unique(regs$lhs)
            slopeInfo <- lapply(dvs, slopeMaker, parameters = paramTable,
                                regs = regs, report = report)
            names(slopeInfo) <- dvs
            ## title for regression collection
            title <- list(title = c("Regression Slopes"),
                          markup = paste0("_BOMC", length(report), "__UL__CONTENT__EOUL__EOMC_"),
                          colnum = 2)
            attr(slopeInfo, "title") <- title
            class(slopesInfo) <- c("trowsList", class(slopesInfo))
            reslt[["slopes"]] <- slopeInfo
        }
        
        if("intercepts" %in% params){
            variables <- attr(paramTable, "variables")
            reslt[["intercepts"]] <- interceptMaker(variables, parameters = paramTable,  report = report)
        }
        
        if("means" %in% params){
            variables <- attr(paramTable, "variables")
            regs <- paramTable[which(paramTable$op == "~"),]
            reslt[["means"]] <- observedMeanMaker(variables, parameters = paramTable, regs = regs, report = report)
        }
        
        if("thresholds" %in% params){
            variables <- attr(paramTable, "variables")
            reslt[["thresholds"]] <- thresholdMaker(variables, parameters = paramTable, report = report)
        }
        if("residuals" %in% params){
            variables <- attr(paramTable, "variables")
            reslt[["residuals"]] <- residualMaker(variables, parameters = paramTable, covariance = FALSE, report = report)
        }
        if("covariances" %in% params){
            variables <- attr(paramTable, "variables")
            reslt[["covariances"]] <- residualMaker(variables, parameters = paramTable, covariance = TRUE, report = report)
        }
        
        if("latentvariances" %in% params){
            latents <- attr(paramTable, "latents")
            reslt[["latentvariances"]] <- latentMaker(latents, parameters = paramTable, report = report)
        }
        
        if("latentmeans" %in% params){
            latents <- attr(paramTable, "latents")
            reslt[["latentmeans"]] <- latentMeanMaker(latents, parameters = paramTable, report = report)
        }
        reslt
    }

    extractModelSummary <- function(object){
        sumry <- list()
        if (1) {
            rowempty <- character(length = 1 + length(report))
            rowempty <- "_BOMC3_ _FIXED_ Indicates parameters fixed for model identification._EOMC__LB_\n" 
            sumry[["fixedparam"]] <- rowempty
        }
        
        if (!is.null(fit)) {
            rowempty <- character(length = 1 + length(report))
            rowempty <- paste("_BOMC3_", fitMaker(object), "_EOMC__LB_")
            sumry[["summaries"]] <- rowempty
        }
    }

    ## ## Combine 2 of the trows objects, copying attributes too
    ## mergeTables <- function(x, y){
    ##     resltmerge <- merge(x, y, by =c("row.names", "col1"), sort = FALSE, all = TRUE)
    ##     attr(resltmerge, "title") <- attr(x, "title")
    ##     ## TODO: consider edit markup to be shifted right
    ##     resltmerge
    ## }

    ## ## put various fits together
    ## mergeResults(aList) {
    ##     targets <- c("loadings", "slopes", "intercepts", "means",
    ##                  "thresholds", "residuals", "covariances",
    ##                  "latentvariances", "latentmeans")
    ##     loadings <- lapply(aList, function(x) x["loadings"])

    ## }
    ## Give in a list of tables and this puts them
    ## together side by side, with markup
    buildSubtable <- function(tablList){
        ## if one table is NULL, replace with empty DF
        ## for others, expand rows to same size

        parmnames <- unique(do.call(rbind, lapply(tablList, function(x) cbind(rownames = rownames(x), col1 = x[ , "col1"]))))
        rownames(parmnames) <- parmnames[ , "rownames"]
        for(x in names(tablList)){
            if (is.null(tablList[[x]])) {
                y <- data.frame(col1 = parmnames[ , "col1"],
                                matrix("-", ncol = length(colNames),
                                       nrow = NROW(parmnames),
                                       dimnames = list(parmnames[ , "rownames"],
                                                       names(colNames))))
                y[ , match("col1", colnames(y))] <- NULL
                tablList[[x]] <- y
            } else {
                y <- tablList[[x]][rownames(parmnames), ]
                ## y[ , "col1"] <- parmnames[ , "col1"]
                rownames(y) <- rownames(parmnames)
                y[ , match("col1", colnames(y))] <- NULL
                tablList[[x]] <- y
                if(is.null(attr(tablList, "title"))) attr(tablList, "title") <- attr(tablList[[x]], "title")
            }
        }
        ## that has a title attribute
        tablMatrix <- do.call(cbind,  tablList)
        tablMatrix <- cbind("col1" = parmnames[ , "col1"], tablMatrix)
        attr(tablMatrix, "title") <- attr(tablList, "title")
        markupTable(tablMatrix)
    }
    
    browser()
    
    ## in previous version, "report" was column designation:
    #  ("est", "se", "z", "p"),
    
    ## I simplify from "est(se)" to "estse" 
    names(colNames) <- gsub("est(se)", "estse", names(colNames))
   
    ##reslt1 <- extractParameters(object, colNames)

    parmList <- lapply(object, extractParameters, colNames)

    parmSetsFound <- unique(unlist(lapply(parmList, function(x) names(x))))

    parmSetsOrdering <- c("loadings"= "Factor Loadings",
                          "slopes" = "Regression Slopes",
                          "intercepts" = "Intercepts",
                          "means"= "Means",
                          "residuals" = ,
                          "covariances" = "Covariances",
                          "variances" = "Variances",
                          "latentvariances" = "Latent Var/Covariances",
                          "latentmeans" = "Latent Means/Intercept",
                          "thresholds"= "Thresholds")
    ## re-order paramSetsFound according to standard list
    parmSetNames <- intersect(parmSetsOrdering, parmSetsFound)

    results <- list()
    for (parmSetName in parmSetNames){
        tablList <- lapply(parmList, function(x) x[[parmSetName]])
        if(parmSetName %in% c("loadings", "slopes")){
            ## get the varnames
            varnames <- unique(unlist(lapply(tablList, names)))
            hh <- list()
            for(i in varnames) {
                subList <- lapply(tablList, function(x) x[[i]])
                hh[[i]] <- buildSubtable(subList)
            }
            res <- paste0(hh, collapse = " ")
            results[[parmSetName]] <- res
        } else {
            results[[parmSetName]] <- buildSubtable(tablList)
        }
    }
        
    
    ## test case with different column request
    ## reslt1 <- extractParameters(object, colNames = c("estse" = "Est(SE)"))
    ## reslt2 <- extractParameters(stdFit, colNames = c("estse" = "Est(SE)"))

    if(isTRUE(includeGroup)){
        report <- c(report, "group")
        colNames <- c(colNames, "Groups")
    }
    
    ## Now work on the markup
    ## Iterate through reslt, treating "loadings" and "slopes" differently
    ## Handle one list of estimates
    reslt <- reslt1
    resmark <- paste(unlist(lapply(names(reslt), function(tab){
        if(length(grep(tab, c("loadings", "slopes"))) > 0){
            header <- getTitleMarkup(reslt[[tab]])
            subtables <- vapply(reslt[[tab]], markupTable,
                                FUN.VALUE = character(1), report = names(colNames))
            c(header, subtables)
        } else {
            markupTable(reslt[[tab]], report = names(colNames))
        }
    })), collapse = " ")
    resmark <- paste("_BT_\n", resmark, "_EOT_\n")

    result <- markupConvert(resmark, type = c("latex", "html", "csv"),
                            longtable = longtable, file = "../jasper",
                            colNames = colNames)
    
}




##' Convert marked-up characters to latex, html, or csv
##'
##' The conversion key tables are included in the code of the function
##' @param marked A character string
##' @param type Output type, can be a vector or any one of "latex",
##'     "html", and "csv".
##' @param longtable should a tabular or a longtable object be created?
##' @param file A file stub, to which ".tex", ".html", or ".csv" can be added
##' @param colNames For SEM table, the colNames object
##' @return a list of marked up character objects
##' @author Paul Johnson
markupConvert <- function(marked, type = c("latex", "html", "csv"),
                        longtable = FALSE, file = NULL, colNames)
{
    report <- names(colNames)
    ## Replacement strings for LaTeX output
    latexreplace <- c(
        "_LB_" = "//\n",
        "_EOC_" =  "",
        "_BOC_" = "& ", 
        "_EOMC_" = "}",
        "_EOR_" = "\\\\tabularnewline",
        "_BRU_" = "",
        "_BRT_" = "", 
        "_BOCU_" = "& ",
        "_BR_" = "",
        "_BT_" = if(longtable) paste0("\\\\begin{longtable}{l", paste0(rep("r", length(report)), collapse = ""), "}")
                 else paste0("\\\\begin{tabular}{l", paste0(rep("r", length(report)), collapse = ""), "}"),
        "_EOL_" = "\n",
        "_HL_" = "\\\\hline", 
        "_UL_" = "\\\\underline{",
        "_EOUL_" = "}",
        "_SEPU_" = " &", 
        "_SEP_" = " &", 
        "_EOT_" = if (longtable) "\\\\end{longtable}" else "\\\\end{tabular}",
        "_BOMR1_" = "& \\\\multirow{1}{c}{",
        "_BOMR2_" = "& \\\\multirow{2}{c}{",
        "_BOMC1_" = "& \\\\multicolumn{1}{c}{",
        "_BOMC2_" = "& \\\\multicolumn{2}{c}{",
        "_BOMC3_" = "& \\\\multicolumn{3}{c}{",
        "_BOMC4_" = "& \\\\multicolumn{4}{c}{",
        "_BOMCT1_" = "& \\\\multicolumn{1}{c}{",
        "_BOMCT2_" = "& \\\\multicolumn{2}{c}{",
        "_BOMCT3_" = "& \\\\multicolumn{3}{c}{",
        "_BOMCT4_" = "& \\\\multicolumn{4}{c}{",
        "_HTMLHL_" = "",
        "_CHI2_" = "$\\\\chi^2)$",
        "_R2_" = "$R^2$",
        "_SIGMA_" = "$\\\\sigma$",
        "_NBSP_" = " ",
        "_FIXED_" = "$^+$"
    )

    ## Replacement strings for HTML output
    htmlreplace <- c(
        "_LB_" = "<br>",
        "_EOC_" = "</td>",
        "_BOC_" = "<td>",
        "_EOMC_" = "</td>",
        "_EOR_" = "</tr>",
        "_BRU_" = paste("<tr><td style=\"border-bottom: solid thin black; border-collapse:collapse;\">&nbsp;"),
        "_BRT_" = paste("<tr><td style=\"border-top: solid thin black; border-collapse:collapse;\">&nbsp;"),
        "_BOCU_" = paste("<td style=\"border-bottom: solid thin black; border-collapse:collapse;\">&nbsp;"),
        "_BR_" = "<tr><td>",
        "_BT_" =  "<table>\n",
        "_EOL_" = "\n",
        "_HL_" =  "",
        "_UL_" =  "<span style=\"text-decoration: underline;\">",
        "_EOUL_" = "</span>",
        "_SEPU_" = "</td><td style=\"border-bottom: solid thin black; border-collapse:collapse;\">&nbsp;",
        "_SEP_" = "</td><td>",
        "_EOT_" = "</table>",
        "_BOMR1_" = "<td rowspan = '1'>",
        "_BOMR2_" = "<td rowspan = '2'>",
        "_BOMC1_" = "<td colspan = '1'>",
        "_BOMC2_" = "<td colspan = '2'>",
        "_BOMC3_" = "<td colspan = '3'>",
        "_BOMC4_" = "<td colspan = '4'; align = 'center'>",
        "_BOMCT1_" = "<td colspan = '1'; style=\"border-top: solid thin black; border-collapse:collapse;\">&nbsp;",
        "_BOMCT2_" = "<td colspan = '2'; style=\"border-top: solid thin black; border-collapse:collapse;\">&nbsp;",
        "_BOMCT3_" = "<td colspan = '3'; style=\"border-top: solid thin black; border-collapse:collapse;\">&nbsp;",
        "_BOMCT4_" = "<td colspan = '4'; align = 'center'; ; style=\"border-top: solid thin black; border-collapse:collapse;\">&nbsp;",
        "_HTMLHL_" = "<tr><td colspan = '5'; align = 'center'; ; style=\"border-top: solid thin black; border-collapse:collapse;\">&nbsp;</tr>",
        "_CHI2_" = "&chi;<sup>2</sup>",
        "_R2_" = "R<sup>2</sup>",
        "_SIGMA_" = "&sigma;",
        "_NBSP_" = "&nbsp;",
        "_FIXED_" = "<sup>+</sup>"
    )
    
    ## Replacement strings for CSV output
    csvreplace <- c(
        "_LB_" = "\n",
        "_EOC_" =  ",",
        "_BOC_" = "", 
        "_EOMC_" = ",",
        "_EOR_" = "\n",
        "_BRU_" = "",
        "_BRT_" = "", 
        "_BOCU_" = ",",
        "_BR_" = "",
        "_BT_" = "",
        "_EOL_" = "\n",
        "_HL_" = "", 
        "_UL_" = "",
        "_EOUL_" = "",
        "_SEPU_" = "", 
        "_SEP_" = ",", 
        "_EOT_" = "",
        "_BOMR1_" = "",
        "_BOMR2_" = "",
        "_BOMC1_" = "",
        "_BOMC2_" = "",
        "_BOMC3_" = "",
        "_BOMC4_" = "",
        "_BOMCT1_" = "",
        "_BOMCT2_" = "",
        "_BOMCT3_" = "",
        "_BOMCT4_" = "",
        "_HTMLHL_" = "",
        "_CHI2_" = "chi^2",
        "_R2_" = "R^2",
        "_SIGMA_" = "sigma",
        "_NBSP_" = " ",
        "_FIXED_" = "+"
    )

    result <- list()
    if (!is.na(match("latex", type))) {
        latexmarkup <- mgsub(names(latexreplace), latexreplace, marked)
        if (!is.null(file)){
            cat(latexmarkup, file = paste0(file, ".tex"))
        }
        result[["latex"]] <- latexmarkup
    }

    if (!is.na(match("html", type))) {
         htmlmarkup <- mgsub(names(htmlreplace), htmlreplace, marked)
         if (!is.null(file)){
             cat(htmlmarkup, file = paste0(file, ".html"))
         }
         result[["html"]] <- latexmarkup
    }

    if (!is.na(match("csv", type))) {
        csvmarkup <- mgsub(names(csvreplace), csvreplace, marked)
        if (!is.null(file)){
            cat(csvmarkup, file = paste0(file, ".csv"))
        }
        result[["csv"]] <- csvmarkup
    }
    result
}
