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
##' \item "residualvariances" are the observed variable residual variances.
##' \item "residualcovariances" are the observed covariances among
##' residuals of observed variables.
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
##'     c("est(se)" = "Estimate(Std.Err.)". If one vector of columns
##'     is supplied, it is used for all model objects. Different
##'     columns for different models can be requested by supplying a
##'     list of named vectors. Names must be same names supplied in
##'     the \code{object} list of models.
##' @param paramSets Parameter sets to be included. Valid values are
##'     \code{"all"} or a vector including any of the following:
##'     \code{c("loadings", "slopes", "intercepts", "residualvariances",
##'     "residualcovariances", "latentmeans", "latentvariances",
##'     "latentcovariances", "thresholds")}. Default is "all", which
##'     means that any of the parameters present in the fitted model
##'     that are listed in the previous sentence will be included in
##'     the output. If \code{object} is a list of fitted models, and
##'     paramSets is "all" or a vector, then same parameter sets are
##'     reported for all models. Different parameter sets can be
##'     requested by supplying a list of named vectors. Names must be
##'     same names supplied in the \code{object} list of models.
##' @param paramSetsLabels Named vector, used to supply alternative
##'     pretty printing labels for parameter sets. See default in declaration
##'     of this function.
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
##' @param fitLabels Labels for the fit measures requested by the fit
##'     parameter.  Must have same number of elements as fit.  For
##'     example, fit = c("cfi.scaled", "tli.scaled"), fitLabels =
##'     c("CFI", "TLI"). Default is upper-case of the fit.
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
##' fit1 <- cfa(HS.model, data = HolzingerSwineford1939, std.lv = TRUE, meanstructure = TRUE)
##' fit1.t1 <- semTable(fit1, fit = c("chi-square", "rmsea"), file = "../jasper")
##' fit1.t1 <- semTable(fit1, fit = c("chi-square", "rmsea"),
##'            colNames = c("est" = "Est",  se = "Std. Err."))
##' fit1.t1 <- semTable(fit1, fit = c("chi-square", "rmsea"),
##'            colNames = c("estse" = "Estimate(Std.Err.)"))
##'
##' ## Fit same model with standardization
##' fit1.std <- update(fit1, std.lv = TRUE, std.ov = TRUE, meanstructure = TRUE) 
##' ## include 2 models in table request
##' fit1.t2 <- semTable(list("Ordinary" = fit1, "Standardized" = fit1.std),
##'                     file = "../jasper")
##'
##' semTable(list("Ordinary" = fit1, "Standardized" = fit1.std),
##'     colNames = list("Ordinary" = c("est", "se"), "Standardized" = c("est")))
##' 
##' cat(fit1.t1)
##' fit1.t2 <- semTable(fit1, fit = c("chisq", "rmsea"), standardized = TRUE)
##' cat(fit1.t2)
##' fit1.t3 <- semTable(fit1, fit = c("chisq", "rmsea", "tli"),
##'                     colNames = c("est" = "Estimate", "se" = "Std.Err."),
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
##' fit2 <- sem(regmodel, data = HolzingerSwineford1939, std.lv = TRUE, meanstructure = TRUE)
##'
##' fit2.std <- update(fit1, std.lv = TRUE, std.ov = TRUE, meanstructure = TRUE) 
##'
fit2.t <- semTable(list("Ordinary" = fit2, "Standardized" = fit2.std), fit = "rmsea",
                   colNames = list("Ordinary" = c("est" = "Est", "se" = "Std.Err.", "p" = "p"),
                                   "Standardized" = c("est" = "Standardized Est.")),
                   file = "../jasper", type = c("latex", "csv"))
##'
##' 
##' fit2.t <- semTable(list("Ordinary" = fit2, "Standardized" = fit2.std), fit = "rmsea", type = "html")
##'
##' 
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
##' fit4.t1 <- semTable(fit4, paramSets = c("loadings", "thresholds", "residualvariances"),
##'     fit = c("tli", "chi-square"),
##'     names_fit = c("TLI", "chi-square"), type = "html")
##' fit4.t2 <- semTable(fit4, fit = c("rmsea", "tli", "chi-square"),
##'     names_fit = c("RMSEA", "TLI", "chi-square"), type = "latex")
##' 
##' 
##' ## Example with file output requested in the command
##' ## semTable(output, file = "catTable.tex",
##' ##    paramSets = c("loadings", "thresholds", "residualvariances"),
##' ##    fit = c("tli", "chi-square"),
##' ##    names_fit = c("TLI", "chi-square"), type = "latex")
##' }
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
semTable <-
    function(object, file = NULL,
             paramSets = "all",
             paramSetsLabels = c("loadings"= "Factor Loadings",
                                "slopes" = "Regression Slopes",
                                "intercepts" = "Intercepts",
                                "means"= "Means",
                                "residualvariances" = "Residual Variances",
                                "residualcovariances" = "Residual Covariances",
                                "variances" = "Variances",
                                "latentvariances" = "Latent Variances",
                                "latentcovariances" = "Latent Covariances", 
                                "latentmeans" = "Latent Intercepts",
                                "thresholds"= "Thresholds"),
             fit = c("chi-square", "cfi", "tli", "rmsea"),
             fitLabels = toupper(fit),
             colNames = c("est" = "Estimate", "se" = "SE", "z" = "z", "p" = "p"),
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
    frnd <- function(x, rnd = 2, digits = 2) {
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
    ## 1. remove "variances" and insert "residualvariancess" "residualcovariances" "latentvariances"
    ## 2. remove "means" and insert "intercepts" and "latentmeans"
    fixParamSets <- function(paramSets, parameters) {
        if (paramSets != "all") {
            paramSets <- unique(paramSets)
            if (any(paramSets %in% names(
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
                params <- c(params, "residualvariances")
            }
            if (any(parameters$lhs %in% variables &
                             parameters$rhs %in% variables &
                             parameters$lhs != parameters$rhs &
                             parameters$op == "~~")) {
                params <- c(params, "residualcovariances")
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
            ## remove "means", insert "intercepts" and/or
            ## "latentmeans"
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


    insertParamRowType <- function(parameters){
        paramops <- c("=~" = "loadings", "~" = "slopes", "~1" = "means",
                      "~~" = "variances", "|" = "thresholds")

        variables <- attr(parameters, "variables")
        latents <- attr(parameters, "latents")
        
        parameters[ , "rowType"] <- NA
        parameters[ , "rowType"] <- paramops[parameters$op]
        
        parameters[(parameters$lhs %in% variables &
                    parameters$rhs %in% variables &
                    parameters$lhs == parameters$rhs &
                    parameters$op == "~~"), "rowType"] <- "residualvariances"

        parameters[parameters$lhs %in% variables &
                   parameters$rhs %in% variables &
                   parameters$lhs != parameters$rhs &
                   parameters$op == "~~", "rowType"] <- "residualcovariances"
        parameters[parameters$rhs %in% latents &
                    parameters$lhs %in% latents &
                    parameters$lhs == parameters$rhs &
                    parameters$op == "~~", "rowType"] <- "latentvariances"
        parameters[parameters$rhs %in% latents &
                   parameters$lhs %in% latents &
                   parameters$lhs != parameters$rhs &
                   parameters$op == "~~", "rowType"] <- "latentcovariances"
        parameters[parameters$lhs %in% variables &
                            parameters$op == "~1", "rowType"] <- "intercepts"
        parameters[parameters$lhs %in% latents &
                   parameters$op == "~1", "rowType"] <- "latentmeans"
        parameters
        
    }
    
    getParamTable <- function(object){
        
        createEstSE <- function(dframe, stars = FALSE){
            dframe <- roundSubtable(dframe)
            ifelse(dframe[ , "free"] != 0,
                   paste0(dframe[ , "est"], "(", dframe[ , "se"], ")", if(stars)dframe[ , "starsig"]),
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
        parameters$estsestars <- createEstSE(parameters, stars = TRUE)
        
        ## items previously global are specialized to this model
        attr(parameters, "variables") <- unique(unlist(object@Data@ov.names))
        attr(parameters, "latents") <- unique(unlist(object@pta$vnames$lv))
        attr(parameters, "params") <- fixParamSets(paramSets, parameters)

        parameters2 <- insertParamRowType(parameters)
        parameters2 <- roundSubtable(parameters2)
        parameters2
    }

    
    makeSubtableTitle <- function(title, colnum = 1, width = 1,  underline = TRUE){
        prefix <- if (colnum > 1) paste("_EOC_", rep("_BOC__EOC_", colnum - 2))
        markup <- if(underline) {
                      paste0(prefix, "_BOMC", width, "__UL__CONTENT__EOUL__EOMC_")
                  } else {
                      paste0(prefix, "_BOMC", width, "__CONTENT__EOMC_")
                  }
        title <- list(title = title, markup = markup,
                      colnum = colnum, width = width)
    }

    
    ##works for loadings or slopes
    loadingMaker <- function(parameters, rowType = "loadings", colNames, modelName) {
        browser()
        report <- names(colNames[[modelName]])
        totalNcolumns <- min(9,  length(unname(unlist(colNames))))
        trows <- parameters[parameters$rowType == rowType,  , drop = TRUE]
        trowsplit <- split(trows, f = trows$lhs, drop = FALSE)
        info <- lapply(trowsplit, function(x){
            vname <- unique(x$lhs)
            rownames(x) <- paste(rowType, vname, x[ , "rhs"], sep = ".")
            x <- data.frame(col1 = x$rhs, x[ , report, drop = FALSE])
            ## don't put "_BOC_" at beginning if in colnum 1
            attr(x, "title") <- makeSubtableTitle(vname, colnum = 1, width = 2, underline = TRUE)
            class(x) <- c("trows", class(x))
            x
        })
        info
    }

    ## ## gives back a list of tables, 1 for each DV
    ## slopeMaker <- function(parameters, report = c("est", "se", "z", "p")){
    ##     jj <- "slopes"
    ##     trows <- parameters[parameters$rowType == jj, , drop = FALSE]
    ##     trowsplit <- split(trows, f = trows$lhs, drop = FALSE)
    ##     slopelist <- lapply(trowsplit, function(x){
    ##         vname <- unique(x$lhs)
    ##         rownames(x) <- paste(jj, vname, x[ , "rhs"], sep = ".")
    ##         x <- data.frame(col1 = x$rhs, x[ , report, drop = FALSE])
    ##         makeSubtableTitle(dvname, colnum = 1, width = 2, underline = TRUE) 
    ##         ## title <- list(title = dvname,
    ##         ##               markup = "_UL__CONTENT__EOUL__EOC_",
    ##         ##               colnum = 1)
    ##         attr(x, "title") <- title
    ##         class(x) <- c("trows", class(x))
    ##         x
    ##     })
    ##     slopelist
    ## }

    
    ## interceptMaker <- function(parameters, report = c("est", "se", "z", "p")){
    ##     jj <- "intercepts"
    ##     trows <- parameters[parameters$rowType == jj, , drop = FALSE]
    ##     if(NROW(trows) == 0) return(NULL)
    ##     rownames(trows) <- paste0(jj, ".", trows[ , "lhs"])
    ##     trows <- data.frame(col1 = trows$lhs, trows[, report, drop = FALSE])
    ##     title <- list(title = paramSetsLabels[jj],
    ##                   markup = paste0("_BOMC", length(report), "__UL__CONTENT__EOUL__EOMC_"),
    ##                   colnum = 2)
    ##     attr(trows, "title") <- title
    ##     class(trows) <- c("trows", class(trows))
    ##     trows
    ## }

    ## observedMeanMaker <- function(parameters, report = c("est", "se", "z", "p")){
    ##     jj <- "means"
    ##     trows <- parameters[parameters$rowType == jj, , drop = FALSE]
    ##     if(NROW(trows) == 0) return(NULL)
    ##     rownames(trows) <- paste0(jj, ".", trows[ , "lhs"])
    ##     trows <- data.frame(col1 = trows$lhs, trows[, report, drop = FALSE])
    ##     title <- list(title = paramSetsLabels[jj],
    ##                   markup = paste0("_BOMC", length(report), "__UL__CONTENT__EOUL__EOMC_"),
    ##                   colnum = 2)
    ##     attr(trows, "title") <- title
    ##     class(trows) <- c("trows", class(trows))
    ##     trows
    ## }


    ## thresholdMaker <- function(parameters, rowType, colNames, modelName){
    ##     report <- names(colNames[[modelName]])
    ##     totalNcolumns <- min(9,  length(unname(unlist(colNames))))
    ##     trows <- parameters[parameters$rowType == rowType, ,
    ##                         drop = FALSE]
    ##     if(dim(trows)[1] == 0) return(NULL)
    ##     thresnum <- substring(trows$rhs, 2, nchar(trows$rhs))
    ##     trows$col1 <- paste0(trows$lhs, "(", thresnum, ")")
    ##     rownames(trows) <- paste(jj, trows[ , "lhs"], thresnum, sep = ".")
    ##     trows <- data.frame(col1 = trows$col1, trows[ , report, drop = FALSE])
    ##     attr(trows, "title") <- makeSubtableTitle(paramSetsLabels[rowType],
    ##                                               colnum = 2,
    ##                                               width = totalNcolumns,
    ##                                               underline = TRUE)
    ##     class(trows) <- c("trows", class(trows))
    ##     trows
    ## }

        

    ## residualvarianceMaker <- function(parameters, rowType, colNames, modelName){
    ##     report <- names(colNames[[modelName]])
    ##     totalNcolumns <- min(9,  length(unname(unlist(colNames))))
    ##     trows <- parameters[parameters$rowType == rowType,, drop = FALSE ]
    ##     if(dim(trows)[1] == 0) return(NULL)
    ##     trows <- trows[which(trows$rhs == trows$lhs),, drop = FALSE]
    ##     rownames(trows) <- paste("residualvariances", trows[ , "lhs"], sep = ".")
    ##     trows <- data.frame(col1 = trows$lhs, trows[ , report, drop = FALSE])
    ##     attr(trows, "title") <- makeSubtableTitle(paramSetsLabels[rowType],
    ##                                               colnum = 2,
    ##                                               width = totalNcolumns,
    ##                                               underline = TRUE)
    ##     class(trows) <- c("trows", class(trows))
    ##     trows
    ## }

    ## residualcovarianceMaker <- function(parameters, rowType, colNames, modelName){
    ##     report <- names(colNames[[modelName]])
    ##     totalNcolumns <- min(9,  length(unname(unlist(colNames))))
    ##     trows <- parameters[parameters$rowType == rowType,, drop = FALSE ]
    ##     if(dim(trows)[1] == 0) return(NULL)
    ##     trows <- trows[which(trows$rhs != trows$lhs), ]
    ##     rownames(trows) <- paste0(rowType, trows[ , "lhs"], sep = ".")
    ##     trows <- data.frame(col1 = trows$lhs, trows[ , report, drop = FALSE])
        
    ##     attr(trows, "title") <- makeSubtableTitle(paramSetsLabels[rowType],
    ##                                               colnum = 2,
    ##                                               width = totalNcolumns,
    ##                                               underline = TRUE)
    ##     class(trows) <- c("trows", class(trows))
    ##     trows
    ## }

    
    ## latentvarianceMaker <- function(parameters, report = c("est", "se", "z", "p")){
    ##     jj <- "latentvariances"
    ##     trows <- parameters[parameters$rowType == jj, , drop = FALSE]
    ##     if(dim(trows)[1] == 0) return(NULL)
    ##     rownames(trows) <- paste0(jj, ".", trows[ , "lhs"])
    ##     trows[ , "lhs"] <- trows[ , "lhs"]
    ##     trows <- data.frame(col1 = trows$lhs, trows[ , report, drop = FALSE])
    ##     title <- list(title = paramSetsLabels[jj],
    ##                   markup = paste0("_BOMC", length(report), "__UL__CONTENT__EOUL__EOMC_"),
    ##                   colnum = 2)
    ##     attr(trows, "title") <- title
    ##     class(trows) <- c("trows", class(trows))
    ##     trows
    ## }


    ## latentcovarianceMaker <- function(parameters, rowType, colNames, modelName){
    ##     report <- names(colNames[[modelName]])
    ##     totalNcolumns <- min(9,  length(unname(unlist(colNames))))
    ##     trows <- parameters[parameters$rowType == rowType, , drop = FALSE]
    ##     if(dim(trows)[1] == 0) return(NULL)
    ##     rownames(trows) <- paste0(rowType, ".", trows[ , "lhs"],
    ##                              ".", trows[ , "rhs"])
    ##     trows[ , "lhs"] <- paste0(trows[ , "lhs"], " w/", trows[ , "rhs"])
    ##     trows <- data.frame(col1 = trows$lhs, trows[ , report, drop = FALSE])
    ##     attr(trows, "title") <- makeSubtableTitle(paramSetsLabels[rowType],
    ##                                               colnum = 2,
    ##                                               width = totalNcolumns,
    ##                                               underline = TRUE)
    ##     class(trows) <- c("trows", class(trows))
    ##     trows
    ## }


    

    ## latentMeanMaker <- function(parameters, report = c("est", "se", "z", "p")){
    ##     trows <- parameters[parameters$rowType == "latentmeans",, drop = FALSE]
    ##     if(dim(trows)[1] == 0) return (NULL)
    ##     rownames(trows) <- paste("latentmeans", trows[ , "lhs"], sep = ".")
    ##     trows <- data.frame(col1 = trows$lhs, trows[ , report, drop = FALSE])
    ##     title <- list(title = "Latent Means/Intercept",
    ##                   markup = paste0("_BOMC", length(report), "__UL__CONTENT__EOUL__EOMC_"),
    ##                   colnum = 2)
    ##     attr(trows, "title") <- title
    ##     class(trows) <- c("trows", class(trows))
    ##     trows
    ##  }

    ## want to replace all/many of the "maker" functions with one function.
    ## works for:
    ## rowType in c("intercepts", "means" "latentmeans", "latentvariances","residualcovariances"
    ##
    ## complications: "latentcovariances" "residualvariances" "thresholds" "covariances"
    parTableMaker <- function(trows, rowType, colNames, modelName, col1name = "lhs"){
        report <- names(colNames[[modelName]])
        totalNcolumns <- min(9,  length(unname(unlist(colNames))))
        ## trows <- parameters[parameters$rowType == rowType,, drop = FALSE]
        if(dim(trows)[1] == 0) return (NULL)
      
        if (rowType == "thresholds"){
            thresnum <- substring(trows$rhs, 2, nchar(trows$rhs))
            trows$col1 <- paste0(trows$lhs, "(", thresnum, ")")
            rownames(trows) <- paste(rowType, trows[ , "lhs"], thresnum, sep = ".")
        } else if (rowType == "residualvariances"){
            rownames(trows) <- paste(rowType, trows[ , "lhs"], sep = ".")
            trows$col1 <- trows[ , col1name]
        } else if (rowType == "residualcovariances"){
            trows$col1 = trows$lhs
            rownames(trows) <- paste0(rowType, trows[ , "lhs"], sep = ".")
        } else if (rowType == "latentcovariances"){
            browser()
            ## Needed?
            ## trows <- parameters[parameters$rowType == rowType, , drop = FALSE]
            trows$col1 = trows$lhs
            rownames(trows) <- paste0(rowType, ".", trows[ , "lhs"],
                           ".", trows[ , "rhs"])
        } else {
            rownames(trows) <- paste(rowType, trows[ , col1name], sep = ".")
            trows$col1 <- trows[ , col1name]
        }
        trows <- data.frame(col1 = trows$col1, trows[ , report, drop = FALSE])
        attr(trows, "title") <- makeSubtableTitle(paramSetsLabels[rowType],
                                                  colnum = 2,
                                                  width = totalNcolumns,
                                                  underline = TRUE)
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
            fitLabels <- toupper(fitLabels)
            names(fit) <- fitLabels
        }else{
            names(fit) <- fitLabels
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
    ## formats that
    getTitleMarkup <- function(title){
        header  <- "_BR_"
        ## Tricky business b/c row markup does not call for _BOC_ in column 1,
        ## that's provided by "_BR_".
        header <- paste(header, gsub("_CONTENT_", title$title, title$markup), "_EOR_")
        header
    }
       
    ## A trows object is a matrix, this inserts formatting markup.
    ## trows also has attribute "title", which is used to create
    ## row 1 in the markup result
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
            header <- getTitleMarkup(attr(trows, "title"))
            res <- paste(header, "\n", res, "\n")
        }
        return(res)
    } 


    ## For each fitted SEM model, cycle through process of
    ## 1. Retrieve parameters
    ## Extract parameters into a data.frame
    extractParameters <- function(object, colNames, modelName){
        paramTable <- getParamTable(object)
        params <- attr(paramTable, "params")
        ## report was name used for varnames of columns for
        ## keeing, ex: c("est" "se" "z" "p")
        report <- names(colNames[modelName])

        ## explore following as alternative concept to manage calculations
        ## creates a list of tables for each parameter type, so no need to do
        ## data selection within the table maker function
        paramTableSplit <- split(paramTable, f = paramTable$rowType, drop = FALSE)
        ## Still must treat loadings and slopes differently
        reslt <- list()
        for(jj in names(paramTableSplit)){
            if (jj %in% c("loadings", "slopes")){
                info <- loadingMaker(paramTableSplit[[jj]], rowType = jj, colNames, modelName)
                if (!is.null(info)){
                    attr(info, "title") <- makeSubtableTitle(paramSetsLabels["loadings"],
                                                             colnum = 2,
                                                             width = length(report))
                    class(info) <- c("trowsList", class(info))
                }
                reslt[[jj]] <- info
            } else {
                reslt[[jj]] <- parTableMaker(paramTableSplit[[jj]], rowType = jj, colNames,
                                            modelName = modelName, col1name = "lhs")
            }
        }
                
        ## reslt <- list()
        ## for (jj in c("loadings", "slopes")){
        ##     info <- loadingMaker(paramTable, rowType = jj, colNames)
            
        ##     if (!is.null(info)){
        ##         attr(info, "title") <- makeSubtableTitle(paramSetsLabels["loadings"],
        ##                                                  colnum = 2,
        ##                                                  width = length(report))
        ##         class(info) <- c("trowsList", class(loadingInfo))
        ##     }
        ##     reslt[[i]] <- info
        ## }

        ## for(i in c("intercepts", "means", "latentvariances", "latentmeans")){
        ##     reslt[[i]] <- parTableMaker(paramTable, rowType = i, colNames,
        ##                                 modelName = modelName, col1name = "lhs")
        ## }

      
        ## if("thresholds" %in% params){
        ##     reslt[["thresholds"]] <-
        ##         thresholdMaker(parameters = paramTable,
        ##                        rowType = "thresholds",
        ##                        colNames = colnames,
        ##                        modelName = modelName)
        ## }
        
        ## if("residualvariancess" %in% params){
        ##     reslt[["residualvariances"]] <-
        ##         residualvarianceMaker(parameters = paramTable,
        ##                               rowType = "thresholds",
        ##                               colNames = colnames,
        ##                               modelName = modelName)
        ## }

        ## if("residualcovariances" %in% params){
        ##     reslt[["residualcovariances"]] <-
        ##         residualcovarianceMaker(parameters = paramTable,
        ##                                 rowType = "thresholds",
        ##                                 colNames = colnames,
        ##                                 modelName = modelName)
        ## }
        ##  if("latentcovariances" %in% params){
        ##     reslt[["latentcovariances"]] <-
        ##         latentcovarianceMaker(parameters = paramTable,
        ##                               rowType = "thresholds",
        ##                               colNames = colnames,
        ##                               modelName = modelName)
        ## }
       
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

    ## Given a list of tables (say, 3 variances tables) this puts them
    ## together side by side, with markup.
    ## If one table is NULL, replace with empty DF
    ## for others, expand rows to same size
    buildParamSetSubtable <- function(tablList, colNames){
        xxx <- lapply(tablList, function(x) cbind(rownames = rownames(x), col1 = x[ , "col1"]))
        paramnames <- unique(do.call(rbind, xxx))
        rownames(paramnames) <- paramnames[ , "rownames"]
        for(jj in names(tablList)){
            ## if paramSet lacks desired table, make empty table for it
            if (is.null(tablList[[jj]])) {
                y <- data.frame(col1 = paramnames[ , "col1"],
                                matrix("-", ncol = length(colNames[[jj]]),
                                       nrow = NROW(paramnames),
                                       dimnames = list(paramnames[ , "rownames"],
                                                       names(colNames[[jj]]))))
                y[ , match("col1", colnames(y))] <- NULL
                tablList[[jj]] <- y
            } else {
                y <- tablList[[jj]][rownames(paramnames), ]
                ## y[ , "col1"] <- paramnames[ , "col1"]
                rownames(y) <- rownames(paramnames)
                y[ , match("col1", colnames(y))] <- NULL
                tablList[[jj]] <- y
                if(is.null(attr(tablList, "title"))) attr(tablList, "title") <- attr(tablList[[jj]], "title")
            }
        }
        ## that has a title attribute
        tablMatrix <- do.call(cbind,  tablList)
        tablMatrix <- cbind("col1" = paramnames[ , "col1"], tablMatrix)
        attr(tablMatrix, "title") <- attr(tablList, "title")
        markupTable(tablMatrix)
    }


    
    
    if (!is.list(object)) {
        object <- list("Model" = object)
    }
    
    if (!is.list(colNames)) {
        colNames.orig <- colNames
        colNames <- lapply(object, function(x) colNames)
    } else {
        if (length(colNames) != length(object) || names(colNames) != names(object)){
            MESSG <- "object list and colNames list must match"
        }
    }

    ## clean up name, since est(se) is not a legal R name
    colNames <- lapply(colNames, function(x){
        names(x) <- gsub("est(se)", "estse", names(x))
        x
    })
    
    ## each model object's parameters are pulled
    ## paramList <- lapply(names(object, extractParameters, colNames)

    paramList <- list()
    for(ii in names(object)){
        paramList[[ii]] <- extractParameters(object[[ii]], colNames, modelName = ii)
    }
    
    paramSetsFound <- unique(unlist(lapply(paramList, function(x) names(x))))
    ## re-order paramSetsFound according to standard list
    paramSetNames <- intersect(names(paramSetsLabels), paramSetsFound)
    
    finalizeMarkup <- function(paramSetNames, colNames){
        colNameCounts <- lapply(colNames, length)
        colHeaderRow <- unname(unlist(colNames))
        totalNcolumns <- min(9,  length(colHeaderRow))## If more than 9, give up at centering
        results <- lapply(paramSetNames, function(jj) {
            ## retrieve param jj from each model
            tablList <- lapply(paramList, function(x) x[[jj]])
            ## treat loadings and slopes differently because they are
            ## lists of tables
            if(jj %in% c("loadings", "slopes")){
                ## get the varnames
                varnames <- unique(unlist(lapply(tablList, names)))
                hh <- list()
                for(i in varnames) {
                    subList <- lapply(tablList, function(x) x[[i]])
                    hh[[i]] <- buildParamSetSubtable(subList, colNames)
                }
                res <- paste0(hh, collapse = " ")
                ## Improvise a section heading for loadings and slopes
                title <- list(title = paramSetsLabels[jj],
                              markup = paste0("_BOMC", totalNcolumns, "__UL__CONTENT__EOUL__EOMC_"),
                              colnum = 2)
                header <- getTitleMarkup(title)
                res <- paste(header, res)
            } else {
                res <- buildParamSetSubtable(tablList, colNames)
            }
            res
        })
        
        results2 <- paste(results, collapse = " ")
        colHeaderRow <- paste("_BR__EOC__BOC_", paste(colHeaderRow, collapse = "_EOC__BOC_"), "_EOC__EOR_\n")
        ## manual prototype
        ## modelHeaderRow <- paste0("_BR__EOC_", 
        ##                          "_BOMC", colNameCounts[1],"__UL_", names(colNameCounts[1]) , "_EOUL__EOMC_",
        ##                          "_BOMC", colNameCounts[2],"__UL_", names(colNameCounts[2]), "_EOUL__EOMC_", "_EOR_")
        modelHeaderRow <- paste0("_BR__EOC_",
                                 paste0("_BOMC", colNameCounts, "__UL_", names(colNameCounts), "_EOUL__EOMC_", collapse = " "),
                                 "_EOR_\n", collapse = " ")
        resmark <- paste("_BT_\n", modelHeaderRow, colHeaderRow, results2, "_EOT_\n")
        resmark
    }

    markedResults <- finalizeMarkup(paramSetNames, colNames)
    
    result <- markupConvert(markedResults, type = type,
                            longtable = longtable, file = file,
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
##' @param colNames For SEM table, the list of colNames objects
##' @return a list of marked up character objects
##' @author Paul Johnson
markupConvert <- function(marked, type = c("latex", "html", "csv"),
                        longtable = FALSE, file = NULL, colNames)
{
    report <- names(unlist(colNames))
    
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
        "_BT_" = if(longtable) paste0("\\\\begin{longtable}{l",
                                      paste0(rep("r", length(report)), collapse = ""), "}")
                 else paste0("\\\\begin{tabular}{l",
                             paste0(rep("r", length(report)), collapse = ""), "}"),
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
        "_BOMC5_" = "& \\\\multicolumn{5}{c}{",
        "_BOMC6_" = "& \\\\multicolumn{6}{c}{",
        "_BOMC7_" = "& \\\\multicolumn{7}{c}{",
        "_BOMC8_" = "& \\\\multicolumn{8}{c}{",
        "_BOMC9_" = "& \\\\multicolumn{9}{c}{",
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
        "_BOMC1_" = "<td colspan = '1'; align = 'center'>",
        "_BOMC2_" = "<td colspan = '2'; align = 'center'>",
        "_BOMC3_" = "<td colspan = '3'; align = 'center'>",
        "_BOMC4_" = "<td colspan = '4'; align = 'center'>",
        "_BOMC5_" = "<td colspan = '5'; align = 'center'>",
        "_BOMC6_" = "<td colspan = '6'; align = 'center'>",
        "_BOMC7_" = "<td colspan = '7'; align = 'center'>",
        "_BOMC8_" = "<td colspan = '8'; align = 'center'>",
        "_BOMC9_" = "<td colspan = '9'; align = 'center'>",
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
        "_BOMC5_" = "",
        "_BOMC6_" = "",
        "_BOMC7_" = "",
        "_BOMC8_" = "",
        "_BOMC9_" = "",
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

    if(length(type) == 1){
        return(result[[1]])
    }
    result
        
}
