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
##' \item "fits" the summary indicators of the mismatch between
##' the theoretical and observed covariance matrices, such as
##' RMSEA, CLI, TFI.
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
##' @param object A lavaan object (e.g., returned by cfa() or sem()),
##'     or a named list of lavaan objects, e.g., \code{list("Model A"
##'     = obj1, "Model B" = obj2)}. Results will be displayed side by
##'     side.
##' @param colLabels Specify parameter columns to appear for each
##'     model, along with their labels.  The allowed columns are
##'     "est", "se", "z", "p", which have the usual meanings, along
##'     with "estse", and "estestars". "estse" can be thought of as
##'     "est(se)", output displays as "estimate(std.error)" and
##'     "estsetars" (ne "est(se)stars") displays as
##'     "estimate(std.error)***". The default is for a 4 column table,
##'     \code{c(est = "Estimate", se = "SE", z = "z", p = "p")}. If
##'     one vector is supplied, it is applied for all model
##'     objects. It is allowed to request different columns for each
##'     model by providing a list of model names and columns, as in
##'     list("Model A" = c(est = "Estimate", se = "Std. Err."),
##'     "Model B" = c(p = "p value")). The names in this list must be
##'     same names supplied in the \code{object} list of models.
##' @param paramSets Parameter sets to be included for each fitted
##'     object.  Can be one vector (applied to all fit objects) or a
##'     list of vectors (one for each element in the list of fitted
##'     models). Valid values of the vector are \code{"all"} or a any
##'     of the following: \code{c("loadings", "slopes", "intercepts",
##'     "residualvariances", "residualcovariances", "latentmeans",
##'     "latentvariances", "latentcovariances", "thresholds",
##'     "fits")}. Default is "all", any of the estimates present in
##'     the fitted model that are listed in the previous sentence will
##'     be included in the output. If \code{object} is a list of
##'     fitted models, and paramSets is "all" or a vector, then same
##'     parameter sets are reported for all models. Different
##'     parameter sets can be requested by supplying a list of named
##'     vectors. Names for vectors within that list must be same names
##'     supplied in the \code{object} list of models.
##' @param paramSetsLabels Named vector, used to supply alternative
##'     pretty printing labels for parameter sets. See default in
##'     declaration of this function. Input can either be a vector of
##'     the same lenth as \code{paramSets}, meaning all labels or are
##'     user-specified, or a named vector of new labels for some of
##'     the parameter sets. The default values are \code{c("loadings"=
##'     "Factor Loadings", "slopes" = "Regression Slopes",
##'     "intercepts" = "Intercepts", "means"= "Means",
##'     "residualvariances" = "Residual Variances",
##'     "residualcovariances" = "Residual Covariances", "variances" =
##'     "Variances", "latentvariances" = "Latent Variances",
##'     "latentcovariances" = "Latent Covariances", "latentmeans" =
##'     "Latent Intercepts", "thresholds" = "Thresholds", "fits" =
##'     "Fit Indices")}
##' @param fit Summary indicators to be included. The allowed values
##'     are provided by \code{lavaan::fitMeasures(object)}. Currently,
##'     they are "npar", "fmin", "chisq", "df", "pvalue",
##'     "baseline.chisq", "baseline.df", "baseline.pvalue", "cfi",
##'     "tli", "nnfi", "rfi", "nfi", "pnfi", "ifi", "rni", "logl",
##'     "unrestricted.logl", "aic", "bic", "ntotal", "bic2", "rmsea",
##'     "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "rmr",
##'     "rmr_nomean", "srmr", "srmr_bentler", "srmr_bentler_nomean",
##'     "srmr_bollen", "srmr_bollen_nomean", "srmr_mplus",
##'     "srmr_mplus_nomean", "cn_05", "cn_01", "gfi", "agfi", "pgfi",
##'     "mfi", "ecvi". The return for "chisq" will include markup for
##'     the p value, \code{X^2(p=0.04)}.
##' @param fitLabels Labels for the fit measures requested by the fit
##'     parameter.  Can be either a named vector, eg \code {c(rmsea =
##'     "RMSEA", cli = "CLI")} or a vector of the same length as
##'     fit. In the named vector, the names must correspond to the fit
##'     indices specified in the \code{fit}. Defaults are created as
##'     upper-case values of fit, except for "chisq" which is labeled
##'     by the greek letter "Chi".
##' @param type Choose "latex", "html", "csv", or a vector including
##'     any or all of these. If several are specified, ie,
##'     \code{c("latex", "html", "csv")}, a list of 3 sets of markup
##'     will be returned.
##' @param groupLabels If object is just one SEM, but there are
##'     several groups within it, the parameters for each group will
##'     be displayed side by side.  If user does not want all groups
##'     to be displayed, a vector of group names can be provided here
##'     to indicate which groups should be included.
##' @param file Base name for output file.
##' @param longtable If TRUE, use longtable for LaTeX
##'     documents. Default is FALSE.
##' @param alpha Thresholds for p-values that determine number of
##'     stars.  Defaults as \code{c(0.05, 0.01, 0.001)} for
##'     \code{c("*", "**", "***")}.
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
##' fit1.t1 <- semTable(fit1, fit = c("chisq", "rmsea"), file = "../jasper")
##' fit1.t1 <- semTable(fit1, fit = c("chisq", "rmsea"),
##'            colLabels = c("est" = "Est",  se = "Std. Err."))
##' fit1.t1 <- semTable(fit1, fit = c("chisq", "rmsea"),
##'            colLabels = c("estse" = "Estimate(Std.Err.)"))
##' ## 2 groups
##' fit1.g <- cfa(HS.model, data = HolzingerSwineford1939, std.lv = TRUE, group = "school")
##' semTable(fit1.g)
##' 
##' ## Fit same model with standardization
##' fit1.std <- update(fit1, std.lv = TRUE, std.ov = TRUE, meanstructure = TRUE) 
##' ## include 2 models in table request
##' fit1.t2 <- semTable(list("Ordinary" = fit1, "Standardized" = fit1.std),
##'                     file = "../jasper")
##' semTable(list("Ordinary" = fit1, "Standardized" = fit1.std),
##'     colLabels = list("Ordinary" = c("est", "se"), "Standardized" = c("est")))
##' 
##' cat(fit1.t1)
##' fit1.t2 <- semTable(fit1, fit = c("chisq", "rmsea"), standardized = TRUE)
##' cat(fit1.t2)
##' fit1.t3 <- semTable(fit1, fit = c("chisq", "rmsea", "tli"),
##'                     colLabels = c("est" = "Estimate", "se" = "Std.Err."),
##'                     standardized = TRUE)
##' cat(fit1.t3)
##' 
##' ## Can create file if desired
##' ## cat(fit1.t2, file = "table1.t2.tex")
##' ## Basic SEM
##' regmodel1 <- 'visual  =~ x1 + x2 + x3
##'              textual =~ x4 + x5 + x6
##'              speed   =~ x7 + x8 + x9
##'              visual ~ textual + speed
##' '
##'
##' fit2 <- sem(regmodel1, data = HolzingerSwineford1939, std.lv = TRUE, meanstructure = TRUE)
##'
##' fit2.std <- update(fit2, std.lv = TRUE, std.ov = TRUE, meanstructure = TRUE) 
##'
##' fit2.t <- semTable(list("Ordinary" = fit2, "Standardized" = fit2.std), fit = "rmsea",
##'                    colLabels = list("Ordinary" = c("est" = "Est", "se" = "Std.Err.", "p" = "p"),
##'                                    "Standardized" = c("estsestars" = "Standardized Est.")),
##'                    paramSets = c("loadings", "slopes", "latentcovariances"),
##'                    file = "../jasper", type = c("latex", "csv"))
##' cat(fit2.t)
##' 
##' fit2.t <- semTable(list("Ordinary" = fit2, "Standardized" = fit2.std), type = "html")
##'
##' regmodel2 <- 'visual  =~ x1 + x2 + x3
##'              textual =~ x4 +  x6
##'              speed   =~  x8 + x9
##'              visual ~ textual 
##' '
##' fit3 <- sem(regmodel2, data = HolzingerSwineford1939, std.lv = TRUE, meanstructure = TRUE)
##' 
##' fit3.std <- update(fit2, std.lv = TRUE, std.ov = TRUE)
##'
##' semTable(list("Mod 1" = fit2, "Mod 1 std" = fit2.std, "Mod 2" = fit3, "Mod 3 std" = fit3.std),
##'                  colLabels = c("estse" = "Est(S.E.)"), file = "../jasper")
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
##' fit3.t2 <- semTable(fit3, colLabels = c("est" = "Est (MLE)", "se" = "Std.Err."))
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
##'     fit = c("tli", "chisq"),
##'     names_fit = c("TLI", "chisq"), type = "html")
##' fit4.t2 <- semTable(fit4, fit = c("rmsea", "tli", "chisq"),
##'     names_fit = c("RMSEA", "TLI", "chisq"), type = "latex")
##' 
##' 
##' ## Example with file output requested in the command
##' ## semTable(output, file = "catTable.tex",
##' ##    paramSets = c("loadings", "thresholds", "residualvariances"),
##' ##    fit = c("tli", "chisq"),
##' ##    names_fit = c("TLI", "chisq"), type = "latex")
##' }
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
                                "thresholds" = "Thresholds",
                                "fits" = "Fit Indices"),
             fit = c("chisq", "cfi", "tli", "rmsea"),
             fitLabels = toupper(fit),
             colLabels = c("est" = "Estimate", "se" = "SE", "z" = "z", "p" = "p"),
             standardized = FALSE, 
             names_upper = TRUE, type = "latex",
             group = NULL, longtable = FALSE, alpha =  c(0.05, 0.01, 0.001))
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
        colLabels <- intersect(c("est", "se", "stdest", "stdse", "z", "rsquare"), colnames(trows))
        for (i in colLabels) trows[ , i] <- frnd(trows[ , i])
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
            if (any(!paramSets %in% names(paramSetsLabels))){
                MESSG <- "fixParamSets: invalid paramSets"
                stop(MESSG)
            }
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

    ## relabel the "rowType" column in a parTable data frame
    ##
    ## Re-lable the "rowType", the parameterSet indicator for each
    ## row in a parTable. Identifies "residualvariances",
    ## "residualcovariances", "latentvariances" "latentcovariances",
    ## "intercepts" and "latentmeans"
    ##  
    ## @param parameters A parTable data frame, with estimates and some labels. 
    ## @return 
    ## @author Paul Johnson
    relabelParamSets <- function(parameters){
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
                   paste0(dframe[ , "est"], "(", dframe[ , "se"], ")",
                          if(stars)dframe[ , "starsig"] else ""),
                   paste0(dframe[ , "est"]))
        }
        
        if(class(object)[1] != "lavaan"){
            stop("object is not a lavaan output object.")
        }

        parList <- object@ParTable[
                              intersect(names(object@ParTable),
                                        c("lhs", "op", "rhs", "free", "group",
                                          "est", "se", "label", "plabel"))]
        parameters <- as.data.frame(parList, stringsAsFactors=FALSE)
        rsquare <- lavInspect(object, what = "rsquare")
        
        parameters$z <- ifelse(parameters$free != 0 & parameters$se != 0,
                               parameters$est/parameters$se, NA)
        parameters$p <- 2*pnorm(abs(parameters$z), lower.tail = FALSE)
        parameters$starsig <- starsig(parameters$p, alpha = alpha, symbols = starsymbols)
        parameters$estse <- createEstSE(parameters)
        parameters$estsestars <- createEstSE(parameters, stars = TRUE)

        if(length(object@Data@group.label) > 0L) {
                parameters$group.label <- unlist(onemodel@Data@group.label)[parameters$group]
        } else {
            parameters$group.label <- parameters$group
        }
        ## items previously global are specialized to this model
        attr(parameters, "variables") <- unique(unlist(object@Data@ov.names))
        attr(parameters, "latents") <- unique(unlist(object@pta$vnames$lv))
        attr(parameters, "params") <- fixParamSets(paramSets, parameters)

        parameters2 <- relabelParamSets(parameters)
        ## parse rsquare among
        ## "indicators"
        parameters2[ , "rsquare"] <- as.numeric("")
        parameters2[parameters2$rowType == "loadings", "rsquare"] <-
            rsquare[parameters2$rhs[parameters2$rowType == "loadings"]]
        ## "latentvariances"
        ## "latentsquares" are NA if there is no regression fitted,
        ## must be cautious here
        latentrsquares <- rsquare[parameters2[parameters2$rowType == "latentvariances", "lhs"]]
        parameters2[parameters2$rowType == "latentvariances", "rsquare"] <-
            ifelse(!is.na(latentrsquares), latentrsquares, NA)
        parameters2 <- roundSubtable(parameters2)
        parameters2
    }

    ## The trows objects have an attribute "title" and this
    ## formats that
    applyTitleMarkup <- function(title){
        header  <- "_BR_"
        ## Tricky business b/c row markup does not call for _BOC_ in column 1,
        ## that's provided by "_BR_".
        header <- paste0(header, gsub("_CONTENT_", title$title, title$markup), "_EOR_")
        header
    }
    
    makeSubtableTitle <- function(title, colnum = 1, width = 1, center = TRUE, underline = TRUE){
        colalign <- if(center) "_BOMC" else "_BOML"
        ## Need a separator if colnum > 1. This is ripple effect of removing & from _BOMC_ definition.
        ## Because we assume it is always BOMC, now must add _BOC_ before _BOMC_
        prefix <- if (colnum > 1) paste0("_EOC_", rep("_BOC__EOC_", colnum - 2), "_BOC_")
        markup <- if(underline) {
                      paste0(prefix, colalign, width, "__UL__CONTENT__EOUL__EOMC_")
                  } else {
                      paste0(prefix, colalign, width, "__CONTENT__EOMC_")
                  }
        title <- list(title = title, markup = markup,
                      colnum = colnum, width = width)
    }

    
    ##works for paramSets = "loadings" or "slopes"
    loadingMaker <- function(parameters, rowType = "loadings", colLabels, modelName) {
        report <- names(colLabels[[modelName]])
        totalNcolumns <- min(9,  length(unname(unlist(colLabels))))
        trows <- parameters[parameters$rowType == rowType,  , drop = TRUE]
        ## The split gotcha
        ## prevent accidental reordering of rows when the factor is created
        ## automatially
        lhsfactor <- factor(trows$lhs, levels = unique(trows$lhs))
        trowsplit <- split(trows, f = lhsfactor, drop = FALSE)
        info <- lapply(trowsplit, function(x){
            ## drop gotcha, if x has one row from df now it is a list.
            if(class(x)[1] == "list") x <- as.data.frame(x)
            vname <- unique(x$lhs)
            ##if(rowType == "slopes") browser()
            rownames(x) <- paste(rowType, vname, x[ , "rhs"], sep = ".")
            x <- data.frame(col1 = x$rhs, x[ , report, drop = FALSE])
            ## don't put "_BOC_" at beginning if in colnum 1
            attr(x, "title") <- makeSubtableTitle(vname, colnum = 1, width = 1,
                                    center = FALSE,  underline = TRUE)
            class(x) <- c("trows", class(x))
            x
        })
        info
    }

    ## works for other paramSets, ones that are simple tables, no lists.
    ## replace the "maker" functions with one function.
    ## Only difference between maker functions was in calculation of col1 and
    ## the rownames, which we can customize here
    ## rowType in c("intercepts", "means" "latentmeans", "latentvariances","residualcovariances"
    ## Check for complications: "latentcovariances" "residualvariances"
    ## "thresholds" "covariances"
    parTableMaker <- function(trows, rowType, colLabels, modelName, col1name = "lhs"){
        report <- names(colLabels[[modelName]])
        totalNcolumns <- min(9,  length(unname(unlist(colLabels))))
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
            trows$col1 <- trows$lhs
            rownames(trows) <- paste0(rowType, trows[ , "lhs"], sep = ".")
        } else if (rowType == "latentcovariances"){
            ## browser()
            ## Needed?
            ## trows <- parameters[parameters$rowType == rowType, , drop = FALSE]
            trows$col1 <- trows$lhs
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
        ## build response for "chisq"
        chimeas <- object@Fit@test[[1]]
        chimeas$stat <- frnd(chimeas$stat)
        chimeas$pvalue <- frnd(chimeas$pvalue, 3,  3)
        chimeas$pvalue <- gsub("0\\.", "\\.", chimeas$pvalue)
        namenew <- paste0("_CHI2_", "(", chimeas$df, ")")
        chisq <- paste0(namenew, "=", chimeas$stat , "(p=", chimeas$pvalue,")")
        names(chisq) <- "_CHI2_"
        chisq
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
        res <- paste0(apply(trowsf, 1, paste, collapse = ""), collapse = "\n")
        ## TODO 20171028 CAUTION: workaround here b/c title info sometimes missing, must
        ## go back find why it is missing
        if (!is.null(attr(trows, "title"))){
            header <- applyTitleMarkup(attr(trows, "title"))
            res <- paste0(header, "\n", res, "\n")
        }
        return(res)
    } 


    ## Work on one fitted model.  Cycle through process of
    ## 1. Retrieve parameters,
    ## 2. insert title attributes to be used when formatting
    ## Creates a list of tables, one for each parameter type.
    ## These are found by splitting on the "rowType" variable in
    ## the paramTable.
    extractParameters <- function(paramTable, colLabels, modelName){
        params <- attr(paramTable, "params")
        ## report was name used for varnames of columns for
        ## keeing, ex: c("est" "se" "z" "p")
        report <- names(colLabels[[modelName]])

        ## explore following as alternative concept to manage calculations
        ## creates a list of tables for each parameter type, so no need to do
        ## data selection within the table maker function
        paramTableSplit <- split(paramTable, f = paramTable$rowType, drop = FALSE)
        ## Still must treat loadings and slopes differently
        reslt <- list()
        for(jj in names(paramTableSplit)){
            if (jj %in% c("loadings", "slopes")){
                info <- loadingMaker(paramTableSplit[[jj]], rowType = jj,
                                     colLabels, modelName)
                if (!is.null(info)){
                    attr(info, "title") <- makeSubtableTitle(paramSetsLabels["loadings"],
                                                             colnum = 2,
                                                             width = length(report))
                    class(info) <- c("trowsList", class(info))
                }
                reslt[[jj]] <- info
            } else {
                reslt[[jj]] <- parTableMaker(paramTableSplit[[jj]], rowType = jj, colLabels,
                                            modelName = modelName, col1name = "lhs")
            }
        }
        reslt
    }

    ## Gather summary fit indicators
    fitMaker <- function(object, colLabels, modelName) {
        report <- names(colLabels[[modelName]])
        totalNcolumns <- min(9,  length(unname(unlist(colLabels))))
        ## retrieve fit values, will be named vector
        fitmeas <- lavaan::fitMeasures(object)[]
        fitmeas <- sapply(fitmeas, frnd)
        fitmeas <- fitmeas[fit]
        fitmeas[is.na(fitmeas)] <- ""
        
        ## re-label the fit indices
     
        browser()
        ## chisq is a special presentation
        if ("chisq" %in% fit){
            chiresult <- getChiSq(object)
            fitmeas <- c(chiresult, fitmeas)
        }
        
        info <- data.frame(col1 = fitLabels, row.names = fit)
        info[ , report] <- ""
        info[ , report[1]] <- fitmeas
        
        attr(info, "title") <- makeSubtableTitle(paramSetsLabels["fits"],
                                                 colnum = 2,
                                                 width = totalNcolumns,
                                                 underline = TRUE)
        
        class(info) <- c("trowsList", class(info))
        info
    }

    

    ## Given a list of tables (say, 3 variances tables) this puts them
    ## together side by side, with markup.
    ## If one table is NULL, replace with empty DF
    ## for others, expand rows to same size
    buildParamSetSubtable <- function(tablList, colLabels){
        ## xxx table unique rownames and col1 values, to find all possible vars
        xxx <- lapply(tablList, function(x) cbind(rownames = rownames(x),
                                                  col1 = x[ , "col1"]))
        paramnames <- unique(do.call(rbind, xxx))
        rownames(paramnames) <- paramnames[ , "rownames"]
        for(jj in names(tablList)){
            ## if paramSet lacks desired table, make empty table for it
            if (is.null(tablList[[jj]])) {
                y <- data.frame(col1 = paramnames[ , "col1"],
                                matrix("-", ncol = length(colLabels[[jj]]),
                                       nrow = NROW(paramnames),
                                       dimnames = list(paramnames[ , "rownames"],
                                                       names(colLabels[[jj]]))))
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


    ## take paramList from environment, then iterate through the sections
    ## and impose markup needed.
    finalizeMarkup <- function(paramSetNames, colLabels){
        colNameCounts <- lapply(colLabels, length)
        colHeaderRow <- unname(unlist(colLabels))
        totalNcolumns <- min(9,  length(colHeaderRow))## If more than 9, give up at centering

        buildSubtableForParamSet <- function(jj) {
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
                    hh[[i]] <- buildParamSetSubtable(subList, colLabels)
                }
                res <- paste0(hh, collapse = " ")
                ## Improvise a section heading for loadings and slopes
                title <- makeSubtableTitle(paramSetsLabels[jj], colnum = 2,
                                           width = totalNcolumns, underline = TRUE)
                header <- applyTitleMarkup(title)
                res <- paste0(header, res)
            } else {
                res <- buildParamSetSubtable(tablList, colLabels)
            }
            res
        }
        
        results <- lapply(paramSetNames, buildSubtableForParamSet)
        results2 <- paste(results, collapse = "")
        colHeaderRow <- paste("_BR__EOC__BOC_", paste(colHeaderRow, collapse = "_EOC__BOC_"), "_EOC__EOR_\n _HL_\n")
        modelHeaderRow <- paste0("_BR__EOC__BOC_",
                                 paste0("_BOMC", colNameCounts, "_", names(colNameCounts), "_EOMC_", collapse = "_BOC_"),
                                 "_EOR_\n _HL_\n", collapse = " ")
        tablesuffix <-  paste0("_LB_",
                               paste0("_FIXED_", "Fixed parameter_LB_", "\n"),
                               if (any(c("estsestars") %in% sapply(colLabels, names)))
                                   paste0(starsymbols, " p < ", alpha, collapse = ", "))

        resmark <- paste("_BT_\n", modelHeaderRow, colHeaderRow, results2, "_HL__EOT_\n", tablesuffix)
        resmark
    }


    updateLabels <- function(labelsold, labelsnew, augment = FALSE){
        if (is.null(names(labelsold)) || is.null(names(labelsnew))){
            MESSG <- "updateLabels only works with named vectors"
            stop(MESSG)
        }
        labelsold.names <- names(labelsold)
        labelsnew.names <- names(labelsnew)
        inboth <- intersect(labelsnew.names, labelsold.names)
        if(!augment){
            if(any(!labelsnew.names %in% labelsold.names)){
                MESSG("some labels not in original name vector. Dropped!")
                warning(MESSG)
               
                labelsold[inboth] <- labelsnew[inboth]
                return(labelsold)
            }
        } else {
            labelsold[inboth] <- labelsnew[inboth]
            labelsold <- c(labelsold, labelsnew[subset(labelsold.names, inboth)])
        }
        labelsold    
    }
 
        
        ##
        if (missing(labelsold)) {
            names(labelsnew) <- x
            return(labelsnew)
        }

        if (is.null(names(labelsold))) {
            if(!missing(x)){
                names(labelsold) <- x
            } else {
                MESSG <- "labelsold should have names or x should be supplied"
                stop(MESSG)
            }
        }

        if(is.null(names(labelsnew))){
            if(length(labelsnew) == length(labelsold)){
                labelsold <- labelsnew
            } else {
                MESSG <- "unnamed new labels should match length of old labels"
                stop(MESSG)
            }
            if (!missing(x)){
                names(labelsold) <- x
            }
        }

        
        
        
        if(is.null(names(labelsnew))) names(labelsnew) <- labelsnew
        
    }

    if (missing(xlabels)){
        if(!missing(xlabels) && !is.null
        x <- names(xlabels)
    } 

    parseXandXLabels <- function(x, xlabels){
        xarg <- deparse(substitute(x))
        xlabelsarg <- deparse(substitute(xlabels))
        if(missing(
        if(missing){deparse(substitute(xlabels))}{
             return(defaults)
        ## x has no names, xlabels has no names
        ## does nothing except give names to labels
        if(is.vector(x) && is.vector(xlabels) && is.null(names(x))
           && is.null(names(xlabels))}
            if(length(x) == length(xlabelsnew)){
                names(xlabelsnew) <- x
                return(xlabelsnew)
            } else {
                MESSG <- paste("re-labeling error concerning:", x, "and", xlabels)
            }
        if(length(xlabels)
            names(fit) <- fitLabels
        } else if (!is.null(names(fitLabels))) {
            oldnames <- names(fitLabels)
            newnames <- fitLabels
            namesnow <- names(fit)
            newnamez(fit) <- mgsub(oldnames, newnames, namesnow)
        } else {
            MESSG <- "fitMaker: fit index name error"
        }
        
    starsymbols <- c("_STAR1_", "_STAR2_", "_STAR3_")
    
    paramSetsLabelDefaults <- c("loadings" = "Factor Loadings",
                                "slopes" = "Regression Slopes",
                                "intercepts" = "Intercepts",
                                "means" = "Means",
                                "residualvariances" = "Residual Variances",
                                "residualcovariances" = "Residual Covariances",
                                "variances" = "Variances",
                                "latentvariances" = "Latent Variances",
                                "latentcovariances" = "Latent Covariances", 
                                "latentmeans" = "Latent Intercepts",
                                "thresholds" = "Thresholds",
                                "fits" = "Fit Indices")
            
    ## A generic name for models. If list is not provided, there's
    ## no name, so use this
    mname <- "Model" # modelname
    
    ## Next fixes the colLabels
    ## convert colLabels to a list, one per object
    if (!is.list(colLabels)) {
        colLabels.orig <- colLabels
        if (is.list(object)){
            colLabels <- lapply(object, function(x) colLabels)
        } else {
            colLabels <- list(colLabels)
            names(colLabels) <- mname
        }
    } else {
        if (length(colLabels) != length(object) || names(colLabels) != names(object)){
            MESSG <- "object list and colLabels list must match"
        }
    }

    ## clean up name, since est(se) is not a legal R name
    colLabels <- lapply(colLabels, function(x){
        names(x) <- gsub("est(se)", "estse", names(x))
        x
    })
    
 
    paramList <- list()
    ## If one model is supplied
    if (!is.list(object)) {
    
        object <- list(object)
        names(object)[1] <- mname
    }

    ## if one fitted model and 2 or more groups found in there
    if ((length(object) == 1) && ((G <- (object[[1]])@Data@ngroups) > 1)){
        onemodel <- object[[1]] 
        ## Use group labels, not numbers
        if(length(onemodel@Data@group.label) > 0L) {
            groupNames <- unlist(onemodel@Data@group.label)
        }
        ## CAUTION 20171103 assumes groups are numbered 1, 2, 3
        ## in same order as retrieved by @Data@group.label.
        parTable <- getParamTable(onemodel)
        parTableSplit <- split(parTable,
                               f = factor(parTable$group.label,
                                          levels = unique(parTable$group.label)))
        colLabels <- lapply(parTableSplit, function(x) colLabels.orig)
        for(i in names(parTableSplit)){
            paramList[[i]] <- extractParameters(parTableSplit[[i]], colLabels=colLabels,
                                                modelName = i)
            paramList[[i]][["fits"]] <- fitMaker(onemodel, colLabels, ii)
        }
        
        if(length(onemodel@Data@group.label) > 0L) {
            names(paramList) <- unlist(onemodel@Data@group.label)
        }
        attr(paramList, "G") <- G
    } else {
        ## more models, abort if any are multigroup
        G <- lapply(object, function(x) x@Data@ngroups)
        if(any(G > 1)) {
            MESSG <- "Several Multi Group models are not understandable"
        }
        ## each model object's parameters are pulled
        ## paramList <- lapply(names(object, extractParameters, colLabels)
        for(ii in names(object)){
            paramTable <- getParamTable(object[[ii]])
            paramList[[ii]] <- extractParameters(paramTable, colLabels, modelName = ii)
            paramList[[ii]][["fits"]] <- fitMaker(object[[ii]], colLabels, ii)
        }
    }
    
    paramSetsFound <- unique(unlist(lapply(paramList, function(x) names(x))))
    ## re-order paramSetsFound according to standard list
    paramSetNames <- intersect(names(paramSetsLabels), paramSetsFound)
  
    markedResults <- finalizeMarkup(paramSetNames, colLabels)
    
    result <- markupConvert(markedResults, type = type,
                            longtable = longtable, file = file,
                            colLabels = colLabels)
    attr(result, "markedResults") <- markedResults
    result
}
NULL



##' Convert marked-up characters to latex, html, or csv
##'
##' The conversion key tables are included in the code of the function
##' @param marked A character string
##' @param type Output type, can be a vector or any one of "latex",
##'     "html", and "csv".
##' @param longtable should a tabular or a longtable object be created?
##' @param file A file stub, to which ".tex", ".html", or ".csv" can be added
##' @param colLabels For SEM table, the list of colLabels objects
##' @return a list of marked up character objects
##' @author Paul Johnson
markupConvert <- function(marked, type = c("latex", "html", "csv"),
                        longtable = FALSE, file = NULL, colLabels)
{
    ##num of columns, except for col1
    Ncolumns <- length(unname(unlist(colLabels)))
    
    ## Replacement strings for LaTeX output
    latexreplace <- c(
        "_LB_" = "\\\n",
        "_EOC_" =  "",
        "_BOC_" = "& ", 
        "_EOMC_" = "}",
        "_EOR_" = "\\\\tabularnewline\n",
        "_BRU_" = "",
        "_BRT_" = "", 
        "_BOCU_" = "& ",
        "_BR_" = "",
        "_BT_" = if(longtable) paste0("\\\\begin{longtable}{l",
                                      paste0(rep("r", Ncolumns), collapse = ""), "}")
                 else paste0("\\\\begin{tabular}{l",
                             paste0(rep("r", Ncolumns), collapse = ""), "}"),
        "_EOL_" = "\n",
        "_HL_" = "\\\\hline", 
        "_UL_" = "\\\\underline{",
        "_EOUL_" = "}",
        "_SEPU_" = " &", 
        "_SEP_" = " &", 
        "_EOT_" = if (longtable) "\\\\end{longtable}" else "\\\\end{tabular}",
        "_BOMR1_" = "& \\\\multirow{1}{c}{",
        "_BOMR2_" = "& \\\\multirow{2}{c}{",
        "_BOMC1_" = "\\\\multicolumn{1}{c}{",
        "_BOMC2_" = "\\\\multicolumn{2}{c}{",
        "_BOMC3_" = "\\\\multicolumn{3}{c}{",
        "_BOMC4_" = "\\\\multicolumn{4}{c}{",
        "_BOMC5_" = "\\\\multicolumn{5}{c}{",
        "_BOMC6_" = "\\\\multicolumn{6}{c}{",
        "_BOMC7_" = "\\\\multicolumn{7}{c}{",
        "_BOMC8_" = "\\\\multicolumn{8}{c}{",
        "_BOMC9_" = "\\\\multicolumn{9}{c}{",
        "_BOML1_" = "\\\\multicolumn{1}{l}{",
        "_BOML2_" = "\\\\multicolumn{2}{l}{",
        "_BOML3_" = "\\\\multicolumn{3}{l}{",
        "_BOML4_" = "\\\\multicolumn{4}{l}{",
        "_BOML5_" = "\\\\multicolumn{5}{l}{",
        "_BOML6_" = "\\\\multicolumn{6}{l}{",
        "_BOML7_" = "\\\\multicolumn{7}{l}{",
        "_BOML8_" = "\\\\multicolumn{8}{l}{",
        "_BOML9_" = "\\\\multicolumn{9}{l}{",
        "_BOMCT1_" = "\\\\multicolumn{1}{c}{",
        "_BOMCT2_" = "\\\\multicolumn{2}{c}{",
        "_BOMCT3_" = "\\\\multicolumn{3}{c}{",
        "_BOMCT4_" = "\\\\multicolumn{4}{c}{",
        "_HTMLHL_" = "",
        "_CHI2_" = "$\\\\chi^2)$",
        "_R2_" = "$R^2$",
        "_SIGMA_" = "$\\\\sigma$",
        "_NBSP_" = " ",
        "_FIXED_" = "$^+$",
        "_STAR1_" = "$^{*}$",
        "_STAR2_" = "$^{**}$",
        "_STAR3_" = "$^{***}$"
    )

    ## Replacement strings for HTML output
    ## TODO: 20171102: refactor abbreviations
    ## Problem in output is duplicate <td><td ..>, workaround in last item"
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
        "_BT_" =  "<table cellpadding=\"10\">\n",
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
        "_BOML1_" = "<td colspan = '1'; align = 'left'>",
        "_BOML2_" = "<td colspan = '2'; align = 'left'>",
        "_BOML3_" = "<td colspan = '3'; align = 'left'>",
        "_BOML4_" = "<td colspan = '4'; align = 'left'>",
        "_BOML5_" = "<td colspan = '5'; align = 'left'>",
        "_BOML6_" = "<td colspan = '6'; align = 'left'>",
        "_BOML7_" = "<td colspan = '7'; align = 'left'>",
        "_BOML8_" = "<td colspan = '8'; align = 'left'>",
        "_BOML9_" = "<td colspan = '9'; align = 'left'>",
        "_BOMCT1_" = "<td colspan = '1'; style=\"border-top: solid thin black; border-collapse:collapse;\">&nbsp;",
        "_BOMCT2_" = "<td colspan = '2'; style=\"border-top: solid thin black; border-collapse:collapse;\">&nbsp;",
        "_BOMCT3_" = "<td colspan = '3'; style=\"border-top: solid thin black; border-collapse:collapse;\">&nbsp;",
        "_BOMCT4_" = "<td colspan = '4'; align = 'center'; ; style=\"border-top: solid thin black; border-collapse:collapse;\">&nbsp;",
        "_HTMLHL_" = "<tr><td colspan = '5'; align = 'center'; ; style=\"border-top: solid thin black; border-collapse:collapse;\">&nbsp;</tr>",
        "_CHI2_" = "&chi;<sup>2</sup>",
        "_R2_" = "R<sup>2</sup>",
        "_SIGMA_" = "&sigma;",
        "_NBSP_" = "&nbsp;",
        "_FIXED_" = "<sup>+</sup>",
        "_STAR1_" = "<sup>*</sup>",
        "_STAR2_" = "<sup>**</sup>",
        "_STAR3_" = "<sup>***</sup>",
        "<td><td" = "<td"
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
        "_BOML1_" = "",
        "_BOML2_" = "",
        "_BOML3_" = "",
        "_BOML4_" = "",
        "_BOML5_" = "",
        "_BOML6_" = "",
        "_BOML7_" = "",
        "_BOML8_" = "",
        "_BOML9_" = "",
        "_BOMCT1_" = "",
        "_BOMCT2_" = "",
        "_BOMCT3_" = "",
        "_BOMCT4_" = "",
        "_HTMLHL_" = "",
        "_CHI2_" = "chi^2",
        "_R2_" = "R^2",
        "_SIGMA_" = "sigma",
        "_NBSP_" = " ",
        "_FIXED_" = "+",
        "_STAR1_" = "*",
        "_STAR2_" = "**",
        "_STAR3_" = "**"
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
         result[["html"]] <- htmlmarkup
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
