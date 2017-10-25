##' Creates Structural Equation Modeling Tables
##'
##' Creates LaTeX markup for structural equation modeling output
##' tables in the style of the American Psychological
##' Association(APA). Input objects should be created by the
##' "\code{lavaan}" package.
##'
##' The argument params determines the inclusion of estimate sections.
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
##' variable variances and covariances.
##' \item "latentmeans" are the latent variable
##' means (or intercepts).
##' \item "thresholds" arise in latent
##' response variates (non-numeric indicator data).
##' }
##'
##' The stantardized parameter regulates the number of columns to be
##' included.  standardized=FALSE implies there will be four columns:
##' \enumerate{
##' \item the estimate
##' \item the standard error
##' \item the z-value, and
##' \item the p-value.
##' }
##'
##' When standardized = TRUE, the columns are:
##' \enumerate{
##' \item the parameter estimates,
##' \item the standard errors,
##' \item standardized parameter estimates, and
##' \item standardized standard errors.
##' }
##'
##' The standardized parameters are obtained by updating the output
##' with the options std.lv = TRUE and std.ov = TRUE.  If these
##' options were used when originally creating output, setting
##' standardized = TRUE has no effect.
##'
##' @param object A lavaan object returned by cfa() or sem().
##' @param file Output file name.  Default is NULL, meaning no output
##'     file.
##' @param params Parameter sets to be included. Valid values are
##'     "loadings", "slopes", "intercepts", "means", "residuals",
##'     "covariances", "latentvariances", "latentmeans" and
##'     "thresholds". Default is "all" (but excludes "means"). See
##'     Details.
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
##' @param cols A named vector of column names for main table. Default
##'     is c("est" = "Estimate", "se" = "SE", "z" = "z", "p" = "p").
##'     User may remove some columns from the table by omitting them.
##' @param standardized If TRUE and model was not fit with
##'     std.ov=std.lv=TRUE, refit model with those arguments and add
##'     new report columns named "stdest" and "stdse".  Default is
##'     FALSE.
##' @param names_upper Should the names of the model fit parameters be
##'     forced to be uppercase.  The default is TRUE.  This will also
##'     affect whatever is specified in names_fit.
##' @param single_spaced Default = TRUE. If a double-spaced table is
##'     needed, set single_spaced = FALSE.
##' @param type Choose either "latex" or "html".
##' @param includeGroup Should group membership be reported for
##'     parameters in a column on the right? Defaults to FALSE.
##' @param group Group for which parameters should be reported. Only
##'     relevant for multiple group models. If not supplied,
##'     parameters for all groups will be reported, along with a
##'     column indicating the group (if includeGroup = TRUE).
##' @param longtable If TRUE, use longtable for LaTeX
##'     documents. Default is FALSE.
##' @importFrom stats pnorm
##' @return Markup for SEM table. Use cat to display it.
##' @export
##' @author Ben Kite <bakite@@ku.edu>
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
##' cat(fit1.t1)
##' fit1.t2 <- semTable(fit1, fit = c("chisq", "rmsea"), standardized = TRUE)
##' cat(fit1.t2)
##' fit1.t3 <- semTable(fit1, fit = c("chisq", "rmsea", "tli"),
##'                     cols = c("est" = "Estimates", "se" = "Std.Err."),
##'                     standardized = TRUE)
##' cat(fit1.t3)
##' 
##' ## Can create file if desired
##' ## cat(fit1.2, file = "table1.2.tex")
##' ## Basic SEM
##' regmodel <- "x1 ~ x2 + x3
##' x1 ~1"
##' fit2 <- sem(regmodel, data = HolzingerSwineford1939, std.lv = TRUE, std.ov = TRUE)
##' fit2.t <- semTable(fit2, fit = "rmsea", type = "html")
##' cat(fit2.t)
##' #### Example with file output
##' ##semTable(output1, file = "exampleTable.html", fit = "rmsea",
##' ##standardized = TRUE, params = c("loadings", "latentvariances"),
##' ##type = "html")
##' fit3 <- sem(regmodel, data = HolzingerSwineford1939, group = "school")
##' fit3.t1 <- semTable(fit3)
##' cat(fit3.t1)
##' fit3.t2 <- semTable(fit3, cols = c("est" = "Est (MLE)", "se" = "Std.Err."))
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
##' fit4.t1 <- semTable(fit4, params = c("loadings", "thresholds", "residuals"),
##'     fit = c("tli", "chi-square"),
##'     names_fit = c("TLI", "chi-square"), type = "html")
##' fit4.t2 <- semTable(fit4, fit = c("rmsea", "tli", "chi-square"),
##'     names_fit = c("RMSEA", "TLI", "chi-square"), type = "latex")
##' 
##' 
##' ## Example with file output requested in the command
##' ## semTable(output, file = "catTable.tex",
##' ##    params = c("loadings", "thresholds", "residuals"),
##' ##    fit = c("tli", "chi-square"),
##' ##    names_fit = c("TLI", "chi-square"), type = "latex")
##' }

semTable <-
    function(object, file = NULL, params = "all",
             fit = c("chi-square", "cfi", "tli", "rmsea"),
             names_fit = fit,
             cols = c("est" = "Estimates", "se" = "SE",
                        "z" = "z", "p" = "p"),
             standardized = FALSE, 
             names_upper = TRUE, single_spaced = TRUE, type = "latex",
             includeGroup = FALSE,
             group = NULL, longtable = FALSE)
{

    ## do.call(rbind, alist) does not accept stringsAsFactors=FALSE,
    ## must set globally to avoid hassle
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
        cols <- intersect(c("est", "se", "stdest", "stdse", "z"), colnames(trows))
        for (i in cols) trows[ , i] <- frnd(trows[ , i])
        trows$p <- frnd(trows$p, 3, 3)
        trows$p <- gsub("0\\.", "\\.", trows$p)
        trows$est <- ifelse(trows$free == 0, paste0(trows$est, "*"), trows$est)
        trows[trows$free == 0, intersect(colnames(trows), c("se", "z", "p", "stdse"))] <- ""
        trows
    }


    ## If params != "all", follow user request to select params for table
    ## If params == "all", then
    ## 1. remove "variances" and insert "residuals" "covariances" "latentvariances"
    ## 2. remove "means" and insert "intercepts" and "latentmeans"
    cleanParams <- function(params, parameters) {
        if (params != "all") {
            return(unique(params))
        }
        
        paramops <- c("=~" = "loadings", "~" = "slopes", "~1" = "means",
                      "~~" = "variances", "|" = "thresholds")
        params <- paramops[unique(parameters$op)]
        names(params) <- NULL
        if ("variances" %in% params){
            params <- params[!params %in% "variances"]
            if (length(which(parameters$rhs %in% variables &
                             parameters$lhs %in% variables &
                             parameters$op == "~~")) > 0) {
                params <- c(params, "residuals")
            } 
            if (length(which(parameters$lhs %in% variables &
                             parameters$rhs %in% variables &
                             parameters$lhs != parameters$rhs &
                             parameters$op == "~~")) > 0){
                params <- c(params, "covariances")
            }
            if (length(which(parameters$rhs %in% latents &
                             parameters$lhs %in% latents &
                             parameters$op == "~~")) > 0){
                params <- c(params, "latentvariances")
            }
        }
        if ("means" %in% params){
            params <- params[!params %in% "means"]
            if(length(which(parameters$lhs %in% variables &
                            parameters$op == "~1")) > 0){
                params <- c(params, "intercepts")
            }
            if(length(which(parameters$lhs %in% latents &
                            parameters$op == "~1")) > 0){
                params <- c(params, "latentmeans")
            }
        }
        params
    }
    
    if(class(object)[1] != "lavaan"){
        stop("object is not a lavaan output object.")
    }
    

    ## Create objects to be used within function
    ## Extract parameters into a data.frame
    parTable <- object@ParTable[
                           intersect(names(object@ParTable),
                                     c("lhs", "op", "rhs", "free", "group", "est", "se"))]
    parameters <- as.data.frame(parTable, stringsAsFactors=FALSE)
    parameters$z <- parameters$est/parameters$se
    parameters$p <- 2*pnorm(abs(parameters$z), lower.tail = FALSE)

    variables <- unlist(object@Data@ov.names)
    latents <- unique(unlist(object@pta$vnames$lv))

    params <- cleanParams(params, parameters)
    
    ## Handle which group to make the table for here
    #if (!is.null(group)){
    #    includeGroup <- FALSE
    #    if (!is.null(parameters$group)){
    #        gval <- which(object@Data@group.label %in% as.character(group))
    #        if (!gval%in% unique(parameters$group)){
    #            stop(paste0("The value provided to the group argument is not valid."))
    #        }
    #        parameters <- parameters[parameters$group == gval,]
    #    }
    #} else {
    #    if (!is.null(parameters$group)){
    #        includeGroup <- TRUE
    #    } else {
    #        includeGroup <- FALSE
    #    }
    #}



    template <- "
_BT_
_HL_
STANDARDIZED
_BRU_Parameter_EOC_ REPORT
_HL_
_FACTORLOADINGS__SLOPES__INTERCEPTS__MEANS__THRESHOLDS__RESIDUALS__COVARIANCES__LATENTVARS__LATENTMEANS__HL__HTMLHL_
_EOT_

Note. IDENTNOTEFITINFORMATION
"
    ## in previous, name "report" was used for vector 
    ## ("est", "se", "z", "p")
    report <- names(cols)
    
    ## If standardized and model is not already standardized, fit new standardized
    ## model and grab coefficients as "stdest" and "stdse"
    if(isTRUE(standardized) && (!object@Options$std.lv | !object@Options$std.ov)){
        report <- c(report, "stdest", "stdse")
        cols <- c(cols, "Estimate (Std.)", "SE (Std.)")

        std <- update(object, std.lv = TRUE, std.ov = TRUE)
        parameters$stdest <- std@Fit@est
        parameters$stdse <- std@Fit@se
        ## holder <-  "_BRT__EOC__BOMCT2__UL_Unstandarized_EOUL__EOMC_ _BOMCT2__UL_Standardized_EOUL__EOMC__EOR_"
        ## template <- gsub("STANDARDIZED", holder, template)
        ## holder <- "_BOCU_NAME_EOC_"
        ##reportx <- list()
        ##columnNames <- c("Estimate", "SE", "Estimate", "SE")
        ##if(isTRUE(includeGroup)){
        ##    columnNames <- c(columnNames, "Group")
        ## }
        ## for(i in 1:length(columnNames)){
        ##    reportx[i] <- gsub("NAME", columnNames[i], holder)
        ##
        ##}
        ##columnnames <- paste0(paste0(reportx, collapse = " "), "_EOR_")
    }

    if(isTRUE(includeGroup)){
        report <- c(report, "group")
        cols <- c(cols, "Groups")
    }
    
    ## template <- gsub("REPORT", columnnames, template)

    if (single_spaced == TRUE){
        template <- gsub("SINGLESPACE", "\\\\usepackage{setspace}", template)
    }else{
        template <- gsub("SINGLESPACE", "", template)
    }


    loadingMaker <- function(lvname, report = c("est", "se", "z", "p")){
        trows <- parameters[which(parameters$rhs %in% variables &
                                  parameters$lhs %in% lvname &
                                  parameters$op == "=~"), ,
                            drop = FALSE]
        trows <- roundSubtable(trows)
        trows <- trows[ , c("rhs", report)]
        rownames(trows) <- paste(lvname, trows[ , "rhs"], sep = ".")
        colnames(trows)[1] <- "col1" ## first col name must be homogeneous
        ## don't put "_BOC_" at beginning if in colnum 1
        title <- list(title = lvname,
                      markup = "_UL__CONTENT__EOUL__EOC_",
                      colnum = 1)
        attr(trows, "title") <- title
        trows
      }

    interceptMaker <- function(variables, report = c("est", "se", "z", "p")){
        ints <- parameters[which(parameters$op == "~1" & parameters$lhs %in% variables), "lhs"]
        trows <- parameters[which(parameters$lhs %in% ints & parameters$op == "~1"),, drop = FALSE]
        if(dim(trows)[1] == 0)
            stop("Intercept estimates are requested in the table, but I can't find them in the output!")
        trows <- roundSubtable(trows)
        trows <- trows[ , c("lhs", report)]
        colnames(trows)[1] <- "col1" ## first col name must be homogeneous"
        title <- list(title = "Intercepts",
                      markup = paste0("_BOMC", length(report), "__UL__CONTENT__EOUL__EOMC_"),
                      colnum = 2)
        attr(trows, "title") <- title
        trows
    }

    observedMeanMaker <- function(variables, regs, report = c("est", "se", "z", "p")){
        ivs <- unique(regs$rhs)
        trows <- parameters[which(parameters$lhs %in% ivs & parameters$op == "~1"),, drop = FALSE]
        if(dim(trows)[1] == 0)
            stop("Predictor variable mean estimates missing in output!")
        trows <- roundSubtable(trows)
        trows <- trows[ , c("lhs", report)]
        colnames(trows)[1] <- "col1" ## first col name must be homogeneous
        title <- list(title = "Means",
                      markup = paste0("_BOMC", length(report), "__UL__CONTENT__EOUL__EOMC_"),
                      colnum = 2)
        attr(trows, "title") <- title
        trows
    }

    slopeMaker <- function(dv, regs, report = c("est", "se", "z", "p")){
        dvname <- dv
        trows <- regs[which(regs$lhs == dvname), , drop = FALSE]
        trows <- roundSubtable(trows)
        trows <- trows[ , c("rhs", report)]
        rowempty <- character(NCOL(trows))
        rowempty[2] <- "UL_dvname_EOUL_" 
        trows <- rbind(rowempty, trows)
        colnames(trows)[1] <- "col1" ## first col name must be homogeneous"
        title <- list(title = dvname,
                      markup = "_UL__CONTENT__EOUL__EOC_",
                      colnum = 1)
        attr(trows, "title") <- title
        trows
    }

    thresholdMaker <- function(variables, report = c("est", "se", "z", "p")){
        trows <- parameters[which(parameters$op == "|" & parameters$lhs %in% variables),,
                            drop = FALSE]
        if(dim(trows)[1] == 0)
            stop("Threshold estimates are missing in output!")
        thresnum <- substring(trows$rhs, 2, nchar(trows$rhs))
        trows$lhs <- paste0(trows$lhs, "(", thresnum, ")")
        trows <- roundSubtable(trows)
        trows <- trows[ , c("lhs", report)]
        colnames(trows)[1] <- "col1" ## first col name must be homogeneous"
        title <- list(title = "Thresholds",
                      markup = paste0("_BOMC", length(report), "__UL__CONTENT__EOUL__EOMC_"),
                      colnum = 2)
        attr(trows, "title") <- title
        trows
    }
    

    residualMaker <- function(variables, covariance = FALSE, report = c("est", "se", "z", "p")){
        trows <- parameters[which(parameters$rhs %in% variables &
                                  parameters$lhs %in% variables &
                                  parameters$op == "~~"),,
                            drop = FALSE ]
        if(dim(trows)[1] == 0) stop("residualMaker failure")
        trows <- roundSubtable(trows)
        if (isTRUE(covariance)){
            trows <- trows[which(trows$rhs != trows$lhs),]
            trows <- trows[ , c("lhs", report)]
            colnames(trows)[1] <- "col1" ## first col name must be homogeneous"
            title <- list(title = "Covariances",
                          markup = paste0("_BOMC", length(report), "__UL__CONTENT__EOUL__EOMC_"),
                          colnum = 2)
            attr(trows, "title") <- title
            return(trows)
        } else {
            trows <- trows[which(trows$rhs == trows$lhs),, drop = FALSE]
            trows <- trows[ , c("lhs", report)]
            colnames(trows)[1] <- "col1" ## first col name must be homogeneous"
            title <- list(title = "Variances",
                          markup = paste0("_BOMC", length(report), "__UL__CONTENT__EOUL__EOMC_"),
                          colnum = 2)
            attr(trows, "title") <- title
            return(trows)
        }
    }

    latentMaker <- function(latents, report = c("est", "se", "z", "p")){
        trows <- parameters[which(parameters$rhs %in% latents &
                                  parameters$lhs %in% latents &
                                  parameters$op == "~~"),,
                            drop = FALSE]
        if(dim(trows)[1] == 0){
            stop("Latent variance/covariance estimates missing in output!")
        }
        trows <- roundSubtable(trows)
        trows[ , "lhs"] <- paste(trows[ , "lhs"], " w/ ", trows[ , "rhs"])
        trows <- trows[ , c("lhs", report)]
        colnames(trows)[1] <- "col1" ## first col name must be homogeneous
        title <- list(title = "Latent Var/Cov",
                      markup = paste0("_BOMC", length(report), "__UL__CONTENT__EOUL__EOMC_"),
                      colnum = 2)
        attr(trows, "title") <- title
        trows
     }

    latentMeanMaker <- function(latents, report = c("est", "se", "z", "p")){
        trows <- parameters[which(parameters$lhs %in% latents & parameters$op == "~1"),,
                            drop = FALSE]
        if(dim(trows)[1] == 0)
            stop("Latent mean estimates missing in output!")
        
        trows <- roundSubtable(trows)
        trows <- trows[ , c("lhs", report)]
        colnames(trows)[1] <- "col1" ## first col name must be homogeneous"
        title <- list(title = "Latent Means/Intercept",
                      markup = paste0("_BOMC", length(report), "__UL__CONTENT__EOUL__EOMC_"),
                      colnum = 2)
        attr(trows, "title") <- title
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
       
    ## Receive a trows object, which has attribute "title",
    ## intersperse rows with column markup, then insert title
    ## information
    ## format = c("allcol", "1col")
    markupTable <- function(trows, format = "4col") {
        trowsf <- trows
        for(i in 1:(NCOL(trowsf) -1)){
            trowsf[ , i] <- paste0(trowsf[ , i], "_EOC__BOC_")
        }
        trowsf[ , 1] <- paste0("_BR_", trowsf[, 1])
        trowsf[ , NCOL(trows)] <- paste0(trowsf[ , NCOL(trowsf)], "_EOC__EOR_")
        res <- paste(apply(trowsf, 1, paste, collapse = " "), collapse = "\n")
        header <- getTitleMarkup(trows)
        paste(header, "\n", res, "\n")
    }
    
    reslt <- list()
    if("loadings" %in% params){
        loadingInfo <- lapply(latents, loadingMaker, report = report)
        ## a list of 3 trows objects
        ## this is a title for the collection of lists
        title <- list(title = c("Factor Loadings"),
                      markup = paste0("_BOMC", length(report), "__UL__CONTENT__EOUL__EOMC_"),
                      colnum = 2)
        attr(loadingInfo, "title") <- title
        reslt[["loadings"]] <- loadingInfo
    }
        
    if("slopes" %in% params){
        regs <- parameters[which(parameters$op == "~"), ]
        dvs <- unique(regs$lhs)
        slopeInfo <- lapply(dvs, slopeMaker, regs = regs, report = report)
        slopeInfo <- do.call(rbind, slopeInfo)
        title <- list(title = c("Regression Slopes"),
                      markup = paste0("_BOMC", length(report), "__UL__CONTENT__EOUL__EOMC_"),
                      colnum = 2)
        attr(slopeInfo, "title") <- title
        reslt[["slopes"]] <- slopeInfo
    }
    
    if("intercepts" %in% params){
        reslt[["intercepts"]] <- interceptMaker(variables, report = report)
    }

    if("means" %in% params){
        regs <- parameters[which(parameters$op == "~"),]
        reslt[["means"]] <- observedMeanMaker(variables, regs = regs, report = report)
    }

    if("thresholds" %in% params){
        reslt[["thresholds"]] <- thresholdMaker(variables, report = report)
    }
    if("residuals" %in% params){
        reslt[["residuals"]] <- residualMaker(variables, covariance = FALSE, report = report)
    }
    if("covariances" %in% params){
        reslt[["covariances"]] <- residualMaker(variables, covariance = TRUE, report = report)
    }

    if("latentvariances" %in% params){
        reslt[["latentvariances"]] <- latentMaker(latents, report = report)
    }

    if("latentmeans" %in% params){
        reslt[["latentmeans"]] <- latentMeanMaker(latents, report = report)
    }

    sumry <- list()
    if (1){
        rowempty <- character(length = 1 + length(report))
        rowempty <- "_BOMC3_ * Indicates parameters fixed for model identification._EOMC__LB_\n" 
        sumry[["fixedparam"]] <- rowempty
    }
    
    if (!is.null(fit)){
        rowempty <- character(length = 1 + length(report))
        rowempty <- paste("_BOMC3_", fitMaker(object), "_EOMC__LB_")
        sumry[["summaries"]] <- rowempty
    }
    
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
        "_NBSP_" = " "
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
        "_NBSP_" = "&nbsp;"
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
        "_NBSP_" = " "
    )

    ## Now work on the markup
    ## Iterate through reslt, treating "loadings" and "slopes" differently
    browser()
    resmark <- paste(unlist(lapply(names(reslt), function(tab){
        if(length(grep(tab, c("loadings", "slopes"))) > 0){
            header <- getTitleMarkup(reslt[[tab]])
            subtables <- vapply(reslt[[tab]], markupTable, character(1))
            c(header, subtables)
        } else {
            markupTable(reslt[[tab]])
        }
    })), collapse = " ")

    
    resmark <- paste("_BT_\n", resmark, "_EOT_\n")
    if (type == "latex"){
        latexmarkup <- mgsub(names(latexreplace), latexreplace, resmark)
        if (!is.null(file)){
            cat(latexmarkup, file = file)
        }
    }

    if (type == "html"){
         htmlmarkup <- mgsub(names(htmlreplace), htmlreplace, resmark)
         if (!is.null(file)){
             cat(htmlmarkup, file = file)
         }
    }

    csvmarkup <- mgsub(names(csvreplace), csvreplace, resmark)
   

    list(latex = latexmarkup, html= htmlmarkup, csv = csvmarkup)
}

