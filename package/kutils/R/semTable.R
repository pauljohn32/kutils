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
##' standardized = TRUE will yield two identical sets of two columns.
##'
##' @param object A lavaan object returned by cfa() or sem().
##' @param file Character string for file name.  Default is NULL,
##'     meaning no output file.
##' @param params Parameters to be included. Valid values are
##'     "loadings", "slopes", "intercepts", "means", "residuals",
##'     "covariances", "latentvariances", "latentmeans" and
##'     "thresholds". Defaults to "all" which includes all available
##'     parameters (but excludes "means"). See Details.
##' @param fit A vector of fit measures that to be included in the
##'     note. Listing "chi-square" will do special formatting to the
##'     chi-square value in the note. Any other measures listed must
##'     correspond to measures found in fitMeasures(object).
##' @param names_fit Names for the fit measures requested by the fit
##'     parameter.  Must have same number of elements as fit.  For
##'     example, fit = c("cfi.scaled", "tli.scaled"), names_fit =
##'     c("CFI", "TLI").
##' @param standardized Should standarized results be presented along
##'     with unstandardized?  Default is FALSE. See Details.
##' @param names_upper Should the names of the model fit parameters be
##'     forced to be uppercase.  The default is TRUE.  This will also
##'     affect whatever is specified in names_fit.
##' @param single_spaced Default = TRUE. If a double-spaced table is
##'     needed, set single_spaced = FALSE.
##' @param type Type of output table ("latex" or "html"). Defaults to
##'     "latex".
##' @param group Group for which parameters should be
##'     reported. Provide the value in the data that indicates the
##'     desired group. Only necessary for multiple group
##'     models. Defaults to NULL.
##' @param longtable Should a latex longtable be generated? Defaults
##'     to FALSE, which makes the table tabular. Ignored if type =
##'     "html".
##' @importFrom stats pnorm
##' @return SEM table of desired type.
##' @export
##' @author Ben Kite <bakite@@ku.edu>
##' @examples
##' \donttest{
##' ## These run longer than 5 seconds
##' ## CFA model
##' require(lavaan)
##' HS.model <- ' visual  =~ x1 + x2 + x3
##' textual =~ x4 + x5 + x6
##' speed   =~ x7 + x8 + x9'
##' output1 <- cfa(HS.model, data = HolzingerSwineford1939, std.lv = TRUE)
##' semTable(output1, fit = "rmsea",
##' standardized = TRUE, type = "latex")
##' ## Basic SEM model
##' regmodel <- "x1 ~ x2 + x3
##' x1 ~1"
##' output1a <- sem(regmodel, data = HolzingerSwineford1939, std.lv = TRUE)
##' semTable(output1a, fit = "rmsea", type = "html")
##' #### Example with file output
##' ##semTable(output1, file = "exampleTable.html", fit = "rmsea",
##' ##standardized = TRUE, params = c("loadings", "latentvariances"),
##' ##type = "html")
##'
##' model <- "factor =~ .7*y1 + .7*y2 + .7*y3 + .7*y4
##' y1 | -1*t1 + 1*t2
##' y2 | -.5*t1 + 1*t2
##' y3 | -.2*t1 + 1*t2
##' y4 | -1*t1 + 1*t2
##' "
##' dat <- simulateData(model, sample.nobs = 300)
##' testmodel <- "ExampleFactor =~ y1 + y2 + y3 + y4"
##' output2 <- cfa(testmodel, data = dat, ordered = colnames(dat),
##'     std.lv = FALSE)
##' semTable(output2,
##'     params = c("loadings", "thresholds", "residuals"),
##'     fit = c("tli", "chi-square"),
##'     names_fit = c("TLI", "chi-square"), type = "html")
##'
##' ## Example with file output
##' ## semTable(output, file = "catTable.tex",
##' ##    params = c("loadings", "thresholds", "residuals"),
##' ##    fit = c("tli", "chi-square"),
##' ##    names_fit = c("TLI", "chi-square"), type = "latex")
##' }

semTable <-
    function(object, file = NULL, params = "all",
             fit = c("chi-square", "cfi", "tli", "rmsea"),
             names_fit = fit, standardized= FALSE,
             names_upper = TRUE, single_spaced = TRUE, type = "latex",
             group = NULL, longtable = FALSE)
{
    if(class(object)[1] != "lavaan"){
        stop(paste("The object does not appear to be",
                   "a lavaan object.  Only lavaan cfa object can be",
                   "used with the CFATable function."))
    }
    if (!is.null(fit)){
        if(names_upper == TRUE){
            names(fit) <- toupper(names_fit)
        }else{
            names(fit) <- names_fit
        }

        fitmeas <- lavaan::fitMeasures(object)[]
        fitmeas <- sapply(fitmeas, function(x) formatC(round(x, 3),
                                                       format = 'f', digits = 2))
    }
    chimeas <- object@Fit@test[[1]]
    chimeas$stat <- formatC(round(chimeas$stat, 3), format = 'f', digits = 2)
    chimeas$pvalue <- formatC(round(chimeas$pvalue, 3), format = 'f', digits = 3)
    chimeas$pvalue <- gsub("0\\.", "\\.", chimeas$pvalue)
    parameters <- data.frame(object@ParTable)[,c("lhs", "op", "rhs", "free", "group")[c("lhs", "op", "rhs", "free", "group") %in% names(object@ParTable)]]
    parameters$est <- object@Fit@est
    parameters$se <- object@Fit@se
    ##parameters <- parameters[which(parameters$free > 0),]
    parameters$z <- parameters$est/parameters$se
    parameters$p <- 2*pnorm(abs(parameters$z), lower.tail = FALSE)
    parameters[,"est"] <- round(parameters[,"est"], 2)
    parameters[,"se"] <- round(parameters[,"se"], 2)
    parameters[,"z"] <- round(parameters[,"z"], 2)
    parameters[,"p"] <- round(parameters[,"p"], 3)
    parameters[,"rhs"] <- as.character(parameters[,"rhs"])
    parameters[,"lhs"] <- as.character(parameters[,"lhs"])
    variables <- unlist(object@Data@ov.names)
    latents <- unique(unlist(object@pta$vnames$lv))
    if (length(params) == 1){
        if (params == "all"){
            params <- unique(as.character(parameters$op))
            paramops <- c("=~" = "loadings", "~" = "slopes", "~1" = "means",
                          "~~" = "variances", "|" = "thresholds")
            params <- paramops[params]
            names(params) <- NULL
            if ("variances" %in% params){
                params <- params[!params %in% "variances"]
                if (length(which(parameters$rhs %in% variables & parameters$lhs %in% variables & parameters$op == "~~")) > 0){
                    params <- c(params, "residuals")
                }
                if (length(which(parameters$lhs %in% variables & parameters$rhs %in% variables & parameters$lhs != parameters$rhs & parameters$op == "~~")) > 0){
                    params <- c(params, "covariances")
                }
                if (length(which(parameters$rhs %in% latents & parameters$lhs %in% latents & parameters$op == "~~")) > 0){
                    params <- c(params, "latentvariances")
                }
            }
            if ("means" %in% params){
                params <- params[!params %in% "means"]
                if(length(which(parameters$lhs %in% variables & parameters$op == "~1")) > 0){
                    params <- c(params, "intercepts")
                }
                if(length(which(parameters$lhs %in% latents & parameters$op == "~1")) > 0){
                    params <- c(params, "latentmeans")
                }
            }
        }
    }

    ## Handle which group to make the table for here
    if (!is.null(group)){
        if (!is.null(parameters$group)){
            gval <- which(object@Data@group.label %in% as.character(group))
            if (!gval%in% unique(parameters$group)){
                stop(paste0("The value provided to the group argument is not valid."))
            }
            parameters <- parameters[parameters$group == gval,]
        }
    }

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

    if(standardized == TRUE){
        report <- c("est", "se", "stdest", "stdse")
        std <- update(object, std.lv = TRUE, std.ov = TRUE)
        parameters$stdest <- std@Fit@est
        parameters$stdse <- std@Fit@se
        holder <-  "_BRT__EOC__BOMCT2__UL_Unstandarized_EOUL__EOMC_ _BOMCT2__UL_Standardized_EOUL__EOMC__EOR_"
        template <- gsub("STANDARDIZED", holder, template)
        holder <- "_BOCU_NAME_EOC_"
        reportx <- list()
        columnNames <- c("Estimate", "SE", "Estimate", "SE")
        for(i in 1:length(columnNames)){
            reportx[i] <- gsub("NAME", columnNames[i], holder)

        }
        columnnames <- paste0(paste0(reportx, collapse = " "), "_EOR_")

    }else{
        report <- c("est", "se", "z", "p")
        parameters$stdest <- NA
        parameters$stdse <- NA

        template <- gsub("STANDARDIZED", "", template)

        holder <- "_BOCU_NAME_EOC_"
        reportx <- list()
        columnNames <- c("Estimate", "SE", "z", "p")
        for(i in 1:length(columnNames)){
            reportx[i] <- gsub("NAME", columnNames[i], holder)

        }

        columnnames <- paste0(paste0(reportx, collapse = " "), "_EOR_")
    }

    template <- gsub("REPORT", columnnames, template)

    if (single_spaced == TRUE){
        template <- gsub("SINGLESPACE", "\\\\usepackage{setspace}", template)
    }else{
        template <- gsub("SINGLESPACE", "", template)
    }

    loads <- as.list(latents)
    ints <- parameters[which(parameters$op == "~1" & parameters$lhs %in% names(dat)), "lhs"]
    regs <- parameters[which(parameters$op == "~"),]
    dvs <- unique(regs$lhs)
    ivs <- unique(regs$rhs)

    loadingMaker <- function(loads, report = c("est", "se", "z", "p")){
        lvname <- loads
        trows <- parameters[which(parameters$rhs %in% variables & parameters$lhs %in% lvname & parameters$op == "=~"), c("lhs", "rhs", "est", "se", "z", "p", "free", "stdest", "stdse")]
        trows$est <- formatC(round(trows$est, 3), format = 'f', digits = 2)
        trows$se <- formatC(round(trows$se, 3), format = 'f', digits = 2)
        trows$stdest <- formatC(round(trows$stdest, 3), format = 'f', digits = 2)
        trows$stdse <- formatC(round(trows$stdse, 3), format = 'f', digits = 2)
        trows$z <- formatC(round(trows$z, 3), format = 'f', digits = 2)
        trows$p <- formatC(round(trows$p, 3), format = 'f', digits = 3)
        trows$p <- gsub("0\\.", "\\.", trows$p)
        trows$est <- ifelse(trows$free == 0, paste0(trows$est, "*"), trows$est)
        trows$z <- ifelse(trows$free == 0, "", trows$z)
        trows$p <- ifelse(trows$free == 0, "", trows$p)
        tmpx <- "_BR__UL_FACTOR_EOUL__EOC_ _BOC__EOC_ _BOC__EOC_ _BOC__EOC_ _BOC__EOC_ _EOR_
ROWINFORMATION"
        tmpx <- gsub("FACTOR", lvname, tmpx)
        rowinfo <- paste0("_BR_", paste0(trows[1,c("rhs", report)], collapse = " _EOC__BOC_ "), "_EOR_\n")
        for (i in 2:nrow(trows)){
            rowinfo <- paste0(rowinfo, paste0("_BR_", paste0(trows[i,c("rhs", report)], collapse = " _EOC__BOC_ "), "_EOR_\n"))
        }
        tmpx <- gsub("ROWINFORMATION", rowinfo, tmpx)
        tmpx
    }

    interceptMaker <- function(variables, report = c("est", "se", "z", "p")){
        trows <- parameters[which(parameters$lhs %in% ints & parameters$op == "~1"), c("lhs", "rhs", "est", "se", "z", "p", "free", "stdest", "stdse")]
        if(dim(trows)[1] == 0){
            stop("Intercept estimates are requested in the table, but I can't find them in the output!")
            #return(print("It appears that no intercept estimates are present in the lavaan output"))
        }else{
        trows$est <- formatC(round(trows$est, 3), format = 'f', digits = 2)
        trows$se <- formatC(round(trows$se, 3), format = 'f', digits = 2)
        trows$stdest <- formatC(round(trows$stdest, 3), format = 'f', digits = 2)
        trows$stdse <- formatC(round(trows$stdse, 3), format = 'f', digits = 2)
        trows$z <- formatC(round(trows$z, 3), format = 'f', digits = 2)
        trows$p <- formatC(round(trows$p, 3), format = 'f', digits = 3)
        trows$p <- gsub("0\\.", "\\.", trows$p)
        trows$est <- ifelse(trows$free == 0, paste0(trows$est, "*"), trows$est)
        trows$z <- ifelse(trows$free == 0, "", trows$z)
        trows$p <- ifelse(trows$free == 0, "", trows$p)
        tmpx <- "_BR__EOC_ _BOMC4__UL_Intercepts_EOUL__EOMC_ _EOR_
ROWINFORMATION"
        rowinfo <- paste0("_BR_", paste0(trows[1,c("lhs", report)], collapse = " _EOC__BOC_ "), "_EOR_\n")
        if (nrow(trows) > 1){
            for (i in 2:nrow(trows)){
                rowinfo <- paste0(rowinfo, paste0("_BR_", paste0(trows[i,c("lhs", report)], collapse = " _EOC__BOC_ "), "_EOR_\n"))
            }
        }
        tmpx <- gsub("ROWINFORMATION", rowinfo, tmpx)
        tmpx
        }
    }

    observedMeanMaker <- function(variables, report = c("est", "se", "z", "p")){
        trows <- parameters[which(parameters$lhs %in% ivs & parameters$op == "~1"), c("lhs", "rhs", "est", "se", "z", "p", "free", "stdest", "stdse")]
        if(dim(trows)[1] == 0){
            stop("Predictor variable mean estimates are requested in the table, but I can't find them in the output!")
            #return(print("It appears that no intercept estimates are present in the lavaan output"))
        }else{
        trows$est <- formatC(round(trows$est, 3), format = 'f', digits = 2)
        trows$se <- formatC(round(trows$se, 3), format = 'f', digits = 2)
        trows$stdest <- formatC(round(trows$stdest, 3), format = 'f', digits = 2)
        trows$stdse <- formatC(round(trows$stdse, 3), format = 'f', digits = 2)
        trows$z <- formatC(round(trows$z, 3), format = 'f', digits = 2)
        trows$p <- formatC(round(trows$p, 3), format = 'f', digits = 3)
        trows$p <- gsub("0\\.", "\\.", trows$p)
        trows$est <- ifelse(trows$free == 0, paste0(trows$est, "*"), trows$est)
        trows$z <- ifelse(trows$free == 0, "", trows$z)
        trows$p <- ifelse(trows$free == 0, "", trows$p)
        tmpx <- "_BR__EOC_ _BOMC4__UL_Means_EOUL__EOMC_ _EOR_
ROWINFORMATION"
        rowinfo <- paste0("_BR_", paste0(trows[1,c("lhs", report)], collapse = " _EOC__BOC_ "), "_EOR_\n")
        if (nrow(trows) > 1){
            for (i in 2:nrow(trows)){
                rowinfo <- paste0(rowinfo, paste0("_BR_", paste0(trows[i,c("lhs", report)], collapse = " _EOC__BOC_ "), "_EOR_\n"))
            }
        }
        tmpx <- gsub("ROWINFORMATION", rowinfo, tmpx)
        tmpx
        }
    }

    slopeMaker <- function(dv, regs, report = c("est", "se", "z", "p")){
        dvname <- dv
        trows <- regs[which(regs$lhs == dvname), c("lhs", "rhs", "est", "se", "z", "p", "free", "stdest", "stdse")]
        trows$est <- formatC(round(trows$est, 3), format = 'f', digits = 2)
        trows$se <- formatC(round(trows$se, 3), format = 'f', digits = 2)
        trows$stdest <- formatC(round(trows$stdest, 3), format = 'f', digits = 2)
        trows$stdse <- formatC(round(trows$stdse, 3), format = 'f', digits = 2)
        trows$z <- formatC(round(trows$z, 3), format = 'f', digits = 2)
        trows$p <- formatC(round(trows$p, 3), format = 'f', digits = 3)
        trows$p <- gsub("0\\.", "\\.", trows$p)
        trows$est <- ifelse(trows$free == 0, paste0(trows$est, "*"), trows$est)
        trows$z <- ifelse(trows$free == 0, "", trows$z)
        trows$p <- ifelse(trows$free == 0, "", trows$p)
        tmpx <- "_BR__UL_DEPENDENTVAR_EOUL__EOC_ _BOC__EOC_ _BOC__EOC_ _BOC__EOC_ _BOC__EOC_ _EOR_
ROWINFORMATION"
        tmpx <- gsub("DEPENDENTVAR", dvname, tmpx)
        rowinfo <- paste0("_BR_", paste0(trows[1,c("rhs", report)], collapse = " _EOC__BOC_ "), "_EOR_\n")
        if (nrow(trows) > 1){
            for (i in 2:nrow(trows)){
                rowinfo <- paste0(rowinfo, paste0("_BR_", paste0(trows[i,c("rhs", report)], collapse = " _EOC__BOC_ "), "_EOR_\n"))
            }
        }
        tmpx <- gsub("ROWINFORMATION", rowinfo, tmpx)
        tmpx
    }

    thresholdMaker <- function(variables, report = c("est", "se", "z", "p")){
        trows <- parameters[which(parameters$op == "|" & parameters$lhs %in% variables), c("lhs", "rhs", "est", "se", "z", "p", "free", "stdest", "stdse")]
        if(dim(trows)[1] == 0){
            stop("Threshold estimates are requested in the table, but I can't find them in the output!")
        }else{
        thresnum <- substring(trows$rhs, 2, nchar(trows$rhs))
        trows$lhs <- paste0(trows$lhs, "(", thresnum, ")")
        trows$est <- formatC(round(trows$est, 3), format = 'f', digits = 2)
        trows$se <- formatC(round(trows$se, 3), format = 'f', digits = 2)
        trows$stdest <- formatC(round(trows$stdest, 3), format = 'f', digits = 2)
        trows$stdse <- formatC(round(trows$stdse, 3), format = 'f', digits = 2)
        trows$z <- formatC(round(trows$z, 3), format = 'f', digits = 2)
        trows$p <- formatC(round(trows$p, 3), format = 'f', digits = 3)
        trows$p <- gsub("0\\.", "\\.", trows$p)
        trows$est <- ifelse(trows$free == 0, paste0(trows$est, "*"), trows$est)
        trows$z <- ifelse(trows$free == 0, "", trows$z)
        trows$p <- ifelse(trows$free == 0, "", trows$p)
        tmpx <- "_BR__EOC_ _BOMC4__UL_Thresholds_EOUL__EOMC_ _EOR_
ROWINFORMATION"
        rowinfo <- paste0("_BR_", paste0(trows[1,c("lhs", report)], collapse = " _EOC__BOC_ "), "_EOR_\n")
        for (i in 2:nrow(trows)){
            rowinfo <- paste0(rowinfo, paste0("_BR_", paste0(trows[i, c("lhs", report)], collapse = " _EOC__BOC_ "), "_EOR_\n"))
        }
        tmpx <- gsub("ROWINFORMATION", rowinfo, tmpx)
        tmpx
        }
    }

    residualMaker <- function(variables, covariance = FALSE, report = c("est", "se", "z", "p")){
        trows <- parameters[which(parameters$rhs %in% variables & parameters$lhs %in% variables & parameters$op == "~~"), c("lhs", "rhs", "est", "se", "z", "p", "free", "stdest", "stdse")]
        if (isTRUE(covariance)){
            trows <- trows[which(trows$rhs != trows$lhs),]
            tmpx <- "_BR__EOC_ _BOMC4__UL_Covariances_EOUL__EOMC__EOR_
ROWINFORMATION"
            if(dim(trows)[1] == 0){
                stop("Observed variable covariance estimates are requested in the table, but I can't find them in the output!")
            }
        } else {
            trows <- trows[which(trows$rhs == trows$lhs),]
            tmpx <- "_BR__EOC_ _BOMC4__UL_Variances_EOUL__EOMC__EOR_
ROWINFORMATION"
            if(dim(trows)[1] == 0){
                stop("Variance estimates are requested in the table, but I can't find them in the output!")
            }
        }
        trows$est <- formatC(round(trows$est, 3), format = 'f', digits = 2)
        trows$se <- formatC(round(trows$se, 3), format = 'f', digits = 2)
        trows$stdest <- formatC(round(trows$stdest, 3), format = 'f', digits = 2)
        trows$stdse <- formatC(round(trows$stdse, 3), format = 'f', digits = 2)
        trows$z <- formatC(round(trows$z, 3), format = 'f', digits = 2)
        trows$p <- formatC(round(trows$p, 3), format = 'f', digits = 3)
        trows$z <- ifelse(trows$free == 0, "", trows$z)
        trows$p <- ifelse(trows$free == 0, "", trows$p)
        trows$p <- gsub("0\\.", "\\.", trows$p)
        if (isTRUE(covariance)){
            rowinfo <- paste0("_BR_", trows[1,"lhs"], " with ", trows[1,"rhs"], " _EOC__BOC_ ", paste0(trows[1,report], collapse = " _EOC__BOC_ "), "_EOR_\n")
            if (nrow(trows) > 1){
                for (i in 2:nrow(trows)){
                    rowinfo <- paste0(rowinfo, paste0("_BR_", trows[i,"lhs"], " with ", trows[i,"rhs"], " _EOC__BOC_ ", paste0(trows[i,report], collapse = " _EOC__BOC_ "), "_EOR_\n"))
                }
            }
        } else {
            rowinfo <- paste0("_BR_", paste0(trows[1,c("lhs", report)], collapse = " _EOC__BOC_ "), "_EOR_\n")
            if (nrow(trows) > 1){
                for (i in 2:nrow(trows)){
                    rowinfo <- paste0(rowinfo, paste0("_BR_", paste0(trows[i,c("lhs", report)], collapse = " _EOC__BOC_ "), "_EOR_\n"))
                }
            }

        }
        tmpx <- gsub("ROWINFORMATION", rowinfo, tmpx)
        tmpx
    }

    latentMaker <- function(latents, report = c("est", "se", "z", "p")){
        trows <- parameters[which(parameters$rhs %in% latents & parameters$lhs %in% latents & parameters$op == "~~"), c("lhs", "rhs", "est", "se", "z", "p", "free", "stdest", "stdse")]
        if(dim(trows)[1] == 0){
            stop("Latent variance/covariance estimates are requested in the table, but I can't find them in the output!")
            #return(print("It appears that no intercept estimates are present in the lavaan output"))
        }else{
            trows$est <- formatC(round(trows$est, 3), format = 'f', digits = 2)
            trows$se <- formatC(round(trows$se, 3), format = 'f', digits = 2)
            trows$stdest <- formatC(round(trows$stdest, 3), format = 'f', digits = 2)
            trows$stdse <- formatC(round(trows$stdse, 3), format = 'f', digits = 2)
            trows$z <- formatC(round(trows$z, 3), format = 'f', digits = 2)
            trows$p <- formatC(round(trows$p, 3), format = 'f', digits = 3)
            trows$p <- gsub("0\\.", "\\.", trows$p)
            trows$est <- ifelse(trows$free == 0, paste0(trows$est, "*"), trows$est)
            trows$z <- ifelse(trows$free == 0, "", trows$z)
            trows$p <- ifelse(trows$free == 0, "", trows$p)
            tmpx <- "_BR__EOC__BOMC4__UL_Latent Variances/Covariances_EOUL__EOMC__EOR_
ROWINFORMATION"
            rowinfo <- paste0("_BR_", trows[1,1], " with ", trows[1,2], " _EOC__BOC_ ", paste0(trows[1,report], collapse = " _EOC__BOC_ "), "_EOR_\n")
            if (nrow(trows) > 1){
                for (i in 2:nrow(trows)){
                    rowinfo <- paste0(rowinfo, paste0("_BR_", trows[i,1], " with ", trows[i,2], " _EOC__BOC_ ", paste0(trows[i,report], collapse = " _EOC__BOC_ "), "_EOR_\n"))
                }
            }
            tmpx <- gsub("ROWINFORMATION", rowinfo, tmpx)
            tmpx
        }
    }

    latentMeanMaker <- function(latents, report = c("est", "se", "z", "p")){
        trows <- parameters[which(parameters$lhs %in% latents & parameters$op == "~1"), c("lhs", "rhs", "est", "se", "z", "p", "free", "stdest", "stdse")]
        if(dim(trows)[1] == 0){
            stop("Latent mean estimates are requested in the table, but I can't find them in the output!")
            #return(print("It appears that no intercept estimates are present in the lavaan output"))
        }else{
            trows$est <- formatC(round(trows$est, 3), format = 'f', digits = 2)
            trows$se <- formatC(round(trows$se, 3), format = 'f', digits = 2)
            trows$stdest <- formatC(round(trows$stdest, 3), format = 'f', digits = 2)
            trows$stdse <- formatC(round(trows$stdse, 3), format = 'f', digits = 2)
            trows$z <- formatC(round(trows$z, 3), format = 'f', digits = 2)
            trows$p <- formatC(round(trows$p, 3), format = 'f', digits = 3)
            trows$p <- gsub("0\\.", "\\.", trows$p)
            trows$est <- ifelse(trows$free == 0, paste0(trows$est, "*"), trows$est)
            trows$z <- ifelse(trows$free == 0, "", trows$z)
            trows$p <- ifelse(trows$free == 0, "", trows$p)
            tmpx <- "_BR__EOC_ _BOMC4__UL_Latent Means/Intercepts_EOUL__EOMC_ _EOR_
ROWINFORMATION"
            rowinfo <- paste0("_BR_", paste0(trows[1,c("lhs", report)], collapse = " _EOC__BOC_ "), "_EOR_\n")
            if (nrow(trows) > 1){
                for (i in 2:nrow(trows)){
                    rowinfo <- paste0(rowinfo, paste0("_BR_", paste0(trows[i,c("lhs", report)], collapse = " _EOC__BOC_ "), "_EOR_\n"))
                }
            }
            tmpx <- gsub("ROWINFORMATION", rowinfo, tmpx)
            tmpx
        }
    }

    if("loadings" %in% params){
        loadingInfo <- lapply(loads, loadingMaker, report)
        loadingInfo <- paste0(unlist(loadingInfo), collapse = "")
        loadingInfo <- paste0("_BR__EOC__BOMC4__UL_Factor Loadings_EOUL__EOMC__EOR_", loadingInfo)
        template <- gsub("_FACTORLOADINGS_", loadingInfo, template)
    }else{
        template <- gsub("_FACTORLOADINGS_", "", template)
    }

    if("slopes" %in% params){
        slopeInfo <- lapply(dvs, slopeMaker, regs, report)
        slopeInfo <- paste0(unlist(slopeInfo), collapse = "")
        slopeInfo <- paste0("_BR__EOC__BOMC4__UL_Regression Slopes_EOUL__EOMC__EOR_", slopeInfo)
        template <- gsub("_SLOPES_", slopeInfo, template)
    }else{
        template <- gsub("_SLOPES_", "", template)
    }

    if("intercepts" %in% params){
        interceptInfo <- interceptMaker(variables, report)
        template <- gsub("_INTERCEPTS_", interceptInfo, template)
    }else{
        template <- gsub("_INTERCEPTS_", "", template)
    }

    if("means" %in% params){
        interceptInfo <- observedMeanMaker(variables, report)
        template <- gsub("_MEANS_", interceptInfo, template)
    }else{
        template <- gsub("_MEANS_", "", template)
    }

    if("thresholds" %in% params){
        thresholdInfo <- thresholdMaker(variables, report)
        template <- gsub("_THRESHOLDS_", thresholdInfo, template)
    }else{
        template <- gsub("_THRESHOLDS_", "", template)
    }
    if("residuals" %in% params){
        residualInfo <- residualMaker(variables, report)
        template <- gsub("_RESIDUALS_", residualInfo, template)
    }else{
        template <- gsub("_RESIDUALS_", "", template)
    }

    if("covariances" %in% params){
        residualInfo <- residualMaker(variables, covariance = TRUE, report)
        template <- gsub("_COVARIANCES_", residualInfo, template)
    }else{
        template <- gsub("_COVARIANCES_", "", template)
    }

    if("latentvariances" %in% params){
        latentInfo <- latentMaker(latents, report)
        template <- gsub("_LATENTVARS_", latentInfo, template)
    }else{
        template <- gsub("_LATENTVARS_", "", template)
    }

    if("latentmeans" %in% params){
        latentMeans <- latentMeanMaker(latents, report)
        template <- gsub("_LATENTMEANS_", latentMeans, template)
    }else{
        template <- gsub("_LATENTMEANS_", "", template)
    }


    #template <- gsub("TITLE", caption, template)
    if("chi-square" %in% fit){
        if (type == "latex"){
            fitinfotmpchi <- "$\\\\chi^{2}$(DF)= CHI, \\\\textit{p} = PVAL"
            fitinfotmpchi <- gsub("CHI", chimeas$stat, fitinfotmpchi)
            fitinfotmpchi <- gsub("DF", chimeas$df, fitinfotmpchi)
            fitinfotmpchi <- gsub("PVAL", chimeas$pvalue, fitinfotmpchi)
        }else{
            fitinfotmpchi <- paste0("&chi;(", chimeas$df, ") = ", chimeas$stat, ", p = ", chimeas$pvalue)
        }
    } else {
        fitinfotmpchi <- NULL
    }
    if(length(grep("[0-9]\\* ", template))){
       template <- gsub("IDENTNOTE", "* Indicates parameters fixed for model identification._LB_", template)
    }else{
        template <- gsub("IDENTNOTE", "", template)
    }

    xxx <- list()
    if (!is.null(fit)){
        for(i in names(fit)[fit != "chi-square"]){
            tmp <- paste0(i, " = value")
            if(fit[i] %in% names(fitmeas)){
                fitmeastmp <- fitmeas[fit[i]]
                xxx[i] <- gsub("value",  fitmeas[fit[i]], tmp)
            }else{
                stop(paste0("I can't find the model fit index \"", i, "\" in the lavaan output."))
            }
        }

        fitinfoothers <- paste0(unlist(xxx), collapse = "; ")

        measures <- c(fitinfotmpchi, fitinfoothers)
        if(length(xxx) > 0){
            fitinfo <- paste0(measures, collapse = "; ")
        }else{
            fitinfo <- fitinfotmpchi
        }
        template <- gsub("FITINFORMATION", paste0(fitinfo, "."), template)
    } else {
        template <- gsub("FITINFORMATION", "", template)
    }

    markup <- function(x, type) {
        if (type == "latex")
            LATEX <- TRUE
        else LATEX <- FALSE
        x <- gsub("_LB_", ifelse(LATEX, "\n", "<br>"), x)
        x <- gsub("_EOC_", ifelse(LATEX, "", "</td>"), x)
        x <- gsub("_BOC_", ifelse(LATEX, "& ", "<td>"), x)
        x <- gsub("_EOMC_", ifelse(LATEX, "}", "</td>"), x)
        x <- gsub("_EOR_", ifelse(LATEX, "\\\\tabularnewline",
            "</tr>"), x)
        x <- gsub("_BRU_", ifelse(LATEX, "", paste("<tr><td style=\"border-bottom: solid thin black; border-collapse:collapse;\">&nbsp;")),
                  x)
        x <- gsub("_BRT_", ifelse(LATEX, "", paste("<tr><td style=\"border-top: solid thin black; border-collapse:collapse;\">&nbsp;")),
                  x)
        x <- gsub("_BOCU_", ifelse(LATEX, "& ", paste("<td style=\"border-bottom: solid thin black; border-collapse:collapse;\">&nbsp;")),
                  x)
        x <- gsub("_BR_", ifelse(LATEX, "", "<tr><td>"), x)
        if (longtable){
            x <- gsub("_BT_", ifelse(LATEX, "\\\\begin{longtable}{lrrrr}", "<table>\n"), x)
        }else{
            x <- gsub("_BT_", ifelse(LATEX, "\\\\begin{tabular}{lrrrr}", "<table>\n"), x)
        }
        x <- gsub("_EOL_", "\n", x)
        x <- gsub("_HL_", ifelse(LATEX, "\\\\hline", ""), x)
        x <- gsub("_UL_", ifelse(LATEX, "\\\\underline{", "<span style=\"text-decoration: underline;\">"), x)
        x <- gsub("_EOUL_", ifelse(LATEX, "}", "</span>"), x)
        x <- gsub("_SEPU_", ifelse(LATEX, " &", paste("</td><td style=\"border-bottom: solid thin black; border-collapse:collapse;\">&nbsp;")),
            x)
        x <- gsub("_SEP_", ifelse(LATEX, " &", "</td><td>"),
            x)
        if (longtable){
            x <- gsub("_EOT_", ifelse(LATEX, "\\\\end{longtable}", "</table>"), x)
        }else{
            x <- gsub("_EOT_", ifelse(LATEX, "\\\\end{tabular}", "</table>"), x)
        }
        x <- gsub("_BOMR1_", ifelse(LATEX, "& \\\\multirow{1}{c}{",
            "<td rowspan = '1'>"), x)
        x <- gsub("_BOMR2_", ifelse(LATEX, "& \\\\multirow{2}{c}{",
            "<td rowspan = '2'>"), x)
        x <- gsub("_BOMC1_", ifelse(LATEX, "& \\\\multicolumn{1}{c}{",
            "<td colspan = '1'>"), x)
        x <- gsub("_BOMC2_", ifelse(LATEX, "& \\\\multicolumn{2}{c}{",
                                    "<td colspan = '2'>"), x)
        x <- gsub("_BOMC4_", ifelse(LATEX, "& \\\\multicolumn{4}{c}{",
                                    "<td colspan = '4'; align = 'center'>"), x)
        x <- gsub("_BOMCT1_", ifelse(LATEX, "& \\\\multicolumn{1}{c}{",
            "<td colspan = '1'; style=\"border-top: solid thin black; border-collapse:collapse;\">&nbsp;"), x)
        x <- gsub("_BOMCT2_", ifelse(LATEX, "& \\\\multicolumn{2}{c}{",
                                    "<td colspan = '2'; style=\"border-top: solid thin black; border-collapse:collapse;\">&nbsp;"), x)
        x <- gsub("_BOMCT4_", ifelse(LATEX, "& \\\\multicolumn{4}{c}{",
                                     "<td colspan = '4'; align = 'center'; ; style=\"border-top: solid thin black; border-collapse:collapse;\">&nbsp;"), x)
        x <- gsub("_HTMLHL_", ifelse(LATEX, "",
            "<tr><td colspan = '5'; align = 'center'; ; style=\"border-top: solid thin black; border-collapse:collapse;\">&nbsp;</tr>"), x)
        x <- gsub("_X2_", ifelse(LATEX, "$-2LLR (Model \\chi^2)$",
            "&chi;<sup>2</sup>"), x)
        x <- gsub("_R2_", ifelse(LATEX, "$R^2$", "R<sup>2</sup>"),
            x)
        x <- gsub("_SIGMA_", ifelse(LATEX, "$\\\\sigma$", "&sigma;"),
            x)
        x <- gsub("_NBSP_", ifelse(LATEX, " ", "&nbsp;"), x)
    }
    template <- markup(template, type)
    if(!is.null(file)){
        write(template, file)
    }
    cat(template)
    invisible(template)
}

