##' Creates Confirmatory Factor Analysis Tables
##'
##' Creates LaTeX markup for confirmatory
##' factor analysis output tables in the style of the American
##' Psychological Association(APA). Input objects should
##' be created by the "\code{lavaan}" package.
##'
##' The argument params determines the inclusion of estimate sections.
##' \itemize{
##' \item "loadings" are the factor loadings in the model.
##' \item "intercepts" are
##' the indicator variable intercepts.
##' \item "residuals" are the indicator
##' variable residual variances.
##' \item "latentvariances" are the latent
##' variable variances and covariances.
##' \item "thresholds" arise in latent
##' response variates (non-numeric indicator data).
##' }
##'
##' The stantardized parameter regulates the number of columns to be
##' included.  standardized=FALE implies there will be four columns:
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
##' @param object A cfa object from lavaan
##' @param file Name of output file. May include the path, but not the extension
##' (which is determined by the type argument).
##' Defaults to NULL which saves no file.
##' @param params Measurement parameters to be included. Valid values
##' are "loadings", "intercepts", "residuals", "latentvariances",
##' and "thresholds". See Details.
##' @param fit A vector of fit measures that to be included. Listing
##' "chi-square" will do special formatting to the chi-square
##' value in the note. Any other measures listed must correspond
##' to measures found in fitMeasures(object).
##' @param names_fit Names for the fit measures requested by the fit
##'     parameter.  Must have same number of elements as fit.  For
##'     example, fit = c("cfi.scaled", "tli.scaled"), names_fit =
##'     c("CFI", "TLI").
##' @param standardized Should standarized results be presented along
##'     with unstandardized?  Default is FALSE. See Details.
##' @param names_upper Should the names of the model fit parameters be
##'     forced to be uppercase.  The default is TRUE.  This
##'     will also affect whatever is specified in names_fit.
##' @param single_spaced Default = TRUE. If a double-spaced table is
##'     needed, set single_spaced = FALSE.
##' @param type Type of output table ("latex" or "html"). Defaults to "latex".
##' @importFrom stats pnorm
##' @return CFA table of desired type.
##' @export
##' @author Ben Kite <bakite@@ku.edu>
##' @examples
##' require(lavaan)
##' HS.model <- ' visual  =~ x1 + x2 + x3
##' textual =~ x4 + x5 + x6
##' speed   =~ x7 + x8 + x9 '
##' output1 <- cfa(HS.model, data = HolzingerSwineford1939, std.lv = TRUE)
##' cfaTable(output1, file = "exampleTable", fit = "rmsea", standardized = TRUE, 
##'          params = c("loadings", "latentvariances"), type = "latex")
##'
##'
##' model <- "factor =~ .7*y1 + .7*y2 + .7*y3 + .7*y4
##' y1 | -1*t1 + 1*t2
##' y2 | -.5*t1 + 1*t2
##' y3 | -.2*t1 + 1*t2
##' y4 | -1*t1 + 1*t2
##' "
##' dat <- simulateData(model, sample.nobs = 300)
##' testmodel <- "ExampleFactor =~ y1 + y2 + y3 + y4"
##' output <- cfa(testmodel, data = dat, ordered = colnames(dat), std.lv = FALSE)
##' cfaTable(output, file = "catTable",
##' params = c("loadings", "thresholds", "residuals"), fit = c("tli", "chi-square"),
##' names_fit = c("TLI", "chi-square"), type = "latex")

cfaTable <-
    function(object, file = NULL, params = c("loadings", "intercepts"),
             fit = c("chi-square", "cfi", "tli", "rmsea"),
             names_fit = fit, standardized= FALSE,
             names_upper = TRUE, single_spaced = TRUE, type = "latex")
{
    if(class(object)[1] != "lavaan"){
        stop(paste("The object that does not appear to be",
                   "a lavaan object.  Only lavaan cfa object can be",
                   "used with the CFATable function."))
    }
    if(object@Options$model.type != "cfa"){
        stop(paste("The output object is not cfa object.",
                   "Fit your model with the cfa function and try again."))
    }
    if(names_upper == TRUE){
        names(fit) <- toupper(names_fit)
    }else{
        names(fit) <- names_fit
    }
    fitmeas <- lavaan::fitMeasures(object)[]
    fitmeas <- sapply(fitmeas, function(x) formatC(round(x, 3), format = 'f', digits = 2))
    chimeas <- object@Fit@test[[1]]
    chimeas$stat <- formatC(round(chimeas$stat, 3), format = 'f', digits = 2)
    chimeas$pvalue <- formatC(round(chimeas$pvalue, 3), format = 'f', digits = 3)
    chimeas$pvalue <- gsub("0\\.", "\\.", chimeas$pvalue)
    parameters <- data.frame(object@ParTable)[,c("lhs", "op", "rhs", "free")]
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
    latents <- unlist(object@pta$vnames$lv)

    template <- "
_BT_
_HL_
STANDARDIZED
_BRU_Parameter_EOC_ REPORT
_HL_
FACTORLOADINGS
INTERCEPTS
THRESHOLDS
RESIDUALS
LATENTVARS
_HL_
_HTMLHL_
_EOT_

Note. IDENTNOTE
FITINFORMATION
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

    loadingMaker <- function(loads, report = c("est", "se", "z", "p")){
        lvname <- loads
        trows <- parameters[which(parameters$rhs %in% variables & parameters$lhs %in% lvname), c("lhs", "rhs", "est", "se", "z", "p", "free", "stdest", "stdse")]
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
        tmpx <- "
_BR__UL_FACTOR_EOUL__EOC_ _BOC__EOC_ _BOC__EOC_ _BOC__EOC_ _BOC__EOC_ _EOR_\n
ROWINFORMATION
"
        tmpx <- gsub("FACTOR", lvname, tmpx)
        rowinfo <- paste0("_BR_", paste0(trows[1,c("rhs", report)], collapse = " _EOC__BOC_ "), "_EOR_\n")
        for (i in 2:nrow(trows)){
            rowinfo <- paste0(rowinfo, paste0("_BR_", paste0(trows[i,c("rhs", report)], collapse = " _EOC__BOC_ "), "_EOR_\n"))
        }
        tmpx <- gsub("ROWINFORMATION", rowinfo, tmpx)
        tmpx
    }

    interceptMaker <- function(variables, report = c("est", "se", "z", "p")){
        trows <- parameters[which(parameters$lhs %in% variables & parameters$op == "~1"), c("lhs", "rhs", "est", "se", "z", "p", "free", "stdest", "stdse")]
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
        tmpx <- "
 _BR__EOC_ _BOMC4__UL_Intercepts_EOUL__EOMC_ _EOR_
ROWINFORMATION
"
        rowinfo <- paste0(paste0(trows[1,c("lhs", report)], collapse = " _EOC__BOC_ "), "_EOR_\n")
        for (i in 2:nrow(trows)){
            rowinfo <- paste0(rowinfo, paste0(paste0(trows[i,c("lhs", report)], collapse = " _EOC__BOC_ "), "_EOR_\n"))
        }
        tmpx <- gsub("ROWINFORMATION", rowinfo, tmpx)
        tmpx
        }
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
        tmpx <- "
 _BR__EOC_ _BOMC4__UL_Thresholds_EOUL__EOMC_ _EOR_
ROWINFORMATION
"
        rowinfo <- paste0("_BR_", paste0(trows[1,c("lhs", report)], collapse = " _EOC__BOC_ "), "_EOR_\n")
        for (i in 2:nrow(trows)){
            rowinfo <- paste0(rowinfo, paste0("_BR_", paste0(trows[i, c("lhs", report)], collapse = " _EOC__BOC_ "), "_EOR_\n"))
        }
        tmpx <- gsub("ROWINFORMATION", rowinfo, tmpx)
        tmpx
        }
    }

    residualMaker <- function(variables, report = c("est", "se", "z", "p")){
        trows <- parameters[which(parameters$rhs %in% variables & parameters$lhs %in% variables & parameters$op == "~~"), c("lhs", "rhs", "est", "se", "z", "p", "free", "stdest", "stdse")]
        trows$est <- formatC(round(trows$est, 3), format = 'f', digits = 2)
        trows$se <- formatC(round(trows$se, 3), format = 'f', digits = 2)
        trows$stdest <- formatC(round(trows$stdest, 3), format = 'f', digits = 2)
        trows$stdse <- formatC(round(trows$stdse, 3), format = 'f', digits = 2)
        trows$z <- formatC(round(trows$z, 3), format = 'f', digits = 2)
        trows$p <- formatC(round(trows$p, 3), format = 'f', digits = 3)
        trows$z <- ifelse(trows$free == 0, "", trows$z)
        trows$p <- ifelse(trows$free == 0, "", trows$p)
        trows$p <- gsub("0\\.", "\\.", trows$p)
        tmpx <- "
_BR__EOC_ _BOMC4__UL_Residual Variances_EOUL__EOMC__EOR_
ROWINFORMATION
"
        rowinfo <- paste0("_BR_", paste0(trows[1,c("lhs", report)], collapse = " _EOC__BOC_ "), "_EOR_\n")
        for (i in 2:nrow(trows)){
            rowinfo <- paste0(rowinfo, paste0("_BR_", paste0(trows[i,c("lhs", report)], collapse = " _EOC__BOC_ "), "_EOR_\n"))
        }
        tmpx <- gsub("ROWINFORMATION", rowinfo, tmpx)
        tmpx
    }

    latentMaker <- function(latents, report = c("est", "se", "z", "p")){
        trows <- parameters[which(parameters$rhs %in% latents & parameters$lhs %in% latents), c("lhs", "rhs", "est", "se", "z", "p", "free", "stdest", "stdse")]
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
        tmpx <- "
 _BR__EOC__BOMC4__UL_Latent Variances/Covariances_EOUL__EOMC__EOR_
ROWINFORMATION
"
        rowinfo <- paste0("_BR_", trows[1,1], " with ", trows[1,2], " _EOC__BOC_ ", paste0(trows[1,report], collapse = " _EOC__BOC_ "), "_EOR_\n")
        for (i in 2:nrow(trows)){
            rowinfo <- paste0(rowinfo, paste0("_BR_", trows[i,1], " with ", trows[i,2], " _EOC__BOC_ ", paste0(trows[i,report], collapse = " _EOC__BOC_ "), "_EOR_\n"))
        }
        tmpx <- gsub("ROWINFORMATION", rowinfo, tmpx)
        tmpx
    }

    if("loadings" %in% params){
        loadingInfo <- lapply(loads, loadingMaker, report)
        loadingInfo <- paste0(unlist(loadingInfo), collapse = "")
        loadingInfo <- paste0("_BR__EOC__BOMC4__UL_Factor Loadings_EOUL__EOMC__EOR_", loadingInfo)
        template <- gsub("FACTORLOADINGS", loadingInfo, template)
    }else{
        template <- gsub("FACTORLOADINGS", "", template)
    }
    if("intercepts" %in% params){
        interceptInfo <- interceptMaker(variables, report)
        template <- gsub("INTERCEPTS", interceptInfo, template)
    }else{
        template <- gsub("INTERCEPTS", "", template)
    }
    if("thresholds" %in% params){
        thresholdInfo <- thresholdMaker(variables, report)
        template <- gsub("THRESHOLDS", thresholdInfo, template)
    }else{
        template <- gsub("THRESHOLDS", "", template)
    }
    if("residuals" %in% params){
        residualInfo <- residualMaker(variables, report)
        template <- gsub("RESIDUALS", residualInfo, template)
    }else{
        template <- gsub("RESIDUALS", "", template)
    }
    if("latentvariances" %in% params){
        latentInfo <- latentMaker(latents, report)
        template <- gsub("LATENTVARS", latentInfo, template)
    }else{
        template <- gsub("LATENTVARS", "", template)
    }
    #template <- gsub("TITLE", caption, template)
    fitinfotmpchi <- "$\\\\chi^{2}$(DF)= CHI, \\\\textit{p} = PVAL"

    if("chi-square" %in% fit){
        fitinfotmpchi <- gsub("CHI", chimeas$stat, fitinfotmpchi)
        fitinfotmpchi <- gsub("DF", chimeas$df, fitinfotmpchi)
        fitinfotmpchi <- gsub("PVAL", chimeas$pvalue, fitinfotmpchi)
    }else{
        fitinfotmpchi <- NULL
    }
    if(length(grep("[0-9]\\* ", template))){
       template <- gsub("IDENTNOTE", "* Indicates parameters fixed for model identification.", template)
    }else{
        template <- gsub("IDENTNOTE", "", template)
    }

    xxx <- list()
    for(i in fit[fit != "chi-square"]){
        tmp <- paste0(names(fit[fit == i]), " = value")
        if(i %in% names(fitmeas)){
            xxx[i] <- gsub("value",  fitmeas[i], tmp)
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
    template <- gsub("FITINFORMATION", "", template) ## Ignoring this for now
    markup <- function(x, type) {
        if (type == "latex")
            LATEX <- TRUE
        else LATEX <- FALSE
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
        x <- gsub("_BT_", ifelse(LATEX, "\\\\begin{tabular}{lrrrr}", "<table>\n"),
            x)
        x <- gsub("_EOL_", "\n", x)
        x <- gsub("_HL_", ifelse(LATEX, "\\\\hline", ""), x)
        x <- gsub("_UL_", ifelse(LATEX, "\\\\underline{", "<span style=\"text-decoration: underline;\">"), x)
        x <- gsub("_EOUL_", ifelse(LATEX, "}", "</span>"), x)
        x <- gsub("_SEPU_", ifelse(LATEX, " &", paste("</td><td style=\"border-bottom: solid thin black; border-collapse:collapse;\">&nbsp;")),
            x)
        x <- gsub("_SEP_", ifelse(LATEX, " &", "</td><td>"),
            x)
        x <- gsub("_EOT_", ifelse(LATEX, "\\\\end{tabular}",
                                  "</table>"), x)
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
        if(type == "latex") exten <- ".tex" else exten <- ".html"
        write(template, paste0(file, exten))
    }
    template
}

