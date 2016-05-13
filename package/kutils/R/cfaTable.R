
##' Builds APA style results tables in .tex code for confirmatory factor analysis output from the "cfa" function in the "lavaan" package.
##'
##' @title cfaTable
##' @param output Output returned by the "cfa" function in lavaan.
##' @param tablename Name of table as it should appear in manuscript.
##' @param outfile Location and name of the resulting .tex file.
##' @param params Measurement parameters to be included in table.  Options are "loadings", "intercepts", "residuals", "latentvariances", and "thresholds".  "loadings" are the factor loadings in the model.  "intercepts" are the indicator variable intercepts". "residuals" are the indicator variable residual variances.  "latentvariances" are the latent variable variances and covariances.  "thresholds" are the thresholds on latent response variates in models with ordered-categorical or dichotomous data.
##' @param fit A vector listing model fit measures that should be included in the note of the table. Listing "chi-square" will do special formatting to the chi-square value in the note. Any other measures listed must correspond to measures found in fitMeasures(output).
##' @param names_fit Names for the fit measures requested by fit. Set equal to fit by default. For an example of use, if fit = c("cfi.scaled", "tli.scaled") is specified, names_fit = c("CFI", "TLI") can be used to produce cleaner names.
##' @param standardized Should standarized results be presented along with unstandardized.  Default is set to FALSE, this provides four columns in the table: 1) the estimate of the parameter, 2) the standard error of the parameter, 3) the z-value for the parameter estimate, and 4) the corresponding z-value.  When equal to TRUE, four columns are provided: 1) the parameter estimates provided in output, 2) the standard errors provided in output, 3) standardized parameter estimates, and 4) standardized standard errors.  The standardized parameters are obtained by updating the output with the options std.lv = TRUE and std.ov = TRUE.  If these options were used when originally creating output, setting standardized = TRUE will yield two identical sets of two columns.
##' @param names_upper Should the names of the model fit parameters be forced to be uppercase.  The default is set to TRUE.  This will also affect whatever is specified in names_fit.
##' @param single_spaced Should the table be presented as single spaced.  The default is set to TRUE. When set to false the table is double-spaced, which may make some tables too large for a single page.
##' @param preamble Should the .tex file contain the latex premable and the begin and end document lines.  Default is set to TRUE. When set to FALSE, only the tex code including and between the \begin{table} and \end{table} lines is included.
##' @return File saved as outfile.
##' @export
##' @author Ben Kite bakite@@ku.edu
cfaTable <- function(output, tablename, outfile, params = c("loadings", "intercepts"), fit = c("chi-square", "cfi", "tli", "rmsea"), names_fit = fit, standardized= FALSE, names_upper = TRUE, single_spaced = TRUE, preamble = TRUE){

    if(class(output)[1] != "lavaan"){
        stop("The output that you provided does not appear to be lavaan output.  Only lavaan cfa output can be used with the CFATable function.")
    }
    if(output@Options$model.type != "cfa"){
        stop("The output object you have provided appears to be from lavaan, but it is not cfa output.  Fit your model with the cfa function and try again.")
    }
    if(names_upper == TRUE){
        names(fit) <- toupper(names_fit)
    }else{
        names(fit) <- names_fit
    }
    fitmeas <- fitMeasures(output)[]
    fitmeas <- sapply(fitmeas, function(x) formatC(round(x, 3), format = 'f', digits = 2))
    chimeas <- output@Fit@test[[1]]
    chimeas$stat <- formatC(round(chimeas$stat, 3), format = 'f', digits = 2)
    chimeas$pvalue <- formatC(round(chimeas$pvalue, 3), format = 'f', digits = 3)
    chimeas$pvalue <- gsub("0\\.", "\\.", chimeas$pvalue)
    parameters <- data.frame(output@ParTable)[,c("lhs", "op", "rhs", "free")]
    parameters$est <- output@Fit@est
    parameters$se <- output@Fit@se
    ##parameters <- parameters[which(parameters$free > 0),]
    parameters$z <- parameters$est/parameters$se
    parameters$p <- 2*pnorm(abs(parameters$z), lower.tail = FALSE)
    parameters[,"est"] <- round(parameters[,"est"], 2)
    parameters[,"se"] <- round(parameters[,"se"], 2)
    parameters[,"z"] <- round(parameters[,"z"], 2)
    parameters[,"p"] <- round(parameters[,"p"], 3)
    parameters[,"rhs"] <- as.character(parameters[,"rhs"])
    parameters[,"lhs"] <- as.character(parameters[,"lhs"])
    variables <- unlist(output@Data@ov.names)
    latents <- unlist(output@pta$vnames$lv)

    if(preamble == TRUE){
    template <- "
\\documentclass[english, man]{apa}
\\usepackage[T1]{fontenc}
\\usepackage[latin9]{inputenc}
\\usepackage{array}
\\usepackage{multirow}
\\usepackage{amsmath}
\\usepackage{amsthm}
\\PassOptionsToPackage{normalem}{ulem}
\\usepackage{ulem}
\\providecommand{\\tabularnewline}{\\\\}
\\author{Author} % hack around some bugs in apa.cls
\\affiliation{Affiliation} % hack around some bugs in apa.cls
\\numberwithin{equation}{section}
\\numberwithin{figure}{section}
\\makeatother
\\usepackage{babel}
\\usepackage{dcolumn}
\\newcolumntype{d}[1]{D{.}{.}{-1}}
\\makeatletter
SINGLESPACE

\\begin{document}
\\begin{table}

\\caption{TITLE}

\\begin{tabular}{m{4cm} d{0} d{2} d{2} d{2}}
\\hline
STANDARDIZED
Parameter & REPORT\\tabularnewline
\\hline
FACTORLOADINGS
INTERCEPTS
THRESHOLDS
RESIDUALS
LATENTVARS
\\hline
\\end{tabular}

\\textit{Note. }* Indicates parameters fixed for model identification.\\\\
FITINFORMATION.
\\end{table}
\\end{document}
"
    }else{
        template <- "
\\begin{table}

\\caption{TITLE}

\\begin{tabular}{m{4cm} d{0} d{2} d{2} d{2}}
\\hline
STANDARDIZED
Parameter & REPORT\\tabularnewline
\\hline
FACTORLOADINGS
INTERCEPTS
THRESHOLDS
RESIDUALS
LATENTVARS
\\hline
\\end{tabular}

\\textit{Note. }* Indicates parameters fixed for model identification.\\\\
FITINFORMATION.
\\end{table}
"
    }

    if(standardized == TRUE){
        report <- c("est", "se", "stdest", "stdse")
        std <- update(output, std.lv = TRUE, std.ov = TRUE)
        parameters$stdest <- std@Fit@est
        parameters$stdse <- std@Fit@se
        holder <-  "& \\\\multicolumn{2}{c}{\\\\uline{Unstandarized}} & \\\\multicolumn{2}{c}{\\\\uline{Standardized}}\\\\tabularnewline"
        template <- gsub("STANDARDIZED", holder, template)
        holder <- "\\\\multicolumn{1}{c}{NAME}"
        reportx <- list()
        columnNames <- c("Estimate", "SE", "Estimate", "SE")
        for(i in 1:length(columnNames)){
            reportx[i] <- gsub("NAME", columnNames[i], holder)

        }
        columnnames <- paste0(reportx, collapse = " & ")

    }else{
        report <- c("est", "se", "z", "p")
        parameters$stdest <- NA
        parameters$stdse <- NA

        template <- gsub("STANDARDIZED", "", template)

        holder <- "\\\\multicolumn{1}{c}{NAME}"
        reportx <- list()
        columnNames <- c("Estimate", "SE", "z", "p")
        for(i in 1:length(columnNames)){
            reportx[i] <- gsub("NAME", columnNames[i], holder)

        }

        columnnames <- paste0(reportx, collapse = " & ")
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
\\\\multirow{1}{*}{\\\\uline{FACTOR}} &  & \\\\multirow{1}{*}{} & \\\\tabularnewline\n
ROWINFORMATION
"
        tmpx <- gsub("FACTOR", lvname, tmpx)
        rowinfo <- paste0(paste0(trows[1,c("rhs", report)], collapse = " & "), "\\\\\\\\tabularnewline\n")
        for (i in 2:nrow(trows)){
            rowinfo <- paste0(rowinfo, paste0(paste0(trows[i,c("rhs", report)], collapse = " & "), "\\\\\\\\tabularnewline\n"))
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
 & \\\\multicolumn{4}{c}{\\\\uline{Intercepts}}\\\\tabularnewline
ROWINFORMATION
"
        rowinfo <- paste0(paste0(trows[1,c("lhs", report)], collapse = " & "), "\\\\\\\\tabularnewline\n")
        for (i in 2:nrow(trows)){
            rowinfo <- paste0(rowinfo, paste0(paste0(trows[i,c("lhs", report)], collapse = " & "), "\\\\\\\\tabularnewline\n"))
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
 & \\\\multicolumn{4}{c}{\\\\uline{Thresholds}}\\\\tabularnewline
ROWINFORMATION
"
        rowinfo <- paste0(paste0(trows[1,c("lhs", report)], collapse = " & "), "\\\\\\\\tabularnewline\n")
        for (i in 2:nrow(trows)){
            rowinfo <- paste0(rowinfo, paste0(paste0(trows[i, c("lhs", report)], collapse = " & "), "\\\\\\\\tabularnewline\n"))
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
 & \\\\multicolumn{4}{c}{\\\\uline{Residual Variances}}\\\\tabularnewline
ROWINFORMATION
"
        rowinfo <- paste0(paste0(trows[1,c("lhs", report)], collapse = " & "), "\\\\\\\\tabularnewline\n")
        for (i in 2:nrow(trows)){
            rowinfo <- paste0(rowinfo, paste0(paste0(trows[i,c("lhs", report)], collapse = " & "), "\\\\\\\\tabularnewline\n"))
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
 & \\\\multicolumn{4}{c}{\\\\uline{Latent Variances/Covariances}}\\\\tabularnewline
ROWINFORMATION
"
        rowinfo <- paste0(trows[1,1], " with ", trows[1,2], " & ", paste0(trows[1,report], collapse = " & "), "\\\\\\\\tabularnewline\n")
        for (i in 2:nrow(trows)){
            rowinfo <- paste0(rowinfo, paste0(trows[i,1], " with ", trows[i,2], " & ", paste0(trows[i,report], collapse = " & "), "\\\\\\\\tabularnewline\n"))
        }
        tmpx <- gsub("ROWINFORMATION", rowinfo, tmpx)
        tmpx
    }

    if("loadings" %in% params){
        loadingInfo <- lapply(loads, loadingMaker, report)
        loadingInfo <- paste0(unlist(loadingInfo), collapse = "")
        loadingInfo <- paste0(" & \\\\multicolumn{4}{c}{\\\\uline{Factor Loadings}}\\\\tabularnewline", loadingInfo)
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
    template <- gsub("TITLE", tablename, template)
    fitinfotmpchi <- "$\\\\chi^{2}$(DF)= CHI, \\\\textit{p} = PVAL"

    if("chi-square" %in% fit){
        fitinfotmpchi <- gsub("CHI", chimeas$stat, fitinfotmpchi)
        fitinfotmpchi <- gsub("DF", chimeas$df, fitinfotmpchi)
        fitinfotmpchi <- gsub("PVAL", chimeas$pvalue, fitinfotmpchi)
    }else{
        fitinfotmpchi <- NULL
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

    fitinfo <- paste0(measures, collapse = "; ")
    template <- gsub("FITINFORMATION", fitinfo, template)
    write(template, paste0(outfile))
}



