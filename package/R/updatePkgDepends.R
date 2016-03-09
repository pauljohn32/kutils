##' Gather, install, and update packages and their dependencies
##'
##' Beneficial to dictate which repositories you'd like to use before
##' running function
##' @param ask Whether to ask user to select packages or not
##' @param checkBuilt If TRUE, packages built under earlier versions
##'     of R are to be considered 'old'
##' @return A list of packages
##' @author Kenna Whitley <kennamarie@@ku.edu>
##' @examples
##' updatePackages(ask = FALSE)
##' 
updatePackages <- function(ask = FALSE, checkBuilt = TRUE){
    pkgdeps <- function(pkg, which = c("Depends", "Imports", "LinkingTo")){
        library(tools)
        deps <- package_dependencies(pkg, db = available.packages(),
                                     which = c("Depends", "Imports", "LinkingTo"),
                                     recursive = TRUE)
        grab <- sapply(deps[[1]], packageDescription, fields = "Priority")
        deps <- c(deps[[1]][is.na(grab)])
        deps
    }
    installedPackages <- rownames(installed.packages())
    targets <- lapply(installedPackages, pkgdeps)
    targets <- unique(targets)
    targets <- unlist(targets[lapply(targets,length)>0])
    
    alreadyHave <- targets %in% installedPackages
    lapply(targets[!alreadyHave], install.packages)
    print(targets[!alreadyHave])
    
    update.packages(ask = FALSE, checkBuilt = TRUE)
    targets
}

NULL
