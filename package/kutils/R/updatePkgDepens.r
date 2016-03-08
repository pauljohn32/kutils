## Kenna Whitley
## 20160302

#### Update Packages and Package Dependencies ####

## Specify Repos
CRAN <- "http://rweb.crmda.ku.edu/cran"
KRAN <- "http://rweb.crmda.ku.edu/kran"

options(repos = c(KRAN, CRAN))

## El Function
updatePackages <- function(ask = FALSE, checkBuilt = TRUE){
    pkgdeps <- function(pkg, which = c("Depends", "Imports", "LinkingTo")){
        library(tools)
        deps <- package_dependencies(pkg, db = available.packages(), which = c("Depends", "Imports", "LinkingTo"), recursive = TRUE)
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

updatePackages()
