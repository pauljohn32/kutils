##' Install and update packages and their dependencies
##'
##' Checks installed packages for package dependencies, updates
##' packages, and installs/updates packages dependencies
##' @param ask If TRUE, asks user to select packages to update
##' @param checkBuilt If TRUE, packages built under earlier versions
##'     of R are to be considered 'old'
##' @return A vector of packages being inserted to complete update
##' @author Kenna Whitley <kennamarie@@ku.edu>
##' @importFrom tools package_dependencies
##' @export
##' @importFrom("utils", "available.packages", "install.packages",
##'           "installed.packages", "packageDescription", "update.packages")
##' @examples
##' options(repos = c("http://rweb.crmda.ku.edu/kran", "http://rweb.crmda.ku.edu/cran"))
##' ## not run
##' ##updatePackages(ask = FALSE)
##'
updatePackages <- function(ask = FALSE, checkBuilt = TRUE){
    pkgdeps <- function(pkg, which = c("Depends", "Imports", "LinkingTo")){
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
