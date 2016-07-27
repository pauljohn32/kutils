##' Update packages, spot new dependencies, and install them
##'
##' Addresses the problem that updates for previously installed R
##' packages may insert new dependencies. R's update.packages does not
##' trigger the installation of packages that are added as new
##' requirements in existing packages.
##'
##' This function checks for existence of updates, ascertains whether
##' those packages impose new requirements, and installs any new
##' requirements. Then it conducts the update.
##' 
##' This function is valuable in system maintenance because sometimes
##' existing packages adopt new requirements and the update.packages
##' function does not notice.  Another possible case would be that a user
##' accidentally deletes some packages without realizing other packages
##' depend on them.
##'
##' If this is run as the root/administrator privileged, then base R
##' packages may be updated, but if user is not root/administrator,
##' there will be a warning that packages were not updated because
##' permissions were lacking. For example
##'
##' "Warning: package 'boot' in library '/usr/lib/R/library' will not
##' be updated.
##' 
##' This warning does not interfere with rest of purpose of this
##' function, since the new dependencies can be installed in a place
##' where the user has privileges, either by specifying libnew as a
##' full directory name or by setting it to NULL, which puts new
##' packages in $R_LIBS_USER
##' @param ask If TRUE, asks user to select packages to update
##' @param checkBuilt If TRUE, packages built under earlier versions
##'     of R are to be considered 'old'
##' @param dependencies A vector specifying which type of dependencies
##'     need to be taken into account. We default to c("Depends",
##'     "Imports", "LinkingTo").
##' @param libnew The R library folder into which the new packages
##'     must be installed.  Defaults to "/usr/share/R/library", which
##'     is where EL7 likes those things.  To install packages in
##'     personal user directory, put libnew = NULL.
##' @param repos A vector of repositories on which to search for
##'     packages. Same definition as in R's install.packages or
##'     install.packages.
##' @param ... additional arguments passed to update.packages and
##'     install.packages
##' @return A vector of new packages being installed
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @importFrom tools package_dependencies
##' @export
##' @importFrom utils available.packages install.packages
##'     installed.packages packageDescription update.packages
##' @examples
##' ## options(repos = c("http://rweb.crmda.ku.edu/kran", "http://rweb.crmda.ku.edu/cran"))
##' ## not run
##' ## updatePackages()
##' ## New packages will go to user directory
##' ## updatePackages(ask = FALSE, libnew = NULL)
##' ## otherwise must have write privileges in libnew.
updatePackages <- function(ask = FALSE, checkBuilt = TRUE,
                           dependencies = c("Depends", "Imports", "LinkingTo"),
                           libnew = "/usr/share/R/library/",
                           repos = c("http://rweb.crmda.ku.edu/cran",
                                     "http://rweb.crmda.ku.edu/kran",
                                     "http://www.bioconductor.org/packages/3.3/bioc"),
                           ...)
{
    ## See if package asks for dependencies not currently installed
    installedPackages <- rownames(installed.packages())
    update.packages(ask = FALSE, checkBuilt = TRUE, repos = repos, ...)
    avail <- available.packages(repos = repos)
    alldeps <- unique(unlist(tools::package_dependencies(installedPackages, which = dependencies,
                                                         db = avail,
                                                         recursive = TRUE)))
    
    targets <- alldeps[!alldeps %in% installedPackages]
    if (length(targets) > 0) {
        install.packages(targets, lib = libnew, repos = repos, dependencies = dependencies, ...)
        print(paste("These new packages were needed", targets))
    }
    if (length(targets) == 0){
        print("No new packages were installed to fulfill dependencies")
        return(NULL)
    } else {
        targets
    }
}
