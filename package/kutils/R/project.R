
##' Create project directories, initialize a git repo, create README
##' and ChangeLog
##'
##' This creates folders for the separate parts of a project. It tries
##' to be clever about which directories are created and where they are
##' placed. Please see details for 3 scenarios for which we have
##' planned.
##'
##' If the dir argument is NULL, as default, then the current working
##' directory will be the place where new directories and the git
##' repository will be created.  Assuming the current working
##' directory's base name is not "R", then folders named "R", "data",
##' and so forth will be created in the current working directory.
##' 
##' If one has a current R working directory with a basename "R"
##' (suppose it is "/tmp/whatever/R"), the function assumes we don't
##' want to create another subdirectory named R inside "/tmp/whatever/R". Instead,
##' it assumes we want
##' the new directories created on same level as R, so it creates
##' "../data", "../workingdata", and so forth. That is, we should end up
##' with directories and a git repo in "/tmp/whatever".
##'
##' If the dir argument is provided by the user, then that is used as
##' the working directory and all materials are created in there.
##'
##' The examples demonstrate all three of these scenarios.
##' @param dir Default NULL, otherwise a legal directory name to serve
##'     as the top level directory for this project
##' @param ddir Data directory, place where "read only" unadjusted
##'     data files are kept. Defaults as "data"
##' @param wdir Working data directory, where recorded, revised, and
##'     cleaned data files are kept. Defaults as "workingdata"
##' @param odir Output directory. Defaults as "output".
##' @param tdir Temporary directory, where trash files can be kept for
##'     safe keeping. Defaults as "tmp".
##' @param ldir Literature directory, where material about the project
##'     can be stored.
##' @param writedir The folder where the project writeup will be
##'     stored. Defaults to "writeup".
##' @param rdir The name to be used for the R files. Defaults to "R".
##' @param ... A list of other directories that the use would like to
##'     create. For example, \code{adir = "admin"}, \code{cdir =
##'     "client_provided"}, \code{bdir = "codebooks"}, \code{sdir =
##'     "Stata")}. These may be grouped in a named vector or list, if
##'     user convenience dictates.
##' @export
##' @return Name of project top level directory
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' tempdir <- tempdir()
##' setwd(tempdir)
##' dir.create("test1")
##' setwd("test1")
##' initProject(admin = "admin", clientfiles = "client")
##' list.files(all.files = TRUE)
##' dir.create("../test2/R", recursive = TRUE)
##' setwd("../test2/R")
##' initProject(lostLuggage = "Trash")
##' list.files(all.files = TRUE)
##' initProject(paste(tempdir, "test3", sep = "/"))
##' list.files(all.files = TRUE)
##' setwd(tempdir)
##' initProject(paste(tempdir, "test4", sep = "/"), list(food = "menu", bev = "drink_orders"))
##' list.files(all.files = TRUE)
##' unlink(c("test1", "test2", "test3", "test4"), recursive = TRUE)
initProject <- function(dir = NULL, ddir = "data",
                    wdir = "workingdata", odir = "output",
                    tdir = "tmp", ldir = "lit",
                    writedir = "writeup",
                    rdir = "R", ...)
{
    wd <- getwd()

    dots <- unlist(list(...))
    
    dirs <- c(ddir, wdir, odir, tdir, ldir, writedir)
    if (length(dots > 0)){
        dirs <- c(dirs, dots)
    }
    
    ## Only create rdir if dir NULL or not now in "R"
    if (!is.null(dir)){
        dirs <- c(dirs, rdir)
        dirs <- paste(dir, dirs, sep = "/")
        if (!file.exists(dir)) dir.create(dir, recursive = TRUE)
    } else {
        ## is name of topdir for records
        if (basename(wd) == "R"){
            dirbase <- basename(dirname(wd))
            dir <- normalizePath("..")
            dirs <- paste0("../", dirs)
        } else {
            dirbase <- basename(wd)
            dir <- normalizePath(".")
            dirs <- c(dirs, rdir)
        }
    }
    
    for (i in dirs){
        if (!file.exists(i)) {
            dir.create(i, recursive = TRUE)
            messg <- paste("Creating:", i, "\n")
            cat(messg)
        }
    }

    setwd(dir)

    
    sysinfo <- Sys.info()
    systime <- Sys.time()

    readme <- paste0("## ", sysinfo[["user"]], "\n", "## ", format(systime,"%Y%m%d\n"))
    cat(readme, file = paste("00-README.txt"))
    
    changelog <- paste(format(systime, "%Y-%b-%d"), sysinfo[["user"]], "\n\n")
    changelog <- paste0(changelog, "\t* " , dir, "(initProject):\n")
    cat(changelog, file = paste("ChangeLog"))

    makeGit <- "git init --shared=group"
    gitout <- system(makeGit, intern = TRUE)

    messg1 <- "git add 00-README.txt ChangeLog"
    system(messg1)
    messg2 <- paste("git commit -a -m \"Initialized project in", dir, "\"")
    system(messg2)
    
    dir
}

    
