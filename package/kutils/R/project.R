
##' Create project directories, initialize a git repo, create README.md
##' ChangeLog, and R template file in R directory
##'
##' This creates folders for the separate parts of a project. It tries
##' to be clever about which directories are created and where they are
##' placed. Please see details for 3 scenarios for which we have
##' planned. If a directory already exists, it will not be damaged or
##' re-created.
##'
##' If the dir argument is NULL, as default, then the current working
##' directory will be the place where new directories and the git
##' repository will be created.  Assuming the current working
##' directory's base name is not "R", then folders named "R", "data",
##' and so forth will be created in the current working directory.
##' 
##' If one has a current R working directory with a basename "R"
##' (suppose it is \code{"/tmp/whatever/R"}), and the user runs
##' \code{initProject()}, something different happens. The function
##' assumes we don't want to create subdirectories inside R. We don't
##' want to end up with \code{"/tmp/whatever/R/R"}. We don't
##' want \code{"/tmp/whatever/R/data"} either.  Instead, it assumes we
##' want the new directories created on same level as R, so it creates
##' \code{"/tmp/whatever/data"}, \code{"/tmp/whatever/workingdata"},
##' and so forth.  From within the R directory, these new directories
##' are seen as \code{"../data"}, \code{"../workingdata"}, and so
##' forth. That is, we should end up with directories and a git repo
##' in \code{"/tmp/whatever"}.
##'
##' If the \code{dir} argument is provided by the user, then that is
##' used as the folder in which directories \code{"R"}, \code{"data"},
##' \code{"workingdate"}, and so forth are created.  All materials are
##' created in \code{dir}, no matter what the current working
##' directory is named (even if it is \code{"R"}).
##'
##' The examples demonstrate all three of these scenarios.
##' @param dir Default NULL, otherwise a legal directory name to serve
##'     as the top level directory for this project
##' @param ddir Data directory, place where "read only" unadjusted
##'     data files are kept. Default is "data". If user sets it as NA
##'     or NULL, the directory will not be created.
##' @param wdir Working data directory, where recorded, revised, and
##'     cleaned data files are kept. Default is "workingdata".
##' @param odir Output directory. Default is "output".
##' @param tdir Temporary directory, where trash files can be kept for
##'     safe keeping. Default is "tmp".
##' @param ldir Literature directory, where material about the project
##'     can be stored. Default is "lit".
##' @param writedir The folder where the project writeup will be
##'     stored. Default is "writeup".
##' @param rdir The name to be used for the R files. Defaults to "R".
##' @param ... A list of other directories that the user would like to
##'     create. For example, \code{adir = "admin"}, \code{cdir =
##'     "client_provided"}, \code{bdir = "codebooks"}, \code{sdir =
##'     "Stata"}, \code{mdir = "Mplus"}. These may be grouped in a
##'     named vector or list, if user convenience dictates.
##' @param gitArgs This function tries to run "git init" and in our
##'     center we add "--shared=group" on a network file server. If
##'     that is undesirable in a user's context, put the argument
##'     gitArgs as "".
##' @importFrom stats na.omit
##' @export
##' @return Name of project top level directory. Leaves the R
##'     working directory unchanged.
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' projdir1 <- file.path(tempdir(), "test1")
##' dir.create(projdir1, recursive = TRUE)
##' initProject(dir = projdir1)
##' list.files(projdir1, all.files = TRUE)
##' projdir2 <- file.path(tempdir(), "test2")
##' dir.create(projdir2, recursive = TRUE)
##' ## demonstrate ability to create other directories
##' initProject(dir = projdir2, admin = "admin", clientfiles = "client")
##' list.files(projdir2, all.files = TRUE)
##' ## demonstrate ability to nullify standard directories
##' projdir3 <- file.path(tempdir(), "test3")
##' dir.create(projdir3, recursive = TRUE)
##' initProject(projdir3, odir = NA, tdir = NA, writedir = NA)
##' list.files(projdir3, all.files = TRUE)
##' unlink(c("projdir1", "projdir2", "projdir3"), recursive = TRUE)
initProject <- function(dir = NULL, ddir = "data",
                    wdir = "workingdata", odir = "output",
                    tdir = "tmp", ldir = "lit",
                    writedir = "writeup",
                    rdir = "R", ..., gitArgs = "--shared=group")
{
    wd <- getwd()

    dots <- unlist(list(...))
    
    dirs <- c(ddir, wdir, odir, tdir, ldir, writedir)
    if (length(dots > 0)){
        dirs <- c(dirs, dots)
    }
    dirs <- na.omit(dirs)
    
    ## Only create rdir if dir NULL or not now in "R"
    if (!is.null(dir)){
        dirs <- c(dirs, rdir)
        dirs <- paste(dir, dirs, sep = "/")
        if (!file.exists(dts(dir))) dir.create(dts(dir), recursive = TRUE)
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

    fileheader <- paste0("## ", sysinfo[["user"]], "  \n", "##", format(systime,"%Y%m%d  \n"))
    cat(fileheader, file = paste("README.md"))

    ## TODO 20161207: Think harder on following, which assumes
    ## all of those directories have not been set at NA by user.
    rheader <- paste0(fileheader, 
                      paste0("\n\nwdir <- \"../", wdir, "\"\n",
                             "ddir <- \"../", ddir, "\"\n",
                             "odir <- \"../", odir, "\"\n",
                             "tdir <- \"../", tdir, "\"\n\n",
                             "library(kutils)\n\n",
                             "pdf.options(onefile=FALSE, family=\"Times\", paper=\"special\", height=4,\n",
                             "            width=6, pointsize=10)\n"))
                             
    if (!is.na(rdir)){
        cat(rheader,
            file = paste0(rdir, "/template.R"))
    }
    changelog <- paste(format(systime, "%Y-%m-%d"), sysinfo[["user"]], "\n\n")
    changelog <- paste0(changelog, "\t* " , dir, "(initProject):\n")
    cat(changelog, file = paste("ChangeLog"))

    git <- Sys.which("git")
    if (git != ""){
        makeGit <- paste("git init", gitArgs)
        gitout <- system(makeGit, intern = TRUE)
        
        messg1 <- "git add README.md ChangeLog"
        log1 <- system(messg1, intern = TRUE)
        messg2 <- paste("git commit -a -m \"Initialized project in", dir, "\"")
        log2 <- system(messg2, intern = TRUE)
        messg3 <- "Please consider creating a remote repository to which this repo should be linked"
        
        cat(gitout, log1, log2, messg3, fill = TRUE)
    } else {
        messg4 <- paste("The git executable was not found.",
                        "Thus, a git repo was not created in the working directory.",
                        "Please install put git's executable in your path.")
        cat(messg4)
    }
    ## Reset user's working directory
    setwd(wd)
    dir
}

    
