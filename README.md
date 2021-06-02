kutils is on CRAN, but if there is a newer package version,
you might find it here and download it.

The work is in the package directory. THere is a build
script that works for me in Ubuntu Linux

Here are some rules.

1. Line length should be less than 80, and this is
absolutely required in sections for documentation.
2. Use correct Roxygen markup.
We don't have an idiot's guide for this yet, so please
consult Websites. These have been helpful:

[http://r-pkgs.had.co.nz/man.html#text-formatting][]

3. examples sections should be included with functions, these
should run. Can be IN code right before function or as
separate files saved under inst/examples. See rockchalk
for examples like that.

# Here is a rant

A BIG Problem so far has been inappropriate Roxygen2
markup. If the markup is not valid, the package does not compile.
In order to prove that markup is valid, we
**insist** the user *run the script buildPackage.sh* to
completion before pushing changes back to repository.

To learn how to get the markup right!

Here is an example of a successuful run:

```bash
pauljohn@dellap14:package$ ./buildPackage.sh

R version 3.3.0 (2016-05-03) -- "Supposedly Educational"
Copyright (C) 2016 The R Foundation for Statistical Computing
Platform: x86_64-pc-linux-gnu (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

> options(repos = c("http://rweb.crmda.ku.edu/kran", "http://rweb.crmda.ku.edu/cran"))
> library(roxygen2)
> roxygenize("kutils")
Loading required package: xtable
Loading required package: lavaan
This is lavaan 0.5-20
lavaan is BETA software! Please report any bugs.
Writing cfaTable.Rd
Warning message:
@param [cfaTable.R#68]: requires a value
> roxygenize("kutils.gitex")
Writing assignMissing.Rd
Writing cfaTable.Rd
Writing colnamesReplace.Rd
Writing deleteBogusRows.Rd
Writing reverse.Rd
Writing likert.Rd
Writing storageGen.Rd
Writing modelcheck.Rd
Writing pValRePresent.Rd
Writing CFAModComptab.Rd
Writing updatePackages.Rd
Writing dts.Rd
Writing floorvar.Rd
Writing stars.Rd
Warning message:
@param [cfaTable.R#68]: requires a value
>
>
* checking for file ‘kutils.gitex/DESCRIPTION’ ... OK
* preparing ‘kutils’:
* checking DESCRIPTION meta-information ... OK
* checking for LF line-endings in source and make files
* checking for empty or unneeded directories
* building ‘kutils_0.05.tar.gz’

enter name of tarball: kutils_0.05.tar.gz
* using log directory ‘/home/pauljohn/GIT/CRMDA/software/kutils/package/kutils.Rcheck’
* using R version 3.3.0 (2016-05-03)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--as-cran’
* checking for file ‘kutils/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘kutils’ version ‘0.05’
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Kenna Whitley <yourfault@somewhere.net>’

New submission

Version contains leading zeroes (0.05)
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘kutils’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking use of S3 registration ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd line widths ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking examples ... OK
* checking PDF version of manual ... OK
* DONE

Status: 1 NOTE
See
  ‘/home/pauljohn/GIT/CRMDA/software/kutils/package/kutils.Rcheck/00check.log’
for details.
```
 
