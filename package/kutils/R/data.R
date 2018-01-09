## natlongsurv <- read.csv2("../extdata/natlongsurv.csv")
## save(natlongsurv, file = "../data/natlongsurv.RData", compress = TRUE)


##' Smoking, Happiness, and other survey responses
##'
##' An idiosyncratic selection of 29 variables from the Original
##' Cohort-Young Women 1968-2003 edition of the US National
##' Longitudinal Survey.  This originally included 5159 rows, but
##' subset includes only 2867 rows, so sample frequencies will not
##' match the values listed in the codebook.  A snapshot of the
##' codebook, "natlongsurv.cdb.txt", which we have trimmed down, is
##' included in the package.
##' 
##' All variables are for the 2003 year, except where otherwise noted.
##'
##' @name natlongsurv
##' @docType data
##' @usage data(natlongsurv)
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @source National Longitudinal Surveys public-use data set (Bureau of Labor Statistics, 2018).
##' @references Bureau of Labor Statistics. 2018.  NLS Original Cohors: Mature and Young Women, US National Longitudinal Surveys Public Use Data Sets \url{https://www.bls.gov/nls/nlsorig.htm}.
##' @format A data frame with 2867 rows and 29 variables:
##'
##'\itemize{
##'   \item	R0000100	IDENTIFICATION CODE
##'   \item	R0003300	MARITAL STATUS, 1968
##'   \item	R0005700	AGE WHEN STOPPED ATTENDING SCHOOL, 1968
##'   \item	R0060300	IQ SCORE, 1968
##'   \item	R1051600	HIGHEST GRADE COMPLETED
##'   \item	R1302000	SMOKING - DOES R SMOKE, 1991
##'   \item	R1302100	SMOKING - NUMBER OF CIGARETTES R SMOKES PER DAY, 91 (PRESENT SMOKER)
##'   \item	R6235600	HIGHEST GRADE COMPLETED
##'   \item	R6502300	IS RESIDENCE/LIVING QUARTERS HOME/APARTMENT/OTHER?
##'   \item	R6513700	HOUSEHOLD RECORD - HOUSEHOLD MEMBER - AGE CALCULATED FROM BIRTH DATE
##'   \item	R6516200	CURRENT MARITAL STATUS
##'   \item	R6520300	HIGHEST GRADE COMPLETED OF HUSBAND
##'   \item	R6553600	HIGHEST GRADE COMPLETED OF PARTNER
##'   \item	R7289200	SMOKING - CURRENTLY SMOKE CIGARETTES
##'   \item	R7289400	ALCOHOL USE - HAS R CONSUMED ANY ALCOHOLIC BEVERAGES IN PAST MONTH?
##'   \item	R7293430	YOUNG WOMEN 20-ITEM CES-D ITEM RESPONSE SCORE
##'   \item	R7312300	INCOME FROM WAGES/SALARY IN PAST YEAR
##'   \item	R7329900	INCOME ADEQUACY: R OPINION OF HER HAPPINESS WITH HER/FAMILY INCOME
##'   \item	R7330000	INCOME ADEQUACY: R OPINION OF AMOUNT NEEDED TO MAKE ENDS MEET \$ AMOUNT
##'   \item	R7337600	R HAS ATTENDED/COMPLETED TWO/MORE YEARS OF COLLEGE
##'   \item	R7344600	ATTITUDE TOWARD FEELINGS OVERALL
##'   \item	R7344700	DID R DO ANY UNPAID VOLUNTEER WORK IN PAST YEAR?
##'   \item	R7347500	ATTITUDE TOWARD SOCIAL SECURITY - PERCENT WOULD INVEST IN STOCKS? 2004
##'   \item	R7347600	ATTITUDE TOWARD SOCIAL SECURITY - PERCENT WOULD INVEST IN BONDS OF PRIVATE COMPANIES? 2004
##'   \item	R7347700	ATTITUDE TOWARD SOCIAL SECURITY - PERCENT WOULD INVEST IN U.S. GOVERNMENT BONDS? 2004
##'   \item	R7477700	TOTAL CHILDREN IN ROSTER
##'   \item	R7477800	COUNT ELIGIBLE HOUSEHOLD CHILDREN
##'   \item	R7610300	REGION OF RESIDENCE
##'}
##'@examples
##' data(natlongsurv)
##' peek(natlongsurv, ask = FALSE, file = paste0(tempdir(), "/","peek.pdf"))
NULL

