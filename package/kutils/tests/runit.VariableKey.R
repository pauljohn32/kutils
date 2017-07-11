library(RUnit)
library(kutils)

test.safeInteger <- function(){
    checkEquals(kutils::safeInteger(1.001), NULL)
    checkEquals(kutils::safeInteger(1.0000000000001), 1)
}


test.keyTemplate <- function(){
    dat <- data.frame("Score" = c(1, 2, 3, 42, 4, 2),
                      "Gender" = c("M", "M", "M", "F", "F", "F"),
                      "x" = rnorm(6))
    kt <- keyTemplate(dat)
    checkEquals(kt$class_old, c("integer", "factor", "numeric"))
    checkEquals(kt$value_new, c("1|2|3|4|42", "F|M", ""))
}
