library(RUnit)
library(kutils)


#testsuite.variableKey <- defineTestSuite("VariableKey",
#                                         dirs = file.path(system.file("test", package="kutils")),
#                                         testFileRegexp = "^runit.+\\.R",
#                                         testFuncRegexp = "^test.+",
#                                         rngKind = "Marsaglia-Multicarry",
#                                         rngNormalKind = "Kinderman-Ramage")

#testResult <- runTestSuite(testsuite.variableKey)
#printTextProtocol(testResult)

files <- list.files(pattern = "^runit.")

for (f in files){
    tmp <- runTestFile(f)
    printTextProtocol(tmp)
}
