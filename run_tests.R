library('RUnit')
library('testthat')

source("C:/Users/alex/Documents/R/CDNOW/RFMTC_CDNOW_KFOLD/MyRLibs/myRFMLib.R")
source("C:/Users/alex/Documents/R/CDNOW/RFMTC_CDNOW_KFOLD/RFMTC_CDNOW_v6.R")


test.suite <- defineTestSuite("test.suite",
                              dirs = file.path("C:/Users/alex/Documents/R/CDNOW/RFMTC_CDNOW_KFOLD/tests"),
                              testFileRegexp = '^\\d+\\.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)
