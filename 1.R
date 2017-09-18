# check for string mismatches for the global constants and main section
test.String_Mismatches_Main <- function()
{
  checkEquals(
    "CDNOW RFMTC K-FOLD CROSS VALIDATION DATASET",
    RUN_TITLE,
    "<RUN_TITLE> string mismatch!"
  )
  checkEquals("Recency (months)",
              recencyColName,
              "<recencyColName> string mismatch!")
  checkEquals("Frequency (times)",
              frequencyColName,
              "<frequencyColName> string mismatch!")
  checkEquals("Monetary ($)",
              monetaryColName,
              "<monetaryColName> string mismatch!")
  checkEquals("Time (months)",
              timesColName,
              "<timesColName> string mismatch!")
  checkEquals("TRAINING_OPTIMALS.csv",
              TRAINING_OPTIMALS,
              "<TRAINING_OPTIMALS> string mismatch!")
}


# check for data type mismatches in the main section
test.Data_Type_Mismatches <- function()
{
  checkTrue(class(fileList) == "list", "<fileList> data type mismatch")
  checkTrue(class(file) == "character", "<file> data type mismatch")
  checkTrue(class(rowIndx) == "integer", "<rowIndx> data type mismatch")
  checkTrue(class(avgResDF) == "data.frame",
            "<avgResDF> data type mismatch")
  checkTrue(class(custIDvector) == "integer",
            "<custIDvector> data type mismatch")
}


# check for length mismatches
test.length_Mismatches <- function()
{
  checkTrue(avgResDFLen == 2357, "<avgResDFLen> length mismatch")
  checkTrue(length(custIDvector) == 2357, "<custIDvector> length mismatch")
}


# check for boolean mismatches for the global constants and main sections
test.String_Mismatches_MainProgram <- function()
{
  checkTrue(hasCustID == TRUE, "<hasCustID> boolean mismatch")
}


#  check for certain numeric values in the the global constants and main sections
test.numeric_Mismatches_MainProgram <- function()
{
  checkEqualsNumeric(custChurnColIndex, 6, "<custChurnColIndex> numeric mismatch")
}


#  check for certain lists in the the global constants section
test.list_Mismatches_GlobalConstants <- function()
{

}


# check kFoldPartitionCustomerDSToTrainingTestDS()
test.kFoldPartitionCustomerDSToTrainingTestDS <- function()
{
  expect_that(
    kFoldPartitionCustomerDSToTrainingTestDS(CustDSFile,
                                             hasCustID,
                                             K_FOLDS,
                                             i_fold,
                                             fileList[["TRAINING"]],
                                             fileList[["TEST"]]),
    prints_text("kFoldPartitionCustomerDSToTrainingTestDS()")
  )
  checkEquals("ID", idColName, "<idColName> string mismatch!")
  checkEquals("CustID", custIdColName, "<custIdColName> string mismatch!")
}


# check the input/output of the prepareRFMTCdatase() function
test.prepareRFMTCdataset <- function()
{
  # check the input
  checkEquals("Churn (0/1)",
              churnColName,
              "<churnColName> string mismatch!")
  checkEquals("R-Score", rScoreColName, "<rScoreColName> string mismatch!")
  checkEquals("F-Score", fScoreColName, "<fScoreColName> string mismatch!")
  checkEquals("M-Score", mScoreColName, "<mScoreColName> string mismatch!")
  checkEquals("(T-R)/(F-1)", tRF1ColName, "<tRF1ColName> string mismatch!")
  checkEquals("P[B]", pBColName, "<pBColName> string mismatch!")
  checkEquals("E[X[L]|L=1]", eXL1ColName, "<eXL1ColName> string mismatch!")
  checkEquals("(E[X|L=1]-P[B])^2", eXL1_pB2ColName, "<eXL1_pB2ColName> string mismatch!")
  checkEquals("RFM-Score",
              rfmScoreColName,
              "<rfmScoreColName> string mismatch!")
  # check the output
  checkTrue(class(rfmtcReadyDF) == "data.frame",
            "<rfmtcReadyDF> data type mismatch")
  checkEqualsNumeric(ncol(rfmtcReadyDF), 11, "<rfmtcReadyDF> numeric mismatch")
  expect_that(
    prepareRFMTCdataset(
      file,
      c(
        idColName,
        custIdColName,
        recencyColName,
        frequencyColName,
        monetaryColName,
        timesColName,
        churnColName,
        rScoreColName,
        fScoreColName,
        mScoreColName,
        rfmScoreColName
      )
    ),
    prints_text("prepareRFMTCdataset()")
  )
}



# check the output of the calc_b_RangeConstraints() function
test.calc_b_RangeConstraints <- function()
{
  checkEqualsNumeric(length(resList[[1]]), 11, "<resList> columns length mismatch")
  # check function func_T_R_F_1
  expect_that(func_T_R_F_1(2,2,2), equals(0))
  expect_that(func_T_R_F_1(1,2,3), equals(2))
  expect_that(func_T_R_F_1(1,2,3), equals(2))
  # check if all elements of exl1 column of the returned list are equal to zero 
  exl1 = sapply(resList, function (x) x[9])
  exl1.numeric = as.numeric(unlist(x)) == 0
  boolean.check.if.exl1.elem.are.zero = all(exl1.numeric == TRUE)
  checkTrue(boolean.check.if.col.elem.are.zero == TRUE, "<resultList <exl1> column> length mismatch")
  
  # check if all elements of exl1 column of the returned list are equal to zero 
  Pb = sapply(resList, function (x) x[10])
  Pb.numeric = as.numeric(unlist(x)) == 0
  boolean.check.if.Pb.elem.are.zero = all(Pb.numeric == TRUE)
  checkTrue(boolean.check.if.Pb.elem.are.zero == TRUE, "<resultList <Pb> column> length mismatch")
  
  # check if all elements of exl1 column of the returned list are equal to zero 
  exl1Pb = sapply(resList, function (x) x[11])
  exl1Pb.numeric = as.numeric(unlist(x)) == 0
  boolean.check.if.exl1Pb.elem.are.zero = all(exl1Pb.numeric == TRUE)
  checkTrue(boolean.check.if.exl1Pb.elem.are.zero == TRUE, "<resultList <exl1Pb> column> length mismatch")
}


# check the input/output of the calc_b_RangeConstraints() function
test.discoverOptimalInitialParams <- function()
{
  # check the input
  checkTrue(((OPTIMIZATION_METHOD_VECTOR == OPTIMIZATION_METHOD_VECTOR_FULL) ||
               (OPTIMIZATION_METHOD_VECTOR == OPTIMIZATION_METHOD_VECTOR_SMALL)) == TRUE,
            "<OPTIMIZATION_METHOD_VECTOR> wrong initiation of vector"
  )
  checkTrue(((RFMTC_PARAM_RANGE_LIST[1] %in% RFMTC_PARAM_RANGE_LIST_MIN[1]) 
             && (RFMTC_PARAM_RANGE_LIST[2] %in% RFMTC_PARAM_RANGE_LIST_MIN[2])
             && (RFMTC_PARAM_RANGE_LIST[3] %in% RFMTC_PARAM_RANGE_LIST_MIN[3])) == TRUE, 
            "<RFMTC_PARAM_RANGE_LIST> wrong initiation of list"
  )
  # check the output
  # check the resultList
  checkTrue(length(resultList) == 5, "<resultList> length mismatch")
  expect_that(
    discoverOptimalInitialParams(
      rfmtcReadyDF,
      RFMTC_PARAM_RANGE_LIST,
      OPTIMIZATION_METHOD_VECTOR
    ),
    prints_text("discoverOptimalInitialParams()")
  )
}


# check the input/output of the findOptimumValues() function
test.findOptimumValues <- function()
{
  # check the input 
  checkEqualsNumeric(length(b_RangeDetailsList), 3, "<b_RangeDetailsList> columns length mismatch")
  if (((RFMTC_PARAM_RANGE_LIST[1] %in% RFMTC_PARAM_RANGE_LIST_MIN[1]) 
       && (RFMTC_PARAM_RANGE_LIST[2] %in% RFMTC_PARAM_RANGE_LIST_MIN[2])
       && (RFMTC_PARAM_RANGE_LIST[3] %in% RFMTC_PARAM_RANGE_LIST_MIN[3])) == TRUE)
  {
    expect_equal(b_RangeDetailsList[["MAX"]],14.47827, tolerance=1e-3)
    expect_equal(b_RangeDetailsList[["MIN"]],0.9048921, tolerance=1e-3)
  }
  # check the output
  expect_that(
    findOptimumValues(
      resultList,
      rfmtcReadyDF,
      b_RangeDetailsList[["MIN"]],
      b_RangeDetailsList[["MAX"]]
    ),
    prints_text("findOptimumValues()")
  )
}


# check the input/output of the calc_b_RangeConstraints() function
test.exl1_OptimalCalc <- function()
{
  # check the input c(Q, g, b)
  checkTrue(length(optimumParams$x) == 4, "<length(optimumParams$x)> length mismatch")
  #checkEqualsNumeric(c(Q, g, b), c(0.111, 3.72, 9.52), "<REFERENCE_OPTIMALS> wrong initiation")
  checkTrue((optimumParams$x[2] >= 0.01) && (optimumParams$x[2] <= 0.2),
            "<optimumParams$x[2] <Q>> wrong value")
  checkTrue((optimumParams$x[3] >= 0.25) && (optimumParams$x[3] <= 4),
            "<optimumParams$x[3] <g>> wrong value")
  checkTrue((optimumParams$x[4] >= 1.96) && (optimumParams$x[4] <= 31.4),
            "<optimumParams$x[4] <b>> wrong value")
  # check the output ourRfmtcScoredDF
  checkEqualsNumeric(length(ourRfmtcScoredDF), 11, "<ourRfmtcScoredDF> columns length mismatch")
  # ourRfmtcScoredDF
  expect_that(
    exl1_OptimalCalc(c(Q, g, b), rfmtcReadyDF),
    prints_text("exl1_OptimalCalc()")
  )
}
