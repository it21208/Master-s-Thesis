options(warn = -1) # 0

# installs and loads the packages needed automatically
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret, SDMTools, e1071, party, randomForest,
               kernlab, lattice, klaR, rpart, class, cluster, stats,
               KernelKnn, pls, gdata, nnet, glmnet, mda, MASS, C50,
               RWeka, C50,  RWeka, adabag, mlbench, ipred, Cubist, ada, CORElearn)

# ---------------------- Install Packages Manually If Needed -----------------------------------
# install.packages("e1071"); install.packages("SDMTools"); install.packages("caret"); install.packages("randomForest"); 
# install.packages("kernlab"); install.packages("party"); install.packages("lattice"); install.packages("class");

# ---------------------- Load Libraries Manually If Needed -----------------------------------
# library(caret); library(SDMTools); library(e1071); library(party); library(randomForest); library(kernlab); library(lattice); 
# library(klaR); library(rpart); library(class); library(cluster)

# read training and test results of the RFMTC model using the optimal initial params given by I-Cheng Yeh
RFMTC_REF_BTS_TRAINING  = read.csv(file = "REF_RFMTC_TRAINING.csv", header = TRUE, sep = ",")
RFMTC_REF_BTS_TEST  = read.csv(file = "REF_RFMTC_TEST.csv", header = TRUE, sep = ",")

# extract the observed values and the predicted values
EXL_L1_RFMTC_REF_BTS_TRAINING = RFMTC_REF_BTS_TRAINING[, c("Churn..0.1.", "E.X.L..L.1.")]
EXL_L1_RFMTC_REF_BTS_TEST = RFMTC_REF_BTS_TEST[, c("Churn..0.1.", "E.X.L..L.1.")]

# round the predicted values
ROUNDED_EXL_L1_RFMTC_REF_BTS_TRAINING = round(EXL_L1_RFMTC_REF_BTS_TRAINING[, c("E.X.L..L.1.")])
ROUNDED_EXL_L1_RFMTC_REF_BTS_TEST = round(EXL_L1_RFMTC_REF_BTS_TEST[, c("E.X.L..L.1.")])

# calculate the confusion matrices for the RFMTC
RFMTC_confusionMatrix_Training = confusion.matrix(RFMTC_REF_BTS_TRAINING[, c("Churn..0.1.")],
                                                  ROUNDED_EXL_L1_RFMTC_REF_BTS_TRAINING,
                                                  threshold = 0.5)
RFMTC_confusionMatrix_Test = confusion.matrix(RFMTC_REF_BTS_TEST[, c("Churn..0.1.")],
                                              ROUNDED_EXL_L1_RFMTC_REF_BTS_TEST,
                                              threshold = 0.5)
RFMTCaccuracy <- sum(diag(RFMTC_confusionMatrix_Test)) / sum(RFMTC_confusionMatrix_Test)

# print the confusion matrices for the RFMTC model
print("1.Confusion Matrix for the RFMTC predictions using the training set of the Blood Transfusion dataset")
print(RFMTC_confusionMatrix_Training)
print("-----------------------------------------------------------------------------------------------------------")
print("2.Confusion Matrix for the RFMTC predictions using the test set of the Blood Transfusion dataset")
print(RFMTC_confusionMatrix_Test)
cat("Accuracy: %s", RFMTCaccuracy);cat("\n");
print("-----------------------------------------------------------------------------------------------------------")

# train and test set in the format needed from the rest of the algorithms
trainset = RFMTC_REF_BTS_TRAINING[2:6]
testset = RFMTC_REF_BTS_TEST[2:5]

# SVM
svm.model <- svm(Churn..0.1. ~ ., data = trainset)
svm.pred  <- predict(svm.model, testset)
rounded.svm.pred = round(svm.pred)
SVM_confusionMatrix_Test = confusion.matrix(RFMTC_REF_BTS_TEST[, c("Churn..0.1.")], rounded.svm.pred, threshold = 0.5)
SVMaccuracy <- sum(diag(SVM_confusionMatrix_Test)) / sum(SVM_confusionMatrix_Test)
print("3.Confusion Matrix for the SVM model predictions")
print(SVM_confusionMatrix_Test)
cat("Accuracy: %s", SVMaccuracy);cat("\n");
print("-----------------------------------------------------------------------------------------------------------")

# KSVM
ksvm.model <- ksvm(Churn..0.1. ~ ., data = trainset)
ksvm.pred  <- predict(ksvm.model, testset)
rounded.ksvm.pred = round(ksvm.pred)
KSVM_confusionMatrix_Test = confusion.matrix(RFMTC_REF_BTS_TEST[, c("Churn..0.1.")], rounded.ksvm.pred, threshold = 0.5)
KSVMaccuracy <- sum(diag(KSVM_confusionMatrix_Test)) / sum(KSVM_confusionMatrix_Test)
print("4.Confusion Matrix for the KSVM model predictions")
print(KSVM_confusionMatrix_Test)
cat("Accuracy: %s", KSVMaccuracy);cat("\n");
print("-----------------------------------------------------------------------------------------------------------")

# Random forest
randomforest.model <- randomForest(Churn..0.1. ~ ., data = trainset, importance = TRUE, proximity = TRUE)
randomforest.pred  <- predict(randomforest.model, testset)
rounded.randomforest.pred = round(randomforest.pred)
RF_confusionMatrix_Test = confusion.matrix(RFMTC_REF_BTS_TEST[, c("Churn..0.1.")], rounded.randomforest.pred, threshold = 0.5)
RFaccuracy <- sum(diag(RF_confusionMatrix_Test)) / sum(RF_confusionMatrix_Test)
print("5.Confusion Matrix for the Random Forest algorithm predictions")
print(RF_confusionMatrix_Test)
cat("Accuracy: %s", RFaccuracy);cat("\n");
print("-----------------------------------------------------------------------------------------------------------")

# Naïve Bayes
naive.bayes.model <- naiveBayes(as.factor(Churn..0.1.) ~ ., data = trainset)
naive.bayes.pred <- predict(naive.bayes.model, newdata = testset)
NaiveBayes_confusionMatrix_Test = confusion.matrix(RFMTC_REF_BTS_TEST[, c("Churn..0.1.")],
                                                   as.numeric(as.character(naive.bayes.pred)),
                                                   threshold = 0.5)
NaiveBayesaccuracy <- sum(diag(NaiveBayes_confusionMatrix_Test)) / sum(NaiveBayes_confusionMatrix_Test)
print("6.Confusion Matrix for the Naive Bayes model predictions")
print(NaiveBayes_confusionMatrix_Test)
cat("Accuracy: %s", NaiveBayesaccuracy);cat("\n");
print("-----------------------------------------------------------------------------------------------------------")

# CARET
RegressionTreeMethod.model <- rpart(Churn..0.1. ~ ., data = trainset, method = "anova")
RegressionTreeMethod.pred <- predict(RegressionTreeMethod.model, newdata = testset)
RegressionTreeMethod_confusionMatrix_Test = confusion.matrix(RFMTC_REF_BTS_TEST[,c("Churn..0.1.")],
                                                             RegressionTreeMethod.pred,
                                                             threshold = 0.5)
CARTaccuracy <- sum(diag(RegressionTreeMethod_confusionMatrix_Test)) / sum(RegressionTreeMethod_confusionMatrix_Test)
print("7.Confusion Matrix for the CARET model predictions")
print(RegressionTreeMethod_confusionMatrix_Test)
cat("Accuracy: %s", CARTaccuracy);cat("\n");
print("-----------------------------------------------------------------------------------------------------------")

# K- Nearest Neighbors
y = trainset[, ncol(trainset)]
temp_testset = RFMTC_REF_BTS_TEST[2:6]
knn.model.pred = KernelKnn(trainset, TEST_data = temp_testset, y, k = 2 , method = 'euclidean', regression = TRUE, Levels = 2)
rounded_knn.pred = round(knn.model.pred)
knn.model_confusionMatrix_Test = confusion.matrix(RFMTC_REF_BTS_TEST[,c("Churn..0.1.")], rounded_knn.pred, threshold = 0.5)
KNNaccuracy <- sum(diag(knn.model_confusionMatrix_Test)) / sum(knn.model_confusionMatrix_Test)
print("8.Confusion Matrix for the K- Nearest Neighbors model predictions")
print(knn.model_confusionMatrix_Test)
cat("Accuracy: %s", KNNaccuracy);cat("\n");
print("-----------------------------------------------------------------------------------------------------------")

# K-Means
KMeans.model <- kmeans(testset, 2)
KMeans.pred <- as.numeric(unlist(KMeans.model[1]))
KMeans.pred[KMeans.pred == 2] <- 0
KMeans_confusionMatrix_Test = confusion.matrix(RFMTC_REF_BTS_TEST[,c("Churn..0.1.")], KMeans.pred, threshold = 0.5)
KMeansaccuracy <- sum(diag(KMeans_confusionMatrix_Test)) / sum(KMeans_confusionMatrix_Test)
print("9.Confusion Matrix for the K-Means model predictions")
print(KMeans_confusionMatrix_Test)
cat("Accuracy: %s", KMeansaccuracy);cat("\n");
print("-----------------------------------------------------------------------------------------------------------")

# Flexible Discriminant Analysis
ctree.model <- ctree(Churn..0.1. ~ ., data = trainset, controls = ctree_control(minsplit = 2,minbucket = 2,testtype = "Univariate"))
ctree.pred <- predict(ctree.model, testset)
rounded_ctree.pred <- round(ctree.pred)
ctree_confusionMatrix_Test = confusion.matrix(RFMTC_REF_BTS_TEST[,c("Churn..0.1.")], rounded_ctree.pred, threshold = 0.5)
ctreeaccuracy <- sum(diag(ctree_confusionMatrix_Test)) / sum(ctree_confusionMatrix_Test)
print("10.Confusion Matrix for the Flexible Discriminant Analysis model predictions")
print(ctree_confusionMatrix_Test)
cat("Accuracy: %s", ctreeaccuracy);cat("\n");
print("-----------------------------------------------------------------------------------------------------------")

# Model Trees
M5P.model <- M5P(Churn..0.1. ~ ., data = trainset)
M5P.pred <- predict(M5P.model, testset)
M5P.rounded_ctree.pred <- round(M5P.pred)
M5P_confusionMatrix_Test = confusion.matrix(RFMTC_REF_BTS_TEST[,c("Churn..0.1.")], rounded_ctree.pred, threshold = 0.5)
M5Paccuracy <- sum(diag(M5P_confusionMatrix_Test)) / sum(M5P_confusionMatrix_Test)
print("11.Confusion Matrix for the Model Trees model predictions")
print(M5P_confusionMatrix_Test)
cat("Accuracy: %s", M5Paccuracy);cat("\n");
print("-----------------------------------------------------------------------------------------------------------")

# Boosted C5.0
C5.0.model <- C5.0(as.factor(Churn..0.1.) ~ ., data = trainset,  rules = TRUE)
C5.0.pred <- predict(C5.0.model, testset)
C5.0_confusionMatrix_Test = confusion.matrix(RFMTC_REF_BTS_TEST[,c("Churn..0.1.")], C5.0.pred, threshold = 0.5)
C50accuracy <- sum(diag(C5.0_confusionMatrix_Test)) / sum(C5.0_confusionMatrix_Test)
print("12.Confusion Matrix for the Boosted C5.0 model predictions")
print(C5.0_confusionMatrix_Test)
cat("Accuracy: %s", C50accuracy);cat("\n");
print("-----------------------------------------------------------------------------------------------------------")

# LDA
lda.model <- lda(Churn..0.1.~., data = trainset)
lda.pred <- predict(lda.model, testset)
lda_confusionMatrix_Test = confusion.matrix(RFMTC_REF_BTS_TEST[,c("Churn..0.1.")], unlist(lda.pred[1]), threshold = 0.5)
lda.accuracy <- sum(diag(lda_confusionMatrix_Test)) / sum(lda_confusionMatrix_Test)
print("13.Confusion Matrix for the LDA model predictions")
print(lda_confusionMatrix_Test)
cat("Accuracy: %s", lda.accuracy);cat("\n");
print("-----------------------------------------------------------------------------------------------------------")

#  Rule System
M5Rules.model <- M5Rules(Churn..0.1. ~ ., data = trainset)
M5Rules.pred <- predict(M5Rules.model, testset)
rounded_M5Rules.pred = round(M5Rules.pred)
M5Rules_confusionMatrix_Test = confusion.matrix(RFMTC_REF_BTS_TEST[, c("Churn..0.1.")], rounded_M5Rules.pred, threshold = 0.5)
M5R.accuracy <- sum(diag(M5Rules_confusionMatrix_Test)) / sum(M5Rules_confusionMatrix_Test)
print("14.Confusion Matrix for the Rule System model predictions")
print(M5Rules_confusionMatrix_Test)
cat("Accuracy: %s", M5R.accuracy);cat("\n");
print("-----------------------------------------------------------------------------------------------------------")

# Partial Least Squares 
# prepare train test and fix column names
plsda.model <- plsda(trainset[,1:4], as.factor(trainset[,5]), probMethod = "Bayes")
plsda.pred <- predict(plsda.model, testset)
plsda_confusionMatrix_Test = confusion.matrix(RFMTC_REF_BTS_TEST[, c("Churn..0.1.")], plsda.pred, threshold = 0.5)
plsda.accuracy <- sum(diag(plsda_confusionMatrix_Test)) / sum(plsda_confusionMatrix_Test)
print("15.Confusion Matrix for the Partial Least Squares model predictions")
print(plsda_confusionMatrix_Test)
cat("Accuracy: %s", plsda.accuracy);cat("\n");
print("-----------------------------------------------------------------------------------------------------------")

# Ada for Stochastic Boosting
ada.model = ada(as.matrix(trainset[,1:4]), as.factor(trainset[,5]))
ada.pred = predict(ada.model, testset)
ada_confusionMatrix_Test = confusion.matrix(RFMTC_REF_BTS_TEST[, c("Churn..0.1.")], ada.pred, threshold = 0.5)
ada.accuracy <- sum(diag(ada_confusionMatrix_Test)) / sum(ada_confusionMatrix_Test)
print("16.Confusion Matrix for the Ada Stochastic Boosting model predictions")
print(ada_confusionMatrix_Test)
cat("Accuracy: %s", ada.accuracy);cat("\n");
print("-----------------------------------------------------------------------------------------------------------")

# RFM 
RFM_OUR_BTS_TRAINING  = read.csv(file = "OUR_RFM_TRAINING.csv", header = TRUE, sep = ",")
RFM_OUR_BTS_TEST  = read.csv(file = "OUR_RFM_TEST.csv", header = TRUE, sep = ",")

RFM_RESP_PROB_OUR_BTS_TRAINING = RFM_OUR_BTS_TRAINING[, c("Churn..0.1.", "RFM.Resp.Prob")]
RFM_RESP_PROB_OUR_BTS_TEST = RFM_OUR_BTS_TEST[, c("Churn..0.1.", "RFM.Resp.Prob")]

# round the predicted values
ROUNDED_RFM_RESP_PROB_OUR_BTS_TRAINING = round(RFM_RESP_PROB_OUR_BTS_TRAINING[, c("RFM.Resp.Prob")])
ROUNDED_RFM_RESP_PROB_OUR_BTS_TEST = round(RFM_RESP_PROB_OUR_BTS_TEST[, c("RFM.Resp.Prob")])

# calculate the confusion matrices for the RFM
RFM_confusionMatrix_Training = confusion.matrix(RFM_OUR_BTS_TRAINING[, c("Churn..0.1.")],
                                                  ROUNDED_RFM_RESP_PROB_OUR_BTS_TRAINING,
                                                  threshold = 0.5)
RFM_confusionMatrix_Test = confusion.matrix(RFM_OUR_BTS_TEST[, c("Churn..0.1.")],
                                              ROUNDED_RFM_RESP_PROB_OUR_BTS_TEST,
                                              threshold = 0.5)
RFMTC.accuracy.test <- sum(diag(RFM_confusionMatrix_Test)) / sum(RFM_confusionMatrix_Test)
print("17.Confusion Matrix for the RFM model predictions")
print(RFM_confusionMatrix_Test)
cat("Accuracy: %s", RFMTC.accuracy.test);cat("\n");
print("-----------------------------------------------------------------------------------------------------------")


# WRFM 
WRFM_OUR_BTS_TEST  = read.csv(file = "OUR_WRFM_TEST.csv", header = TRUE, sep = ",")
WRFM_RESP_PROB_OUR_BTS_TEST = WRFM_OUR_BTS_TEST[, c("Churn..0.1.", "RFM.Resp.Prob")]
ROUNDED_WRFM_RESP_PROB_OUR_BTS_TEST = round(WRFM_RESP_PROB_OUR_BTS_TEST[, c("RFM.Resp.Prob")])

WRFM_confusionMatrix_Test = confusion.matrix(WRFM_OUR_BTS_TEST[, c("Churn..0.1.")],
                                              ROUNDED_WRFM_RESP_PROB_OUR_BTS_TEST,
                                              threshold = 0.5)

WRFM.accuracy.test <- sum(diag(WRFM_confusionMatrix_Test)) / sum(WRFM_confusionMatrix_Test)
print("18.Confusion Matrix for the WRFM model predictions")
print(WRFM_confusionMatrix_Test)
cat("Accuracy: %s", WRFM.accuracy.test);cat("\n");
print("-----------------------------------------------------------------------------------------------------------")

# Dimensionality Reduction Algorithms
pca <- princomp(trainset[,1:4], cor = FALSE)
train_reduced  <- predict(pca, trainset[,1:4])
test_reduced  <- predict(pca,testset)
princomp_confusionMatrix_Test = confusion.matrix(RFMTC_REF_BTS_TEST[, c("Churn..0.1.")], ada.pred, threshold = 0.5)
princomp.accuracy <- sum(diag(princomp_confusionMatrix_Test)) / sum(princomp_confusionMatrix_Test)
print("19.Confusion Matrix for the Princomp model predictions")
print(princomp_confusionMatrix_Test)
cat("Accuracy: ", princomp.accuracy);cat("\n");
print("-----------------------------------------------------------------------------------------------------------")

# CORElearn - Regression Tree
model.CoreModel <-
   CoreModel(
      Churn..0.1.~.,
      data = trainset,
      model = "regTree",
      maxThreads = 1,
      costMatrix = NULL
   )

CoreModel.pred = predict(model.CoreModel , newdata = testset)
Rounded.CoreModel.pred <- round(CoreModel.pred)
CoreModel_confusionMatrix_Test = confusion.matrix(RFMTC_REF_BTS_TEST[, c("Churn..0.1.")], Rounded.CoreModel.pred, threshold = 0.5)
CoreModel.accuracy <- sum(diag(CoreModel_confusionMatrix_Test)) / sum(CoreModel_confusionMatrix_Test)
print("20.Confusion Matrix for the Core model predictions")
print(CoreModel_confusionMatrix_Test)
cat("Accuracy: ", CoreModel.accuracy);cat("\n");
print("-----------------------------------------------------------------------------------------------------------")

# Dimensionality Reduction Algorithms
pca <- princomp(~ ., data = trainset)
train_reduced  <- predict(pca, trainset)
temp_testset <- RFMTC_REF_BTS_TEST[2:6]
test_reduced  <- predict(pca, temp_testset)
print("21.Show the Importance of each feature (head()) for each record with the Dimensionality Reduction Algorithms model predictions")
print(head(test_reduced))
cat("\n")
