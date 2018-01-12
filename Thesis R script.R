SimIm <- function(data, p = 0.1) {
  vec <- c(unlist(data))
  missing <- rbinom(length(vec), 1, p)
  vec[missing == 1] <- NA
  dim(vec) <- dim(data)
  return(vec)
}


if (F) {
## Install packages ----------------------------------------------------------------------------------------------------
install.packages("readxl")
# install.packages("simglm")
install.packages("caret")
install.packages("devtools")
install.packages("VIM")
install.packages("roughrf")
install.packages("DMwR")
install.packages("mice")
install.packages("missForest", dependencies = TRUE)
install.packages("e1071")
install.packages("imputeR")
install.packages("ForImp")
install_github("lebebr01/simglm", build_vignettes = TRUE) # in this section because of different install procedure
install.packages("ggplot2")
install.packages("rpart")
install.packages("hot.deck")
install.packages("HotDeckImputation")
install.packages("bnstruct")
install.packages("randomForest")
# install.packages("slam")
}



## Loading packages ----------------------------------------------------------------------------------------------------
library("readxl")
library("simglm")
library("caret")
library("devtools")
library("VIM")
library("roughrf")
library("DMwR")
library("mice")
library("missForest")
library("e1071")
library("imputeR")
library("ForImp")
library("ggplot2")
library("rpart")
library("hot.deck")
library("HotDeckImputation")
library("bnstruct")
library("randomForest")



## Loading the databases -------------------------------------------------------------------------------------------------------------
ipumsdeel1 <- read_excel("D:/Amber/Documenten/School/Tilburg University/Master/Thesis/IPUMS2001 deel 1.xlsx")
ipumsdeel2 <- read_excel("D:/Amber/Documenten/School/Tilburg University/Master/Thesis/IPUMS2001 deel 2.xlsx")
# ipumsdeel1 <- read_excel("IPUMS2001 deel 1.xlsx")
# ipumsdeel2 <- read_excel()



## Combining databases by rows -------------------------------------------------------------------------------------------------------
#ipums <- bind_rows(y,z)
ipums <- rbind(ipumsdeel1, ipumsdeel2)

tail(ipumsdeel1, 5)
head(ipumsdeel2, 5)

slice(ipums, 89996:90005) #to check if deel 2's first row comes after deel 1's last row. 



## Open database ---------------------------------------------------------------------------------------------------------------------
View(ipums)



## Erase column 'nr' -----------------------------------------------------------------------------------------------------------------
ipums <- ipums[-1]



## Erase column 'Gewicht' (not relevant) ---------------------------------------------------------------------------------------------
ipums$Gewicht <- NULL 



## Creating missing values (MCAR with a 5% maximum treshold) -------------------------------------------------------------------------
MCAR <- SimIm(ipums, p = 0.05)       #https://cran.r-project.org/web/packages/imputeR/imputeR.pdf  
  # How can I keep the variable column names?
  
View(MCAR)
  
  # Counting NA's in dataset MCAR 
  sum(is.na(MCAR))

  prop.table(table(is.na(MCAR)))

  # Looking at the missing data pattern
  md.pattern(MCAR) 

  # Visualizing missing data pattern
  tableplot(MCAR)
  aggr(MCAR, prop = T, numbers = T)
  matrixplot(MCAR, interactive = F)
  
  # https://datascienceplus.com/imputing-missing-data-with-r-mice-package/ 


  
## Adding column names to MCAR ------------------------------------------------------------------------------------------------------
# colnames(MCAR, do.NULL = FALSE, prefix = "v")
  
  
## Creating subset of IPUMS en MCAR dataset -----------------------------------------------------------------------------------------
subset_IPUMS <- ipums[c(1:500), c(1:12)]
subset_MCAR <- MCAR[c(1:500), c(1:12)]

DFSUBMCAR <- data.frame(subset_MCAR)

View(subset_MCAR)



## Mode Imputation with 'ForImp' ------------------------------------------------------------------------------------------------------
set.seed(1)
mode_imputation1 <- modeimp(subset_MCAR)

  # Are all NA's replaced?
  anyNA(mode_imputation1)

  # Computing error  
  MOItotal_error1 <- regr.eval(subset_IPUMS, mode_imputation1)
  
  
  #r-statistics.co/Missing-Value-Treatment-With-R.html 

  

## Mode Imputation with 'roughrf' ------------------------------------------------------------------------------------------------------  
set.seed(2)
mode_imputation2 <- mfix(subset_MCAR, mmmm = 2)  
  
  # Are all NA's replaced?
  anyNA(mode_imputation2)
  
  # Computing error 
  MOItotal_error2 <- regr.eval(subset_IPUMS, mode_imputation2)
  
  

## Mode Imputation with 'imputeR' ------------------------------------------------------------------------------------------------------!!!
set.seed(3)
mode_imputation3 <- impute(subset_MCAR)  
  
  # Are all NA's replaced?
  anyNA(mode_imputationR)
  
  # Computing error
  MOItotal_error3 <- regr.eval(subset_IPUMS, mode_imputation3)
  
  
  
## Hot Deck Imputation with 'hot.deck' ----------------------------------------------------------------------------------------------!!!
set.seed(4)
hotdeck_imputation1 <- hot.deck(subset_MCAR, m = 5, method = "best.cell") # why did I choose which method?
#hot.deck_imputation2 <- hot.deck(subset_MCAR, m = 5, method = "p.draw")

  # Are all NA's replaced?
  anyNA(hotdeck_imputation1)
  #anyNA(hotdeck_imputation2)
  
  # Computing error 
  HDtotal_error1 <- regr.eval(subset_IPUMS, hotdeck_imputation1)
  #HDtotal_error1 <- regr.eval(subset_IPUMS, hotdeck_imputation2)
  
  
  
## Hot Deck Imputation with 'VIM' ---------------------------------------------------------------------------------------------------  
set.seed(5)
hotdeck_imputation2 <- hotdeck(subset_MCAR, imp_var = FALSE) 
  
  # Are all NA's replaced?
  anyNA(hotdeck_imputation2)
  
  # Computing error
  HDtotal_error2 <- regr.eval(subset_IPUMS, hotdeck_imputation2)

  

## Hot Deck Imputation with 'HotDeckImputation' ------------------------------------------------------------------------------------- Loading problem
set.seed(6)
hotdeck_imputation3 <- impute.SEQ_HD(subset_MCAR, modifyinplace = TRUE)
  
  # Are all NA's replaced?
  anyNA(hotdeck_imputation3)
  
  # Computing error 
  HDtotal_error3 <- regr.eval(subset_IPUMS, hotdeck_imputation3)
  

  
## Multiple Imputation with 'MICE' ------------------------------------------------------------------------------------------------- SKIPS V10, V11, V12. No predictive value? + Can't get complete() to work
set.seed(7)               
multiple_imputation <- mice(DFSUBMCAR, predictorMatrix = (diag(1, ncol(DFSUBMCAR))), m = 5)
multiple_imputation <- complete(multiple_imputation)
summary(multiple_imputation)
  
  # Get complete data
  multiple_imputation_output <- complete(multiple_imputation)
  
  # Are all NA's replaced?
  anyNA(multiple_imputation)
  
  # Computing error 
  MItotal_error <- regr.eval(ipumsdeel1, multiple_imputation_output) 
  
  
  # https://datascienceplus.com/imputing-missing-data-with-r-mice-package/

  
  
## Random Forest Imputation with 'missForest' -----------------------------------------------------------------------------------
set.seed(8)
random_forest1 <- missForest(subset_MCAR, ntree = 30, maxiter = 5, replace = TRUE)
  
  # Are all NA's replaced?
  anyNA(random_forest1)
  
  # Computing error
  RFtotal_error1 <- regr.eval(subset_IPUMS, random_forest1$ximp)
  
  # https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/
  # https://stat.ethz.ch/education/semesters/ss2013/ams/paper/missForest_1.2.pdf

  
  
## Random Forest Imputation with 'MICE' ---------------------------------------------------------------------------------------- Do not know how to define ry = 
set.seed(9)
random_forest2 <- mice.impute.rf(x = subset_MCAR, ry = length(subset_MCAR), subset_IPUMS, ntree = 30)

  # Are all NA's replaced?
  anyNA(random_forest2)
  
  # Computing error
  RFtotal_error2 <- regr.eval(subset_IPUMS, random_forest2)
  
  
  
## Random Forest Imputation with 'randomForest' -------------------------------------------------------------------------------------------------
set.seed(10)
  
  # Setting total error to '0'
  RFtotal_error3 <- 0 
  
  # Splitting the datasets into train sets: NBtrain_1 <- NB_mcar[!is.na(NB_mcar[,1]),] and NBtest_1 <- NB_mcar[is.na(NB_mcar[,1]),]
  RFcreate_train <- function(data, column){
    data[!is.na(data[,column]),]
  }
  
  RFtrain_1 <- RFcreate_train(DFSUBMCAR, 1)
  RFtrain_2 <- RFcreate_train(DFSUBMCAR, 2)
  RFtrain_3 <- RFcreate_train(DFSUBMCAR, 3)
  RFtrain_4 <- RFcreate_train(DFSUBMCAR, 4)
  RFtrain_5 <- RFcreate_train(DFSUBMCAR, 5)
  RFtrain_6 <- RFcreate_train(DFSUBMCAR, 6)
  RFtrain_7 <- RFcreate_train(DFSUBMCAR, 7)
  RFtrain_8 <- RFcreate_train(DFSUBMCAR, 8)
  RFtrain_9 <- RFcreate_train(DFSUBMCAR, 9)
  RFtrain_10 <- RFcreate_train(DFSUBMCAR, 10)
  RFtrain_11 <- RFcreate_train(DFSUBMCAR, 11)
  RFtrain_12 <- RFcreate_train(DFSUBMCAR, 12)
  
  # Splitting the datasets into test sets 
  RFcreate_test <- function(data, column){
    data[is.na(data[,column]),]
  }
  
  RFtest_1 <- RFcreate_test(DFSUBMCAR, 1)
  RFtest_2 <- RFcreate_test(DFSUBMCAR, 2)
  RFtest_3 <- RFcreate_test(DFSUBMCAR, 3)
  RFtest_4 <- RFcreate_test(DFSUBMCAR, 4)
  RFtest_5 <- RFcreate_test(DFSUBMCAR, 5)
  RFtest_6 <- RFcreate_test(DFSUBMCAR, 6)
  RFtest_7 <- RFcreate_test(DFSUBMCAR, 7)
  RFtest_8 <- RFcreate_test(DFSUBMCAR, 8)
  RFtest_9 <- RFcreate_test(DFSUBMCAR, 9)
  RFtest_10 <- RFcreate_test(DFSUBMCAR, 10)
  RFtest_11 <- RFcreate_test(DFSUBMCAR, 11)
  RFtest_12 <- RFcreate_test(DFSUBMCAR, 12)
  
  # Building the predicting models for all 12 columns 
  RFmodel_1 <- randomForest(formula = X1 ~ ., data = RFtrain_1, ntree = 30)   # get error... but there has to be missing values in object?
  RFmodel_2 <- randomForest(formula = X2 ~ ., data = RFtrain_2, ntree = 30)
  RFmodel_3 <- randomForest(formula = X3 ~ ., data = RFtrain_3, ntree = 30)
  RFmodel_4 <- randomForest(formula = X4 ~ ., data = RFtrain_4, ntree = 30)
  RFmodel_5 <- randomForest(formula = X5 ~ ., data = RFtrain_5, ntree = 30)
  RFmodel_6 <- randomForest(formula = X6 ~ ., data = RFtrain_6, ntree = 30)
  RFmodel_7 <- randomForest(formula = X7 ~ ., data = RFtrain_7, ntree = 30)
  RFmodel_8 <- randomForest(formula = X8 ~ ., data = RFtrain_8, ntree = 30)
  RFmodel_9 <- randomForest(formula = X9 ~ ., data = RFtrain_9, ntree = 30)
  RFmodel_10 <- randomForest(formula = X10 ~ ., data = RFtrain_10, ntree = 30)
  RFmodel_11 <- randomForest(formula = X11 ~ ., data = RFtrain_11, ntree = 30)
  RFmodel_12 <- randomForest(formula = X11 ~ ., data = RFtrain_12, ntree = 30)
  
  # Imputing values for all columns: predictions_1 <- predict(model_1, newdata = test_1)
  RFimpute_values <- function(model, newdata){
    predict(model, newdata = testdata)
  }
  
  RFpredictions_1 <- RFimpute_values(RFmodel_1, RFtest_1)
  RFpredictions_2 <- RFimpute_values(RFmodel_2, RFtest_2)
  RFpredictions_3 <- RFimpute_values(RFmodel_3, RFtest_3)
  RFpredictions_4 <- RFimpute_values(RFmodel_4, RFtest_4)
  RFpredictions_5 <- RFimpute_values(RFmodel_5, RFtest_5)
  RFpredictions_6 <- RFimpute_values(RFmodel_6, RFtest_6)
  RFpredictions_7 <- RFimpute_values(RFmodel_7, RFtest_7)
  RFpredictions_8 <- RFimpute_values(RFmodel_8, RFtest_8)
  RFpredictions_9 <- RFimpute_values(RFmodel_9, RFtest_9)
  RFpredictions_10 <- RFimpute_values(RFmodel_10, RFtest_10)
  RFpredictions_11 <- RFimpute_values(RFmodel_11, RFtest_11)
  RFpredictions_12 <- RFimpute_values(RFmodel_12, RFtest_12)
  
  # Comparing against the true values: true_1 <- ipumsdeel1[is.na(RF_mcar[,1]), 1]
  RFcompare_true <- funtion(data, dataNA, column){
    data[is.na(dataNA[,column]), column]
  }
  
  RFtrue_1 <- RFcompare_true(subset_IPUMS, DFSUBMCAR, 1)
  RFtrue_2 <- RFcompare_true(subset_IPUMS, DFSUBMCAR, 2)
  RFtrue_3 <- RFcompare_true(subset_IPUMS, DFSUBMCAR, 3)
  RFtrue_4 <- RFcompare_true(subset_IPUMS, DFSUBMCAR, 4)
  RFtrue_5 <- RFcompare_true(subset_IPUMS, DFSUBMCAR, 5)
  RFtrue_6 <- RFcompare_true(subset_IPUMS, DFSUBMCAR, 6)
  RFtrue_7 <- RFcompare_true(subset_IPUMS, DFSUBMCAR, 7)
  RFtrue_8 <- RFcompare_true(subset_IPUMS, DFSUBMCAR, 8)
  RFtrue_9 <- RFcompare_true(subset_IPUMS, DFSUBMCAR, 9)
  RFtrue_10 <- RFcompare_true(subset_IPUMS, DFSUBMCAR, 10)
  RFtrue_11 <- RFcompare_true(subset_IPUMS, DFSUBMCAR, 11)
  RFtrue_12 <- RFcompare_true(subset_IPUMS, DFSUBMCAR, 12)
  
  # Computing error for all columns 
  RFerror <- sum(!RFtrue_1 == RFpredictions_1, !RFtrue_2 == RFpredictions_2, !RFtrue_3 == RFpredictions_3, 
                 !RFtrue_4 == RFpredictions_4, !RFtrue_5 == RFpredictions_5, !RFtrue_6 == RFpredictions_6, 
                 !RFtrue_7 == RFpredictions_7, !RFtrue_8 == RFpredictions_8, !RFtrue_9 == RFpredictions_9, 
                 !RFtrue_10 == RFpredictions_10, !RFtrue_11 == RFpredictions_11, !RFtrue_12 == RFpredictions_12)
  
  # Computing total error from all columns
  RFtotal_error3 <- RFtotal_error3 + RFerror
  

  
## naiveBayes Imputation with 'e1071' -----------------------------------------------------------------------------------------------------------
set.seed(11)
  
  # Making dataframe 
  NBmcar <- data.frame(subset_MCAR)
  
  # Setting total error to '0'
  NBtotal_error <- 0
  
  # Converting to factors for prediction: NB_mcar$Geslacht <- factor(NB_mcar$Geslacht) and NB_mcar$Leeftijd <- factor(NB_mcar$Leeftijd)
  convert_factor <- function(variable, data){
    data$variable <- factor(data$variable)
  }
  
  convert_1 <- convert_factor("Geslacht", NBmcar)
  convert_2 <- convert_factor("Leeftijd", NBmcar)
  convert_3 <- convert_factor("HH_Pos", NBmcar)
  convert_4 <- convert_factor("HH_grootte", NBmcar)
  convert_5 <- convert_factor("Woonregio vorig jaar", NBmcar)
  convert_6 <- convert_factor("Nationaliteit", NBmcar)
  convert_7 <- convert_factor("Geboorteland", NBmcar)
  convert_8 <- convert_factor("Onderwijsniveau", NBmcar)
  convert_9 <- convert_factor("Econ.status", NBmcar)
  convert_10 <- convert_factor("Beroep", NBmcar)
  convert_11 <- convert_factor("SBI", NBmcar)
  convert_12 <- convert_factor("Burg.staat", NBmcar)
  
  # Splitting the datasets into train sets: NBtrain_1 <- NB_mcar[!is.na(NB_mcar[,1]),] and NBtest_1 <- NB_mcar[is.na(NB_mcar[,1]),]
  NBcreate_train <- function(data, column){
    data[!is.na(data[,column]),]
  }
  
  NBtrain_1 <- NBcreate_train(NBmcar, 1)
  NBtrain_2 <- NBcreate_train(NBmcar, 2)
  NBtrain_3 <- NBcreate_train(NBmcar, 3)
  NBtrain_4 <- NBcreate_train(NBmcar, 4)
  NBtrain_5 <- NBcreate_train(NBmcar, 5)
  NBtrain_6 <- NBcreate_train(NBmcar, 6)
  NBtrain_7 <- NBcreate_train(NBmcar, 7)
  NBtrain_8 <- NBcreate_train(NBmcar, 8)
  NBtrain_9 <- NBcreate_train(NBmcar, 9)
  NBtrain_10 <- NBcreate_train(NBmcar, 10)
  NBtrain_11 <- NBcreate_train(NBmcar, 11)
  NBtrain_12 <- NBcreate_train(NBmcar, 12)
  
  # Splitting the datasets into test sets 
  NBcreate_test <- function(data, column){
    data[is.na(data[,column]),]
  }
  
  NBtest_1 <- NBcreate_test(NBmcar, 1)
  NBtest_2 <- NBcreate_test(NBmcar, 2)
  NBtest_3 <- NBcreate_test(NBmcar, 3)
  NBtest_4 <- NBcreate_test(NBmcar, 4)
  NBtest_5 <- NBcreate_test(NBmcar, 5)
  NBtest_6 <- NBcreate_test(NBmcar, 6)
  NBtest_7 <- NBcreate_test(NBmcar, 7)
  NBtest_8 <- NBcreate_test(NBmcar, 8)
  NBtest_9 <- NBcreate_test(NBmcar, 9)
  NBtest_10 <- NBcreate_test(NBmcar, 10)
  NBtest_11 <- NBcreate_test(NBmcar, 11)
  NBtest_12 <- NBcreate_test(NBmcar, 12)
  
  # Building the predicting models for all 12 columns 
  NBmodel_1 <- naiveBayes(formula = X1 ~ ., data = NBtrain_1)
  NBmodel_2 <- naiveBayes(formula = X2 ~ ., data = NBtrain_2)
  NBmodel_3 <- naiveBayes(formula = X3 ~ ., data = NBtrain_3)
  NBmodel_4 <- naiveBayes(formula = X4 ~ ., data = NBtrain_4)
  NBmodel_5 <- naiveBayes(formula = X5 ~ ., data = NBtrain_5)
  NBmodel_6 <- naiveBayes(formula = X6 ~ ., data = NBtrain_6)
  NBmodel_7 <- naiveBayes(formula = X7 ~ ., data = NBtrain_7)
  NBmodel_8 <- naiveBayes(formula = X8 ~ ., data = NBtrain_8)
  NBmodel_9 <- naiveBayes(formula = X9 ~ ., data = NBtrain_9)
  NBmodel_10 <- naiveBayes(formula = X10 ~ ., data = NBtrain_10)
  NBmodel_11 <- naiveBayes(formula = X11 ~ ., data = NBtrain_11)
  NBmodel_12 <- naiveBayes(formula = X12 ~ ., data = NBtrain_12)
  
  # Imputing values for all columns: predictions_1 <- predict(model_1, newdata = test_1)
  NBimpute_values <- function(model, newdata){
    predict(model, newdata = testdata)
  }
  
  NBpredictions_1 <- NBimpute_values(NBmodel_1, NBtest_1)
  NBpredictions_2 <- NBimpute_values(NBmodel_2, NBtest_2)
  NBpredictions_3 <- NBimpute_values(NBmodel_3, NBtest_3)
  NBpredictions_4 <- NBimpute_values(NBmodel_4, NBtest_4)
  NBpredictions_5 <- NBimpute_values(NBmodel_5, NBtest_5)
  NBpredictions_6 <- NBimpute_values(NBmodel_6, NBtest_6)
  NBpredictions_7 <- NBimpute_values(NBmodel_7, NBtest_7)
  NBpredictions_8 <- NBimpute_values(NBmodel_8, NBtest_8)
  NBpredictions_9 <- NBimpute_values(NBmodel_9, NBtest_9)
  NBpredictions_10 <- NBimpute_values(NBmodel_10, NBtest_10)
  NBpredictions_11 <- NBimpute_values(NBmodel_11, NBtest_11)
  NBpredictions_12 <- NBimpute_values(NBmodel_12, NBtest_12)

  # Comparing against the true values: true_1 <- ipumsdeel1[is.na(NB_mcar[,1]), 1]
  NBcompare_true <- funtion(data, dataNA, column){
    data[is.na(dataNA[,column]), column]
  }
  
  NBtrue_1 <- NBcompare_true(subset_IPUMS, NBmcar, 1)
  NBtrue_2 <- NBcompare_true(subset_IPUMS, NBmcar, 2)
  NBtrue_3 <- NBcompare_true(subset_IPUMS, NBmcar, 3)
  NBtrue_4 <- NBcompare_true(subset_IPUMS, NBmcar, 4)
  NBtrue_5 <- NBcompare_true(subset_IPUMS, NBmcar, 5)
  NBtrue_6 <- NBcompare_true(subset_IPUMS, NBmcar, 6)
  NBtrue_7 <- NBcompare_true(subset_IPUMS, NBmcar, 7)
  NBtrue_8 <- NBcompare_true(subset_IPUMS, NBmcar, 8)
  NBtrue_9 <- NBcompare_true(subset_IPUMS, NBmcar, 9)
  NBtrue_10 <- NBcompare_true(subset_IPUMS, NBmcar, 10)
  NBtrue_11 <- NBcompare_true(subset_IPUMS, NBmcar, 11)
  NBtrue_12 <- NBcompare_true(subset_IPUMS, NBmcar, 12)
  
  # Computing error for all columns 
  NBerror <- sum(!NBtrue_1 == NBpredictions_1, !NBtrue_2 == NBpredictions_2, !NBtrue_3 == NBpredictions_3, 
                 !NBtrue_4 == NBpredictions_4, !NBtrue_5 == NBpredictions_5, !NBtrue_6 == NBpredictions_6, 
                 !NBtrue_7 == NBpredictions_7, !NBtrue_8 == NBpredictions_8, !NBtrue_9 == NBpredictions_9, 
                 !NBtrue_10 == NBpredictions_10, !NBtrue_11 == NBpredictions_11, !NBtrue_12 == NBpredictions_12)
  
  # Computing total error from all columns
  NBtotal_error <- NBtotal_error + NBerror

  
  
## k-Nearest Neighbor Imputation with 'DMwR' ------------------------------------------------------------------------------------------------------ # Not sufficient complete cases 
xcomplete <- subset_MCAR[setdiff(1:nrow(subset_MCAR), which(!complete.cases(subset_MCAR))),]
nrow(xcomplete)
sum(complete.cases(subset_MCAR))

if (nrow(xcomplete) < k) 
    stop("Not sufficient complete cases for computing neighbors.")

set.seed(12)
kNN1 <- knnImputation(subset_MCAR, k = 10) # Not any k-value is working  

  # Are all NA's replaced?
  anyNA(kNN1)
   
  # Computing error
  kNNtotal_error1 <- regr.eval(subset_IPUMS, kNN1)
  
  
  # https://www.youtube.com/watch?v=u8XvfhBdbMw 
  # https://www.rdocumentation.org/packages/bnstruct/versions/1.0.2/topics/knn.impute

    

## k-Nearest Neighbor Imputation with 'VIM' ------------------------------------------------------------------------------------------------------ # argument train not found
set.seed(13)
kNN2 <- kNN(subset_MCAR, k = 10)  
  
  # Are all NA's replaced?
  anyNA(kNN2)
  
  # Computing error
  kNNtotal_error2 <- regr.eval(subset_IPUMS, kNN2)
  
  
  
## k-Nearest Neighbor Imputation with 'bnstruct' ------------------------------------------------------------------------------------------------------
set.seed(14)
kNN3 <- knn.impute(subset_MCAR, k = 10) 
  
  # Are all NA's replaced?
  anyNA(kNN3)  

  # Computing error
  kNNtotal_error3 <- regr.eval(subset_IPUMS, kNN3)
  
  
  
## k-Nearest Neighbor Imputation with 'caret' ------------------------------------------------------------------------------------------------------
## Class has no predicht.knn function, caret does.
set.seed(15)
  
  # Setting total error to '0'
  kNNtotal_error4 <- 0 
  
  # Splitting the datasets into train sets: SVMtrain_1 <- MCAR[!is.na(MCAR[,1]),] and SVMtest_1 <- MCAR[is.na(MCAR[,1]),]
  kNNcreate_train <- function(data, column){
    data[!is.na(data[,column]),]
  }
  
  kNNtrain_1 <- kNNcreate_train(DFSUBMCAR, 1)
  kNNtrain_2 <- kNNcreate_train(DFSUBMCAR, 2)
  kNNtrain_3 <- kNNcreate_train(DFSUBMCAR, 3)
  kNNtrain_4 <- kNNcreate_train(DFSUBMCAR, 4)
  kNNtrain_5 <- kNNcreate_train(DFSUBMCAR, 5)
  kNNtrain_6 <- kNNcreate_train(DFSUBMCAR, 6)
  kNNtrain_7 <- kNNcreate_train(DFSUBMCAR, 7)
  kNNtrain_8 <- kNNcreate_train(DFSUBMCAR, 8)
  kNNtrain_9 <- kNNcreate_train(DFSUBMCAR, 9)
  kNNtrain_10 <- kNNcreate_train(DFSUBMCAR, 10)
  kNNtrain_11 <- kNNcreate_train(DFSUBMCAR, 11)
  kNNtrain_12 <- kNNcreate_train(DFSUBMCAR, 12)
  
  # Splitting the datasets into test sets 
  kNNcreate_test <- function(data, column){
    data[is.na(data[,column]),]
  }
  
  kNNtest_1 <- kNNcreate_test(DFSUBMCAR, 1)
  kNNtest_2 <- kNNcreate_test(DFSUBMCAR, 2)
  kNNtest_3 <- kNNcreate_test(DFSUBMCAR, 3)
  kNNtest_4 <- kNNcreate_test(DFSUBMCAR, 4)
  kNNtest_5 <- kNNcreate_test(DFSUBMCAR, 5)
  kNNtest_6 <- kNNcreate_test(DFSUBMCAR, 6)
  kNNtest_7 <- kNNcreate_test(DFSUBMCAR, 7)
  kNNtest_8 <- kNNcreate_test(DFSUBMCAR, 8)
  kNNtest_9 <- kNNcreate_test(DFSUBMCAR, 9)
  kNNtest_10 <- kNNcreate_test(DFSUBMCAR, 10)
  kNNtest_11 <- kNNcreate_test(DFSUBMCAR, 11)
  kNNtest_12 <- kNNcreate_test(DFSUBMCAR, 12)
  
  # Building the predicting models for all 12 columns
  kNNmodel_1 <- knn3(formula = X1 ~ ., data = kNNtrain_1, k = 10) 
  kNNmodel_2 <- knn3(formula = X2 ~ ., data = kNNtrain_2, k = 10)
  kNNmodel_3 <- knn3(formula = X3 ~ ., data = kNNtrain_3, k = 10)
  kNNmodel_4 <- knn3(formula = X4 ~ ., data = kNNtrain_4, k = 10)
  kNNmodel_5 <- knn3(formula = X5 ~ ., data = kNNtrain_5, k = 10)
  kNNmodel_6 <- knn3(formula = X6 ~ ., data = kNNtrain_6, k = 10)
  kNNmodel_7 <- knn3(formula = X7 ~ ., data = kNNtrain_7, k = 10)
  kNNmodel_8 <- knn3(formula = X8 ~ ., data = kNNtrain_8, k = 10)
  kNNmodel_9 <- knn3(formula = X9 ~ ., data = kNNtrain_9, k = 10)
  kNNmodel_10 <- knn3(formula = X10 ~ ., data = kNNtrain_10, k = 10)
  kNNmodel_11 <- knn3(formula = X11 ~ ., data = kNNtrain_11, k = 10)
  kNNmodel_12 <- knn3(formula = X12 ~ ., data = kNNtrain_12, k = 10)
  
  # Imputing values for all columns: predictions_1 <- predict(model_1, newdata = test_1)                                                            # object X1 not found
  kNNimpute_values <- function(model, newdata){
    predict(model, newdata = testdata)
  }
  
  kNNpredictions_1 <- kNNimpute_values(kNNmodel_1, kNNtest_1)
  kNNpredictions_2 <- kNNimpute_values(kNNmodel_2, kNNtest_2)
  kNNpredictions_3 <- kNNimpute_values(kNNmodel_3, kNNtest_3)
  kNNpredictions_4 <- kNNimpute_values(kNNmodel_4, kNNtest_4)
  kNNpredictions_5 <- kNNimpute_values(kNNmodel_5, kNNtest_5)
  kNNpredictions_6 <- kNNimpute_values(kNNmodel_6, kNNtest_6)
  kNNpredictions_7 <- kNNimpute_values(kNNmodel_7, kNNtest_7)
  kNNpredictions_8 <- kNNimpute_values(kNNmodel_8, kNNtest_8)
  kNNpredictions_9 <- kNNimpute_values(kNNmodel_9, kNNtest_9)
  kNNpredictions_10 <- kNNimpute_values(kNNmodel_10, kNNtest_10)
  kNNpredictions_11 <- kNNimpute_values(kNNmodel_11, kNNtest_11)
  kNNpredictions_12 <- kNNimpute_values(kNNmodel_12, kNNtest_12)
  
  # Comparing against the true values: true_1 <- ipumsdeel1[is.na(MCAR[,1]), 1]
  kNNcompare_true <- funtion(data, dataNA, column){
    data[is.na(dataNA[,column]), column]
  }
  
  kNNtrue_1 <- kNNcompare_true(subset_IPUMS, DFSUBMCAR, 1)
  kNNtrue_2 <- kNNcompare_true(subset_IPUMS, DFSUBMCAR, 2)
  kNNtrue_3 <- kNNcompare_true(subset_IPUMS, DFSUBMCAR, 3)
  kNNtrue_4 <- kNNcompare_true(subset_IPUMS, DFSUBMCAR, 4)
  kNNtrue_5 <- kNNcompare_true(subset_IPUMS, DFSUBMCAR, 5)
  kNNtrue_6 <- kNNcompare_true(subset_IPUMS, DFSUBMCAR, 6)
  kNNtrue_7 <- kNNcompare_true(subset_IPUMS, DFSUBMCAR, 7)
  kNNtrue_8 <- kNNcompare_true(subset_IPUMS, DFSUBMCAR, 8)
  kNNtrue_9 <- kNNcompare_true(subset_IPUMS, DFSUBMCAR, 9)
  kNNtrue_10 <- kNNcompare_true(subset_IPUMS, DFSUBMCAR, 10)
  kNNtrue_11 <- kNNcompare_true(subset_IPUMS, DFSUBMCAR, 11)
  kNNtrue_12 <- kNNcompare_true(subset_IPUMS, DFSUBMCAR, 12)
  
  # Computing error for all columns 
  kNNerror <- sum(!kNNtrue_1 == kNNpredictions_1, !kNNtrue_2 == kNNpredictions_2, !kNNtrue_3 == kNNpredictions_3, 
                  !kNNtrue_4 == kNNpredictions_4, !kNNtrue_5 == kNNpredictions_5, !kNNtrue_6 == kNNpredictions_6, 
                  !kNNtrue_7 == kNNpredictions_7, !kNNtrue_8 == kNNpredictions_8, !kNNtrue_9 == kNNpredictions_9, 
                  !kNNtrue_10 == kNNpredictions_10, !kNNtrue_11 == kNNpredictions_11, !kNNtrue_12 == kNNpredictions_12)
  
  # Computing total error from all columns
  kNNtotal_error4 <- kNNtotal_error + kNNerror
  
  
  
## Support Vector Machine Imputation with 'e1071' -------------------------------------------------------------------------------------------------
set.seed(16)
  
  # Setting total error to '0'
  SVMtotal_error <- 0
  
  # Splitting the datasets into train sets: SVMtrain_1 <- MCAR[!is.na(MCAR[,1]),] and SVMtest_1 <- MCAR[is.na(MCAR[,1]),]
  SVMcreate_train <- function(data, column){
    data[!is.na(data[,column]),]
  }
  
  SVMtrain_1 <- SVMcreate_train(DFSUBMCAR, 1)
  SVMtrain_2 <- SVMcreate_train(DFSUBMCAR, 2)
  SVMtrain_3 <- SVMcreate_train(DFSUBMCAR, 3)
  SVMtrain_4 <- SVMcreate_train(DFSUBMCAR, 4)
  SVMtrain_5 <- SVMcreate_train(DFSUBMCAR, 5)
  SVMtrain_6 <- SVMcreate_train(DFSUBMCAR, 6)
  SVMtrain_7 <- SVMcreate_train(DFSUBMCAR, 7)
  SVMtrain_8 <- SVMcreate_train(DFSUBMCAR, 8)
  SVMtrain_9 <- SVMcreate_train(DFSUBMCAR, 9)
  SVMtrain_10 <- SVMcreate_train(DFSUBMCAR, 10)
  SVMtrain_11 <- SVMcreate_train(DFSUBMCAR, 11)
  SVMtrain_12 <- SVMcreate_train(DFSUBMCAR, 12)
  
  # Splitting the datasets into test sets 
  SVMcreate_test <- function(data, column){
    data[is.na(data[,column]),]
  }
  
  SVMtest_1 <- SVMcreate_test(DFSUBMCAR, 1)
  SVMtest_2 <- SVMcreate_test(DFSUBMCAR, 2)
  SVMtest_3 <- SVMcreate_test(DFSUBMCAR, 3)
  SVMtest_4 <- SVMcreate_test(DFSUBMCAR, 4)
  SVMtest_5 <- SVMcreate_test(DFSUBMCAR, 5)
  SVMtest_6 <- SVMcreate_test(DFSUBMCAR, 6)
  SVMtest_7 <- SVMcreate_test(DFSUBMCAR, 7)
  SVMtest_8 <- SVMcreate_test(DFSUBMCAR, 8)
  SVMtest_9 <- SVMcreate_test(DFSUBMCAR, 9)
  SVMtest_10 <- SVMcreate_test(DFSUBMCAR, 10)
  SVMtest_11 <- SVMcreate_test(DFSUBMCAR, 11)
  SVMtest_12 <- SVMcreate_test(DFSUBMCAR, 12)
  
  # Building the predicting models for all 12 columns                                                                         # Cannot scale data 
  SVMmodel_1 <- svm(formula = X1 ~ ., data = SVMtrain_1, kernel = "radial")
  SVMmodel_2 <- svm(formula = X2 ~ ., data = SVMtrain_2, kernel = "radial")
  SVMmodel_3 <- svm(formula = X3 ~ ., data = SVMtrain_3, kernel = "radial")
  SVMmodel_4 <- svm(formula = X4 ~ ., data = SVMtrain_4, kernel = "radial")
  SVMmodel_5 <- svm(formula = X5 ~ ., data = SVMtrain_5, kernel = "radial")
  SVMmodel_6 <- svm(formula = X6 ~ ., data = SVMtrain_6, kernel = "radial")
  SVMmodel_7 <- svm(formula = X7 ~ ., data = SVMtrain_7, kernel = "radial")
  SVMmodel_8 <- svm(formula = X8 ~ ., data = SVMtrain_8, kernel = "radial")
  SVMmodel_9 <- svm(formula = X9 ~ ., data = SVMtrain_9, kernel = "radial")
  SVMmodel_10 <- svm(formula = X10 ~ ., data = SVMtrain_10, kernel = "radial")
  SVMmodel_11 <- svm(formula = X11 ~ ., data = SVMtrain_11, kernel = "radial")
  SVMmodel_12 <- svm(formula = X12 ~ ., data = SVMtrain_12, kernel = "radial")
  
  # Imputing values for all columns: predictions_1 <- predict(model_1, newdata = test_1)
  SVMimpute_values <- function(model, newdata){
    predict(model, newdata = testdata)
  }
  
  SVMpredictions_1 <- SVMimpute_values(SVMmodel_1, SVMtest_1)
  SVMpredictions_2 <- SVMimpute_values(SVMmodel_2, SVMtest_2)
  SVMpredictions_3 <- SVMimpute_values(SVMmodel_3, SVMtest_3)
  SVMpredictions_4 <- SVMimpute_values(SVMmodel_4, SVMtest_4)
  SVMpredictions_5 <- SVMimpute_values(SVMmodel_5, SVMtest_5)
  SVMpredictions_6 <- SVMimpute_values(SVMmodel_6, SVMtest_6)
  SVMpredictions_7 <- SVMimpute_values(SVMmodel_7, SVMtest_7)
  SVMpredictions_8 <- SVMimpute_values(SVMmodel_8, SVMtest_8)
  SVMpredictions_9 <- SVMimpute_values(SVMmodel_9, SVMtest_9)
  SVMpredictions_10 <- SVMimpute_values(SVMmodel_10, SVMtest_10)
  SVMpredictions_11 <- SVMimpute_values(SVMmodel_11, SVMtest_11)
  SVMpredictions_12 <- SVMimpute_values(SVMmodel_12, SVMtest_12)
  
  # Comparing against the true values: true_1 <- ipumsdeel1[is.na(MCAR[,1]), 1]
  SVMcompare_true <- funtion(data, dataNA, column){
    data[is.na(dataNA[,column]), column]
  }
  
  SVMtrue_1 <- SVMcompare_true(subset_IPUMS, DFSUBMCAR, 1)
  SVMtrue_2 <- SVMcompare_true(subset_IPUMS, DFSUBMCAR, 2)
  SVMtrue_3 <- SVMcompare_true(subset_IPUMS, DFSUBMCAR, 3)
  SVMtrue_4 <- SVMcompare_true(subset_IPUMS, DFSUBMCAR, 4)
  SVMtrue_5 <- SVMcompare_true(subset_IPUMS, DFSUBMCAR, 5)
  SVMtrue_6 <- SVMcompare_true(subset_IPUMS, DFSUBMCAR, 6)
  SVMtrue_7 <- SVMcompare_true(subset_IPUMS, DFSUBMCAR, 7)
  SVMtrue_8 <- SVMcompare_true(subset_IPUMS, DFSUBMCAR, 8)
  SVMtrue_9 <- SVMcompare_true(subset_IPUMS, DFSUBMCAR, 9)
  SVMtrue_10 <- SVMcompare_true(subset_IPUMS, DFSUBMCAR, 10)
  SVMtrue_11 <- SVMcompare_true(subset_IPUMS, DFSUBMCAR, 11)
  SVMtrue_12 <- SVMcompare_true(subset_IPUMS, DFSUBMCAR, 12)
  
  # Computing error for all columns 
  SVMerror <- sum(!SVMtrue_1 == SVMpredictions_1, !SVMtrue_2 == SVMpredictions_2, !SVMtrue_3 == SVMpredictions_3, 
                  !SVMtrue_4 == SVMpredictions_4, !SVMtrue_5 == SVMpredictions_5, !SVMtrue_6 == SVMpredictions_6, 
                  !SVMtrue_7 == SVMpredictions_7, !SVMtrue_8 == SVMpredictions_8, !SVMtrue_9 == SVMpredictions_9, 
                  !SVMtrue_10 == SVMpredictions_10, !SVMtrue_11 == SVMpredictions_11, !SVMtrue_12 == SVMpredictions_12)
  
  # Computing total error from all columns
  SVMtotal_error <- SVMtotal_error + SVMerror
  

  
## Decision Tree Imputation with 'mice' -----------------------------------------------------------------------------------------           # argument 'ry' is not defined
set.seed(17)
decision_tree1 <- mice.impute.cart(subset_MCAR, minbucket = 5)
plot(decision_tree) #?
  
  # Checking if NA's are gone
  summary(decision_tree)
  
  # Computing accuracy
  DT_total_error1 <- regr.eval(subset_IPUMS, decision_tree)
  
            
  
  # https://rdrr.io/cran/simputation/man/impute_tree.html
  # https://www.kaggle.com/captcalculator/imputing-missing-data-with-the-mice-package-in-r/code
  
  
  
## Decision Tree Imputation with 'rpart' ---------------------------------------------------------------------------------------
set.seed(18)
  
  # Setting total error to '0'
  DT_total_error2 <- 0
  
  # Splitting the datasets into train sets: SVMtrain_1 <- MCAR[!is.na(MCAR[,1]),] and SVMtest_1 <- MCAR[is.na(MCAR[,1]),]
  DT_create_train <- function(data, column){
    data[!is.na(data[,column]),]
  }
  
  DT_train_1 <- DT_create_train(DFSUBMCAR, 1)
  DT_train_2 <- DT_create_train(DFSUBMCAR, 2)
  DT_train_3 <- DT_create_train(DFSUBMCAR, 3)
  DT_train_4 <- DT_create_train(DFSUBMCAR, 4)
  DT_train_5 <- DT_create_train(DFSUBMCAR, 5)
  DT_train_6 <- DT_create_train(DFSUBMCAR, 6)
  DT_train_7 <- DT_create_train(DFSUBMCAR, 7)
  DT_train_8 <- DT_create_train(DFSUBMCAR, 8)
  DT_train_9 <- DT_create_train(DFSUBMCAR, 9)
  DT_train_10 <- DT_create_train(DFSUBMCAR, 10)
  DT_train_11 <- DT_create_train(DFSUBMCAR, 11)
  DT_train_12 <- DT_create_train(DFSUBMCAR, 12)
  
  # Splitting the datasets into test sets 
  DT_create_test <- function(data, column){
    data[is.na(data[,column]),]
  }
  
  DT_test_1 <- DT_create_test(DFSUBMCAR, 1)
  DT_test_2 <- DT_create_test(DFSUBMCAR, 2)
  DT_test_3 <- DT_create_test(DFSUBMCAR, 3)
  DT_test_4 <- DT_create_test(DFSUBMCAR, 4)
  DT_test_5 <- DT_create_test(DFSUBMCAR, 5)
  DT_test_6 <- DT_create_test(DFSUBMCAR, 6)
  DT_test_7 <- DT_create_test(DFSUBMCAR, 7)
  DT_test_8 <- DT_create_test(DFSUBMCAR, 8)
  DT_test_9 <- DT_create_test(DFSUBMCAR, 9)
  DT_test_10 <- DT_create_test(DFSUBMCAR, 10)
  DT_test_11 <- DT_create_test(DFSUBMCAR, 11)
  DT_test_12 <- DT_create_test(DFSUBMCAR, 12)
  
  # Building the predicting models for all 12 columns 
  DT_model_1 <- rpart(formula = X1 ~ ., data = DT_train_1, method = "class") # or "anova"?
  DT_model_2 <- rpart(formula = X2 ~ ., data = DT_train_2, method = "class")
  DT_model_3 <- rpart(formula = X3 ~ ., data = DT_train_3, method = "class")
  DT_model_4 <- rpart(formula = X4 ~ ., data = DT_train_4, method = "class")
  DT_model_5 <- rpart(formula = X5 ~ ., data = DT_train_5, method = "class")
  DT_model_6 <- rpart(formula = X6 ~ ., data = DT_train_6, method = "class")
  DT_model_7 <- rpart(formula = X7 ~ ., data = DT_train_7, method = "class")
  DT_model_8 <- rpart(formula = X8 ~ ., data = DT_train_8, method = "class")
  DT_model_9 <- rpart(formula = X9 ~ ., data = DT_train_9, method = "class")
  DT_model_10 <- rpart(formula = X10 ~ ., data = DT_train_10, method = "class")                                   # Number of rows and matrices must match
  DT_model_11 <- rpart(formula = X11 ~ ., data = DT_train_11, method = "class")
  DT_model_12 <- rpart(formula = X12 ~ ., data = DT_train_12, method = "class")
  
  # Imputing values for all columns: predictions_1 <- predict(model_1, newdata = test_1)                          # error object'x1'not found
  DT_impute_values <- function(model, newdata){
    predict(model, newdata = testdata)
  }
  
  DT_predictions_1 <- DT_impute_values(DT_model_1, DT_test_1)
  DT_predictions_2 <- DT_impute_values(DT_model_2, DT_test_2)
  DT_predictions_3 <- DT_impute_values(DT_model_3, DT_test_3)
  DT_predictions_4 <- DT_impute_values(DT_model_4, DT_test_4)
  DT_predictions_5 <- DT_impute_values(DT_model_5, DT_test_5)
  DT_predictions_6 <- DT_impute_values(DT_model_6, DT_test_6)
  DT_predictions_7 <- DT_impute_values(DT_model_7, DT_test_7)
  DT_predictions_8 <- DT_impute_values(DT_model_8, DT_test_8)
  DT_predictions_9 <- DT_impute_values(DT_model_9, DT_test_9)
  DT_predictions_10 <- DT_impute_values(DT_model_10, DT_test_10)
  DT_predictions_11 <- DT_impute_values(DT_model_11, DT_test_11)
  DT_predictions_12 <- DT_impute_values(DT_model_12, DT_test_12)
  
  # Comparing against the true values: true_1 <- ipumsdeel1[is.na(MCAR[,1]), 1]
  DT_compare_true <- funtion(data, dataNA, column){
    data[is.na(dataNA[,column]), column]
  }
  
  DT_true_1 <- DT_compare_true(subset_IPUMS, DFSUBMCAR, 1)
  DT_true_2 <- DT_compare_true(subset_IPUMS, DFSUBMCAR, 2)
  DT_true_3 <- DT_compare_true(subset_IPUMS, DFSUBMCAR, 3)
  DT_true_4 <- DT_compare_true(subset_IPUMS, DFSUBMCAR, 4)
  DT_true_5 <- DT_compare_true(subset_IPUMS, DFSUBMCAR, 5)
  DT_true_6 <- DT_compare_true(subset_IPUMS, DFSUBMCAR, 6)
  DT_true_7 <- DT_compare_true(subset_IPUMS, DFSUBMCAR, 7)
  DT_true_8 <- DT_compare_true(subset_IPUMS, DFSUBMCAR, 8)
  DT_true_9 <- DT_compare_true(subset_IPUMS, DFSUBMCAR, 9)
  DT_true_10 <- DT_compare_true(subset_IPUMS, DFSUBMCAR, 10)
  DT_true_11 <- DT_compare_true(subset_IPUMS, DFSUBMCAR, 11)
  DT_true_12 <- DT_compare_true(subset_IPUMS, DFSUBMCAR, 12)

  # Computing error for all columns 
  DT_error <- sum(!DT_true_1 == DT_predictions_1, !DT_true_2 == DT_predictions_2, !DT_true_3 == DT_predictions_3, 
                  !DT_true_4 == DT_predictions_4, !DT_true_5 == DT_predictions_5, !DT_true_6 == DT_predictions_6, 
                  !DT_true_7 == DT_predictions_7, !DT_true_8 == DT_predictions_8, !DT_true_9 == DT_predictions_9, 
                  !DT_true_10 == DT_predictions_10, !DT_true_11 == DT_predictions_11, !DT_true_12 == DT_predictions_12)
  
  # Computing total error from all columns
  DT_total_error2 <- DT_total_error + DT_error

  
  
## Making a dataframe with computed errors ---------------------------------------------------------------------------------------
df_results <- data.frame(Imputation.method = c("Mode Imputation with 'ForImp'",
                                               "Mode Imputation with 'roughrf'", 
                                               "Mode Imputation with 'imputeR'", 
                                               "Hot Deck Imputation with 'hot.deck'", 
                                               "Hot Deck Imputation with 'VIM'", 
                                               "Hot Deck Imputation with 'HotDeckImputation'", 
                                               "Multiple Imputation with 'MICE'", 
                                               "Random Forest Imputation with 'MissForest'", 
                                               "Random Forest Imputation with 'MICE'",
                                               "Random Forest Imputation with randomForest",
                                               "naiveBayes Imputation with 'e1071'", 
                                               "k-Nearest Neighbor Imputation with 'DMwR'", 
                                               "k-Nearest Neighbor Imputation with 'bnstruct'",
                                               "k-Nearest Neighbor Imputation with 'VIM'",
                                               "k-Nearest Neighbor Imputation with 'caret'",
                                               "Support Vector Machine Imputation with 'e1071'", 
                                               "Decision Tree Imputation with 'MICE'", 
                                               "Decision Tree Imputation with 'rpart'"), 
                         
                         Total.error = c(# MOItotal_error1, MOItotal_error2, MOItotal_error3, 
                                         # HDtotal_error1, HDtotal_error2, HDtotal_error3, 
                                         # MItotal_error, 
                                         # RFtotal_error1, RFtotal_error2, RFtotal_error3, 
                                         # NBtotal_error, 
                                         # kNNtotal_error1, kNNtotal_error2, kNNtotal_error3, kNNtotal_error4, 
                                         # SVMtotal_error, 
                                         # DT_total_error1, DT_total_error2)
                                         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), 
                         
                         Rank = c(0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0))   


# Ranking errors from best to worst 
# sort (and put also in dataframe?)

