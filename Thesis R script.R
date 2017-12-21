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
install.packages("simglm")
install.packages("caret")
install.packages("devtools")
install.packages("VIM")
install.packages("Hmisc")
install.packages("DMwR")
install.packages("mice")
install.packages("missForest", dependencies = TRUE)
install.packages("e1071")
install.packages("imputeR")
install.packages("ForImp")
install.packages("missMDA")
install_github("lebebr01/simglm", build_vignettes = TRUE) # in this section because of different install procedure
install.packages("ggplot2")
install.packages("rpart")
}


## Loading packages ----------------------------------------------------------------------------------------------------
library("readxl")
library("simglm")
library("caret")
library("devtools")
library("VIM")
library("Hmisc")
library("DMwR")
library("mice")
library("missForest")
library("e1071")
library("imputeR")
library("ForImp")
library("missMDA")
library("ggplot2")
library("rpart")


## Loading the databases -----------------------------------------------------------------------------------------------
#ipumsdeel1 <- read_excel("D:/Amber/Documenten/School/Tilburg University/Master/Thesis/IPUMS2001 deel 1.xlsx")
ipumsdeel1 <- read_excel("IPUMS2001 deel 1.xlsx")


## Combining databases by rows -----------------------------------------------------------------------------------------
#ipums <- bind_rows(y,z)
#ipums <- rbind(ipumsdeel1, ipumsdeel2)

#slice(ipums, 90000:90001) to check if deel 2's first row comes after deel 1's last row. 

## Open database -------------------------------------------------------------------------------------------------------
View(ipums)


## Erase column 'nr' ---------------------------------------------------------------------------------------------------
ipums <- ipums[-1]


## Erase column 'Gewicht' (not relevant) -------------------------------------------------------------------------------
ipums$Gewicht <- NULL 


## Creating missing values (MCAR with a 5% maximum treshold) -----------------------------------------------------------
MCAR <- SimIm(ipums, p = 0.05)       #https://cran.r-project.org/web/packages/imputeR/imputeR.pdf 
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

  
## Creating subset of IPUMS en MCAR dataset ---------------------------------------------------------------------------------
subset_IPUMS <- ipums[c(1:500), c(1:12)]
subset_MCAR <- MCAR[c(1:500), c(1:12)]


View(subset_MCAR)


## Mode imputation -----------------------------------------------------------------------------------------------------
set.seed(1)
mode_imputation <- modeimp(MCAR)

  # Are all NA's replaced?
  anyNA(mode_imputation)

  # Computing error  
  MOItotal_error <- regr.eval(ipums, mode_imputation)
  
  densityplot(mode_imputation)
  
  #r-statistics.co/Missing-Value-Treatment-With-R.html 

  

## Multiple imputation -------------------------------------------------------------------------------------------------
multiple_imputation <- mice(MCAR, m = 5, maxit = 50, meth = "pmm", seed = 2)
summary(multiple_imputation)
  
  # Get complete data (3rd out of 5)
  multiple_imputation_output <- complete(multiple_imputation, 3)
  
  # Are all NA's replaced?
  anyNA(multiple_imputation_output)
  
  # Computing error 
  MItotal_error <- regr.eval(ipumsdeel1, multiple_imputation_output)
  
  
  # https://datascienceplus.com/imputing-missing-data-with-r-mice-package/

  
  
## (1) Random forest imputation --------------------------------------------------------------------------------------------
set.seed(3)
random_forest <- missForest(MCAR[ ,1:14], xtrue = ipumsdeel1[ ,1:13], verbose = TRUE)
  
  # Are all NA's replaced?
  anyNA(random_forest)
  
  # Computing error
  RFtotal_error1 <- regr.eval(ipumsdeel1, random_forest)
  
  # OOB Error
  random_forest$OOBerror
  
  # The true error using xtrue
  random_forest$error
  
  #NRMSE = error continuous error
  #PFC = the proportion of falsely classified entries in the categorical part of the imputed data set 
  #Good = close to 0, bad is close to 1
  
  # Additional performance output: estimated errors, difference and running time with max. 10 iterations   
  set.seed(3)
  randomforest <- missForest(missing_ipums, verbose = TRUE, maxiter = 10)
  
  # Comparing actual data accuracy
  randomforest_error <- mixError(randomforest$ximp, MCAR, ipumsdeel1)
  
  # https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/
  # https://stat.ethz.ch/education/semesters/ss2013/ams/paper/missForest_1.2.pdf

  
  
## (2) Random forest imputation with MICE ------------------------------------------
set.seed(4)
random_forest2 <- mice.impute.rf(MCAR, ipumsdeel1, ntree = 10)

  # Are all NA's replaced?
  anyNA(random_forest2)
  
  # Computing error
  RFtotal_error2 <- regr.eval(ipumsdeel1, random_forest2)
  
  
## Naive Bayes model imputation ------------------------------------------------------------------------------------------
  
  # Making dataframe 
  NB_mcar <- data.frame(MCAR) #Is this necessary?
  
  #names(my_mcar) <- paste("value", 1:13, sep="")
  
  # Setting total error to '0'
  NBtotal_error <- 0
  
  # Converting to factors for prediction: NB_mcar$Geslacht <- factor(NB_mcar$Geslacht) and NB_mcar$Leeftijd <- factor(NB_mcar$Leeftijd)
  convert_factor <- function(variable, data){
    data$variable <- factor(data$variable)
  }
  
  convert_1 <- convert_factor("Geslacht", NB_mcar)
  convert_2 <- convert_factor("Leeftijd", NB_mcar)
  convert_3 <- convert_factor("HH_Pos", NB_mcar)
  convert_4 <- convert_factor("HH_grootte", NB_mcar)
  convert_5 <- convert_factor("Woonregio vorig jaar", NB_mcar)
  convert_6 <- convert_factor("Nationaliteit", NB_mcar)
  convert_7 <- convert_factor("Geboorteland", NB_mcar)
  convert_8 <- convert_factor("Onderwijsniveau", NB_mcar)
  convert_9 <- convert_factor("Econ.status", NB_mcar)
  convert_10 <- convert_factor("Beroep", NB_mcar)
  convert_11 <- convert_factor("SBI", NB_mcar)
  convert_12 <- convert_factor("Burg.staat", NB_mcar)
  
  # Splitting the datasets into train sets: NBtrain_1 <- NB_mcar[!is.na(NB_mcar[,1]),] and NBtest_1 <- NB_mcar[is.na(NB_mcar[,1]),]
  NBcreate_train <- function(data, column){
    data[!is.na(data[,column]),]
  }
  
  NBtrain_1 <- NBcreate_train(NB_mcar, 1)
  NBtrain_2 <- NBcreate_train(NB_mcar, 2)
  NBtrain_3 <- NBcreate_train(NB_mcar, 3)
  NBtrain_4 <- NBcreate_train(NB_mcar, 4)
  NBtrain_5 <- NBcreate_train(NB_mcar, 5)
  NBtrain_6 <- NBcreate_train(NB_mcar, 6)
  NBtrain_7 <- NBcreate_train(NB_mcar, 7)
  NBtrain_8 <- NBcreate_train(NB_mcar, 8)
  NBtrain_9 <- NBcreate_train(NB_mcar, 9)
  NBtrain_10 <- NBcreate_train(NB_mcar, 10)
  NBtrain_11 <- NBcreate_train(NB_mcar, 11)
  NBtrain_12 <- NBcreate_train(NB_mcar, 12)
  
  # Splitting the datasets into test sets 
  NBcreate_test <- function(data, column){
    data[is.na(data[,column]),]
  }
  
  NBtest_1 <- NBcreate_test(NB_mcar, 1)
  NBtest_2 <- NBcreate_test(NB_mcar, 2)
  NBtest_3 <- NBcreate_test(NB_mcar, 3)
  NBtest_4 <- NBcreate_test(NB_mcar, 4)
  NBtest_5 <- NBcreate_test(NB_mcar, 5)
  NBtest_6 <- NBcreate_test(NB_mcar, 6)
  NBtest_7 <- NBcreate_test(NB_mcar, 7)
  NBtest_8 <- NBcreate_test(NB_mcar, 8)
  NBtest_9 <- NBcreate_test(NB_mcar, 9)
  NBtest_10 <- NBcreate_test(NB_mcar, 10)
  NBtest_11 <- NBcreate_test(NB_mcar, 11)
  NBtest_12 <- NBcreate_test(NB_mcar, 12)
  
  # Building the predicting models for all 12 columns 
  set.seed(5)
  NBmodel_1 <- naiveBayes(formula = "Geslacht" ~ ., data = NBtrain_1)
  NBmodel_2 <- naiveBayes(formula = "Leeftijd" ~ ., data = NBtrain_2)
  NBmodel_3 <- naiveBayes(formula = "HH_Pos" ~ ., data = NBtrain_3)
  NBmodel_4 <- naiveBayes(formula = "HH_grootte" ~ ., data = NBtrain_4)
  NBmodel_5 <- naiveBayes(formula = "Woonregio vorig jaar" ~ ., data = NBtrain_5)
  NBmodel_6 <- naiveBayes(formula = "Nationaliteit" ~ ., data = NBtrain_6)
  NBmodel_7 <- naiveBayes(formula = "Geboorteland" ~ ., data = NBtrain_7)
  NBmodel_8 <- naiveBayes(formula = "Onderwijsniveau" ~ ., data = NBtrain_8)
  NBmodel_9 <- naiveBayes(formula = "Econ.status" ~ ., data = NBtrain_9)
  NBmodel_10 <- naiveBayes(formula = "Beroep" ~ ., data = NBtrain_10)
  NBmodel_11 <- naiveBayes(formula = "SBI" ~ ., data = NBtrain_11)
  NBmodel_12 <- naiveBayes(formula = "Burg.Staat" ~ ., data = NBtrain_12)
  
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
  
  NBtrue_1 <- NBcompare_true(ipums, NB_mcar, 1)
  NBtrue_2 <- NBcompare_true(ipums, NB_mcar, 2)
  NBtrue_3 <- NBcompare_true(ipums, NB_mcar, 3)
  NBtrue_4 <- NBcompare_true(ipums, NB_mcar, 4)
  NBtrue_5 <- NBcompare_true(ipums, NB_mcar, 5)
  NBtrue_6 <- NBcompare_true(ipums, NB_mcar, 6)
  NBtrue_7 <- NBcompare_true(ipums, NB_mcar, 7)
  NBtrue_8 <- NBcompare_true(ipums, NB_mcar, 8)
  NBtrue_9 <- NBcompare_true(ipums, NB_mcar, 9)
  NBtrue_10 <- NBcompare_true(ipums, NB_mcar, 10)
  NBtrue_11 <- NBcompare_true(ipums, NB_mcar, 11)
  NBtrue_12 <- NBcompare_true(ipums, NB_mcar, 12)
  
  # Computing error for all columns 
  NBerror <- sum(!NBtrue_1 == NBpredictions_1, !NBtrue_2 == NBpredictions_2, !NBtrue_3 == NBpredictions_3, !NBtrue_4 == NBpredictions_4, 
               !NBtrue_5 == NBpredictions_5, !NBtrue_6 == NBpredictions_6, !NBtrue_7 == NBpredictions_7, !NBtrue_8 == NBpredictions_8, 
               !NBtrue_9 == NBpredictions_9, !NBtrue_10 == NBpredictions_10, !NBtrue_11 == NBpredictions_11, !NBtrue_12 == NBpredictions_12)
  
  # Computing total error from all columns
  NBtotal_error <- NBtotal_error + NBerror

  
  
## kNN imputation ------------------------------------------------------------------------------------------------------
str(MCAR)

set.seed(6)
knearestneighbor <- kNN(MCAR, k = 10)

KNN1 <-knn.impute(MCAR, k = 10) install("bnstruct")

  # Checking if NA's are gone
  summary(knearestneighbor)
   
  # Computing accuracy
  kNNtotal_error1 <- regr.eval(ipums, knearestneighbor)
  
  # Computing accuracy
  kNNtotal_error2 <- regr.eval(ipums, KNN1)
  
  
  # https://www.youtube.com/watch?v=u8XvfhBdbMw 
  # https://www.rdocumentation.org/packages/bnstruct/versions/1.0.2/topics/knn.impute
  
  
  
## Support Vector Machine imputation -----------------------------------------------------------------------------------

  # Setting total error to '0'
  SVMtotal_error <- 0
  
  # Splitting the datasets into train sets: SVMtrain_1 <- MCAR[!is.na(MCAR[,1]),] and SVMtest_1 <- MCAR[is.na(MCAR[,1]),]
  SVMcreate_train <- function(data, column){
    data[!is.na(data[,column]),]
  }
  
  SVMtrain_1 <- SVMcreate_train(MCAR, 1)
  SVMtrain_2 <- SVMcreate_train(MCAR, 2)
  SVMtrain_3 <- SVMcreate_train(MCAR, 3)
  SVMtrain_4 <- SVMcreate_train(MCAR, 4)
  SVMtrain_5 <- SVMcreate_train(MCAR, 5)
  SVMtrain_6 <- SVMcreate_train(MCAR, 6)
  SVMtrain_7 <- SVMcreate_train(MCAR, 7)
  SVMtrain_8 <- SVMcreate_train(MCAR, 8)
  SVMtrain_9 <- SVMcreate_train(MCAR, 9)
  SVMtrain_10 <- SVMcreate_train(MCAR, 10)
  SVMtrain_11 <- SVMcreate_train(MCAR, 11)
  SVMtrain_12 <- SVMcreate_train(MCAR, 12)
  
  # Splitting the datasets into test sets 
  SVMcreate_test <- function(data, column){
    data[is.na(data[,column]),]
  }
  
  SVMtest_1 <- SVMcreate_test(MCAR, 1)
  SVMtest_2 <- SVMcreate_test(MCAR, 2)
  SVMtest_3 <- SVMcreate_test(MCAR, 3)
  SVMtest_4 <- SVMcreate_test(MCAR, 4)
  SVMtest_5 <- SVMcreate_test(MCAR, 5)
  SVMtest_6 <- SVMcreate_test(MCAR, 6)
  SVMtest_7 <- SVMcreate_test(MCAR, 7)
  SVMtest_8 <- SVMcreate_test(MCAR, 8)
  SVMtest_9 <- SVMcreate_test(MCAR, 9)
  SVMtest_10 <- SVMcreate_test(MCAR, 10)
  SVMtest_11 <- SVMcreate_test(MCAR, 11)
  SVMtest_12 <- SVMcreate_test(MCAR, 12)
  
  # Building the predicting models for all 12 columns 
  set.seed(7)
  SVMmodel_1 <- svm(formula = "Geslacht" ~ ., data = SVMtrain_1, kernel = "radial")
  SVMmodel_2 <- svm(formula = "Leeftijd" ~ ., data = SVMtrain_2, kernel = "radial")
  SVMmodel_3 <- svm(formula = "HH_Pos" ~ ., data = SVMtrain_3, kernel = "radial")
  SVMmodel_4 <- svm(formula = "HH_grootte" ~ ., data = SVMtrain_4, kernel = "radial")
  SVMmodel_5 <- svm(formula = "Woonregio vorig jaar" ~ ., data = SVMtrain_5, kernel = "radial")
  SVMmodel_6 <- svm(formula = "Nationaliteit" ~ ., data = SVMtrain_6, kernel = "radial")
  SVMmodel_7 <- svm(formula = "Geboorteland" ~ ., data = SVMtrain_7, kernel = "radial")
  SVMmodel_8 <- svm(formula = "Onderwijsniveau" ~ ., data = SVMtrain_8, kernel = "radial")
  SVMmodel_9 <- svm(formula = "Econ.status" ~ ., data = SVMtrain_9, kernel = "radial")
  SVMmodel_10 <- svm(formula = "Beroep" ~ ., data = SVMtrain_10, kernel = "radial")
  SVMmodel_11 <- svm(formula = "SBI" ~ ., data = SVMtrain_11, kernel = "radial")
  SVMmodel_12 <- svm(formula = "Burg.Staat" ~ ., data = SVMtrain_12, kernel = "radial")
  
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
  
  SVMtrue_1 <- SVMcompare_true(ipums, MCAR, 1)
  SVMtrue_2 <- SVMcompare_true(ipums, MCAR, 2)
  SVMtrue_3 <- SVMcompare_true(ipums, MCAR, 3)
  SVMtrue_4 <- SVMcompare_true(ipums, MCAR, 4)
  SVMtrue_5 <- SVMcompare_true(ipums, MCAR, 5)
  SVMtrue_6 <- SVMcompare_true(ipums, MCAR, 6)
  SVMtrue_7 <- SVMcompare_true(ipums, MCAR, 7)
  SVMtrue_8 <- SVMcompare_true(ipums, MCAR, 8)
  SVMtrue_9 <- SVMcompare_true(ipums, MCAR, 9)
  SVMtrue_10 <- SVMcompare_true(ipums, MCAR, 10)
  SVMtrue_11 <- SVMcompare_true(ipums, MCAR, 11)
  SVMtrue_12 <- SVMcompare_true(ipums, MCAR, 12)
  
  # Computing error for all columns 
  SVMerror <- sum(!SVMtrue_1 == SVMpredictions_1, !SVMtrue_2 == SVMpredictions_2, !SVMtrue_3 == SVMpredictions_3, 
                  !SVMtrue_4 == SVMpredictions_4, !SVMtrue_5 == SVMpredictions_5, !SVMtrue_6 == SVMpredictions_6, 
                  !SVMtrue_7 == SVMpredictions_7, !SVMtrue_8 == SVMpredictions_8, !SVMtrue_9 == SVMpredictions_9, 
                  !SVMtrue_10 == SVMpredictions_10, !SVMtrue_11 == SVMpredictions_11, !SVMtrue_12 == SVMpredictions_12)
  
  # Computing total error from all columns
  SVMtotal_error <- SVMtotal_error + SVMerror
  

  
## Decision Tree imputation with 'mice' (1) --------------------------------------------------------------------------------
set.seed(8)
decision_tree <- mice.impute.cart(MCAR, minbucket = 5)
plot(decision_tree) #?
  
  # Checking if NA's are gone
  summary(decision_tree)
  
  # Computing accuracy
  DT_total_error <- regr.eval(ipums, decision_tree)
  
            
  
  # https://rdrr.io/cran/simputation/man/impute_tree.html
  # https://www.kaggle.com/captcalculator/imputing-missing-data-with-the-mice-package-in-r/code
  
## Decision Tree imputation with 'rpart' (2) ------------------------------------------------------------------------------

  # Setting total error to '0'
  DT_total_error <- 0
  
  # Splitting the datasets into train sets: SVMtrain_1 <- MCAR[!is.na(MCAR[,1]),] and SVMtest_1 <- MCAR[is.na(MCAR[,1]),]
  DT_create_train <- function(data, column){
    data[!is.na(data[,column]),]
  }
  
  DT_train_1 <- DT_create_train(MCAR, 1)
  DT_train_2 <- DT_create_train(MCAR, 2)
  DT_train_3 <- DT_create_train(MCAR, 3)
  DT_train_4 <- DT_create_train(MCAR, 4)
  DT_train_5 <- DT_create_train(MCAR, 5)
  DT_train_6 <- DT_create_train(MCAR, 6)
  DT_train_7 <- DT_create_train(MCAR, 7)
  DT_train_8 <- DT_create_train(MCAR, 8)
  DT_train_9 <- DT_create_train(MCAR, 9)
  DT_train_10 <- DT_create_train(MCAR, 10)
  DT_train_11 <- DT_create_train(MCAR, 11)
  DT_train_12 <- DT_create_train(MCAR, 12)
  
  # Splitting the datasets into test sets 
  DT_create_test <- function(data, column){
    data[is.na(data[,column]),]
  }
  
  DT_test_1 <- DT_create_test(MCAR, 1)
  DT_test_2 <- DT_create_test(MCAR, 2)
  DT_test_3 <- DT_create_test(MCAR, 3)
  DT_test_4 <- DT_create_test(MCAR, 4)
  DT_test_5 <- DT_create_test(MCAR, 5)
  DT_test_6 <- DT_create_test(MCAR, 6)
  DT_test_7 <- DT_create_test(MCAR, 7)
  DT_test_8 <- DT_create_test(MCAR, 8)
  DT_test_9 <- DT_create_test(MCAR, 9)
  DT_test_10 <- DT_create_test(MCAR, 10)
  DT_test_11 <- DT_create_test(MCAR, 11)
  DT_test_12 <- DT_create_test(MCAR, 12)
  
  # Building the predicting models for all 12 columns 
  DT_model_1 <- rpart(formula = "Geslacht" ~ ., data = DT_train_1, method = "class") # or "anova"?
  DT_model_2 <- rpart(formula = "Leeftijd" ~ ., data = DT_train_2, method = "class")
  DT_model_3 <- rpart(formula = "HH_Pos" ~ ., data = DT_train_3, method = "class")
  DT_model_4 <- rpart(formula = "HH_grootte" ~ ., data = DT_train_4, method = "class")
  DT_model_5 <- rpart(formula = "Woonregio vorig jaar" ~ ., data = DT_train_5, method = "class")
  DT_model_6 <- rpart(formula = "Nationaliteit" ~ ., data = DT_train_6, method = "class")
  DT_model_7 <- rpart(formula = "Geboorteland" ~ ., data = DT_train_7, method = "class")
  DT_model_8 <- rpart(formula = "Onderwijsniveau" ~ ., data = DT_train_8, method = "class")
  DT_model_9 <- rpart(formula = "Econ.status" ~ ., data = DT_train_9, method = "class")
  DT_model_10 <- rpart(formula = "Beroep" ~ ., data = DT_train_10, method = "class")
  DT_model_11 <- rpart(formula = "SBI" ~ ., data = DT_train_11, method = "class")
  DT_model_12 <- rpart(formula = "Burg.Staat" ~ ., data = DT_train_12, method = "class")
  
  # Imputing values for all columns: predictions_1 <- predict(model_1, newdata = test_1)
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
  
  DT_true_1 <- DT_compare_true(ipums, MCAR, 1)
  DT_true_2 <- DT_compare_true(ipums, MCAR, 2)
  DT_true_3 <- DT_compare_true(ipums, MCAR, 3)
  DT_true_4 <- DT_compare_true(ipums, MCAR, 4)
  DT_true_5 <- DT_compare_true(ipums, MCAR, 5)
  DT_true_6 <- DT_compare_true(ipums, MCAR, 6)
  DT_true_7 <- DT_compare_true(ipums, MCAR, 7)
  DT_true_8 <- DT_compare_true(ipums, MCAR, 8)
  DT_true_9 <- DT_compare_true(ipums, MCAR, 9)
  DT_true_10 <- DT_compare_true(ipums, MCAR, 10)
  DT_true_11 <- DT_compare_true(ipums, MCAR, 11)
  DT_true_12 <- DT_compare_true(ipums, MCAR, 12)

  # Computing error for all columns 
  DT_error <- sum(!DT_true_1 == DT_predictions_1, !DT_true_2 == DT_predictions_2, !DT_true_3 == DT_predictions_3, 
                  !DT_true_4 == DT_predictions_4, !DT_true_5 == DT_predictions_5, !DT_true_6 == DT_predictions_6, 
                  !DT_true_7 == DT_predictions_7, !DT_true_8 == DT_predictions_8, !DT_true_9 == DT_predictions_9, 
                  !DT_true_10 == DT_predictions_10, !DT_true_11 == DT_predictions_11, !DT_true_12 == DT_predictions_12)
  
  # Computing total error from all columns
  DT_total_error <- DT_total_error + DT_error

  
## Making a dataframe with computed errors -------------------------------------------------------------------------------
df_results <- data.frame(Imputation_method = c("Mode Imputation", "Multiple Imputation", "Random Forest imputation", 
                         "naiveBayes imputation", "k-Nearest Neighbor imputation", "Support Vector Machine imputation", 
                         "Decision Tree imputation"), 
                         Total_error = c(MOItotal_error, MItotal_error, RFtotal_error, NBtotal_error, kNNtotal_error, 
                                         SVMtotal_error, DT_total_error), 
                         Rank = c(0, 0, 0, 0, 0, 0, 0))   
                         
# Ranking errors from best to worst 
# sort (and put also in dataframe?)

