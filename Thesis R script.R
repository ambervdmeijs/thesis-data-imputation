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


## Loading the database ------------------------------------------------------------------------------------------------
#ipumsdeel1 <- read_excel("D:/Amber/Documenten/School/Tilburg University/Master/Thesis/IPUMS2001 deel 1.xlsx")
ipumsdeel1 <- read_excel("IPUMS2001 deel 1.xlsx")


## Open database -------------------------------------------------------------------------------------------------------
View(ipumsdeel1)


## Erase column 'nr' ---------------------------------------------------------------------------------------------------
ipumsdeel1 <- ipumsdeel1[-1]


## Creating missing values (MCAR with a 5% maximum treshold) -----------------------------------------------------------
MCAR <- SimIm(ipumsdeel1, p = 0.05)       #https://cran.r-project.org/web/packages/imputeR/imputeR.pdf 
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

  
## Creating subset of the MCAR dataset ---------------------------------------------------------------------------------
subset_MCAR <- MCAR[c(1:500), c(1:13)]

View(subset_ipums1)

  

## Mode imputation -----------------------------------------------------------------------------------------------------
mode_imputation <- modeimp(MCAR)

  # Are all NA's replaced?
  anyNA(mode_imputation)

  # Computing error  
  regr.eval(ipumsdeel1, mode_imputation)
  
  densityplot(mode_imputation)
  
  #r-statistics.co/Missing-Value-Treatment-With-R.html 

  

## Multiple imputation -------------------------------------------------------------------------------------------------
multiple_imputation <- mice(MCAR, m = 5, maxit = 50, meth = "pmm", seed = 500)
summary(multiple_imputation)
  
  # Get complete data (3rd out of 5)
  multiple_imputation_output <- complete(multiple_imputation, 3)
  
  # Are all NA's replaced?
  anyNA(multiple_imputation_output)
  
  # Computing error 
  regr.eval(ipumsdeel1, multiple_imputation_output)
  
  
  # https://datascienceplus.com/imputing-missing-data-with-r-mice-package/

  
  
## (1) Random forest imputation --------------------------------------------------------------------------------------------
random_forest <- missForest(MCAR[ ,1:14], xtrue = ipumsdeel1[ ,1:13], verbose = TRUE)
  
  # Are all NA's replaced?
  anyNA(random_forest)
  
  # Computing error
  regr.eval(ipumsdeel1, random_forest)
  
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
random_forest2 <- mice.impute.rf(MCAR, ipumsdeel1, ntree = 10)

  # Are all NA's replaced?
  anyNA(random_forest2)
  
  # Computing error
  regr.eval(ipumsdeel1, random_forest2)
  

  
## Building Naive Bayes model ------------------------------------------------------------------------------------------
  
  # Making dataframe 
  NB_mcar <- data.frame(MCAR)
  
  #names(my_mcar) <- paste("value", 1:13, sep="")
  
  # Setting total error to '0'
  NBtotal_error <- 0
  
  # Converting to factors for prediction
  # NB_mcar$Geslacht <- factor(NB_mcar$Geslacht)
  # NB_mcar$Leeftijd <- factor(NB_mcar$Leeftijd)
  
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
  convert_13 <- convert_factor("Gewicht", NB_mcar)
  
  # Splitting the datasets into train sets
  #NBtrain_1 <- NB_mcar[!is.na(NB_mcar[,1]),]
  #NBtest_1 <- NB_mcar[is.na(NB_mcar[,1]),]
  
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
  NBtrain_13 <- NBcreate_train(NB_mcar, 13)
  
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
  NBtest_13 <- NBcreate_test(NB_mcar, 13)
  
  # Building the predicting models for all 13 columns 
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
  NBmodel_13 <- naiveBayes(formula = "Gewicht" ~ ., data = NBtrain_13)
  
  # Imputing values for all columns
  # predictions_1 <- predict(model_1, newdata = test_1)
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
  NBpredictions_13 <- NBimpute_values(NBmodel_13, NBtest_13)

  # Comparing against the true values
  # true_1 <- ipumsdeel1[is.na(NB_mcar[,1]), 1]
  NBcompare_true <- funtion(data, dataNA, column){
    data[is.na(dataNA[,column]), column]
  }
  
  NBtrue_1 <- NBcompare_true(ipumsdeel1, NB_mcar, 1)
  NBtrue_2 <- NBcompare_true(ipumsdeel1, NB_mcar, 2)
  NBtrue_3 <- NBcompare_true(ipumsdeel1, NB_mcar, 3)
  NBtrue_4 <- NBcompare_true(ipumsdeel1, NB_mcar, 4)
  NBtrue_5 <- NBcompare_true(ipumsdeel1, NB_mcar, 5)
  NBtrue_6 <- NBcompare_true(ipumsdeel1, NB_mcar, 6)
  NBtrue_7 <- NBcompare_true(ipumsdeel1, NB_mcar, 7)
  NBtrue_8 <- NBcompare_true(ipumsdeel1, NB_mcar, 8)
  NBtrue_9 <- NBcompare_true(ipumsdeel1, NB_mcar, 9)
  NBtrue_10 <- NBcompare_true(ipumsdeel1, NB_mcar, 10)
  NBtrue_11 <- NBcompare_true(ipumsdeel1, NB_mcar, 11)
  NBtrue_12 <- NBcompare_true(ipumsdeel1, NB_mcar, 12)
  NBtrue_13 <- NBcompare_true(ipumsdeel1, NB_mcar, 13)
  
  # Computing error for all columns 
  NBerror <- sum(!NBtrue_1 == NBpredictions_1, !NBtrue_2 == NBpredictions_2, !NBtrue_3 == NBpredictions_3, !NBtrue_4 == NBpredictions_4, 
               !NBtrue_5 == NBpredictions_5, !NBtrue_6 == NBpredictions_6, !NBtrue_7 == NBpredictions_7, !NBtrue_8 == NBpredictions_8, 
               !NBtrue_9 == NBpredictions_9, !NBtrue_10 == NBpredictions_10, !NBtrue_11 == NBpredictions_11, !NBtrue_12 == NBpredictions_12, 
               !NBtrue_13 == NBpredictions_13)
  
  # Summing error from all columns
  NBtotal_error <- NBtotal_error + NBerror

  
  
## kNN imputation ------------------------------------------------------------------------------------------------------
str(MCAR)

knearestneighbor <- kNN(MCAR, k = 10)

KNN1 <-knn.impute(MCAR, k = 10) install("bnstruct")

  # Checking if NA's are gone
  summary(knearestneighbor)
   
  # Computing accuracy
  regr.eval(ipumsdeel1, knearestneighbor)
  
  # Computing accuracy
  regr.eval(ipumsdeel1, KNN1)
  
  
  # https://www.youtube.com/watch?v=u8XvfhBdbMw 
  # https://www.rdocumentation.org/packages/bnstruct/versions/1.0.2/topics/knn.impute
  
  
  
## Support Vector Machine imputation -----------------------------------------------------------------------------------

  # Setting total error to '0'
  svmtotal_error <- 0
  
  # Splitting the datasets into train sets
  # SVMtrain_1 <- MCAR[!is.na(MCAR[,1]),]
  # SVMtest_1 <- MCAR[is.na(MCAR[,1]),]
  
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
  SVMtrain_13 <- SVMcreate_train(MCAR, 13)
  
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
  SVMtest_13 <- SVMcreate_test(MCAR, 13)
  
  # Building the predicting models for all 13 columns 
  SVMmodel_1 <- svm(formula = "Geslacht" ~ ., data = SVMtrain_1, kernel = )
  SVMmodel_2 <- svm(formula = "Leeftijd" ~ ., data = SVMtrain_2, kernel = )
  SVMmodel_3 <- svm(formula = "HH_Pos" ~ ., data = SVMtrain_3, kernel = )
  SVMmodel_4 <- svm(formula = "HH_grootte" ~ ., data = SVMtrain_4, kernel = )
  SVMmodel_5 <- svm(formula = "Woonregio vorig jaar" ~ ., data = SVMtrain_5, kernel = )
  SVMmodel_6 <- svm(formula = "Nationaliteit" ~ ., data = SVMtrain_6, kernel = )
  SVMmodel_7 <- svm(formula = "Geboorteland" ~ ., data = SVMtrain_7, kernel = )
  SVMmodel_8 <- svm(formula = "Onderwijsniveau" ~ ., data = SVMtrain_8, kernel = )
  SVMmodel_9 <- svm(formula = "Econ.status" ~ ., data = SVMtrain_9, kernel = )
  SVMmodel_10 <- svm(formula = "Beroep" ~ ., data = SVMtrain_10, kernel = )
  SVMmodel_11 <- svm(formula = "SBI" ~ ., data = SVMtrain_11, kernel = )
  SVMmodel_12 <- svm(formula = "Burg.Staat" ~ ., data = SVMtrain_12, kernel = )
  SVMmodel_13 <- svm(formula = "Gewicht" ~ ., data = SVMtrain_13, kernel = )
  
  # Imputing values for all columns
  # predictions_1 <- predict(model_1, newdata = test_1)
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
  SVMpredictions_13 <- SVMimpute_values(SVMmodel_13, SVMtest_13)
  
  # Comparing against the true values
  # true_1 <- ipumsdeel1[is.na(MCAR[,1]), 1]
  SVMcompare_true <- funtion(data, dataNA, column){
    data[is.na(dataNA[,column]), column]
  }
  
  SVMtrue_1 <- SVMcompare_true(ipumsdeel1, MCAR, 1)
  SVMtrue_2 <- SVMcompare_true(ipumsdeel1, MCAR, 2)
  SVMtrue_3 <- SVMcompare_true(ipumsdeel1, MCAR, 3)
  SVMtrue_4 <- SVMcompare_true(ipumsdeel1, MCAR, 4)
  SVMtrue_5 <- SVMcompare_true(ipumsdeel1, MCAR, 5)
  SVMtrue_6 <- SVMcompare_true(ipumsdeel1, MCAR, 6)
  SVMtrue_7 <- SVMcompare_true(ipumsdeel1, MCAR, 7)
  SVMtrue_8 <- SVMcompare_true(ipumsdeel1, MCAR, 8)
  SVMtrue_9 <- SVMcompare_true(ipumsdeel1, MCAR, 9)
  SVMtrue_10 <- SVMcompare_true(ipumsdeel1, MCAR, 10)
  SVMtrue_11 <- SVMcompare_true(ipumsdeel1, MCAR, 11)
  SVMtrue_12 <- SVMcompare_true(ipumsdeel1, MCAR, 12)
  SVMtrue_13 <- SVMcompare_true(ipumsdeel1, MCAR, 13)
  
  # Computing error for all columns 
  SVMerror <- sum(!SVMtrue_1 == SVMpredictions_1, !SVMtrue_2 == SVMpredictions_2, !SVMtrue_3 == SVMpredictions_3, 
                  !SVMtrue_4 == SVMpredictions_4, !SVMtrue_5 == SVMpredictions_5, !SVMtrue_6 == SVMpredictions_6, 
                  !SVMtrue_7 == SVMpredictions_7, !SVMtrue_8 == SVMpredictions_8, !SVMtrue_9 == SVMpredictions_9, 
                  !SVMtrue_10 == SVMpredictions_10, !SVMtrue_11 == SVMpredictions_11, !SVMtrue_12 == SVMpredictions_12, 
                  !SVMtrue_13 == SVMpredictions_13)
  
  # Summing error from all columns
  SVMtotal_error <- SVMtotal_error + SVMerror
  

  
## Decision Tree imputation --------------------------------------------------------------------------------------------
decision_tree <- mice.impute.cart(MCAR, minbucket = 5) #cart is niet per se decision tree, toch? Issue aanmaken voor Drew.
  
  # OF impute_cart function, en dan uitschrijven zoals hierboven
            
  
  # https://rdrr.io/cran/simputation/man/impute_tree.html
  # https://www.kaggle.com/captcalculator/imputing-missing-data-with-the-mice-package-in-r/code


  
  
  
  
## Making a dataframe with computed errors ------------------------------------------------------------------------------

# Making dataframe

# Ranking errors from best to worst 

