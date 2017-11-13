# evaluation criteria: https://www.imf.org/external/pubs/ft/wp/2011/wp11151.pdf 


# Install packages ----------------------------------------------------------------------------------------------------
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


# Loading packages ----------------------------------------------------------------------------------------------------
library("readxl")
library("simglm")
library("caret")
library("devtools")
install_github("lebebr01/simglm", build_vignettes = TRUE) # in this section because of different install procedure
library("VIM")
library("Hmisc")
library("DMwR")
library("mice")
library("missForest")
library("e1071")
library("imputeR")
library("ForImp")
library("missMDA")


## Loading the database ------------------------------------------------------------------------------------------------
ipumsdeel1 <- read_excel("D:/Amber/Documenten/School/Tilburg University/Master/Thesis/IPUMS2001 deel 1.xlsx")
ipumsdeel1 <- read_excel("//studfiles.campus.uvt.nl/files/home/home06/u1278896/Thesis/IPUMS2001 deel 1.xlsx")

# What is the target variable in this data set?

## Open database -------------------------------------------------------------------------------------------------------
View(ipumsdeel1)

## Erase column 'nr' ---------------------------------------------------------------------------------------------------



## Creating missing values (MCAR with a 5% maximum treshold) -----------------------------------------------------------
MCAR <- SimIm(ipumsdeel1, p = 0.05)       #https://cran.r-project.org/web/packages/imputeR/imputeR.pdf 
View(MCAR)
  
  # Counting NA's in dataset MCAR 
  sum(is.na(MCAR))

  prop.table(table(is.na(MCAR))) # $column naam toevoegen, om proportie missing data van column/variabel te zien

  # Looking at the missing data pattern
  md.pattern(MCAR) #Looking at the missing data pattern

  # Visualizing missing data pattern 



  # https://datascienceplus.com/imputing-missing-data-with-r-mice-package/ 


## Mode imputation -----------------------------------------------------------------------------------------------------
mode_imputation <- modeimp(MCAR)
View(mode_imputation)

  # Computing accuracy / Evaluating method  
  regr.eval(ipumsdeel1, mode_imputation)
  
  densityplot(mode_imputation)
  
  #r-statistics.co/Missing-Value-Treatment-With-R.html 


## Multiple imputation -------------------------------------------------------------------------------------------------
multiple_imputation <- mice(MCAR, m = 5, maxit = 50, meth = "pmm", seed = 500)
summary(multiple_imputation)
  
  # Get complete data (3rd out of 5)
  multiple_imputation_output <- complete(multiple_imputation, 3)
  anyNA(multiple_imputation_output)
  
  # Computing accuracy 
  regr.eval(ipumsdeel1, multiple_imputation_output)
  
  # Building predictive model
  multiple_fit <- with(MCAR, exp = lm())
  
  
  # https://datascienceplus.com/imputing-missing-data-with-r-mice-package/

  
## Random forest imputation --------------------------------------------------------------------------------------------
random_forest <- missForest(MCAR[ ,1:14], xtrue = ipumsdeel1[ ,1:14], verbose = TRUE)
  # How do I decrease computation time?
  
  # Computing accuracy
  regr.eval(ipumsdeel1, random_forest)
  
  # OOB Error
  random_forest$OOBerror
  
  # The true error using xtrue
  random_forest$error


## Building Random Forest model ----------------------------------------------------------------------------------------
randomforest <- missForest(missing_ipums)

  # Calling imputed data matrix
  randomforest$ximp

  # Calling OOB imputation error estimate 
  randomforest$OOBerror 

    #NRMSE = error continuous error
    #PFC = the proportion of falsely classified entries in the categorical part of the imputed data set 
    #Good = close to 0, bad is close to 1

  # Additional performance output: estimated errors, difference and running time with max. 10 iterations   
  set.seed(3)
  randomforest <- missForest(missing_ipums, verbose = TRUE, maxiter = 10)

  # Comparing actual data accuracy
  randomforest_error <- mixError(randomforest$ximp, MCAR, ipumsdeel1)
  randomforest_error
  
    # https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/
    # https://stat.ethz.ch/education/semesters/ss2013/ams/paper/missForest_1.2.pdf


## Building Naive Bayes model ------------------------------------------------------------------------------------------
naivebayes <- naiveBayes(class ~ ., data = ipumsdeel1)
class(apply_naive)
summary(apply_naive)
print(apply_naive)

predict_naive <- predict(apply_naive, newdata = MCAR)

    # https://www.r-bloggers.com/naive-bayes-classification-in-r-part-2/ 


## kNN imputation ------------------------------------------------------------------------------------------------------
str(MCAR)

knearestneighbor <- kNN(MCAR, variable = c("Geslacht", "Leeftijd", "HH_Pos", "HH_grootte", "Woonregio vorig jaar", 
                                            "Nationaliteit", "Geboorteland", "Onderwijsniveau", "Econ. status", 
                                           "Beroep", "SBI", "Burg. Staat", "Gewicht"), k = 10)
knearestneighbor <- kNN(MCAR, k = 10)

  # Checking if NA's are gone
  summary(knearestneighbor)
   
  # Computing accuracy
  regr.eval(ipumsdeel1, knearestneighbor)
  
KNN1 <-knn.impute(MCAR, k = 10) install("bnstruct")

  # Computing accuracy
  regr.eval(ipumsdeel1, KNN1)
  

## Support Vector Machine imputation -----------------------------------------------------------------------------------






















## Creating train and test set -----------------------------------------------------------------------------------------
# Do I need this? Because I have the original and new dataset to compare. 

# Set the seed to make partition reproducible 
set.seed(10)

# Divide data set into train and test set 
trn_indexes <- sample(nrow(MCAR), size = 0.7 * nrow(MCAR))

train <- MCAR[trn_indexes, ]
test <- MCAR[-trn_indexes, ]

# with Cross Validation 

trn_ipumsdeel1_cv <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

trn_meanimputation <- train(x, data = ipumsdeel1, trControl = trn_ipumsdeel1_cv, method = "nb")


# http://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/ 


## Computing accuracy train set 

## Adjusting parameters 

## Computing accuracy test set 


