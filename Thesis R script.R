SimIm <- function(data, p = 0.1) {
  vec <- c(unlist(data))
  missing <- rbinom(length(vec), 1, p)
  vec[missing == 1] <- NA
  dim(vec) <- dim(data)
  return(vec)
}

# This is my code. It doesn't look as much, but it took a lot of research to find the right methods. 
# How many simulations of each method do I have to perform? Is the output different each time or do I have to set a seed, even for the traditional methods?
# How to compute accuracy percentages, so I know which are the optimal parameters of each method is?
# Dataset is too big to plot/visualize. Do you have some tips/tricks/literature?
# Which evaluation criteria('s) do you recommend? Beside the errors I also read something about predictive, ranking, distributional and estimation accuracy?


if (F) {
# Install packages ----------------------------------------------------------------------------------------------------
install.packages("readxl")
#install.packages("simglm")
install.packages("caret")
install.packages("devtools")
install.packages("VIM")
install.packages("Hmisc")
install.packages("DMwR")
install.packages("mice")
install.packages("missForest", dependencies = TRUE)
install.packages("e1071")
install.packages("imputeR")
#install.packages("ForImp")
install.packages("missMDA")
#install_github("lebebr01/simglm", build_vignettes = TRUE) # in this section because of different install procedure
}

# Loading packages ----------------------------------------------------------------------------------------------------
library("readxl")
#library("simglm")
library("caret")
library("devtools")
library("VIM")
library("Hmisc")
library("DMwR")
library("mice")
library("missForest")
library("e1071")
#library("imputeR")
library("ForImp")
library("missMDA")



## Loading the database ------------------------------------------------------------------------------------------------
#ipumsdeel1 <- read_excel("D:/Amber/Documenten/School/Tilburg University/Master/Thesis/IPUMS2001 deel 1.xlsx")
ipumsdeel1 <- read_excel("IPUMS2001 deel 1.xlsx")

# Just to triple check: with imputation I cannot work with target variables right?
# This is one dataset. There is a second part of an equal size. If I want to use them both, I have to (r)bind them together. 
# However, that makes it a huge dataset, and hard to process to get the final results. How do I proceed if I finished my
# code and have to write down the final results (for the whole dataset)?



## Open database -------------------------------------------------------------------------------------------------------
View(ipumsdeel1)



## Erase column 'nr' ---------------------------------------------------------------------------------------------------
ipumsdeel1 <- ipumsdeel1[-1]



## Creating missing values (MCAR with a 5% maximum treshold) -----------------------------------------------------------
MCAR <- SimIm(ipumsdeel1, p = 0.05)       #https://cran.r-project.org/web/packages/imputeR/imputeR.pdf 
View(MCAR)
  
  # Counting NA's in dataset MCAR 
  sum(is.na(MCAR))

  prop.table(table(is.na(MCAR))) # $column naam toevoegen, om proportie missing data van column/variabel te zien

  # Looking at the missing data pattern
  md.pattern(MCAR) #Looking at the missing data pattern

  # Visualizing missing data pattern
  .... # but dataset is too big
  
  # https://datascienceplus.com/imputing-missing-data-with-r-mice-package/ 

  

## Creating subset of the MCAR dataset to test methods for a smaller computation time ----------------------------------
subset_ipums1 <- MCAR[c(1:500), c(1:13)]
View(subset_ipums1)

md.pattern(subset_ipums1)

  # Subset is not yet being used in this code

  

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

  
  
## (1) Random forest imputation --------------------------------------------------------------------------------------------
random_forest <- missForest(MCAR[ ,1:14], xtrue = ipumsdeel1[ ,1:13], verbose = TRUE)
  
  # Computing accuracy
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
  randomforest_error
  
  # https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/
  # https://stat.ethz.ch/education/semesters/ss2013/ams/paper/missForest_1.2.pdf

  
  
## (2) Random forest imputation with MICE ------------------------------------------
  mice.impute.rf()


  
## Building Naive Bayes model ------------------------------------------------------------------------------------------

  # Can't find any R-code to implement the Naive Bayes theory I wrote about in my thesis.


  
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
install.packages("e1071")
library(e1071)

my_mcar <- data.frame(MCAR)

names(my_mcar) <- paste("value", 1:13, sep="")

total_error <- 0

# loop

# convert to factors for predict
my_mcar$value1 <- factor(my_mcar$value1)
my_mcar$value2 <- factor(my_mcar$value2)

# split the dataset into train and test
train_1 <- my_mcar[!is.na(my_mcar[,1]),]
test_1 <- my_mcar[is.na(my_mcar[,1]),]

create_train <- function(data, column){
  data[!is.na(data[,column]),]
}

train_2 <- create_train(my_mcar, 2)
train_3 <- create_train(my_mcar, 3)
train_4 <- create_train(my_mcar, 4)
train_5 <- create_train(my_mcar, 5)
train_6 <- create_train(my_mcar, 6)
train_7 <- create_train(my_mcar, 7)
train_8 <- create_train(my_mcar, 8)
train_9 <- create_train(my_mcar, 9)
train_10 <- create_train(my_mcar, 10)
train_11 <- create_train(my_mcar, 11)
train_12 <- create_train(my_mcar, 12)
train_13 <- create_train(my_mcar, 13)

create_test <- function(data, column){
  data[is.na(data[,column]),]
}

test_2 <- create_test(my_mcar, 2)
test_3 <- create_test(my_mcar, 3)
test_4 <- create_test(my_mcar, 4)
test_5 <- create_test(my_mcar, 5)
test_6 <- create_test(my_mcar, 6)
test_7 <- create_test(my_mcar, 7)
test_8 <- create_test(my_mcar, 8)
test_9 <- create_test(my_mcar, 9)
test_10 <- create_test(my_mcar, 10)
test_11 <- create_test(my_mcar, 11)
test_12 <- create_test(my_mcar, 12)
test_13 <- create_test(my_mcar, 13)

# build the predicting model
model_1 <- naiveBayes(formula = value1 ~ ., data = train_1)

# impute values
predictions_1 <- predict(model_1, newdata = test_1)


#model_1_rf <- rf(formula = value1 ~ ., data = train_1)
#model_1_dt <- dt(formula = value1 ~ ., data = train_1)
#model_1_svm <- svm(formula = value1 ~ ., data = train_1)

#pred_1_rf <- predict(model_1_rf, newdata = test_1)

# compare against the true values
true_1 <- ipumsdeel1[is.na(my_mcar[,1]), 1]

# compute error
error <- sum(!true_1 == predictions_1)

# sum error across columns
total_error <- total_error + error




# mean
model_13_mean <- mean(train_13[,13])
predictions_13_mean <- rep(model_13_mean, nrow(test_13))

                           
  # Can't find any R-code to implement the SVM theory I wrote about in my thesis. 
  
  
  
## Decision Tree imputation --------------------------------------------------------------------------------------------
decisiontree <- mice.impute.cart(MCAR, minbucket = 5)
            
  
  # https://rdrr.io/cran/simputation/man/impute_tree.html
  # https://www.kaggle.com/captcalculator/imputing-missing-data-with-the-mice-package-in-r/code


  
  
  
  

## Creating train and test set -----------------------------------------------------------------------------------------
# Do I need this? Because I have the original and new dataset to compare. 
# Maybe I have to make a train set to find the optimal parameters to run on the test set? And then use the test set to compare with original data set?

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


