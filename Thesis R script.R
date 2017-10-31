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


# Loading the database ------------------------------------------------------------------------------------------------
ipumsdeel1 <- read_excel("D:/Amber/Documenten/School/Tilburg University/Master/Thesis/IPUMS2001 deel 1.xlsx")
#ipumsdeel1 <- read_excel("//studfiles.campus.uvt.nl/files/home/home06/u1278896/Thesis/IPUMS2001 deel 1.xlsx")


# Open database -------------------------------------------------------------------------------------------------------
View(ipumsdeel1)


# Creating missing values (MCAR with a 5% maximum treshold) -----------------------------------------------------------
MCAR <- SimIm(ipumsdeel1, p = 0.05)       #https://cran.r-project.org/web/packages/imputeR/imputeR.pdf 
  
  ## Counting NA's in dataset MCAR 
  sum(is.na(ipumsdeel1))

  prop.table(table(is.na(MCAR))) # $column naam toevoegen, om proportie missing data van column/variabel te zien
  # add clust_var = NULL if generating missing data from a single level data set

  ## Looking at the missing data pattern
  md.pattern(MCAR) #Looking at the missing data pattern

  ## Visualizing missing data pattern 


  # https://datascienceplus.com/imputing-missing-data-with-r-mice-package/ 


# Mode imputation -----------------------------------------------------------------------------------------------------
mode_imputation <- modeimp(MCAR)

  ## Computing accuracy / Evaluating method  
  regr.eval(ipumsdeel1, mode_imputation)
  
  #r-statistics.co/Missing-Value-Treatment-With-R.html 


# Multiple imputation -------------------------------------------------------------------------------------------------

  ## MICE
  #multiple_imputation <- mice(missing_ipums, m = 5, maxit = 50, meth = "pmm", seed = 2)

  #summary(apply_multipleimputation)
  # https://datascienceplus.com/imputing-missing-data-with-r-mice-package/

  ## missMDA
  #number_dimensions <- estim_ncpMCA(MCAR, ncp.max = 5)
  
  #multiple_imputation <- MIMCA(MCAR, ncp = 4, nboot = 10)
  # https://www.r-bloggers.com/multiple-imputation-for-continuous-and-categorical-data/



# Creating a training and a test set for 'ipumsdeel1' dataframe --------------------------------------------------------

## 70% of the sample size 
smp_size <- floor(0.70 * nrow(ipumsdeel1))

## set the seed to make the partition reproductible 
set.seed(1)

trn_indexes <- sample.int(nrow(ipumsdeel1), size = smp_size * nrow(ipumsdeel1)) #sets maken van missing data set?

trn_ipumsdeel1 <- ipumsdeel1[trn_indexes, ]
tst_ipumsdeel1 <- ipumsdeel1[-trn_indexes, ]

# Do I want a hold-out set for final test? Or only train and test?

## with Cross Validation 

trn_ipumsdeel1_cv <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

trn_meanimputation <- train(x, data = ipumsdeel1, trControl = trn_ipumsdeel1_cv, method = "nb")


# http://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/ 


## Computing accuracy train set 

## Adjusting parameters 

## Computing accuracy test set 


# Building Random Forest model ----------------------------------------------------------------------------------------
apply_randomforest <- missForest(missing_ipums)

## Calling imputed data matrix
apply_randomforest$ximp

## Calling OOB imputation error estimate 
apply_randomforest$OOBerror 

    #PFC = the proportion of falsely classified entries in the categorical part of the imputed data set 
    #Good = close to 0, bad is close to 1

## Additional performance output: estimated errors, difference and running time with max. 10 iterations   
set.seed(3)
apply_randomforest <- missForest(missing_ipums, verbose = TRUE, maxiter = 10)

    # https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/
    # https://stat.ethz.ch/education/semesters/ss2013/ams/paper/missForest_1.2.pdf


# Building Naive Bayes model ------------------------------------------------------------------------------------------
apply_naive <- naiveBayes(class ~ ., data = trn_ipumsdeel1)
class(apply_naive)
summary(apply_naive)
print(apply_naive)

predict_naive <- predict(apply_naive, newdata = tst_ipumsdeel1)

    # https://www.r-bloggers.com/naive-bayes-classification-in-r-part-2/ 

