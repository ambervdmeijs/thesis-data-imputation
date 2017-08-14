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


# Loading the database ------------------------------------------------------------------------------------------------
#ipumsdeel1 <- read_excel("D:/Amber/Documenten/School/Tilburg University/Master/Thesis/IPUMS2001 deel 1.xlsx")

#ipumsdeel1 <- read_excel("//studfiles.campus.uvt.nl/files/home/home06/u1278896/Thesis/IPUMS2001 deel 1.xlsx")


# Creating missing values ---------------------------------------------------------------------------------------------
missing_ipums <- missing_data(ipumsdeel1, miss_prop = 0.05, type = 'random')

prop.table(table(is.na(missing_ipums))) # $column naam toevoegen, om proportie missing data van column/variabel te zien
  # add clust_var = NULL if generating missing data from a single level data set

## Pattern of missing data 
md.pattern(missing_ipums)

## Visualizing missing data pattern 
missing_plot <- aggr(missing_ipums, col = c("navyblue", "red"), numbers = TRUE, sortVars = TRUE, 
                     labels = names(ipumsdeel1), cex.axis = 0.7, gap = 3, 
                     ylab = c("Histogram of missing data", "Pattern"))

  # https://datascienceplus.com/imputing-missing-data-with-r-mice-package/ 


# Building Mean imputation model ---------------------------------------------------------------------------------------
apply_meanimputation <- transform(ipumsdeel1, missing_ipums = ifelse(is.na(missing_ipums), 
                                                                     mean(missing_ipums, na.rm = TRUE), missing_ipums))

## Computing accuracy with errors 
actual_data <- ipumsdeel1

predicted_data <- rep(apply_meanimputation, length(actual_data))

regr.eval(actual_data, predicted_data)

print(regr.eval)


## Model applied on train data 
applytrn_meanimputation <- transform(ipumsdeel1, missing_ipums = ifelse(is.na(missing_ipums), 
                                                                     mean(missing_ipums, na.rm=TRUE), missing_ipums))
  
  # Each element of y; if it is NA, we replace it with the mean, otherwise we replace it with the original value









# Building Multiple imputation model ----------------------------------------------------------------------------------
apply_multipleimputation <- mice(missing_ipums, m = 5, maxit = 50, meth = "pmm", seed = 2)

summary(apply_multipleimputation)



  # https://datascienceplus.com/imputing-missing-data-with-r-mice-package/


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

