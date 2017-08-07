
# Install packages ------------------------------------------------------------------------------------------
install.packages("readxl")
install.packages("simglm")
install.packages("caret")

# Loading packages ------------------------------------------------------------------------------------------
library("readxl")
library("simglm")
library("caret")

# Read Excel file -------------------------------------------------------------------------------------------
ipumsdeel1 <- read_excel("D:/Amber/Documenten/School/Tilburg University/Master/Thesis/IPUMS2001 deel 1.xlsx")

# Creating missing values -----------------------------------------------------------------------------------
y <- missing_data(ipumsdeel1, miss_prop = 0.05, type = 'random')

  # add clust_var = NULL if generating missing data from a single level data set

  # prop.table(table(is.na(y$....)))

# Mean imputation -------------------------------
ipumsdeel1 = transform(ipumsdeel1, y = ifelse(is.na(y), mean(y, na.rm=TRUE), y))

  # Each element of y; if it is NA, we replace it with the mean, otherwise we replace 
  # it with the original value

# http://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/ 
# Creating a training and a test set for 'dataframe' dataframe ------------------------------
set.seed(1)
trn_indexes <- sample.int(nrow(dataframe), size = 0.8 * nrow(dataframe))

trn_dataframe <- dataframe[trn_indexes, ]
tst_dataframe <- dataframe[-trn_indexes, ]

# Calculate accuracy with k-fold cross validation ------------

