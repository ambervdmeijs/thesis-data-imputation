# Installing packages -----------------------------------------------------------------------------------------------------------------
install.packages("readxl")
install.packages("e1071")


# Loading packages --------------------------------------------------------------------------------------------------------------------
library("readxl")
library("e1071")


# Function for creating probability ---------------------------------------------------------------------------------------------------
SimIm <- function(data, p = 0.1) {
  vec <- c(unlist(data))
  missing <- rbinom(length(vec), 1, p)
  vec[missing == 1] <- NA
  dim(vec) <- dim(data)
  return(vec)
}


# Reading in 'Ipums2001 deel 1' and 'Ipums2001 deel 2' data sets ----------------------------------------------------------------------
ipumsdeel1 <- read_excel("input/IPUMS2001 deel 1.xlsx")
ipumsdeel2 <- read_excel("input/IPUMS2001 deel 2.xlsx")


# Combining the two data sets ---------------------------------------------------------------------------------------------------------
ipums <- rbind(ipumsdeel1, ipumsdeel2)


# Deleting 'nr' column and the 'Gewicht' column ---------------------------------------------------------------------------------------
ipums <- ipums[-1]
ipums$Gewicht <- NULL 


# Creating missing values 'completely at random' --------------------------------------------------------------------------------------
MCAR <- SimIm(ipums, p = 0.05)  


# Taking random sample from data set for subset ---------------------------------------------------------------------------------------
my_rows <- sample(1:nrow(ipums), 500, replace = F)


# Creating subsets --------------------------------------------------------------------------------------------------------------------
subset_IPUMS <- ipums[my_rows, c(1:12)]
subset_MCAR <- MCAR[my_rows, c(1:12)]


# Turning data set into dataframe -----------------------------------------------------------------------------------------------------
NB_mcar <- data.frame(subset_MCAR)


# Giving variables original names and making variables readable -----------------------------------------------------------------------
names(NB_mcar) <- gsub(" ", "_", names(ipums), fixed=TRUE)


# Converting to factors for categorical prediction ------------------------------------------------------------------------------------
for (i in 1:ncol(NB_mcar)) {
  NB_mcar[, i] <- as.factor(NB_mcar[, i])
}


# Training, testing, predicting and imputing -----------------------------------------------------------------------------------------


# Setting variable 'correct' and 'total' to 0 ----------------------------------------------------------------------------------------
Correct <- 0
Total <- 0


for (column in 1:ncol(NB_mcar)) {
  
  # Splitting the data sets into train set -------------------------------------------------------------------------------------------
  NBtrain <- NB_mcar[!is.na(NB_mcar[,column]),]
  

  # Splitting the data sets into test set --------------------------------------------------------------------------------------------
  NBtest <- NB_mcar[is.na(NB_mcar[,column]),]
  
  
  # Output for testing purposes ------------------------------------------------------------------------------------------------------
  print(paste(column, nrow(NBtest), nrow(NBtrain)))
  
  
  # Building the predicting models for all 12 columns --------------------------------------------------------------------------------
  predicted_name <- names(NBtrain)[column]
  my_formula <- as.formula(paste(predicted_name, "~ ."))
  NBmodel <- naiveBayes(formula = my_formula, data = NBtrain)
  
  
  # Imputing values for all columns --------------------------------------------------------------------------------------------------
  NBpredictions <- predict(NBmodel, NBtest)
  
  
  # Saving predictions into file -----------------------------------------------------------------------------------------------------
  # save(NBpredictions, paste("NB_predictions_", column, ".csv"))
  
  
  # Retrieving the true values that we thought were missing --------------------------------------------------------------------------
  NBtrue <- subset_IPUMS[is.na(NB_mcar[, column]), column]
  

  # Calculating the cumulative accuracy ----------------------------------------------------------------------------------------------
  Correct <- Correct + sum(NBpredictions == NBtrue) 
  Total <- Total + length(NBtrue)
}

# Calculating the overall accuracy ---------------------------------------------------------------------------------------------------
Accuracy <- Correct / Total
