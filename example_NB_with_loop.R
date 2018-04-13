library("readxl")
library(e1071)

SimIm <- function(data, p = 0.1) {
  vec <- c(unlist(data))
  missing <- rbinom(length(vec), 1, p)
  vec[missing == 1] <- NA
  dim(vec) <- dim(data)
  return(vec)
}

ipumsdeel1 <- read_excel("IPUMS2001 deel 1.xlsx")
ipumsdeel2 <- read_excel("IPUMS2001 deel 2.xlsx")
ipums <- rbind(ipumsdeel1, ipumsdeel2)
ipums <- ipums[-1]
ipums$Gewicht <- NULL 

MCAR <- SimIm(ipums, p = 0.05)  

my_rows <- sample(1:nrow(ipums), 500, replace = F)

subset_IPUMS <- ipums[my_rows, c(1:12)]
subset_MCAR <- MCAR[my_rows, c(1:12)]

NBmcar <- data.frame(subset_MCAR)
names(NBmcar) <- gsub(" ", "_", names(ipums), fixed=TRUE)

# Converting to factors for prediction: NB_mcar$Geslacht <- factor(NB_mcar$Geslacht) and NB_mcar$Leeftijd <- factor(NB_mcar$Leeftijd)
for (i in 1:ncol(NBmcar)) {
  NBmcar[, i] <- factor(NBmcar[, i])
}


#######################

correct <- 0
total <- 0

for (column in 1:ncol(NBmcar)) {
  
  # Splitting the datasets into train sets: NBtrain_1 <- NB_mcar[!is.na(NB_mcar[,1]),] and NBtest_1 <- NB_mcar[is.na(NB_mcar[,1]),]
  # Get the training data
  NBtrain <- NBmcar[!is.na(NBmcar[,column]),]
  
  # Splitting the datasets into test sets 
  # Get the missing data to predict for
  NBtest <- NBmcar[is.na(NBmcar[,column]),]
  
  # output for testing purposes
  print(paste(column, nrow(NBtest), nrow(NBtrain)))
  
  # Building the predicting models for all 12 columns
  predicted_name <- names(NBtrain)[column]
  my_formula <- as.formula(paste(predicted_name, "~ ."))
  NBmodel <- naiveBayes(formula = my_formula, data = NBtrain)
  
  
  # Imputing values for all columns: predictions_1 <- predict(model_1, newdata = test_1)
  NBpredictions <- predict(NBmodel, NBtest)
  
  save(NBpredictions, paste("NB_predictions_", column, ".csv"))
  
  # get the true values that we thought were missing
  NBtrue <- subset_IPUMS[is.na(NBmcar[, column]), column]
  
  # calculate cumulative accuracy
  correct <- correct + sum(NBpredictions == NBtrue) 
  total <- total + length(NBtrue)
}

accuracy <- correct / total
