# Installing packages -----------------------------------------------------------------------------------------------------------------
install.packages("readxl")
install.packages("e1071")
install.packages("dplyr")
install.packages("tidyr")
install.packages("plyr")


# Loading packages --------------------------------------------------------------------------------------------------------------------
library("readxl")
library("e1071")
library("dplyr")
library("tidyr")
library("plyr")


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

# Creating dataframe 
NB_df <- data.frame(matrix(vector(), 0, 12, 
                    dimnames=list(c(), c("Geslacht_pred", 
                                         "Leeftijd_pred", 
                                         "HH_Pos_pred", 
                                         "HH_grootte_pred", 
                                         "Woonregio_vorig_jaar_pred", 
                                         "Nationaliteit_pred", 
                                         "Geboorteland_pred", 
                                         "Onderwijsniveau_pred", 
                                         "Econ_status_pred",
                                         "Beroep_pred", 
                                         "SBI_pred",
                                         "Burg_staat_pred"))))  
                    



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
  #print(paste(column, nrow(NBtest), nrow(NBtrain)))
  
  
  # Building the predicting models for all 12 columns --------------------------------------------------------------------------------
  predicted_name <- names(NBtrain)[column]
  my_formula <- as.formula(paste(predicted_name, "~ ."))
  NBmodel <- naiveBayes(formula = my_formula, data = NBtrain)
  
  
  # Imputing values for all columns --------------------------------------------------------------------------------------------------
  NBpredictions <- predict(NBmodel, NBtest)
  print(NBpredictions)
  
  # Saving predictions into file -----------------------------------------------------------------------------------------------------
  # write.table(NBpredictions, file = "NB_predictions_.csv", append = TRUE, col.names = TRUE, row.names = FALSE, sep = ',', eol = "\r")
  #write.table(NBpredictions, file = "NB_predictions_", column, ".csv", append = TRUE, col.names = FALSE, sep = ',')
  #NB_pred <- read.delim(bestanden 1 tm 12)
  
  NB_df[paste(column, sep="")] 
  
  # Retrieving the true values that we thought were missing --------------------------------------------------------------------------
  NBtrue[paste(subset_IPUMS[is.na(NB_mcar[, column]), column])]

}  

  # Calculating the cumulative accuracy ----------------------------------------------------------------------------------------------
  Correct <- Correct + sum(NBpredictions %in% !NBtrue) 
  Total <- Total + length(NBtrue)


  ## Trying to get the imputing to work
df_Geslacht <- as.data.frame(as.matrix(NB_df[['1']]))
df_Leeftijd <- as.data.frame(as.matrix(NB_df[['2']]))
df_HH_Pos <- as.data.frame(as.matrix(NB_df[['3']]))
df_HH_grootte <- as.data.frame(as.matrix(NB_df[['4']]))
df_Woonregio_vorig_jaar <- as.data.frame(as.matrix(NB_df[['5']]))
df_Nationaliteit <- as.data.frame(as.matrix(NB_df[['6']]))
df_Geboorteland <- as.data.frame(as.matrix(NB_df[['7']]))
df_Onderwijsniveau <- as.data.frame(as.matrix(NB_df[['8']]))
df_Econ_status <- as.data.frame(as.matrix(NB_df[['9']]))
df_Beroep <- as.data.frame(as.matrix(NB_df[['10']]))
df_SBI <- as.data.frame(as.matrix(NB_df[['11']]))
df_Burg_staat <- as.data.frame(as.matrix(NB_df[['12']]))


n.obs <- sapply(myList, length)
seq.max <- seq_len(max(n.obs))
mat <- t(sapply(myList, "[", i=seq.max))

View(mat)

o.obs <- sapply(mat, length)
seq.max <- seq_len(max(o.obs))
nb <- t(sapply(mat, "[", i=seq.max))
View(nb)


NBdf <- as.matrix(nb)
NBdf <- t(NBdf) #transpose 
NBdf <- data.frame(NBdf)
for (i in 1:ncol(NBdf)) {
  NBdf[, i] <- as.factor(NBdf[, i])
}

# na's veranderen in 0. 

# Calculating the overall accuracy ---------------------------------------------------------------------------------------------------
Accuracy <- Correct / Total



