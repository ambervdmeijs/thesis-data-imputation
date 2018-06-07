
# Installing packages -----------------------------------------------------------------------------------------------------------------
install.packages("mice")
install.packages("readxl")
install.packages("devtools")


# Loading packages --------------------------------------------------------------------------------------------------------------------
library("mice")
library("readxl")
library("devtools")
install_github("jabiru/tictoc")
library("tictoc")


## Loading MCAR data frames to use ----------------------------------------------------------------------------------------------------
MCAR2.1 <- get(load(file = "MCAR2_1.Rdata"))
MCAR2.2 <- get(load(file = "MCAR2_2.Rdata"))
MCAR2.3 <- get(load(file = "MCAR2_3.Rdata"))
MCAR5.1 <- get(load(file = "MCAR5_1.Rdata"))
MCAR5.2 <- get(load(file = "MCAR5_2.Rdata"))
MCAR5.3 <- get(load(file = "MCAR5_3.Rdata"))
MCAR10.1 <- get(load(file = "MCAR10_1.Rdata"))
MCAR10.2 <- get(load(file = "MCAR10_2.Rdata"))
MCAR10.3 <- get(load(file = "MCAR10_3.Rdata"))


## Turning data set into dataframe -----------------------------------------------------------------------------------------------------
MI_MCAR2.1 <- data.frame(MCAR2.1)
MI_MCAR2.2 <- data.frame(MCAR2.2)
MI_MCAR2.3 <- data.frame(MCAR2.3)
MI_MCAR5.1 <- data.frame(MCAR5.1)
MI_MCAR5.2 <- data.frame(MCAR5.2)
MI_MCAR5.3 <- data.frame(MCAR5.3)
MI_MCAR10.1 <- data.frame(MCAR10.1)
MI_MCAR10.2 <- data.frame(MCAR10.2)
MI_MCAR10.3 <- data.frame(MCAR10.3)


## Giving variables original names and making variables readable -----------------------------------------------------------------------
names(MI_MCAR2.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(MI_MCAR2.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(MI_MCAR2.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(MI_MCAR5.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(MI_MCAR5.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(MI_MCAR5.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(MI_MCAR10.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(MI_MCAR10.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(MI_MCAR10.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)


## Converting to factors for categorical prediction ------------------------------------------------------------------------------------
for (i in 1:ncol(MI_MCAR2.1)) {
  MI_MCAR2.1[, i] <- as.factor(MI_MCAR2.1[, i])
}

for (i in 1:ncol(MI_MCAR2.2)) {
  MI_MCAR2.2[, i] <- as.factor(MI_MCAR2.2[, i])
}

for (i in 1:ncol(MI_MCAR2.3)) {
  MI_MCAR2.3[, i] <- as.factor(MI_MCAR2.3[, i])
}

for (i in 1:ncol(MI_MCAR5.1)) {
  MI_MCAR5.1[, i] <- as.factor(MI_MCAR5.1[, i])
}

for (i in 1:ncol(MI_MCAR5.2)) {
  MI_MCAR5.2[, i] <- as.factor(MI_MCAR5.2[, i])
}

for (i in 1:ncol(MI_MCAR5.3)) {
  MI_MCAR5.3[, i] <- as.factor(MI_MCAR5.3[, i])
}

for (i in 1:ncol(MI_MCAR10.1)) {
  MI_MCAR10.1[, i] <- as.factor(MI_MCAR10.1[, i])
}

for (i in 1:ncol(MI_MCAR10.2)) {
  MI_MCAR10.2[, i] <- as.factor(MI_MCAR10.2[, i])
}

for (i in 1:ncol(MI_MCAR10.3)) {
  MI_MCAR10.3[, i] <- as.factor(MI_MCAR10.3[, i])
}


# Training, testing, predicting and imputing -----------------------------------------------------------------------------------------

## Multiple Imputation with 'mice' ---------------------------------------------------------------------------------------------------
set.seed(8)


# 2% data sets
tic("Multiple Imputation 2.1 processing time...")
multiple_imp2.1 <- mice(MI_MCAR2.1, m = 3)
multiple_imputation2.1 <- complete(multiple_imp2.1)
toc(log = TRUE)
df_multiple_imputation2.1 <- as.data.frame(multiple_imputation2.1)
save(df_multiple_imputation2.1, file = "multiple_imputation21.Rdata")

tic("Multiple Imputation 2.2 processing time...")
multiple_imp2.2 <- mice(MI_MCAR2.2, m = 5)
multiple_imputation2.2 <- complete(multiple_imp2.2)
toc(log = TRUE)
df_multiple_imputation2.2 <- as.data.frame(multiple_imputation2.2)
save(df_multiple_imputation2.2, file = "multiple_imputation22.Rdata")

tic("Multiple Imputation 2.3 processing time...")
multiple_imp2.3 <- mice(MI_MCAR2.3, m = 5)
multiple_imputation2.3 <- complete(multiple_imp2.3)
toc(log = TRUE)
df_multiple_imputation2.3 <- as.data.frame(multiple_imputation2.3)
save(df_multiple_imputation2.3, file = "multiple_imputation23.Rdata")


# 5% data sets 
tic("Multiple Imputation 5.1 processing time...")
multiple_imp5.1 <- mice(MI_MCAR5.1, m = 5)
multiple_imputation5.1 <- complete(multiple_imp5.1)
toc(log = TRUE)
df_multiple_imputation5.1 <- as.data.frame(multiple_imputation5.1)
save(df_multiple_imputation5.1, file = "multiple_imputation51.Rdata")

tic("Multiple Imputation 5.2 processing time...")
multiple_imp5.2 <- mice(MI_MCAR5.2, m = 5)
multiple_imputation5.2 <- complete(multiple_imp5.2)
toc(log = TRUE)
df_multiple_imputation5.2 <- as.data.frame(multiple_imputation5.2)
save(df_multiple_imputation5.2, file = "multiple_imputation52.Rdata")

tic("Multiple Imputation 5.3 processing time...")
multiple_imp5.3 <- mice(MI_MCAR5.3, m = 5)
multiple_imputation5.3 <- complete(multiple_imp5.3)
toc(log = TRUE)
df_multiple_imputation5.3 <- as.data.frame(multiple_imputation5.3)
save(df_multiple_imputation5.3, file = "multiple_imputation53.Rdata")


# 10% data sets
tic("Multiple Imputation 10.1 processing time...")
multiple_imp10.1 <- mice(MI_MCAR10.1, m = 5)
multiple_imputation10.1 <- complete(multiple_imp10.1)
toc(log = TRUE)
df_multiple_imputation10.1 <- as.data.frame(multiple_imputation10.1)
save(df_multiple_imputation10.1, file = "multiple_imputation101.Rdata")

tic("Multiple Imputation 10.2 processing time...")
multiple_imp10.2 <- mice(MI_MCAR10.2, m = 5)
multiple_imputation10.2 <- complete(multiple_imp10.2)
toc(log = TRUE)
df_multiple_imputation10.2 <- as.data.frame(multiple_imputation10.2)
save(df_multiple_imputation10.2, file = "multiple_imputation102.Rdata")

tic("Multiple Imputation 10.3 processing time...")
multiple_imp10.3 <- mice(MI_MCAR10.3, m = 5)
multiple_imputation10.3 <- complete(multiple_imp10.3)
toc(log = TRUE)
df_multiple_imputation10.3 <- as.data.frame(multiple_imputation10.3)
save(df_multiple_imputation10.3, file = "multiple_imputation103.Rdata")


# Check if all values are imputed 
anyNA(c(df_multiple_imputation2.1, df_multiple_imputation2.2, df_multiple_imputation2.3, df_multiple_imputation5.1, df_multiple_imputation5.2, 
        df_multiple_imputation5.3, df_multiple_imputation10.1, df_multiple_imputation10.2, df_multiple_imputation10.3))


## Computing accuracy ---------------------------------------------------------------------------------------------------------------------------------

# Setting correct and total to '0' 
MI2.1_correct <- MI2.2_correct <- MI2.3_correct <- MI5.1_correct <- MI5.2_correct <- MI5.3_correct <- MI10.1_correct <- MI10.2_correct <- MI10.3_correct <- 0
MI2.1_total <- MI2.2_total <- MI2.3_total <- MI5.1_total <- MI5.2_total <- MI5.3_total <- MI10.1_total <- MI10.2_total <- MI10.3_total <- 0


# Computing the correct imputed values 
MI2.1_correct <- MI2.1_correct + sum(ipums == df_multiple_imputation2.1)
MI2.2_correct <- MI2.2_correct + sum(ipums == df_multiple_imputation2.2)
MI2.3_correct <- MI2.3_correct + sum(ipums == df_multiple_imputation2.3)

MI5.1_correct <- MI5.1_correct + sum(ipums == df_multiple_imputation5.1)
MI5.2_correct <- MI5.2_correct + sum(ipums == df_multiple_imputation5.2)
MI5.3_correct <- MI5.3_correct + sum(ipums == df_multiple_imputation5.3)

MI10.1_correct <- MI10.1_correct + sum(ipums == df_multiple_imputation10.1)
MI10.2_correct <- MI10.2_correct + sum(ipums == df_multiple_imputation10.2)
MI10.3_correct <- MI10.3_correct + sum(ipums == df_multiple_imputation10.3)


# Computing the total values in data set
MI2.1_total <- MI2.1_total + sum(!is.na(df_multiple_imputation2.1))
MI2.2_total <- MI2.2_total + sum(!is.na(df_multiple_imputation2.2))
MI2.3_total <- MI2.3_total + sum(!is.na(df_multiple_imputation2.3))

MI5.1_total <- MI5.1_total + sum(!is.na(df_multiple_imputation5.1))
MI5.2_total <- MI5.2_total + sum(!is.na(df_multiple_imputation5.2))
MI5.3_total <- MI5.3_total + sum(!is.na(df_multiple_imputation5.3))

MI10.1_total <- MI10.1_total + sum(!is.na(df_multiple_imputation10.1))
MI10.2_total <- MI10.2_total + sum(!is.na(df_multiple_imputation10.2))
MI10.3_total <- MI10.3_total + sum(!is.na(df_multiple_imputation10.3))


# Computing the accuracy of imputation 
MI2.1_accuracy <- MI2.1_correct / MI2.1_total
MI2.2_accuracy <- MI2.2_correct / MI2.2_total
MI2.3_accuracy <- MI2.3_correct / MI2.3_total

MI5.1_accuracy <- MI5.1_correct / MI5.1_total
MI5.2_accuracy <- MI5.2_correct / MI5.2_total
MI5.3_accuracy <- MI5.3_correct / MI5.3_total

MI10.1_accuracy <- MI10.1_correct / MI10.1_total
MI10.2_accuracy <- MI10.2_correct / MI10.2_total
MI10.3_accuracy <- MI10.3_correct / MI10.3_total


## Dataframe with accuracy -----------------------------------------------------------------------------------------------------------------------------------------------
MI_results <- data.frame(Data.set = c("2% - version 1",
                                      "2% - version 2",
                                      "2% - version 3", 
                                      "5% - version 1", 
                                      "5% - version 2", 
                                      "5% - version 3",
                                      "10% - version 1",
                                      "10% - version 2", 
                                      "10% - version 3"),
                         
                         
                         Accuracy = c(MI2.1_accuracy, 
                                      MI2.2_accuracy, 
                                      MI2.3_accuracy, 
                                      MI5.1_accuracy,
                                      MI5.2_accuracy,
                                      MI5.3_accuracy,
                                      MI10.1_accuracy, 
                                      MI10.2_accuracy,
                                      MI10.3_accuracy))  



# Computing F1-score
install.packages("MLmetrics")
library("MLmetrics")

F1_Score(MCAR2.1, ipums, positive = NULL)

