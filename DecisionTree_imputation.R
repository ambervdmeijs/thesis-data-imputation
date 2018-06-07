
# Installing packages -----------------------------------------------------------------------------------------------------------------
install.packages("readxl")
install.packages("mice")
install.packages("devtools")



#https://arxiv.org/pdf/1711.11394.pdf? 


# Loading packages --------------------------------------------------------------------------------------------------------------------
library("readxl")
library("mice")
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
DT_MCAR2.1 <- data.frame(MCAR2.1)
DT_MCAR2.2 <- data.frame(MCAR2.2)
DT_MCAR2.3 <- data.frame(MCAR2.3)
DT_MCAR5.1 <- data.frame(MCAR5.1)
DT_MCAR5.2 <- data.frame(MCAR5.2)
DT_MCAR5.3 <- data.frame(MCAR5.3)
DT_MCAR10.1 <- data.frame(MCAR10.1)
DT_MCAR10.2 <- data.frame(MCAR10.2)
DT_MCAR10.3 <- data.frame(MCAR10.3)


## Giving variables original names and making variables readable -----------------------------------------------------------------------
names(DT_MCAR2.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(DT_MCAR2.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(DT_MCAR2.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(DT_MCAR5.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(DT_MCAR5.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(DT_MCAR5.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(DT_MCAR10.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(DT_MCAR10.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(DT_MCAR10.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)


## Converting to factors for categorical prediction ------------------------------------------------------------------------------------
for (i in 1:ncol(DT_MCAR2.1)) {
  DT_MCAR2.1[, i] <- as.factor(DT_MCAR2.1[, i])
}

for (i in 1:ncol(DT_MCAR2.2)) {
  DT_MCAR2.2[, i] <- as.factor(DT_MCAR2.2[, i])
}

for (i in 1:ncol(DT_MCAR2.3)) {
  DT_MCAR2.3[, i] <- as.factor(DT_MCAR2.3[, i])
}

for (i in 1:ncol(DT_MCAR5.1)) {
  DT_MCAR5.1[, i] <- as.factor(DT_MCAR5.1[, i])
}

for (i in 1:ncol(DT_MCAR5.2)) {
  DT_MCAR5.2[, i] <- as.factor(DT_MCAR5.2[, i])
}

for (i in 1:ncol(DT_MCAR5.3)) {
  DT_MCAR5.3[, i] <- as.factor(DT_MCAR5.3[, i])
}

for (i in 1:ncol(DT_MCAR10.1)) {
  DT_MCAR10.1[, i] <- as.factor(DT_MCAR10.1[, i])
}

for (i in 1:ncol(DT_MCAR10.2)) {
  DT_MCAR10.2[, i] <- as.factor(DT_MCAR10.2[, i])
}

for (i in 1:ncol(DT_MCAR10.3)) {
  DT_MCAR10.3[, i] <- as.factor(DT_MCAR10.3[, i])
}


# Training, testing, predicting and imputing -----------------------------------------------------------------------------------------

## Decision Tree with 'mice' -------------------------------------------------------------------------------------------------------- 
set.seed(3)

# 2% data sets
tic("Decision Tree 2.1 processing time...")
decision_tree2.1 <- mice(DT_MCAR2.1, meth = 'cart', minbucket = 3)
toc(log = TRUE)
df_decision_tree2.1 <- as.data.frame(decision_tree2.1)
save(df_decision_tree2.1, file = "decision_tree21.Rdata")

tic("Decision Tree 2.2 processing time...")
decision_tree2.2 <- mice(DT_MCAR2.2, meth = 'cart', minbucket = 3)
toc(log = TRUE)
df_decision_tree2.2 <- as.data.frame(decision_tree2.2)
save(df_decision_tree2.2, file = "decision_tree22.Rdata")

tic("Decision Tree 2.3 processing time...")
decision_tree2.3 <- mice(DT_MCAR2.3, meth = 'cart', minbucket = 3)
toc(log = TRUE)
df_decision_tree2.3 <- as.data.frame(decision_tree2.3)
save(df_decision_tree2.3, file = "decision_tree21.Rdata")


# 5% data sets
tic("Decision Tree 5.1 processing time...")
decision_tree5.1 <- mice(DT_MCAR5.1, meth = 'cart', minbucket = 3)
toc(log = TRUE)
df_decision_tree5.1 <- as.data.frame(decision_tree5.1)
save(df_decision_tree5.1, file = "decision_tree51.Rdata")

tic("Decision Tree 5.2 processing time...")
decision_tree5.2 <- mice(DT_MCAR5.2, meth = 'cart', minbucket = 3)
toc(log = TRUE)
df_decision_tree5.2 <- as.data.frame(decision_tree5.2)
save(df_decision_tree5.2, file = "decision_tree52.Rdata")

tic("Decision Tree 5.3 processing time...")
decision_tree5.3 <- mice(DT_MCAR5.3, meth = 'cart', minbucket = 3)
toc(log = TRUE)
df_decision_tree5.3 <- as.data.frame(decision_tree5.3)
save(df_decision_tree5.3, file = "decision_tree53.Rdata")


# 10% data sets 
tic("Decision Tree 10.1 processing time...")
decision_tree10.1 <- mice(DT_MCAR10.1, meth = 'cart', minbucket = 3)
toc(log = TRUE)
df_decision_tree10.1 <- as.data.frame(decision_tree10.1)
save(df_decision_tree10.1, file = "decision_tree101.Rdata")

tic("Decision Tree 10.2 processing time...")
decision_tree10.2 <- mice(DT_MCAR10.2, meth = 'cart', minbucket = 3)
toc(log = TRUE)
df_decision_tree10.2 <- as.data.frame(decision_tree10.2)
save(df_decision_tree10.2, file = "decision_tree10.2.Rdata")

tic("Decision Tree 10.3 processing time...")
decision_tree10.3 <- mice(DT_MCAR10.3, meth = 'cart', minbucket = 3)
toc(log = TRUE)
df_decision_tree10.3 <- as.data.frame(decision_tree10.3)
save(df_decision_tree10.3, file = "decision_tree103.Rdata")


# Check if all values are imputed 
anyNA(c(df_decision_tree2.1, df_decision_tree2.2, df_decision_tree2.3, df_decision_tree5.1, df_decision_tree5.2, 
        df_decision_tree5.3, df_decision_tree10.1, df_decision_tree10.2, df_decision_tree10.3))


## Computing accuracy ---------------------------------------------------------------------------------------------------------------------------------

# Setting correct and total to '0' 
DT2.1_correct <- DT2.2_correct <- DT2.3_correct <- DT5.1_correct <- DT5.2_correct <- DT5.3_correct <- DT10.1_correct <- DT10.2_correct <- DT10.3_correct <- 0
DT2.1_total <- DT2.2_total <- DT2.3_total <- DT5.1_total <- DT5.2_total <- DT5.3_total <- DT10.1_total <- DT10.2_total <- DT10.3_total <- 0


# Computing the correct imputed values 
DT2.1_correct <- DT2.1_correct + sum(ipums == df_decision_tree2.1)
DT2.2_correct <- DT2.2_correct + sum(ipums == df_decision_tree2.2)
DT2.3_correct <- DT2.3_correct + sum(ipums == df_decision_tree2.3)

DT5.1_correct <- DT5.1_correct + sum(ipums == df_decision_tree5.1)
DT5.2_correct <- DT5.2_correct + sum(ipums == df_decision_tree5.2)
DT5.3_correct <- DT5.3_correct + sum(ipums == df_decision_tree5.3)

DT10.1_correct <- DT10.1_correct + sum(ipums == df_decision_tree10.1)
DT10.2_correct <- DT10.2_correct + sum(ipums == df_decision_tree10.2)
DT10.3_correct <- DT10.3_correct + sum(ipums == df_decision_tree10.3)


# Computing the total values in data set
DT2.1_total <- DT2.1_total + sum(!is.na(df_decision_tree2.1))
DT2.2_total <- DT2.2_total + sum(!is.na(df_decision_tree2.2))
DT2.3_total <- DT2.3_total + sum(!is.na(df_decision_tree2.3))

DT5.1_total <- DT5.1_total + sum(!is.na(df_decision_tree5.1))
DT5.2_total <- DT5.2_total + sum(!is.na(df_decision_tree5.2))
DT5.3_total <- DT5.3_total + sum(!is.na(df_decision_tree5.3))

DT10.1_total <- DT10.1_total + sum(!is.na(df_decision_tree10.1))
DT10.2_total <- DT10.2_total + sum(!is.na(df_decision_tree10.2))
DT10.3_total <- DT10.3_total + sum(!is.na(df_decision_tree10.3))


# Computing the accuracy of imputation 
DT2.1_accuracy <- DT2.1_correct / DT2.1_total
DT2.2_accuracy <- DT2.2_correct / DT2.2_total
DT2.3_accuracy <- DT2.3_correct / DT2.3_total

DT5.1_accuracy <- DT5.1_correct / DT5.1_total
DT5.2_accuracy <- DT5.2_correct / DT5.2_total
DT5.3_accuracy <- DT5.3_correct / DT5.3_total

DT10.1_accuracy <- DT10.1_correct / DT10.1_total
DT10.2_accuracy <- DT10.2_correct / DT10.2_total
DT10.3_accuracy <- DT10.3_correct / DT10.3_total


## Dataframe with accuracy -----------------------------------------------------------------------------------------------------------------------------------------------
DT_results <- data.frame(Data.set = c("2% - version 1",
                                      "2% - version 2",
                                      "2% - version 3", 
                                      "5% - version 1", 
                                      "5% - version 2", 
                                      "5% - version 3",
                                      "10% - version 1",
                                      "10% - version 2", 
                                      "10% - version 3"),
                         
                         
                         Accuracy = c(DT2.1_accuracy, 
                                      DT2.2_accuracy, 
                                      DT2.3_accuracy, 
                                      DT5.1_accuracy,
                                      DT5.2_accuracy,
                                      DT5.3_accuracy,
                                      DT10.1_accuracy, 
                                      DT10.2_accuracy,
                                      DT10.3_accuracy))  



# Computing F1-score
install.packages("MLmetrics")
library("MLmetrics")

F1_Score(dt_imputation1$predictorMatrix, subset_IPUMS, positive = 1)
