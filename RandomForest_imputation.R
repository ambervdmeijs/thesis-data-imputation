

## Installing packages -----------------------------------------------------------------------------------------------------------------
install.packages("missForest", dependencies = TRUE)
install.packages("readxl")
install.packages("devtools")


## Loading packages --------------------------------------------------------------------------------------------------------------------
library("missForest")
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
RF_MCAR2.1 <- data.frame(MCAR2.1)
RF_MCAR2.2 <- data.frame(MCAR2.2)
RF_MCAR2.3 <- data.frame(MCAR2.3)
RF_MCAR5.1 <- data.frame(MCAR5.1)
RF_MCAR5.2 <- data.frame(MCAR5.2)
RF_MCAR5.3 <- data.frame(MCAR5.3)
RF_MCAR10.1 <- data.frame(MCAR10.1)
RF_MCAR10.2 <- data.frame(MCAR10.2)
RF_MCAR10.3 <- data.frame(MCAR10.3)


## Giving variables original names and making variables readable -----------------------------------------------------------------------
names(RF_MCAR2.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(RF_MCAR2.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(RF_MCAR2.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(RF_MCAR5.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(RF_MCAR5.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(RF_MCAR5.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(RF_MCAR10.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(RF_MCAR10.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(RF_MCAR10.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)


## Converting to factors for categorical prediction ------------------------------------------------------------------------------------
for (i in 1:ncol(RF_MCAR2.1)) {
  RF_MCAR2.1[, i] <- as.factor(RF_MCAR2.1[, i])
}

for (i in 1:ncol(RF_MCAR2.2)) {
  RF_MCAR2.2[, i] <- as.factor(RF_MCAR2.2[, i])
}

for (i in 1:ncol(RF_MCAR2.3)) {
  RF_MCAR2.3[, i] <- as.factor(RF_MCAR2.3[, i])
}

for (i in 1:ncol(RF_MCAR5.1)) {
  RF_MCAR5.1[, i] <- as.factor(RF_MCAR5.1[, i])
}

for (i in 1:ncol(RF_MCAR5.2)) {
  RF_MCAR5.2[, i] <- as.factor(RF_MCAR5.2[, i])
}

for (i in 1:ncol(RF_MCAR5.3)) {
  RF_MCAR5.3[, i] <- as.factor(RF_MCAR5.3[, i])
}

for (i in 1:ncol(RF_MCAR10.1)) {
  RF_MCAR10.1[, i] <- as.factor(RF_MCAR10.1[, i])
}

for (i in 1:ncol(RF_MCAR10.2)) {
  RF_MCAR10.2[, i] <- as.factor(RF_MCAR10.2[, i])
}

for (i in 1:ncol(RF_MCAR10.3)) {
  RF_MCAR10.3[, i] <- as.factor(RF_MCAR10.3[, i])
}



# Training, testing, predicting and imputing -----------------------------------------------------------------------------------------



## Random Forest Imputation with 'missForest' ----------------------------------------------------------------------------------------
set.seed(8)


# 2% data sets
tic("Random Forest 2.1 processing time...")
random_forest2.1 <- missForest(RF_MCAR2.1, ntree = 30, maxiter = 5, replace = TRUE)
toc(log = TRUE)
df_random_forest2.1 <- as.data.frame(random_forest2.1$ximp)
save(df_random_forest2.1, file = "random_forest21.Rdata")

tic("Random Forest 2.2 processing time...")
random_forest2.2 <- missForest(RF_MCAR2.2, ntree = 30, maxiter = 5, replace = TRUE)
toc(log = TRUE)
df_random_forest2.2 <- as.data.frame(random_forest2.2$ximp)
save(df_random_forest2.2, file = "random_forest22.Rdata")

tic("Random Forest 2.3 processing time...")
random_forest2.3 <- missForest(RF_MCAR2.3, ntree = 30, maxiter = 5, replace = TRUE)
toc(log = TRUE)
df_random_forest2.3 <- as.data.frame(random_forest2.3$ximp)
save(df_random_forest2.3, file = "random_forest23.Rdata")


# 5% data sets 
tic("Random Forest 5.1 processing time...")
random_forest5.1 <- missForest(RF_MCAR5.1, ntree = 30, maxiter = 5, replace = TRUE)
toc(log = TRUE)
df_random_forest5.1 <- as.data.frame(random_forest5.1$ximp)
save(df_random_forest5.1, file = "random_forest51.Rdata")

tic("Random Forest 5.2 processing time...")
random_forest5.2 <- missForest(RF_MCAR5.2, ntree = 30, maxiter = 5, replace = TRUE)
toc(log = TRUE)
df_random_forest5.2 <- as.data.frame(random_forest5.2$ximp)
save(df_random_forest5.2, file = "random_forest52.Rdata")

tic("Random Forest 5.3 processing time...")
random_forest5.3 <- missForest(RF_MCAR5.3, ntree = 30, maxiter = 5, replace = TRUE)
toc(log = TRUE)
df_random_forest5.3 <- as.data.frame(random_forest5.3$ximp)
save(df_random_forest5.3, file = "random_forest53.Rdata")


# 10% data sets
tic("Random Forest 10.1 processing time...")
random_forest10.1 <- missForest(RF_MCAR10.1, ntree = 30, maxiter = 5, replace = TRUE)
toc(log = TRUE)
df_random_forest10.1 <- as.data.frame(random_forest10.1$ximp)
save(df_random_forest10.1, file = "random_forest101.Rdata")

tic("Random Forest 10.2 processing time...")
random_forest10.2 <- missForest(RF_MCAR10.2, ntree = 30, maxiter = 5, replace = TRUE)
toc(log = TRUE)
df_random_forest10.2 <- as.data.frame(random_forest10.2$ximp)
save(df_random_forest10.2, file = "random_forest102.Rdata")

tic("Random Forest 10.3 processing time...")
random_forest10.3 <- missForest(RF_MCAR10.3, ntree = 30, maxiter = 5, replace = TRUE)
toc(log = TRUE)
df_random_forest10.3 <- as.data.frame(random_forest10.3$ximp)
save(df_random_forest10.3, file = "random_forest103.Rdata")


# Check if all values are imputed 
anyNA(c(df_random_forest2.1, df_random_forest2.2, df_random_forest2.3, df_random_forest5.1, df_random_forest5.2, 
        df_random_forest5.3, df_random_forest10.1, df_random_forest10.2, df_random_forest10.3))


## Computing accuracy ---------------------------------------------------------------------------------------------------------------------------------

# Setting correct and total to '0' 
RF2.1_correct <- RF2.2_correct <- RF2.3_correct <- RF5.1_correct <- RF5.2_correct <- RF5.3_correct <- RF10.1_correct <- RF10.2_correct <- RF10.3_correct <- 0
RF2.1_total <- RF2.2_total <- RF2.3_total <- RF5.1_total <- RF5.2_total <- RF5.3_total <- RF10.1_total <- RF10.2_total <- RF10.3_total <- 0


# Computing the correct imputed values 
RF2.1_correct <- RF2.1_correct + sum(ipums == df_random_forest2.1)
RF2.2_correct <- RF2.2_correct + sum(ipums == df_random_forest2.2)
RF2.3_correct <- RF2.3_correct + sum(ipums == df_random_forest2.3)

RF5.1_correct <- RF5.1_correct + sum(ipums == df_random_forest5.1)
RF5.2_correct <- RF5.2_correct + sum(ipums == df_random_forest5.2)
RF5.3_correct <- RF5.3_correct + sum(ipums == df_random_forest5.3)

RF10.1_correct <- RF10.1_correct + sum(ipums == df_random_forest10.1)
RF10.2_correct <- RF10.2_correct + sum(ipums == df_random_forest10.2)
RF10.3_correct <- RF10.3_correct + sum(ipums == df_random_forest10.3)



# Computing the total values in data set
RF2.1_total <- RF2.1_total + sum(!is.na(df_random_forest2.1))
RF2.2_total <- RF2.2_total + sum(!is.na(df_random_forest2.2))
RF2.3_total <- RF2.3_total + sum(!is.na(df_random_forest2.3))

RF5.1_total <- RF5.1_total + sum(!is.na(df_random_forest5.1))
RF5.2_total <- RF5.2_total + sum(!is.na(df_random_forest5.2))
RF5.3_total <- RF5.3_total + sum(!is.na(df_random_forest5.3))

RF10.1_total <- RF10.1_total + sum(!is.na(df_random_forest10.1))
RF10.2_total <- RF10.2_total + sum(!is.na(df_random_forest10.2))
RF10.3_total <- RF10.3_total + sum(!is.na(df_random_forest10.3))


# Computing the accuracy of imputation 
RF2.1_accuracy <- RF2.1_correct / RF2.1_total
RF2.2_accuracy <- RF2.2_correct / RF2.2_total
RF2.3_accuracy <- RF2.3_correct / RF2.3_total

RF5.1_accuracy <- RF5.1_correct / RF5.1_total
RF5.2_accuracy <- RF5.2_correct / RF5.2_total
RF5.3_accuracy <- RF5.3_correct / RF5.3_total

RF10.1_accuracy <- RF10.1_correct / RF10.1_total
RF10.2_accuracy <- RF10.2_correct / RF10.2_total
RF10.3_accuracy <- RF10.3_correct / RF10.3_total


## Dataframe with accuracy -----------------------------------------------------------------------------------------------------------------------------------------------
RF_results <- data.frame(Data.set = c("2% - version 1",
                                      "2% - version 2",
                                      "2% - version 3", 
                                      "5% - version 1", 
                                      "5% - version 2", 
                                      "5% - version 3",
                                      "10% - version 1",
                                      "10% - version 2", 
                                      "10% - version 3"),
                           
                           
                          Accuracy = c(RF2.1_accuracy, 
                                      RF2.2_accuracy, 
                                      RF2.3_accuracy, 
                                      RF5.1_accuracy,
                                      RF5.2_accuracy,
                                      RF5.3_accuracy,
                                      RF10.1_accuracy, 
                                      RF10.2_accuracy,
                                      RF10.3_accuracy))  


# Computing F1-score
install.packages("MLmetrics")
library("MLmetrics")

F1_Score(y_true = subset_IPUMS, y_pred = random_forest$ximp, positive = NULL)





