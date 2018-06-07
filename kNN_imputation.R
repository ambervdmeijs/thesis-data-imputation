
# Installing packages -----------------------------------------------------------------------------------------------------------------
install.packages("bnstruct")
install.packages("readxl")
install.packages("devtools")


# Loading packages --------------------------------------------------------------------------------------------------------------------
library("bnstruct")
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
kNN_MCAR2.1 <- as.matrix(MCAR2.1)
kNN_MCAR2.2 <- as.matrix(MCAR2.2)
kNN_MCAR2.3 <- as.matrix(MCAR2.3)
kNN_MCAR5.1 <- as.matrix(MCAR5.1)
kNN_MCAR5.2 <- as.matrix(MCAR5.2)
kNN_MCAR5.3 <- as.matrix(MCAR5.3)
kNN_MCAR10.1 <- as.matrix(MCAR10.1)
kNN_MCAR10.2 <- as.matrix(MCAR10.2)
kNN_MCAR10.3 <- as.matrix(MCAR10.3)


## Giving variables original names and making variables readable -----------------------------------------------------------------------
names(kNN_MCAR2.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(kNN_MCAR2.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(kNN_MCAR2.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(kNN_MCAR5.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(kNN_MCAR5.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(kNN_MCAR5.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(kNN_MCAR10.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(kNN_MCAR10.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(kNN_MCAR10.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)


## Converting to factors for categorical prediction ------------------------------------------------------------------------------------
for (i in 1:ncol(kNN_MCAR2.1)) {
  kNN_MCAR2.1[, i] <- as.factor(kNN_MCAR2.1[, i])
}

for (i in 1:ncol(kNN_MCAR2.2)) {
  kNN_MCAR2.2[, i] <- as.factor(kNN_MCAR2.2[, i])
}

for (i in 1:ncol(kNN_MCAR2.3)) {
  kNN_MCAR2.3[, i] <- as.factor(kNN_MCAR2.3[, i])
}

for (i in 1:ncol(kNN_MCAR5.1)) {
  kNN_MCAR5.1[, i] <- as.factor(kNN_MCAR5.1[, i])
}

for (i in 1:ncol(kNN_MCAR5.2)) {
  kNN_MCAR5.2[, i] <- as.factor(kNN_MCAR5.2[, i])
}

for (i in 1:ncol(kNN_MCAR5.3)) {
  kNN_MCAR5.3[, i] <- as.factor(kNN_MCAR5.3[, i])
}

for (i in 1:ncol(kNN_MCAR10.1)) {
  kNN_MCAR10.1[, i] <- as.factor(kNN_MCAR10.1[, i])
}

for (i in 1:ncol(kNN_MCAR10.2)) {
  kNN_MCAR10.2[, i] <- as.factor(kNN_MCAR10.2[, i])
}

for (i in 1:ncol(kNN_MCAR10.3)) {
  kNN_MCAR10.3[, i] <- as.factor(kNN_MCAR10.3[, i])
}


# Training, testing, predicting and imputing -----------------------------------------------------------------------------------------


## k-Nearest Neighbor Imputation with 'bnstruct' --------------------------------------------------------------------------------------
set.seed(35)

# 2% data sets
tic("kNN 2.1 processing time...")
kNN2.1 <- knn.impute(kNN_MCAR2.1, k = 10) 
toc(log = TRUE)
df_kNN2.1 <- as.data.frame(kNN2.1)
save(df_kNN2.1, file = "kNN21.Rdata")

tic("kNN 2.2 processing time...")
kNN2.2 <- knn.impute(kNN_MCAR2.2, k = 10) 
toc(log = TRUE)
df_kNN2.2 <- as.data.frame(kNN2.2)
save(df_kNN2.2, file = "kNN22.Rdata")

tic("kNN 2.3 processing time...")
kNN2.3 <- knn.impute(kNN_MCAR2.3, k = 10) 
toc(log = TRUE)
df_kNN2.3 <- as.data.frame(kNN2.3)
save(df_kNN2.3, file = "kNN23.Rdata")


# 5% data sets
tic("kNN 5.1 processing time...")
kNN5.1 <- knn.impute(kNN_MCAR5.1, k = 10) 
toc(log = TRUE)
df_kNN5.1 <- as.data.frame(kNN5.1)
save(df_kNN5.1, file = "kNN51.Rdata")

tic("kNN 5.2 processing time...")
kNN5.2 <- knn.impute(kNN_MCAR5.2, k = 10) 
toc(log = TRUE)
df_kNN5.2 <- as.data.frame(kNN5.2)
save(df_kNN5.2, file = "kNN52.Rdata")

tic("kNN 5.3 processing time...")
kNN5.3 <- knn.impute(kNN_MCAR5.3, k = 10) 
toc(log = TRUE)
df_kNN5.3 <- as.data.frame(kNN5.3)
save(df_kNN5.3, file = "kNN53.Rdata")


# 10% data sets 
tic("kNN 10.1 processing time...")
kNN10.1 <- knn.impute(kNN_MCAR10.1, k = 10) 
toc(log = TRUE)
df_kNN10.1 <- as.data.frame(kNN10.1)
save(df_kNN10.1, file = "kNN101.Rdata")

tic("kNN 10.2 processing time...")
kNN10.2 <- knn.impute(kNN_MCAR10.2, k = 10) 
toc(log = TRUE)
df_kNN10.2 <- as.data.frame(kNN10.2)
save(df_kNN10.2, file = "kNN101.Rdata")

tic("kNN 10.3 processing time...")
kNN10.3 <- knn.impute(kNN_MCAR10.3, k = 10) 
toc(log = TRUE)
df_kNN10.3 <- as.data.frame(kNN10.3)
save(df_kNN10.3, file = "kNN103.Rdata")


# Check if all values are imputed 
anyNA(c(df_kNN2.1, df_kNN2.2, df_kNN2.3, df_kNN5.1, df_kNN5.2, 
        df_kNN5.3, df_kNN10.1, df_kNN10.2, df_kNN10.3))


## Computing accuracy ---------------------------------------------------------------------------------------------------------------------------------

# Setting correct and total to '0' 
kNN2.1_correct <- kNN2.2_correct <- kNN2.3_correct <- kNN5.1_correct <- kNN5.2_correct <- kNN5.3_correct <- kNN10.1_correct <- kNN10.2_correct <- kNN10.3_correct <- 0
kNN2.1_total <- kNN2.2_total <- kNN2.3_total <- kNN5.1_total <- kNN5.2_total <- kNN5.3_total <- kNN10.1_total <- kNN10.2_total <- kNN10.3_total <- 0


# Computing the correct imputed values 
kNN2.1_correct <- kNN2.1_correct + sum(ipums == df_kNN2.1)
kNN2.2_correct <- kNN2.2_correct + sum(ipums == df_kNN2.2)
kNN2.3_correct <- kNN2.3_correct + sum(ipums == df_kNN2.3)

kNN5.1_correct <- kNN5.1_correct + sum(ipums == df_kNN5.1)
kNN5.2_correct <- kNN5.2_correct + sum(ipums == df_kNN5.2)
kNN5.3_correct <- kNN5.3_correct + sum(ipums == df_kNN5.3)

kNN10.1_correct <- kNN10.1_correct + sum(ipums == df_kNN10.1)
kNN10.2_correct <- kNN10.2_correct + sum(ipums == df_kNN10.2)
kNN10.3_correct <- kNN10.3_correct + sum(ipums == df_kNN10.3)


# Computing the total values in data set
kNN2.1_total <- kNN2.1_total + sum(!is.na(df_kNN2.1))
kNN2.2_total <- kNN2.2_total + sum(!is.na(df_kNN2.2))
kNN2.3_total <- kNN2.3_total + sum(!is.na(df_kNN2.3))

kNN5.1_total <- kNN5.1_total + sum(!is.na(df_kNN5.1))
kNN5.2_total <- kNN5.2_total + sum(!is.na(df_kNN5.2))
kNN5.3_total <- kNN5.3_total + sum(!is.na(df_kNN5.3))

kNN10.1_total <- kNN10.1_total + sum(!is.na(df_kNN10.1))
kNN10.2_total <- kNN10.2_total + sum(!is.na(df_kNN10.2))
kNN10.3_total <- kNN10.3_total + sum(!is.na(df_kNN10.3))


# Computing the accuracy of imputation 
kNN2.1_accuracy <- kNN2.1_correct / kNN2.1_total
kNN2.2_accuracy <- kNN2.2_correct / kNN2.2_total
kNN2.3_accuracy <- kNN2.3_correct / kNN2.3_total

kNN5.1_accuracy <- kNN5.1_correct / kNN5.1_total
kNN5.2_accuracy <- kNN5.2_correct / kNN5.2_total
kNN5.3_accuracy <- kNN5.3_correct / kNN5.3_total

kNN10.1_accuracy <- kNN10.1_correct / kNN10.1_total
kNN10.2_accuracy <- kNN10.2_correct / kNN10.2_total
kNN10.3_accuracy <- kNN10.3_correct / kNN10.3_total


## Dataframe with accuracy -----------------------------------------------------------------------------------------------------------------------------------------------
kNN_results <- data.frame(Data.set = c("2% - version 1",
                                      "2% - version 2",
                                      "2% - version 3", 
                                      "5% - version 1", 
                                      "5% - version 2", 
                                      "5% - version 3",
                                      "10% - version 1",
                                      "10% - version 2", 
                                      "10% - version 3"),
                         
                         
                         Accuracy = c(kNN2.1_accuracy, 
                                      kNN2.2_accuracy, 
                                      kNN2.3_accuracy, 
                                      kNN5.1_accuracy,
                                      kNN5.2_accuracy,
                                      kNN5.3_accuracy,
                                      kNN10.1_accuracy, 
                                      kNN10.2_accuracy,
                                      kNN10.3_accuracy))  




