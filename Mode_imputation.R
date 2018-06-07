
# Installing packages -----------------------------------------------------------------------------------------------------------------
install.packages("ForImp")
install.packages("devtools")
install.packages("readxl")

# Loading packages --------------------------------------------------------------------------------------------------------------------
library("ForImp")
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
MO_MCAR2.1 <- data.frame(MCAR2.1)
MO_MCAR2.2 <- data.frame(MCAR2.2)
MO_MCAR2.3 <- data.frame(MCAR2.3)
MO_MCAR5.1 <- data.frame(MCAR5.1)
MO_MCAR5.2 <- data.frame(MCAR5.2)
MO_MCAR5.3 <- data.frame(MCAR5.3)
MO_MCAR10.1 <- data.frame(MCAR10.1)
MO_MCAR10.2 <- data.frame(MCAR10.2)
MO_MCAR10.3 <- data.frame(MCAR10.3)


## Giving variables original names and making variables readable -----------------------------------------------------------------------
names(MO_MCAR2.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(MO_MCAR2.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(MO_MCAR2.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(MO_MCAR5.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(MO_MCAR5.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(MO_MCAR5.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(MO_MCAR10.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(MO_MCAR10.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(MO_MCAR10.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)


# Training, testing, predicting and imputing -----------------------------------------------------------------------------------------


## Mode Imputation with 'ForImp' --------------------------------------------------------------------------------------------------!!!

# 2% data sets
tic("Mode Imputation 2.1 processing time...")
mode_imputation2.1 <- modeimp(MO_MCAR2.1)
toc(log = TRUE)
df_mode_imputation2.1 <- as.data.frame(mode_imputation2.1)
save(df_mode_imputation2.1, file = "mode_imputation21.Rdata")

tic("Mode Imputation 2.2 processing time...")
mode_imputation2.2 <- modeimp(MO_MCAR2.2)
toc(log = TRUE)
df_mode_imputation2.2 <- as.data.frame(mode_imputation2.2)
save(df_mode_imputation2.2, file = "mode_imputation22.Rdata")

tic("Mode Imputation 2.3 processing time...")
mode_imputation2.3 <- modeimp(MO_MCAR2.3)
toc(log = TRUE)
df_mode_imputation2.3 <- as.data.frame(mode_imputation2.3)
save(df_mode_imputation2.3, file = "mode_imputation23.Rdata")


# 5% data sets
tic("Mode Imputation 5.1 processing time...")
mode_imputation5.1 <- modeimp(MO_MCAR5.1)
toc(log = TRUE)
df_mode_imputation5.1 <- as.data.frame(mode_imputation5.1)
save(df_mode_imputation5.1, file = "mode_imputation51.Rdata")

tic("Mode Imputation 5.2 processing time...")
mode_imputation5.2 <- modeimp(MO_MCAR5.2)
toc(log = TRUE)
df_mode_imputation5.2 <- as.data.frame(mode_imputation5.2)
save(df_mode_imputation5.2, file = "mode_imputation52.Rdata")

tic("Mode Imputation 5.3 processing time...")
mode_imputation5.3 <- modeimp(MO_MCAR5.3)
toc(log = TRUE)
df_mode_imputation5.3 <- as.data.frame(mode_imputation5.3)
save(df_mode_imputation5.3, file = "mode_imputation53.Rdata")


# 10% data sets 
tic("Mode Imputation 10.1 processing time...")
mode_imputation10.1 <- modeimp(MO_MCAR10.1)
toc(log = TRUE)
df_mode_imputation10.1 <- as.data.frame(mode_imputation10.1)
save(df_mode_imputation10.1, file = "mode_imputation101.Rdata")

tic("Mode Imputation 10.2 processing time...")
mode_imputation10.2 <- modeimp(MO_MCAR10.2)
toc(log = TRUE)
df_mode_imputation10.2 <- as.data.frame(mode_imputation10.2)
save(df_mode_imputation10.2, file = "mode_imputation102.Rdata")

tic("Mode Imputation 10.3 processing time...")
mode_imputation10.3 <- modeimp(MO_MCAR10.3)
toc(log = TRUE)
df_mode_imputation10.3 <- as.data.frame(mode_imputation10.3)
save(df_mode_imputation10.3, file = "mode_imputation103.Rdata")


# Check if all values are imputed 
anyNA(c(df_mode_imputation2.1, df_mode_imputation2.2, df_mode_imputation2.3, df_mode_imputation5.1, df_mode_imputation5.2, 
        df_mode_imputation5.3, df_mode_imputation10.1, df_mode_imputation10.2, df_mode_imputation10.3))


## Computing accuracy ---------------------------------------------------------------------------------------------------------------------------------

# Setting correct and total to '0' 
MO2.1_correct <- MO2.2_correct <- MO2.3_correct <- MO5.1_correct <- MO5.2_correct <- MO5.3_correct <- MO10.1_correct <- MO10.2_correct <- MO10.3_correct <- 0
MO2.1_total <- MO2.2_total <- MO2.3_total <- MO5.1_total <- MO5.2_total <- MO5.3_total <- MO10.1_total <- MO10.2_total <- MO10.3_total <- 0


# Computing the correct imputed values 
MO2.1_correct <- MO2.1_correct + sum(ipums == df_mode_imputation2.1)
MO2.2_correct <- MO2.2_correct + sum(ipums == df_mode_imputation2.2)
MO2.3_correct <- MO2.3_correct + sum(ipums == df_mode_imputation2.3)

MO5.1_correct <- MO5.1_correct + sum(ipums == df_mode_imputation5.1)
MO5.2_correct <- MO5.2_correct + sum(ipums == df_mode_imputation5.2)
MO5.3_correct <- MO5.3_correct + sum(ipums == df_mode_imputation5.3)

MO10.1_correct <- MO10.1_correct + sum(ipums == df_mode_imputation10.1)
MO10.2_correct <- MO10.2_correct + sum(ipums == df_mode_imputation10.2)
MO10.3_correct <- MO10.3_correct + sum(ipums == df_mode_imputation10.3)


# Computing the total values in data set
MO2.1_total <- MO2.1_total + sum(!is.na(df_mode_imputation2.1))
MO2.2_total <- MO2.2_total + sum(!is.na(df_mode_imputation2.2))
MO2.3_total <- MO2.3_total + sum(!is.na(df_mode_imputation2.3))

MO5.1_total <- MO5.1_total + sum(!is.na(df_mode_imputation5.1))
MO5.2_total <- MO5.2_total + sum(!is.na(df_mode_imputation5.2))
MO5.3_total <- MO5.3_total + sum(!is.na(df_mode_imputation5.3))

MO10.1_total <- MO10.1_total + sum(!is.na(df_mode_imputation10.1))
MO10.2_total <- MO10.2_total + sum(!is.na(df_mode_imputation10.2))
MO10.3_total <- MO10.3_total + sum(!is.na(df_mode_imputation10.3))


# Computing the accuracy of imputation 
MO2.1_accuracy <- MO2.1_correct / MO2.1_total
MO2.2_accuracy <- MO2.2_correct / MO2.2_total
MO2.3_accuracy <- MO2.3_correct / MO2.3_total

MO5.1_accuracy <- MO5.1_correct / MO5.1_total
MO5.2_accuracy <- MO5.2_correct / MO5.2_total
MO5.3_accuracy <- MO5.3_correct / MO5.3_total

MO10.1_accuracy <- MO10.1_correct / MO10.1_total
MO10.2_accuracy <- MO10.2_correct / MO10.2_total
MO10.3_accuracy <- MO10.3_correct / MO10.3_total


## Dataframe with accuracy -----------------------------------------------------------------------------------------------------------------------------------------------
MO_results <- data.frame(Data.set = c("2% - version 1",
                                      "2% - version 2",
                                      "2% - version 3", 
                                      "5% - version 1", 
                                      "5% - version 2", 
                                      "5% - version 3",
                                      "10% - version 1",
                                      "10% - version 2", 
                                      "10% - version 3"),
                         
                         
                         Accuracy = c(MO2.1_accuracy, 
                                      MO2.2_accuracy, 
                                      MO2.3_accuracy, 
                                      MO5.1_accuracy,
                                      MO5.2_accuracy,
                                      MO5.3_accuracy,
                                      MO10.1_accuracy, 
                                      MO10.2_accuracy,
                                      MO10.3_accuracy))  



# Computing F1-score
install.packages("MLmetrics")
library("MLmetrics")

F1_Score(MCAR2.1, ipums, positive = NULL)





