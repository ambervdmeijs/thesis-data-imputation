
## Installing 'readxl' package --------------------------------------------------------------------------------------------------------
install.packages("readxl")
install.packages("imputeR")


## Loading packages --------------------------------------------------------------------------------------------------------------------
library("readxl")
install.packages("imputeR")


## Function for creating probability ---------------------------------------------------------------------------------------------------
SimIm <- function(data, p = 0.1) {
  vec <- c(unlist(data))
  missing <- rbinom(length(vec), 1, p)
  vec[missing == 1] <- NA
  dim(vec) <- dim(data)
  return(vec)
}


## Reading in 'Ipums2001 deel 1' and 'Ipums2001 deel 2' data sets ----------------------------------------------------------------------
ipumsdeel1 <- read_excel("input/IPUMS2001 deel 1.xlsx")
ipumsdeel2 <- read_excel("input/IPUMS2001 deel 2.xlsx")


## Combining the two data sets ---------------------------------------------------------------------------------------------------------
ipums <- rbind(ipumsdeel1, ipumsdeel2)


## Deleting 'nr' column and the 'Gewicht' column ---------------------------------------------------------------------------------------
ipums <- ipums[-1]
ipums$Gewicht <- NULL 


## Creating MCAR data sets with different percentages: 2%, 5% and 10% ------------------------------------------------------------------

# 2% MCAR
MCAR2.1 <- SimIm(ipums, p = 0.02)
MCAR2.2 <- SimIm(ipums, p = 0.02)
MCAR2.3 <- SimIm(ipums, p = 0.02)

# 5% MCAR
MCAR5.1 <- SimIm(ipums, p = 0.05)
MCAR5.2 <- SimIm(ipums, p = 0.05)
MCAR5.3 <- SimIm(ipums, p = 0.05)

# 10% MCAR
MCAR10.1 <- SimIm(ipums, p = 0.10)
MCAR10.2 <- SimIm(ipums, p = 0.10)
MCAR10.3 <- SimIm(ipums, p = 0.10)


## Saving data sets to use in every model/code -----------------------------------------------------------------------
save(MCAR2.1, file = "MCAR2_1.Rdata")
save(MCAR2.2, file = "MCAR2_2.Rdata")
save(MCAR2.3, file = "MCAR2_3.Rdata")

save(MCAR5.1, file = "MCAR5_1.Rdata")
save(MCAR5.2, file = "MCAR5_2.Rdata")
save(MCAR5.3, file = "MCAR5_3.Rdata")

save(MCAR10.1, file = "MCAR10_1.Rdata")
save(MCAR10.2, file = "MCAR10_2.Rdata")
save(MCAR10.3, file = "MCAR10_3.Rdata")