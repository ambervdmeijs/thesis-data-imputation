
# Install packages ------------------------------------------------------------------------------------------
install.packages("readxl")
install.packages("utils")

# Loading packages ------------------------------------------------------------------------------------------
library("readxl")
library("utils")

# Read Excel file -------------------------------------------------------------------------------------------
ipumsdeel1 <- read_excel("D:/Amber/Documenten/School/Tilburg University/Master/Thesis/IPUMS2001 deel 1.xlsx")

# Creating missing values -----------------------------------------------------------------------------------
prop.m = 0.05
mcar   = runif(ni*nj, min=0, max=1)
y.mcar = ifelse(mcar<prop.m, NA, y)  
View(ipumsdeel1)