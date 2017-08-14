
## Load data -------------------------------------------------------------------
cuisines <- read.csv("D:/Amber/Documenten/School/Tilburg University/Master/Programming with R/cuisines.csv")
parking <- read.csv("D:/Amber/Documenten/School/Tilburg University/Master/Programming with R/parking.csv")
payment <- read.csv("D:/Amber/Documenten/School/Tilburg University/Master/Programming with R/payment.csv")
clean_colleges <- read.csv("D:/Amber/Documenten/School/Tilburg University/Master/Programming with R/cleans.csv")
physicians <- read.csv("D:/Amber/Documenten/School/Tilburg University/Master/Programming with R/PhysiciansPer1000.csv")
Tvs <- read.csv("D:/Amber/Documenten/School/Tilburg University/Master/Programming with R/TVsPer1000.csv")
life_expect <- read.csv("D:/Amber/Documenten/School/Tilburg University/Master/Programming with R/FemaleLifeExpectancy.csv")


## Installing packages
install.packages("readr")
install.packages("ggfortify")
install.packages("caret")
install.packages("tidyr")


## Load Library -----------------------------------------------------------------
library("dplyr")
library("ggplot2")
library("readr")
library("ggfortify")
library("caret")
library("tidyr")


## Question 1 -----------------------------------------------------------------------------------------------


return_index_vector <- function(vector, number) {
  which(vector < number)
}

answer1 <- return_index_vector


## Question 2 ------------------------------------------------------------------------------------------------


my_df <- data.frame(col1 = c("dull", "duck"), col2 = c("stuck", "luck"))

change_characters <- function(dataframe, col_number, character_1, character_2) {
  dataframe[ , col_number] <- gsub(character_1, character_2, dataframe[ , col_number])
  return(dataframe)
  
}

answer2 <- change_characters


## Question 3 ------------------------------------------------------------------------------------------------


df_all_cuisines <- left_join(cuisines, payment, by = "PlaceID") %>%
  left_join(parking, "PlaceID")
answer3a <- df_all_cuisines


all_park_no_payment_restaurant <- inner_join(parking, cuisines, by = "PlaceID") %>%
  anti_join(payment, by = "PlaceID")
answer3b <- all_park_no_payment_restaurant


## Question 4 ------------------------------------------------------------------------------------------------


payment_possible <- select(payment, 2:8) %>%
  summarise_each(funs(sum)) %>%
  gather(Payment_Type, Number, c(American_Express, Debit_Cards, Cash, Diners_Club, Discover, MasterCard, VISA))


answer4 <- ggplot(payment_possible, aes(x = Payment_Type, y = Number)) + 
    geom_bar(stat = "identity")


## Question 5 ------------------------------------------------------------------------------------------------


read_students_lang <- read.csv("D:/Amber/Documenten/School/Tilburg University/Master/Programming with R/students_lang.csv", 
                               sep = ";", header = TRUE, skip = 3, stringsAsFactors = FALSE)

answer5a <- read_students_lang

read_colleges <- read.delim("D:/Amber/Documenten/School/Tilburg University/Master/Programming with R/colleges.txt", 
                          sep = ",", header = FALSE, na.strings = "*", stringsAsFactors = FALSE)

answer5b <- read_colleges


## Question 6 ------------------------------------------------------------------------------------------------


lm_colleges <- lm(GradRate ~ InStateTuition + RoomAndBoard + PctPhDsFac*StudFacRatio, 
                  data = clean_colleges) 

answer6 <-summary(lm_colleges)$r.squared



## Question 7 -------------------------------------------------------------------------------------------------


demo_lm <- lm(ExpenditurePerStud ~ State, data = clean_colleges)

create_plot <- function(linear_model, color_code){
  autoplot(linear_model, which = 1:2, colour = color_code)

}

answer7 <- create_plot


## Question 8 -------------------------------------------------------------------------------------------------


cv_colleges <- select(clean_colleges, ApplicationsReceived, ApplicationsAccepted, StudentsEnrolled, FullTimeUG, 
                      PartTimeUG, InStateTuition, RoomAndBoard, BookCosts, PctPhDsFac, StudFacRatio,
                      ExpenditurePerStud, OutOfStateTuition, State, PubOrPriv)

train_colleges <- train(form = OutOfStateTuition ~ ., data = cv_colleges,
                               method = "lm", trControl = trainControl(method = "cv"), number = 5)

answer8 <- train_colleges


## Question 9 ------------------------------------------------------------------------------------------

## Assembling new data set
educational_new <- full_join(life_expect, Tvs, by = "Country") %>%
  full_join(physicians, by = "Country")

## Checking how the linearity of the models look
lm_TVs <- lm(LifeExpectancy ~ Tvs, data = educational_new)
autoplot(lm_TVs, which = 1:2)

lm_physicians <- lm(LifeExpectancy ~ Physicians, data = educational_new)
autoplot(lm_physicians, which = 1:2)

## Transforming data with log() on dependent variable and checking the linearity with autoplot
lm_TVs_log <- lm(log(LifeExpectancy) ~ TVs, data = educational_new)
autoplot(lm_TVs_log, which = 1:2)

lm_physicians_log <- lm(log(LifeExpectancy) ~ Physicians, data = educational_new)
autoplot(lm_physicians_log, which = 1:2)

## Transforming data with log() on independent variable 
lm_TVs_log <- lm(LifeExpectancy ~ log(TVs), data = educational_new)
autoplot(lm_TVs_log, which = 1:2)

lm_physicians_log <- lm(LifeExpectancy ~ log(Physicians), data = educational_new)
autoplot(lm_physicians_log, which = 1:2)

## Which has the best predictor?

## Plot 
ggplot() +
  geom_point(data = houses, aes(x = Lot.Area, y = SalePrice)) +
  geom_smooth(data = houses, aes(x = Lot.Area, y = SalePrice))

## Is there a difference with 1995?



## Question 10 ------------------------------------------------------------------------------------------





