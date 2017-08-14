## Load data -------------------------------------------------------------------
#ufos <- read.csv("D:/Amber/Documenten/School/Tilburg University/Master/Programming with R/ufos.csv")
#vgsales <- read.csv("D:/Amber/Documenten/School/Tilburg University/Master/Programming with R/vgsales.csv")

## Load Library -----------------------------------------------------------------
#library(lubridate)
#library(dplyr)
#library(ggplot2)

## Exercise 0a ------------------------------------------------------------------

ten_ufos <- slice(ufos, 1:10)
answer0a <- ten_ufos


## Exercise 0b ------------------------------------------------------------------

answer0b <- ggplot(ufos, aes(x = shape)) +
  geom_bar()
answer0b


## Exercise 1 -------------------------------------------------------------------

ufo_sight_us <- filter(ufos, country == "us")

ufo_sight_us <- select(datetime, state, shape, duration..seconds.)

right_ufo_sight_us <- names(ufo_sight_us)[6] <- "duration"

answer1 <- ufo_sight_us


## Exercise 2 -------------------------------------------------------------------

ufos_copy <- ufos

levels(ufos_copy$country)

levels(ufos_copy$country) <- c(levels(ufos_copy$country),"is", "in")

ufos_copy$country[297] <- "is"

ufos_copy$country[1185] <- "in"

answer2 <- ufos_copy


## Exercise 3 -------------------------------------------------------------------

##Create an object that summarises, by country, the total UFO sightings of each shape, and the total amount of time
##that shape was seen. Create the object with a meaningful name initially, then copy it into an object called answer3.

ufos <- group_by(ufos, country, shape) 

shape_time_ufo <- summarise(ufos, n())

answer3 <- shape_time_ufo


## Exercise 4 -------------------------------------------------------------------

copy_ufos_realdates <- ufos

copy_ufos_realdates$datetime <- mdy_hm(copy_ufos_realdates$datetime)

copy_ufos_realdates <- mutate(copy_ufos_realdates, day = day(datetime))

copy_ufos_realdates <- mutate(copy_ufos_realdates, month = month(datetime))

copy_ufos_realdates <- mutate(copy_ufos_realdates, year = year(datetime))

answer4 <- copy_ufos_realdatetime


## Exercise 5 -------------------------------------------------------------------

copy_vgsales <- vgsales

copy_vgsales$JP_Proportion <- copy_vgsales$JP_Sales / copy_vgsales$Global_Sales

copy_vgsales <- arrange(copy_vgsales, desc(JP_Proportion))

answer5 <- copy_vgsales


## Exercise 6 -------------------------------------------------------------------

plat_shoot_video <- filter(vgsales, Genre == "Platform" | Genre == "Shooter")

answer6 <- ggplot() +
  geom_point(data = plat_shoot_video, aes(x = EU_Sales, y = JP_Sales, color = Genre)) + 
  scale_x_continuous(name = "European sales (in millions)") + 
  scale_y_continuous(name = "Japanese sales (in millions)")


## Exercise 7 --------------------------------------------------------------------

european_sales_games <- filter(vgsales, Genre == "Adventure" | Genre == "Fighting" | Genre == "Strategy")

answer7 <- ggplot() + 
  geom_boxplot(data = european_sales_games, aes(x = Genre, y = EU_Sales)) + 
  scale_y_continuous(breaks = c(0,1,2,3)) 


## Exercise 8 --------------------------------------------------------------------

vgsales <- group_by(vgsales, Year, Name, Genre)

videos_per_year <- summarise(vgsales, n())

answer8 <- ggplot() + 
  geom_bar(data = videos_per_year, aes(x = Year, color = Genre)) + 
  theme(panel.grid = element_blank())


## Exercise 9 --------------------------------------------------------------------

## plotting per year the total video game sales per genre
ggplot(vgsales, aes(x = Year, y = Global_Sales, color = Genre)) +
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE) + 
  scale_y_continuous(name = "Total Sales (in millions)") + 
  theme(legend.key = element_blank())


## Exercise 10 -------------------------------------------------------------------

# Grouping 'sightings_per_day' by 'datetime'
sightings_per_day <- group_by(ufos, datetime)

# Converting the 'datetime' column into actual date-time objects 
sightings_per_day$datetime <- mdy_hm(sightings_per_day$datetime)

# Extracting the weekday from the date column 
sightings_per_day$weekday <- weekdays(sightings_per_day$datetime)

# Grouping 'sightings_per_day' by 'weekday'
sightings_per_day <- group_by(sightings_per_day, weekday)

# Ordering the day of the week (starting at Monday)
sightings_per_day$weekday <- factor(sightings_per_day$weekday, levels = c("maandag", "dinsdag", 
                                                                          "woensdag", "donderdag", 
                                                                          "vrijdag", "zaterdag", 
                                                                          "zondag"), ordered = TRUE)

# Plotting the number of sights per day 
ggplot(data = sightings_per_day, aes(x = weekday)) +
  geom_bar(fill = "#34925E") + 
  scale_y_continuous(limits = c(0, 15000), name = "UFO sightings per day") +
  scale_x_discrete(labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), 
                              name = "Day of the week")


## Exercise 11 ------------------------------------------------------------------

# Selecting all the necessary countries (and omitting unknown countries)
comments_country <- filter(ufos, country %in% c("au", "ca", "de", "gb", "us"))

# Plotting countries and the length of the comments by using 'nchar()'
ggplot(data = comments_country, aes(x = country, y = nchar(comments))) + 
  geom_boxplot(notch = TRUE, outlier.shape = 9, outlier.size = 3)
 

## Exercise 12 ------------------------------------------------------------------

# Selecting all non-PSP platforms
NonPSP <- filter(vgsales, Platform == "PS" | Platform == "PS2" | Platform == "PS3" | Platform == "PS4") 

# Selecting only the 'Gran Turismo' games in the dataset 'NonPSP'
GT_NonPSP <- grepl("Gran Turismo", NonPSP)

# Ordering the levels of 'Name' column in GT_NonPSP
levels(GT_NonPSP$Name) = c("Gran Turismo", "Gran Turismo 2", "Gran Turismo 3: A-Spec", 
                           "Gran Turismo 4", "Gran Turismo 5", "Gran Turismo 5 Prologue",
                           "Gran Turismo 6"), ordered = TRUE

# Plotting the non-PSP 'Grand Turismo' games and North American Sales
ggplot(data = GT_NonPSP, aes(x = Name, y = NA_Sales)) + 
  geom_bar(stat = "identity") + 
  scale_x_discrete(labels = c("Gran Turismo", "Gran Turismo 2", "Gran Turismo 3: A-Spec", 
                              "Gran Turismo 4", "Gran Turismo 5", "Gran Turismo 5 Prologue",
                              "Gran Turismo 6")) + 
  scale_y_discrete(name = "North American Sales (in millions)") + 
  theme(axis.title.x = element_blank()) 


