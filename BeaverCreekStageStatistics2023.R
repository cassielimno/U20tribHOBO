WHITEFISH LAKE TRIBUTARIES STATS SCRIPT
# River Design Group
# Dakota Whitman, 2/21/20221
#edits made to add 2022, 2023 by cassie 11/15/2024
# Set working directory

setwd("C:/Users/User/Dropbox/database/WF WQ DATA/HOBO-U20/RDG Task 4 - Tributary Analysis/Processed HOBO Data/Beaver Creek")

# Install packages, if necessary (if already in library, skip this step)

install.packages('ggplot2')
install.packages('ggthemes', dependencies = TRUE)
install.packages('dplyr')
install.packages("matrixstats")
install.packages('tidyr')
install.packages('zoo')
install.packages('tibble')
install.packages('lubridate')
install.packages('padr')
install.packages('devtools')
devtools::install_github('Mikata-Project/ggthemr')

# Load the libraries from the installed packages

library(ggplot2)
library(ggthemes)
library(plyr)
library(dplyr)
library(matrixStats)
library(tidyr)
library(zoo)
library(tibble)
library(lubridate)
library(padr)
library(ggthemr)

# WHITEFISH LAKE TRIBUTARIES STATS SCRIPT
# River Design Group
# Dakota Whitman, 2/21/20221



# Import the data sets and assign them as variables
Beaver_Creek_2014 <- read_csv("2014/Clipped/Beaver Creek 2014.csv")
Beaver_Creek_2015 <- read_csv("2015/Clipped/Beaver Creek 2015.csv")
Beaver_Creek_2016 <- read_csv("2016/Clipped/Beaver Creek 2016.csv")
Beaver_Creek_2017 <- read_csv("2017/Clipped/Beaver Creek 2017.csv")
Beaver_Creek_2018 <- read_csv("2018/Clipped/Beaver Creek 2018.csv")
Beaver_Creek_2019 <- read_csv("2019/Clipped/Beaver Creek 2019.csv")
Beaver_Creek_2020 <- read_csv("2020/Clipped/Beaver Creek 2020.csv")
Beaver_Creek_2021 <- read_csv("2021/Clipped/Beaver Creek 2021.csv")
Beaver_Creek_2022 <- read_csv("2022/Clipped/Beaver Creek 2022.csv")
Beaver_Creek_2023 <- read_csv("2023/Clipped/Beaver Creek 2023.csv")


#fix 2022 and 2023 quick
Beaver_Creek_2022<- Beaver_Creek_2022 %>% select(TempF, date, PredCFS, WaterDepth) %>% 
  rename(Temp = TempF, Date = date, Discharge = PredCFS, Stage = WaterDepth)

Beaver_Creek_2023<- Beaver_Creek_2023 %>% select(TempF, date, PredCFS, WaterDepth) %>% 
  rename(Temp = TempF, Date = date, Discharge = PredCFS, Stage = WaterDepth)

# Aggregate data from hourly to daily and calculate min, max, and mean

DF_1 <- Beaver_Creek_2014 
DF_1$Date <- as.Date(DF_1$Date, "%m/%d/%Y")
stat <- function(x) c(min = min(x), max = max(x), mean = mean(x))
Beaver_Creek_2014_Daily <- aggregate(Stage ~ Date, DF_1, stat)

DF_2 <- Beaver_Creek_2015 
DF_2$Date <- as.Date(DF_2$Date, "%m/%d/%Y")
stat <- function(x) c(min = min(x), max = max(x), mean = mean(x))
Beaver_Creek_2015_Daily <- aggregate(Stage ~ Date, DF_2, stat)

DF_3 <- Beaver_Creek_2016 
DF_3$Date <- as.Date(DF_3$Date, "%m/%d/%Y")
stat <- function(x) c(min = min(x), max = max(x), mean = mean(x))
Beaver_Creek_2016_Daily <- aggregate(Stage ~ Date, DF_3, stat)

DF_4 <- Beaver_Creek_2017 
DF_4$Date <- as.Date(DF_4$Date, "%m/%d/%Y")
stat <- function(x) c(min = min(x), max = max(x), mean = mean(x))
Beaver_Creek_2017_Daily <- aggregate(Stage ~ Date, DF_4, stat)

DF_5 <- Beaver_Creek_2018 
DF_5$Date <- as.Date(DF_5$Date, "%m/%d/%Y")
stat <- function(x) c(min = min(x), max = max(x), mean = mean(x))
Beaver_Creek_2018_Daily <- aggregate(Stage ~ Date, DF_5, stat)

DF_6 <- Beaver_Creek_2019 
DF_6$Date <- as.Date(DF_6$Date, "%m/%d/%Y")
stat <- function(x) c(min = min(x), max = max(x), mean = mean(x))
Beaver_Creek_2019_Daily <- aggregate(Stage ~ Date, DF_6, stat)

DF_7 <- Beaver_Creek_2020 
DF_7$Date <- as.Date(DF_7$Date, "%m/%d/%Y")
stat <- function(x) c(min = min(x), max = max(x), mean = mean(x))
Beaver_Creek_2020_Daily <- aggregate(Stage ~ Date, DF_7, stat)

DF_8 <- Beaver_Creek_2021 
DF_8$Date <- as.Date(DF_8$Date, "%m/%d/%Y")
stat <- function(x) c(min = min(x), max = max(x), mean = mean(x))
Beaver_Creek_2021_Daily <- aggregate(Stage ~ Date, DF_8, stat)

DF_9 <- Beaver_Creek_2022 
DF_9$Date <- as.Date(DF_9$Date, "%m/%d/%Y")
stat <- function(x) c(min = min(x), max = max(x), mean = mean(x))
Beaver_Creek_2022_Daily <- aggregate(Stage ~ Date, DF_9, stat)

DF_10 <- Beaver_Creek_2023 
DF_10$Date <- as.Date(DF_10$Date, "%m/%d/%Y")
stat <- function(x) c(min = min(x), max = max(x), mean = mean(x))
Beaver_Creek_2023_Daily <- aggregate(Stage ~ Date, DF_10, stat)

# Mutate the aggregated data into independent columns, eliminate NA values, and calculate min, max, mean, and std. dev for Stage.

Beaver_Creek_2014_Mutate <- 
  Beaver_Creek_2014_Daily %>% group_by(Date) %>% 
  mutate(minStage = min(Stage), maxStage = max(Stage), meanStage = mean(Stage), stdev=sd(Stage), na.rm = TRUE)
Beaver_Creek_2014_Mutate <- Beaver_Creek_2014_Mutate %>%
  mutate_all(~replace(., . == 0, NA))

Beaver_Creek_2015_Mutate <- 
  Beaver_Creek_2015_Daily %>% group_by(Date) %>% 
  mutate(minStage = min(Stage), maxStage = max(Stage), meanStage = mean(Stage), stdev=sd(Stage), na.rm = TRUE)
Beaver_Creek_2015_Mutate <- Beaver_Creek_2015_Mutate %>%
  mutate_all(~replace(., . == 0, NA))

Beaver_Creek_2016_Mutate <- 
  Beaver_Creek_2016_Daily %>% group_by(Date) %>% 
  mutate(minStage = min(Stage), maxStage = max(Stage), meanStage = mean(Stage), stdev=sd(Stage), na.rm = TRUE)
Beaver_Creek_2016_Mutate <- Beaver_Creek_2016_Mutate %>%
  mutate_all(~replace(., . == 0, NA))

Beaver_Creek_2017_Mutate <- 
  Beaver_Creek_2017_Daily %>% group_by(Date) %>% 
  mutate(minStage = min(Stage), maxStage = max(Stage), meanStage = mean(Stage), stdev=sd(Stage), na.rm = TRUE)
Beaver_Creek_2017_Mutate <- Beaver_Creek_2017_Mutate %>%
  mutate_all(~replace(., . == 0, NA))

Beaver_Creek_2018_Mutate <- 
  Beaver_Creek_2018_Daily %>% group_by(Date) %>% 
  mutate(minStage = min(Stage), maxStage = max(Stage), meanStage = mean(Stage), stdev=sd(Stage), na.rm = TRUE)
Beaver_Creek_2018_Mutate <- Beaver_Creek_2018_Mutate %>%
  mutate_all(~replace(., . == 0, NA))

Beaver_Creek_2019_Mutate <- 
  Beaver_Creek_2019_Daily %>% group_by(Date) %>% 
  mutate(minStage = min(Stage), maxStage = max(Stage), meanStage = mean(Stage), stdev=sd(Stage), na.rm = TRUE)
Beaver_Creek_2019_Mutate <- Beaver_Creek_2019_Mutate %>%
  mutate_all(~replace(., . == 0, NA))

Beaver_Creek_2020_Mutate <- 
  Beaver_Creek_2020_Daily %>% group_by(Date) %>% 
  mutate(minStage = min(Stage), maxStage = max(Stage), meanStage = mean(Stage), stdev=sd(Stage), na.rm = TRUE)
Beaver_Creek_2020_Mutate <- Beaver_Creek_2020_Mutate %>%
  mutate_all(~replace(., . == 0, NA))

Beaver_Creek_2021_Mutate <- 
  Beaver_Creek_2021_Daily %>% group_by(Date) %>% 
  mutate(minStage = min(Stage), maxStage = max(Stage), meanStage = mean(Stage), stdev=sd(Stage), na.rm = TRUE)
Beaver_Creek_2021_Mutate <- Beaver_Creek_2021_Mutate %>%
  mutate_all(~replace(., . == 0, NA))

Beaver_Creek_2022_Mutate <- 
  Beaver_Creek_2022_Daily %>% group_by(Date) %>% 
  mutate(minStage = min(Stage), maxStage = max(Stage), meanStage = mean(Stage), stdev=sd(Stage), na.rm = TRUE)
Beaver_Creek_2022_Mutate <- Beaver_Creek_2022_Mutate %>%
  mutate_all(~replace(., . == 0, NA))

Beaver_Creek_2023_Mutate <- 
  Beaver_Creek_2023_Daily %>% group_by(Date) %>% 
  mutate(minStage = min(Stage), maxStage = max(Stage), meanStage = mean(Stage), stdev=sd(Stage), na.rm = TRUE)
Beaver_Creek_2023_Mutate <- Beaver_Creek_2023_Mutate %>%
  mutate_all(~replace(., . == 0, NA))

# Ensure that 'Date' columns are in 'Date' format

class(Beaver_Creek_2014_Mutate$Date)
class(Beaver_Creek_2015_Mutate$Date)
class(Beaver_Creek_2016_Mutate$Date)
class(Beaver_Creek_2017_Mutate$Date)
class(Beaver_Creek_2018_Mutate$Date)
class(Beaver_Creek_2019_Mutate$Date)
class(Beaver_Creek_2020_Mutate$Date)
class(Beaver_Creek_2021_Mutate$Date)
class(Beaver_Creek_2022_Mutate$Date)
class(Beaver_Creek_2023_Mutate$Date)

# Create new data frames for each year 

Day_Of_Year_2014 <- c('')
Beaver_Creek_Dates_2014 <- data.frame(Day_Of_Year_2014)

Day_Of_Year_2015 <- c('')
Beaver_Creek_Dates_2015 <- data.frame(Day_Of_Year_2015)

Day_Of_Year_2016 <- c('')
Beaver_Creek_Dates_2016 <- data.frame(Day_Of_Year_2016)

Day_Of_Year_2017 <- c('')
Beaver_Creek_Dates_2017 <- data.frame(Day_Of_Year_2017)

Day_Of_Year_2018 <- c('')
Beaver_Creek_Dates_2018 <- data.frame(Day_Of_Year_2018)

Day_Of_Year_2019 <- c('')
Beaver_Creek_Dates_2019 <- data.frame(Day_Of_Year_2019)

Day_Of_Year_2020 <- c('')
Beaver_Creek_Dates_2020 <- data.frame(Day_Of_Year_2020)

Day_Of_Year_2021 <- c('')
Beaver_Creek_Dates_2021 <- data.frame(Day_Of_Year_2021)

Day_Of_Year_2022 <- c('')
Beaver_Creek_Dates_2022 <- data.frame(Day_Of_Year_2022)

Day_Of_Year_2023 <- c('')
Beaver_Creek_Dates_2023 <- data.frame(Day_Of_Year_2023)

# Add rows to data frame

Beaver_Creek_Dates_2014[nrow(Beaver_Creek_Dates_2014) + 364,] = 
  c(Day_Of_Year_2014)

Beaver_Creek_Dates_2015[nrow(Beaver_Creek_Dates_2015) + 364,] = 
  c(Day_Of_Year_2015)

Beaver_Creek_Dates_2016[nrow(Beaver_Creek_Dates_2016) + 365,] = 
  c(Day_Of_Year_2016)

Beaver_Creek_Dates_2017[nrow(Beaver_Creek_Dates_2017) + 364,] = 
  c(Day_Of_Year_2017)

Beaver_Creek_Dates_2018[nrow(Beaver_Creek_Dates_2018) + 364,] = 
  c(Day_Of_Year_2018)

Beaver_Creek_Dates_2019[nrow(Beaver_Creek_Dates_2019) + 364,] = 
  c(Day_Of_Year_2019)

Beaver_Creek_Dates_2020[nrow(Beaver_Creek_Dates_2020) + 365,] = 
  c(Day_Of_Year_2020)

Beaver_Creek_Dates_2021[nrow(Beaver_Creek_Dates_2021) + 364,] = 
  c(Day_Of_Year_2021)

Beaver_Creek_Dates_2022[nrow(Beaver_Creek_Dates_2022) + 364,] = 
  c(Day_Of_Year_2022)

Beaver_Creek_Dates_2023[nrow(Beaver_Creek_Dates_2023) + 364,] = 
  c(Day_Of_Year_2023)

# Fill date column with days 

Beaver_Creek_Dates_2014$Day_Of_Year_2014 <- seq(from=as.Date("2014-01-01"), to=as.Date("2014-12-31"), by = "day")
Beaver_Creek_Dates_2015$Day_Of_Year_2015 <- seq(from=as.Date("2015-01-01"), to=as.Date("2015-12-31"), by = "day")
Beaver_Creek_Dates_2016$Day_Of_Year_2016 <- seq(from=as.Date("2016-01-01"), to=as.Date("2016-12-31"), by = "day")
Beaver_Creek_Dates_2017$Day_Of_Year_2017 <- seq(from=as.Date("2017-01-01"), to=as.Date("2017-12-31"), by = "day")
Beaver_Creek_Dates_2018$Day_Of_Year_2018 <- seq(from=as.Date("2018-01-01"), to=as.Date("2018-12-31"), by = "day")
Beaver_Creek_Dates_2019$Day_Of_Year_2019 <- seq(from=as.Date("2019-01-01"), to=as.Date("2019-12-31"), by = "day")
Beaver_Creek_Dates_2020$Day_Of_Year_2020 <- seq(from=as.Date("2020-01-01"), to=as.Date("2020-12-31"), by = "day")
Beaver_Creek_Dates_2021$Day_Of_Year_2021 <- seq(from=as.Date("2021-01-01"), to=as.Date("2021-12-31"), by = "day")
Beaver_Creek_Dates_2022$Day_Of_Year_2022 <- seq(from=as.Date("2022-01-01"), to=as.Date("2022-12-31"), by = "day")
Beaver_Creek_Dates_2023$Day_Of_Year_2023 <- seq(from=as.Date("2023-01-01"), to=as.Date("2023-12-31"), by = "day")


# Join date columns 

Beaver_Creek_2014_Join <- left_join(Beaver_Creek_Dates_2014, Beaver_Creek_2014_Mutate, by = c("Day_Of_Year_2014" = "Date"))
Beaver_Creek_2014_Join$Stage <- NULL
Beaver_Creek_2014_Join$na.rm <- NULL

Beaver_Creek_2015_Join <- left_join(Beaver_Creek_Dates_2015, Beaver_Creek_2015_Mutate, by = c("Day_Of_Year_2015" = "Date"))
Beaver_Creek_2015_Join$Stage <- NULL
Beaver_Creek_2015_Join$na.rm <- NULL

Beaver_Creek_2016_Join <- left_join(Beaver_Creek_Dates_2016, Beaver_Creek_2016_Mutate, by = c("Day_Of_Year_2016" = "Date"))
Beaver_Creek_2016_Join$Stage <- NULL
Beaver_Creek_2016_Join$na.rm <- NULL

Beaver_Creek_2017_Join <- left_join(Beaver_Creek_Dates_2017, Beaver_Creek_2017_Mutate, by = c("Day_Of_Year_2017" = "Date"))
Beaver_Creek_2017_Join$Stage <- NULL
Beaver_Creek_2017_Join$na.rm <- NULL

Beaver_Creek_2018_Join <- left_join(Beaver_Creek_Dates_2018, Beaver_Creek_2018_Mutate, by = c("Day_Of_Year_2018" = "Date"))
Beaver_Creek_2018_Join$Stage <- NULL
Beaver_Creek_2018_Join$na.rm <- NULL

Beaver_Creek_2019_Join <- left_join(Beaver_Creek_Dates_2019, Beaver_Creek_2019_Mutate, by = c("Day_Of_Year_2019" = "Date"))
Beaver_Creek_2019_Join$Stage <- NULL
Beaver_Creek_2019_Join$na.rm <- NULL

Beaver_Creek_2020_Join <- left_join(Beaver_Creek_Dates_2020, Beaver_Creek_2020_Mutate, by = c("Day_Of_Year_2020" = "Date"))
Beaver_Creek_2020_Join$Stage <- NULL
Beaver_Creek_2020_Join$na.rm <- NULL

Beaver_Creek_2021_Join <- left_join(Beaver_Creek_Dates_2021, Beaver_Creek_2021_Mutate, by = c("Day_Of_Year_2021" = "Date"))
Beaver_Creek_2021_Join$Stage <- NULL
Beaver_Creek_2021_Join$na.rm <- NULL

Beaver_Creek_2022_Join <- left_join(Beaver_Creek_Dates_2022, Beaver_Creek_2022_Mutate, by = c("Day_Of_Year_2022" = "Date"))
Beaver_Creek_2022_Join$Stage <- NULL
Beaver_Creek_2022_Join$na.rm <- NULL

Beaver_Creek_2023_Join <- left_join(Beaver_Creek_Dates_2023, Beaver_Creek_2023_Mutate, by = c("Day_Of_Year_2023" = "Date"))
Beaver_Creek_2023_Join$Stage <- NULL
Beaver_Creek_2023_Join$na.rm <- NULL

# Delete leap years from 2016, 2020

Beaver_Creek_2016_Join = Beaver_Creek_2016_Join[-60,]
Beaver_Creek_2020_Join = Beaver_Creek_2020_Join[-60,]

# Multiply the standard deviation by 2 to get 95% C.I.

Beaver_Creek_2014_Join$"95 C.I." <- Beaver_Creek_2014_Join$stdev*2
Beaver_Creek_2015_Join$"95 C.I." <- Beaver_Creek_2015_Join$stdev*2
Beaver_Creek_2016_Join$"95 C.I." <- Beaver_Creek_2016_Join$stdev*2
Beaver_Creek_2017_Join$"95 C.I." <- Beaver_Creek_2017_Join$stdev*2
Beaver_Creek_2018_Join$"95 C.I." <- Beaver_Creek_2018_Join$stdev*2
Beaver_Creek_2019_Join$"95 C.I." <- Beaver_Creek_2019_Join$stdev*2
Beaver_Creek_2020_Join$"95 C.I." <- Beaver_Creek_2020_Join$stdev*2
Beaver_Creek_2021_Join$"95 C.I." <- Beaver_Creek_2021_Join$stdev*2
Beaver_Creek_2022_Join$"95 C.I." <- Beaver_Creek_2022_Join$stdev*2
Beaver_Creek_2023_Join$"95 C.I." <- Beaver_Creek_2023_Join$stdev*2
# Create columns for master data frame

Date <- c('')
min_2014 <- c('')
max_2014 <- c('')
mean_2014 <- c('')
min_2015 <- c('')
max_2015 <- c('')
mean_2015 <- c('')
min_2016 <- c('')
max_2016 <- c('')
mean_2016 <- c('')
min_2017 <- c('')
max_2017 <- c('')
mean_2017 <- c('')
min_2018 <- c('')
max_2018 <- c('')
mean_2018 <- c('')
min_2019 <- c('')
max_2019 <- c('')
mean_2019 <- c('')
min_2020 <- c('')
max_2020 <- c('')
mean_2020 <- c('')
min_2021 <- c('')
max_2021 <- c('')
mean_2021 <- c('')
min_2022 <- c('')
max_2022 <- c('')
mean_2022 <- c('')
min_2023 <- c('')
max_2023 <- c('')
mean_2023 <- c('')
overall_min <- c('')
overall_max <- c('')
overall_mean <- c('')
stdev <- c('')
Conf_int <- c('')
Upper_bound <- c('')
Lower_bound <- c('')

# Create master data frame with created columns from above

Beaver_Creek_Master <- data.frame(Date, min_2014, max_2014, mean_2014, min_2015, max_2015, mean_2015, min_2016, max_2016, mean_2016, 
                                  min_2017, max_2017, mean_2017, min_2018, max_2018, mean_2018, min_2019, max_2019, mean_2019, min_2020, 
                                  max_2020, mean_2020, min_2021, max_2021, mean_2021, min_2022, max_2022, mean_2022, min_2023, max_2023, mean_2023, 
                                  overall_min, overall_max, overall_mean, stdev, Conf_int, 
                                  Upper_bound, Lower_bound)

# Add 365 rows to master data frame

Beaver_Creek_Master[nrow(Beaver_Creek_Master) + 364,] = 
  c(Date, min_2014, max_2014, mean_2014, min_2015, max_2015, mean_2015, min_2016, max_2016, mean_2016, 
    min_2017, max_2017, mean_2017, min_2018, max_2018, mean_2018, min_2019, max_2019, mean_2019, min_2020, 
    max_2020, mean_2020, min_2021, max_2021, mean_2021, min_2022, max_2022, mean_2022, min_2023, max_2023,
    mean_2023, overall_min, overall_max, overall_mean, stdev, Conf_int, 
    Upper_bound, Lower_bound)

# Populate master data frame with dates

Beaver_Creek_Master$Date <- seq(from=as.Date("2001-01-01"), to=as.Date("2001-12-31"), by = "day")

# Copy daily values from columns in each year and paste them into master frame

# 2014 Data
Beaver_Creek_Master$min_2014 <- Beaver_Creek_2014_Join$minStage 
Beaver_Creek_Master$max_2014 <- Beaver_Creek_2014_Join$maxStage
Beaver_Creek_Master$mean_2014 <- Beaver_Creek_2014_Join$meanStage

# 2015 Data
Beaver_Creek_Master$min_2015 <- Beaver_Creek_2015_Join$minStage 
Beaver_Creek_Master$max_2015 <- Beaver_Creek_2015_Join$maxStage
Beaver_Creek_Master$mean_2015 <- Beaver_Creek_2015_Join$meanStage

# 2016 Data
Beaver_Creek_Master$min_2016 <- Beaver_Creek_2016_Join$minStage 
Beaver_Creek_Master$max_2016 <- Beaver_Creek_2016_Join$maxStage
Beaver_Creek_Master$mean_2016 <- Beaver_Creek_2016_Join$meanStage

# 2017 Data
Beaver_Creek_Master$min_2017 <- Beaver_Creek_2017_Join$minStage 
Beaver_Creek_Master$max_2017 <- Beaver_Creek_2017_Join$maxStage
Beaver_Creek_Master$mean_2017 <- Beaver_Creek_2017_Join$meanStage

# 2018 Data
Beaver_Creek_Master$min_2018 <- Beaver_Creek_2018_Join$minStage 
Beaver_Creek_Master$max_2018 <- Beaver_Creek_2018_Join$maxStage
Beaver_Creek_Master$mean_2018 <- Beaver_Creek_2018_Join$meanStage

# 2019 Data
Beaver_Creek_Master$min_2019 <- Beaver_Creek_2019_Join$minStage 
Beaver_Creek_Master$max_2019 <- Beaver_Creek_2019_Join$maxStage
Beaver_Creek_Master$mean_2019 <- Beaver_Creek_2019_Join$meanStage

# 2020 Data
Beaver_Creek_Master$min_2020 <- Beaver_Creek_2020_Join$minStage 
Beaver_Creek_Master$max_2020 <- Beaver_Creek_2020_Join$maxStage
Beaver_Creek_Master$mean_2020 <- Beaver_Creek_2020_Join$meanStage

# 2021 Data
Beaver_Creek_Master$min_2021 <- Beaver_Creek_2021_Join$minStage 
Beaver_Creek_Master$max_2021 <- Beaver_Creek_2021_Join$maxStage
Beaver_Creek_Master$mean_2021 <- Beaver_Creek_2021_Join$meanStage

# 2022 Data
Beaver_Creek_Master$min_2022 <- Beaver_Creek_2022_Join$minStage 
Beaver_Creek_Master$max_2022 <- Beaver_Creek_2022_Join$maxStage
Beaver_Creek_Master$mean_2022 <- Beaver_Creek_2022_Join$meanStage

# 2023 Data
Beaver_Creek_Master$min_2023 <- Beaver_Creek_2023_Join$minStage 
Beaver_Creek_Master$max_2023 <- Beaver_Creek_2023_Join$maxStage
Beaver_Creek_Master$mean_2023 <- Beaver_Creek_2023_Join$meanStage


# Calculate the overall minimums, maximums, means, and std.dev (aka, "the mins of the mins, the max of the maxes, and the mean of the means")

Beaver_Creek_Master$overall_min <- apply(Beaver_Creek_Master[, 2:length(Beaver_Creek_Master)], 1, FUN=min, na.rm = TRUE)
Beaver_Creek_Master$overall_min <- as.numeric(as.character(Beaver_Creek_Master$overall_min))
Beaver_Creek_Master$overall_max <- apply(Beaver_Creek_Master[, 2:length(Beaver_Creek_Master)], 1, FUN=max, na.rm = TRUE)
Beaver_Creek_Master$overall_max <- as.numeric(as.character(Beaver_Creek_Master$overall_max))
Beaver_Creek_Master$overall_mean <- rowMeans(Beaver_Creek_Master[ , c(4,7,10,13,16,19,22,25)], na.rm=TRUE)
Beaver_Creek_Master$stdev = apply(Beaver_Creek_Master[,c(4,7,10,13,16,19,22,25)], 1, sd, na.rm=TRUE)

# Calculate 95% confidence interval, upper bound, and lower bound

Beaver_Creek_Master$Conf_int <- Beaver_Creek_Master$stdev*2
Beaver_Creek_Master$Upper_bound <- Beaver_Creek_Master$Conf_int + Beaver_Creek_Master$overall_mean
Beaver_Creek_Master$Lower_bound <- Beaver_Creek_Master$overall_mean - Beaver_Creek_Master$Conf_int

# Convert columns to numeric

Beaver_Creek_Master$stdev <- as.numeric(as.character(Beaver_Creek_Master$stdev))
Beaver_Creek_Master$Conf_int <- as.numeric(as.character(Beaver_Creek_Master$Conf_int))
Beaver_Creek_Master$Upper_bound <- as.numeric(as.character(Beaver_Creek_Master$Upper_bound))
Beaver_Creek_Master$Lower_bound <- as.numeric(as.character(Beaver_Creek_Master$Lower_bound))

# Convert negative values to zero

Beaver_Creek_Master[Beaver_Creek_Master < 0] <- 0 

# Plot mean Stage data with 95% confidence interval ribbon

Stage_plot <- ggplot(Beaver_Creek_Master, aes(x=Date, y=overall_mean)) + 
  geom_line(size=1, color='gray8', pch=20) + 
  geom_ribbon(aes(ymin=`Lower_bound`, ymax=`Upper_bound`), linetype=2, alpha=0.3, fill= "darkolivegreen4") +
  scale_colour_manual("",values="blue") + scale_fill_manual("",values="grey12") + 
  scale_y_continuous(breaks = seq(0, 2, by = 0.5)) +
  theme_grey() + xlab("Date") + ylab("Stage (ft)") + 
  ggtitle("Beaver Creek")

Stage_plot + scale_x_date(date_labels = "%b", date_breaks = "1 month")
