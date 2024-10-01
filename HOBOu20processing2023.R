#starting to analize hobo trib data for 2023

#trying to process and graph stream data 2022-2023
#load working direc
setwd("C:/Users/User/Dropbox/database/WF WQ DATA/HOBO-U20/U-20CSVs2022-2023/WithDepth")
#load packages
library(tidyverse)
library(dplyr)
library(readr)
library(janitor)
library(lubridate)
library(ggplot2)


#try with one first
#lets start with lazy
lazy<-read.csv("Lazy_Creek_10_4_22_11_14_23_depth.csv", skip = 1)



#cut to needed cols
lazy<-lazy[,2:7]


#rename cols to usable names
colnames(lazy)<-c( "date", "time", "AbsPresPsi", "TempF",
                      "AbsPresBaromPsi", "WaterDepth" )


#graph this
ggplot(data = lazy)+
  geom_point(aes(x = date, y = AbsPresBaromPsi))+
  geom_point(aes(x = date, y = WaterDepth), color = "blue")+
  geom_point(aes(x = date, y = AbsPresPsi), color = "darkgreen")
  geom_point(aes(x = date, y = ))
  #geom_point(aes(x = date, y = TempF), color = "darkred")

#now add the equation to see how it compares
#make it its own col
#upload 

setwd("C:/Users/User/Dropbox/database/WF WQ DATA/HOBO-U20/U-20CSVs2022-2023")
  
lazy2<-read.csv("Lazy_Creek_10_4_22_11_14_23.csv", skip = 1)

#cut to needed cols
lazy2<-lazy2[,2:7]


#rename cols to usable names
colnames(lazy2)<-c( "date", "time", "AbsPresPsi", "TempF",
                   "AbsPresBaromPsi", "SensorDepth" )
  
#combine if you can

lazy<- lazy %>% mutate(SensorDepth = lazy2$SensorDepth, PredDepth = 5.703*(SensorDepth)^1.9181)

# the raw equations y = 5.703x^1.9181

#graph
ggplot(data = lazy)+
  geom_point(aes(x = date, y = WaterDepth), color = "blue")+
geom_point(aes(x = date, y = PredDepth), color = "darkred")


#can this be put in a loop for all of them????

#this is the working code!!! ####

#make a function that creates a col based on csv name
read_csv_filename <- function(filename){
  ret <- read.csv(filename, skip = 1)#note that for this to work on these files skip is needed
  ret$Source <- filename #this determines name of column
  ret
}

#uploading files as a group
setwd("C:/Users/User/Dropbox/database/WF WQ DATA/HOBO-U20/U-20CSVs2022-2023/WithDepth")
temp = list.files(pattern = "\\.csv$")

#empty dataframe for loop
hobo2223<-data.frame()

#for loop to process files
for (i in 1:length(temp)){
  #read in csv file and create a col with file name 
  file.i<-data.frame(read_csv_filename(temp[i]))
  
  #print so I know what its working on
  print(unique(file.i$Source))
  
  
  #cut file
  file.i<-select(file.i, 2:7, "Source")
  
  #rename cols to usable names
  colnames(file.i)<-c( "date", "time", "AbsPresPsi", "TempF",
                       "AbsPresBaromPsi", "WaterDepth", "Source" )
  
  #combine all files together into dataframe
  hobo2223<-rbind(file.i, hobo2223)
  
}


#make sure they all read in properly
unique(hobo2223$Source)

#now change source to station id ####
hobo2223.2<- hobo2223 %>%  mutate(Station_ID = case_when(grepl("Whitefish", Source) ~ "WF-R-SPB",
                                                         grepl("WF", Source) ~ "WF-R-SPB",
                                                         grepl("Walker", Source) ~ "WALKER-CRK-MR",
                                                         grepl("Viking", Source) ~ "VIKING-CRK-WA",
                                                         grepl("SWIFT_Del", Source) ~ "SWIFT-CRK-DEL",
                                                         grepl("Swift", Source) ~ "SWIFT-CRK-DEL",
                                                         grepl("Smith", Source) ~ "SMITH-CRK-ELD",
                                                         grepl("Lazy", Source) ~ "LAZY-CRK-S",
                                                         grepl("Hellroaring", Source) ~ "HELLR-CRK-T",
                                                         grepl("Haskill", Source) ~ "HASK-CRK-T",
                                                         grepl("Cow", Source) ~ "COW-CRK-PA",
                                                         grepl("Beaver", Source) ~ "BEAV-CRK-RR",
                                                         .default = "NA"))




#CHECK TO see if this worked
unique(hobo2223.2$Station_ID)


#now do a same loop for the staff gauge ones ####
setwd("C:/Users/User/Dropbox/database/WF WQ DATA/HOBO-U20/U-20CSVs2022-2023/Staff2023")
temp.staff = list.files(pattern = "\\.csv$")

#empty dataframe for loop
hobo2223.staff<-data.frame()

#for loop to process files
for (i in 1:length(temp.staff)){
  #read in csv file and create a col with file name 
  file.i<-data.frame(read_csv_filename(temp.staff[i]))
  
  #print so I know what its working on
  print(unique(file.i$Source))
  
  
  #cut file
  file.i<-select(file.i, 2:7, "Source")
  
  #rename cols to usable names
  colnames(file.i)<-c( "date", "time", "AbsPresPsi", "TempF",
                       "AbsPresBaromPsi", "SensorDepth", "Source" )
  
  #combine all files together into dataframe
  hobo2223.staff<-rbind(file.i, hobo2223.staff)
  
}


#make sure they all read in properly
unique(hobo2223.staff$Source)

#now change source to station id ####
#note grepl doesnt work sometimes its a thing with plyr i think? 
#but I have replacement code....somewhere
hobo2223.staff.2<- hobo2223.staff %>%  mutate(Station_ID = case_when(grepl("Whitefish", Source) ~ "WF-R-SPB",
                                                         grepl("WF", Source) ~ "WF-R-SPB",
                                                         grepl("Walker", Source) ~ "WALKER-CRK-MR",
                                                         grepl("Viking", Source) ~ "VIKING-CRK-WA",
                                                         grepl("SWIFT_Del", Source) ~ "SWIFT-CRK-DEL",
                                                         grepl("Swift", Source) ~ "SWIFT-CRK-DEL",
                                                         grepl("Smith", Source) ~ "SMITH-CRK-ELD",
                                                         grepl("Lazy", Source) ~ "LAZY-CRK-S",
                                                         grepl("Hellroaring", Source) ~ "HELLR-CRK-T",
                                                         grepl("Haskill", Source) ~ "HASK-CRK-T",
                                                         grepl("Cow", Source) ~ "COW-CRK-PA",
                                                         grepl("Beaver", Source) ~ "BEAV-CRK-RR",
                                                         .default = "NA"))




#CHECK TO see if this worked
unique(hobo2223.staff.2$Station_ID)

#remove source from both
hobo2223.2<- select(hobo2223.2, -Source)
hobo2223.staff.2<- select(hobo2223.staff.2, -Source)

#okay now add the staff gauge col to the other cols
hobo23<-merge(hobo2223.2, hobo2223.staff.2, by.all = Station_ID)  
  
  
#evetually do all of this and actually include the CORRECTED (for barmetric and time zone) 2022 data  #####



#now use equations to graph! #####







