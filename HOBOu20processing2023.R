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
                                                         .default = "uhoh"))




#CHECK TO see if this worked
unique(hobo2223.2$Station_ID)

#change name 
hobo23<-hobo2223.2
  
  
#evetually do all of this and actually include the CORRECTED (for barmetric and time zone) 2022 data  #####



#now use equations to graph! #####


#viking creek
#y = 5.8591x^2 - 2.9511x
#use case when
#cow creek equation might be off because i replaces e with *10 so check on that
hobo23.2<- hobo23 %>% mutate(PredCFS = case_when(grepl("VIKING-CRK-WA", Station_ID) ~ 3.2716*(WaterDepth)^2 - 1.7532*(WaterDepth),
                                                   grepl("LAZY-CRK-S", Station_ID) ~  5.703*(WaterDepth)^1.9181,
                                                   grepl("WF-R-SPB", Station_ID) ~  21.811*(WaterDepth)^2.1612,
                                                   grepl("WALKER-CRK-MR", Station_ID) ~ 6.4718*(WaterDepth)^2 + 0.6276*(WaterDepth),
                                                   grepl("SWIFT-CRK-DEL", Station_ID) ~ 175.31*(WaterDepth) + 35.729,
                                                   grepl("SMITH-CRK-ELD", Station_ID) ~ 15.002*(WaterDepth) - 4.2308,
                                                   grepl("HELLR-CRK-T", Station_ID) ~ 11.674*(WaterDepth)^3.3306,
                                                   grepl("HASK-CRK-T", Station_ID) ~ 12.176*(WaterDepth)^2 + 6.2169*(WaterDepth) - 2.8841,
                                                   grepl("COW-CRK-PA", Station_ID) ~ 0.1096*10^2.0731*(WaterDepth),
                                                   grepl("BEAV-CRK-RR", Station_ID) ~ 5.1051*(WaterDepth) - 1.1008))




#graph against water depth just to see
ggplot(data = hobo23.2 %>% filter(Station_ID == "VIKING-CRK-WA"))+
  geom_point(aes(x = date, y = WaterDepth), color = "blue")+
  geom_point(aes(x = date, y = PredCFS), color = "darkred")


#questions--- are we comparing water depth to CFS??? how are we meant to compare this??
#I think what it actually should be compared against is staff measurements and the PREDDEPth col is CFS whoops



#compare to stream gauge
#upload that data
setwd("C:/Users/User/Dropbox/database/WF WQ DATA/STREAMFLOW/STREAM DATA")
gauges <- read_csv("StreamGaugeTribRTS.csv")
glimpse(gauges)
glimpse(hobo23)

#fix date

#nmln$Activity_Start_Date<-as.POSIXct(nmln$Activity_Start_Date, format = "%M/%D/%Y")
#nmln$Activity_Start_Date<-mdy(nmln$Activity_Start_Date)

gauges$Date<- mdy(gauges$Date)

gauges<- gauges %>% mutate(year = year(Date))

hobo23.2$date<- mdy(hobo23.2$date)

glimpse(hobo23.2)

#filter to just 2022 on 

gauges<- filter(gauges, year > 2021)

#fix station ID
unique(gauges$Site)
gauges.2<- gauges %>%  mutate(Station_ID = case_when(grepl("WF River Outlet", Site) ~ "WF-R-SPB",
                                                                     grepl("Whitefish River", Site) ~ "WF-R-SPB",
                                                                     grepl("WF River", Site) ~ "WF-R-SPB",
                                                                     grepl("Walker", Site) ~ "WALKER-CRK-MR",
                                                                     grepl("Viking", Site) ~ "VIKING-CRK-WA",
                                                                     grepl("SWIFT_Del", Site) ~ "SWIFT-CRK-DEL",
                                                                     grepl("Swift", Site) ~ "SWIFT-CRK-DEL",
                                                                     grepl("Smith", Site) ~ "SMITH-CRK-ELD",
                                                                     grepl("Lazy", Site) ~ "LAZY-CRK-S",
                                                                     grepl("Hellroaring", Site) ~ "HELLR-CRK-T",
                                                                     grepl("Haskill", Site) ~ "HASK-CRK-T",
                                                                     grepl("Cow", Site) ~ "COW-CRK-PA",
                                                                     grepl("Beaver", Site) ~ "BEAV-CRK-RR",
                                                                     .default = "ohno"))


#graph
unique(gauges.2$Station_ID)
ggplot()+
  geom_point(data = hobo23.2 %>% filter(Station_ID == "WF-R-SPB"), 
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "WF-R-SPB"),
             aes(x = Date, y = Staff), color = "darkred", size = 2)


ggplot()+
  geom_point(data = hobo23.2 %>% filter(Station_ID == "BEAV-CRK-RR"), 
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "BEAV-CRK-RR"),
             aes(x = Date, y = Staff), color = "darkred", size = 2) 
#this data is really bad -- why? mostly in 2023
ggplot()+
  geom_point(data = hobo23.2 %>% filter(Station_ID == "COW-CRK-PA"), 
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "COW-CRK-PA"),
             aes(x = Date, y = Staff), color = "darkred", size = 2)

#bad in 2022
ggplot()+
  geom_point(data = hobo23.2 %>% filter(Station_ID == "HASK-CRK-T"), 
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "HASK-CRK-T"),
             aes(x = Date, y = Staff), color = "darkred", size = 2)


ggplot()+
  geom_point(data = hobo23.2 %>% filter(Station_ID == "HELLR-CRK-T"), 
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "HELLR-CRK-T"),
             aes(x = Date, y = Staff), color = "darkred", size = 2)
#bad in 2023 just a bit off
ggplot()+
  geom_point(data = hobo23.2 %>% filter(Station_ID == "LAZY-CRK-S"), 
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "LAZY-CRK-S"),
             aes(x = Date, y = Staff), color = "darkred", size = 2)
#same here
ggplot()+
  geom_point(data = hobo23.2 %>% filter(Station_ID == "SMITH-CRK-ELD"), 
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "SMITH-CRK-ELD"),
             aes(x = Date, y = Staff), color = "darkred", size = 2)

#really bad in 2022
ggplot()+
  geom_point(data = hobo23.2 %>% filter(Station_ID == "SWIFT-CRK-DEL"), 
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "SWIFT-CRK-DEL"),
             aes(x = Date, y = Staff), color = "darkred", size = 2)


ggplot()+
  geom_point(data = hobo23.2 %>% filter(Station_ID == "VIKING-CRK-WA"), 
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "VIKING-CRK-WA"),
             aes(x = Date, y = Staff), color = "darkred", size = 2)


#2023 data is all bad
ggplot()+
  geom_point(data = hobo23.2 %>% filter(Station_ID == "WALKER-CRK-MR"), 
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "WALKER-CRK-MR"),
             aes(x = Date, y = Staff), color = "darkred", size = 2)





#next steps
#qa/qc this look at what rdg did




#code for uploading sensor depth and combining 

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
                                                                     .default = "uhoh"))




#CHECK TO see if this worked
unique(hobo2223.staff.2$Station_ID)

#remove source from both
hobo2223.2<- select(hobo2223.2, -Source)
hobo2223.staff.2<- select(hobo2223.staff.2, -Source)

#okay now add the staff gauge col to the other cols
glimpse(hobo2223.2) 
glimpse(hobo2223.staff.2)
hobo23<-merge(hobo2223.2, hobo2223.staff.2, by.all = c(Station_ID, date, time)) 



#code to do just one individually


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



