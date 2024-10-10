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

hobo23.2<- hobo23.2 %>% mutate(year = year(date), month = month(date), day = yday(date))

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
#this data is really bad -- why? mostly in 2023-- check if it was calibrated wrong??
ggplot()+
  geom_point(data = hobo23.2 %>% filter(Station_ID == "COW-CRK-PA"), 
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "COW-CRK-PA"),
             aes(x = Date, y = Staff), color = "darkred", size = 2)

#bad in 2022 -- maybe calibrated wrong??
ggplot()+
  geom_point(data = hobo23.2 %>% filter(Station_ID == "HASK-CRK-T"), 
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "HASK-CRK-T"),
             aes(x = Date, y = Staff), color = "darkred", size = 2)

#-- check calibration for 2023
ggplot()+
  geom_point(data = hobo23.2 %>% filter(Station_ID == "HELLR-CRK-T"), 
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "HELLR-CRK-T"),
             aes(x = Date, y = Staff), color = "darkred", size = 2)
#bad in 2023 just a bit off-- check calibration for 2023
ggplot()+
  geom_point(data = hobo23.2 %>% filter(Station_ID == "LAZY-CRK-S"), 
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "LAZY-CRK-S"),
             aes(x = Date, y = Staff), color = "darkred", size = 2)
#this one looks decent
ggplot()+
  geom_point(data = hobo23.2 %>% filter(Station_ID == "SMITH-CRK-ELD"), 
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "SMITH-CRK-ELD"),
             aes(x = Date, y = Staff), color = "darkred", size = 2)

#really bad 
ggplot()+
  geom_point(data = hobo23.2 %>% filter(Station_ID == "SWIFT-CRK-DEL"), 
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "SWIFT-CRK-DEL"),
             aes(x = Date, y = Staff), color = "darkred", size = 2)

#pretty good
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





#next steps individual trib QA/QC ####
#this is done manually just by looking at what sticks out 
#then verifying it in the data and with weather data



#whitefish river QA/QC ####
wfriver<-hobo23.2 %>% filter(Station_ID == "WF-R-SPB")
  
  
ggplot()+
  geom_point(data = wfriver %>% filter(date > "2022-08-31"), 
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
 geom_point(data = wfriver %>% filter(Station_ID == "WF-R-SPB"),
             aes(x = date, y = TempF), color = "darkred", size = 2)+
  geom_point(data = wfriver %>% filter(Station_ID == "WF-R-SPB"),
             aes(x = date, y = AbsPresPsi), color = "black", size = 2)


#group by day and test range?
glimpse(wfriver)
wfriver<- wfriver %>% group_by(date) %>% mutate(max = max(WaterDepth), min = min(WaterDepth), range = max-min)

#okay first step lets just remove any days where range is over two feet
#and where water depth falls below 0

wfriver.2<- wfriver %>% filter(range < 1.5) %>% filter(WaterDepth > 0)

#lets see it
ggplot()+
  geom_point(data = wfriver, 
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = wfriver.2, 
             aes(x = date, y = WaterDepth), color = "black", alpha = .5)


p<-ggplot()+
  geom_point(data = wfriver.2 %>% filter(date > "2022-08-31"), 
             aes(x = date, y = WaterDepth), color = "black", alpha = .5)
#now you can hover to pick out problem days
ggplotly(p)

#remove particularyly bad days
wfriver.3 <-  wfriver.2 %>% 
  filter(!date == "2022-12-24", !date == "2022-11-30", !date == "2022-12-21", 
         !date == "2023-01-30", !date == "2023-01-31", !date == "2023-02-22",
         !date == "2023-01-27", !date == "2023-02-23", !date == "2023-03-11",
         !date == "2023-02-27", !date == "2023-02-21", !date == "2023-03-23")


#graph again
p<-ggplot()+
  geom_point(data = wfriver.3 %>% filter(date > "2022-08-31"), 
             aes(x = date, y = WaterDepth), color = "black", alpha = .5)

ggplotly(p)

#now take out some specific times that were bad####
wfriver.4<-wfriver.3 %>% filter(!(date == "2023-04-21" & time == "07:30:00 AM"),
                                !(date == "2023-03-27" & time == "07:00:00 AM"),
                                !(date == "2023-03-27" & time == "07:30:00 AM"))


#graph again
p<-ggplot()+
  geom_point(data = wfriver.4, 
             aes(x = date, y = WaterDepth), color = "black", alpha = .5)

ggplotly(p)







#do smith or lazy next because they are the other two decent ones
smith<- hobo23.2 %>% filter(Station_ID == "SMITH-CRK-ELD")

p <-ggplot()+
  geom_point(data = hobo23.2 %>% filter(Station_ID == "SMITH-CRK-ELD"), 
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "SMITH-CRK-ELD"),
             aes(x = Date, y = Staff), color = "darkred", size = 2)

ggplotly(p)

print(ggplot()+
        geom_point(data = hobo23.2 %>% filter(Station_ID == "SMITH-CRK-ELD"), 
                   aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
        geom_point(data = gauges.2 %>% filter(Station_ID == "SMITH-CRK-ELD"),
                   aes(x = Date, y = Staff), color = "darkred", size = 2))


#group by day and test range?
smith<- smith %>% group_by(date) %>% mutate(max = max(WaterDepth), min = min(WaterDepth), range = max-min)

#filter out specific date and time points
smith.2<-smith %>% filter(!(date == "2023-11-14" & time == "02:30:00 PM"),
                          !(date == "2023-03-14" & time == "03:30:00 AM"))


ggplot()+
  geom_point(data = smith.2, 
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "SMITH-CRK-ELD"),
             aes(x = Date, y = Staff), color = "darkred", size = 2)

#lazy creek QA/QC#####
#i re-did the lazy creek syncing using september and it matches up much better -- so I will use that
#that file is the one that says depth2
lazy <- hobo23.2 %>% filter(Station_ID == "LAZY-CRK-S")
lazy<- lazy %>%  group_by(date) %>% mutate(max = max(WaterDepth), min = min(WaterDepth), range = max-min)

p<-ggplot()+
  geom_point(data = lazy, 
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "LAZY-CRK-S"),
             aes(x = Date, y = Staff), color = "darkred", size = 2)

ggplotly(p)

ggplot()+
  geom_point(data = lazy, 
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "LAZY-CRK-S"),
             aes(x = Date, y = Staff), color = "darkred", size = 2)

#this looks good the high range days all check out and there aren't any isolated points

#cow creek QA/QC #####
#again had to switch this to the september staff gauge to calibrate it so
#the correct file is depth2
cow <- hobo23.2 %>% filter(Station_ID == "COW-CRK-PA")
cow <- cow %>% group_by(date) %>% mutate(max = max(WaterDepth), min = min(WaterDepth), range = max-min)

ggplot()+
  geom_point(data =cow, 
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "COW-CRK-PA"),
             aes(x = Date, y = Staff), color = "darkred", size = 2)

p <- ggplot()+
  geom_point(data = hobo23.2 %>% filter(Station_ID == "COW-CRK-PA"), 
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "COW-CRK-PA"),
             aes(x = Date, y = Staff), color = "darkred", size = 2)

ggplotly(p)

#cut out range over 2
cow.2 <- cow %>% filter(range < 2)

ggplot()+
  geom_point(data = cow.2,  
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "COW-CRK-PA"),
             aes(x = Date, y = Staff), color = "darkred", size = 2)

p<-ggplot()+
  geom_point(data = cow.2,  
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "COW-CRK-PA"),
             aes(x = Date, y = Staff), color = "darkred", size = 2)

ggplotly(p)

#next step -> check all dates on the sticky note maybe also just take out all negative values

#take out specific dates
cow.3<-cow.2 %>% filter(!(date == "2022-11-26"),!(date == "2022-11-24"),!(date == "2022-11-25"),
                      !(date == "2022-12-21"), !(date == "2022-12-22"), !(date == "2022-12-23"),
                      !(date == "2023-11-01"), !(date == "2022-11-12"), !(date == "2022-11-13"),
                      !(date == "2022-11-14"), !(date == "2022-11-15"), !(date == "2022-11-16"),
                      !(date == "2022-11-17"), !(date == "2022-11-18"), !(date == "2022-11-19"),
                      !(date == "2022-11-20"), !(date == "2022-11-21"), !(date == "2022-11-21"),
                      !(date == "2022-11-11"), !(date == "2023-02-27"))


ggplot()+
  geom_point(data = cow.3,  
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "COW-CRK-PA"),
             aes(x = Date, y = Staff), color = "darkred", size = 2)


p<-ggplot()+
  geom_point(data = cow.3,  
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "COW-CRK-PA"),
             aes(x = Date, y = Staff), color = "darkred", size = 2)

ggplotly(p)

#okay I think this one is good? messier than the others but It's hard to count things out when its flashy

#haskill creek QA/QC####
haskill<-hobo23.2 %>% filter(Station_ID == "HASK-CRK-T")

haskill <- haskill %>% group_by(date) %>% mutate(max = max(WaterDepth), min = min(WaterDepth), range = max-min)

ggplot()+
  geom_point(data = haskill,  
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "HASK-CRK-T"),
             aes(x = Date, y = Staff), color = "darkred", size = 2)

p <- ggplot()+
  geom_point(data = haskill,  
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "HASK-CRK-T"),
             aes(x = Date, y = Staff), color = "darkred", size = 2)

ggplotly(p)

#take out certain times and dates


haskill.2 <- haskill %>% filter(!(date == "2022-10-04" & time == "10:30:00 AM"), !(date == "2022-10-04" & time == "10:31:26 AM"),
                                !(date == "2022-10-04" & time == "10:31:27 AM"), !(date == "2022-10-04" & time == "11:00:00 AM"),
                                !(date == "2022-10-04" & time == "11:30:00 AM"), !(date == "2023-11-14" & time == "02:30:00 PM"),
                                !(date == "2023-11-14" & time == "03:00:00 PM"), !(date == "2023-11-14" & time == "03:30:00 PM"),
                                !(date == "2023-11-14" & time == "04:00:00 PM"), !(date == "2023-11-14" & time == "04:30:00 PM"),
                                !(date == "2023-11-14" & time == "05:00:00 PM"), !(date == "2023-11-14" & time == "05:30:00 PM"),
                                !(date == "2023-11-14" & time == "06:00:00 PM"), !(date == "2023-11-14" & time == "06:30:00 PM"),
                                !(date == "2023-11-14" & time == "07:00:00 PM"), !(date == "2023-11-14" & time == "07:30:00 PM"),
                                !(date == "2023-11-14" & time == "08:00:00 PM"), !(date == "2023-11-14" & time == "08:30:00 PM"),
                                !(date == "2023-11-14" & time == "09:00:00 PM"), !(date == "2023-11-14" & time == "09:30:00 PM"),
                                !(date == "2023-11-14" & time == "10:00:00 PM"), !(date == "2023-11-14" & time == "10:30:00 PM"),
                                !(date == "2023-11-14" & time == "11:00:00 PM"), !(date == "2023-11-14" & time == "11:30:00 PM"))
                                  

ggplot()+
  geom_point(data = haskill.2,  
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "HASK-CRK-T"),
             aes(x = Date, y = Staff), color = "darkred", size = 2)

p <- ggplot()+
  geom_point(data = haskill.2,  
             aes(x = date, y = WaterDepth), color = "blue", alpha = .5)+
  geom_point(data = gauges.2 %>% filter(Station_ID == "HASK-CRK-T"),
             aes(x = Date, y = Staff), color = "darkred", size = 2)

ggplotly(p)


#hellroaring re calibrate 2023 ####
















#next steps
#save the edited data
#add to previous edited data
#replicate previous graphs (I think there is script for this)















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



