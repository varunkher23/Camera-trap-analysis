library(tidyverse)
library(camtrapR)

###loading exiftools
exiftool_dir <- "C:/Users/Sourav/Downloads/Compressed" 
exiftoolPath(exiftoolDir = exiftool_dir)
grepl(exiftool_dir, Sys.getenv("PATH"))
Sys.which("exiftool")
#making the table

exifTagNames(inDir = "E:/Ready For Blender/Kanoi")
record_table_sticks <- recordTable(inDir = "E:/Ready For Blender/Kanoi", IDfrom = "directory", 
                                   cameraID="directory", camerasIndependent=T, 
                                   removeDuplicateRecords = F,
                                   deltaTimeComparedTo ="lastRecord",
                                   timeZone = "Asia/Calcutta", stationCol = "station",
                                   writecsv = F,
                                   additionalMetadataTags = c("MakerNotes:SerialNumber","XMP:Subject",
                                                              "MakerNotes:AmbientTemperature",
                                                              "MakerNotes:MotionSensitivity",
                                                              "MakerNotes:UserLabel"))
record_table_sticks1=record_table_sticks%>%
  select(-Camera)%>%
  rename(Grid=station)%>%
  rename(Camera=Species)%>%
  separate(XMP.Subject,c("count","Species","Znew"),sep=", ",extra="drop")

library(readxl)
library(tidyverse)

animator=read_xlsx("F:/Camera Trap/Final Files/Kanoi_Z_news.xlsx")%>%
  rename(Distance=`DISTANCE FROM CAMERA(MTS.)`)%>%
  rename(count=`ANIMAL COUNT`)%>%
  rename(Species=`SPECIES NAME`)%>%
  rename(Grid=`CAMERA TRAP ID`)%>%
  rename(Time=`TIME IMAGE TAKEN`)%>%
  mutate(Time=format(Time,"%H:%M:%S"))%>%
  mutate(Photo=as.character(`FILE PATH`))%>%
  separate(Photo,c("a","b"),sep = "I",extra = "merge", fill = "right")%>%
  select(-a)%>%
  mutate(I="I")%>%
  unite("FileName",I:b,sep = "",na.rm=T,remove = T)%>%
  select(-`ACTUAL DISTANCE MOVED`,-`MARKING TYPE`,-SPEED,-FLASH,-`MARKING TYPE`,-`ANIMAL SIZE`,-`DISTANCE TRAVELLED`,
         -`DATE IMAGE TAKEN`,-`SL NO.`)

Dist_mean=animator%>%
  group_by(Grid,Time,`FILE PATH`,FileName)%>%
  summarize(Distance_mean=mean(Distance))

animator1=animator%>%
  merge(Dist_mean,by=c("Grid","FILE PATH","Time","FileName"),all=T)%>%
  select(Grid,Time,FileName,Distance_mean)

overall=left_join(record_table_sticks1,animator1,by=c("Grid","FileName","Time"))

trial=overall%>%
  filter(!is.na(Distance_mean))%>%
  unique.data.frame()

znew_withoutdistance=record_table_sticks1%>%
  filter(Species!="BLANK")%>%
  filter(Species!="CALIBRATION")%>%
  filter(Species!="MONGOOSE")%>%
  filter(Species!="OTHERS")%>%
  filter(Species!="CATTLE")%>%
  filter(Species!="SHEEP-GOAT")%>%
  filter(Species!="HUMAN-VEHICLE")%>%
  filter(!is.na(Species))%>%
  filter(Znew=="ZNEW")%>%
  left_join(Dist_mean,by=c("Grid","FileName","Time"))%>%
  unique.data.frame()%>%
  filter(is.na(Distance_mean))

write.csv(overall,"C:/Users/Sourav/Desktop/Kanoi.csv")

