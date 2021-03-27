#Author: Vishnupriya Kolipakam
#Date: 6 June 2018
#Process REM data files which have should have the following columns "date" in YY-MM-DD format, "time" in HH:MM:SS
#format. and column "count" which has the animal counts. These counts can be duplicates, i.e. there can me two
#rows with the same date and time, the function can handle it. 
#REM input files need to be .csv files. All additional columns apart from those mentioned will not hinder the out
#-put
# the user needs to specify the start date and time, end date and time of the camera trapping duration in the
#same format as the data sheet
#file name = name of the input file in specified format
#start_date = date of deployment of camera in YY-MM-DD format
#start_time = time of deployment of camera in HH:MM:SS format
#end_date = date of removal of camera in YY-MM-DD format
#end_time = time of removal of camera in HH:MM:SS format
#sampling interval = in seconds. eg. 5 seconds, 300 seconds (incase you want to mention 5 minutes)
#area = in sq km


#Latest edit: Varun Kher
#Date: 27 March 2020
#Looped the process of calculating encounter rates. The new code calculates encounter rates for each species in each 
#grid
#The loop functions neccisitates two more columns - Grid specifying the grid number/camera number/any UID and Species
#Given the new changes, start and end date/time is not required. It is now gathered from the first and last photo
#taken by a particular camera. 
#Timestamp for each photo in "YY-MM-DD HH:MM:SS" format is required
# Everything else remains the same

library(dplyr)
library(lubridate)
library(readr)
library(readxl)

##### Name of the input file - the input file should have columns stating time-stamp, grid number, species name. 

filename="D:/WII - Thar LTEO/Camera trapping/REM/all_sticks_csv_260321.csv"

timeseries_vk <- function(filename,sampling_interval, area, timestamp)
{
  #Load packages
  library(dplyr)
  library(lubridate)
  #read the input data into the format required and assign data to appropriate variables
  data=read.csv(filename)
  data$timestamp=as.POSIXct(data$timestamp)
  Species=unique(data$Species)
  Grid=unique(data$Grid)
  # Prepare a blank matrix to store the results.
  # The number of cells is equal to (Number of grids)x(No. of species) - this value is stated as nrow 
  summary=matrix(NA,nrow = length(Species)*length(Grid),ncol = 3)%>%
    as.data.frame()
  colnames(summary)=c("Grid","Species","ER")
  
  # Loop for grid-wise analysis - same analysis will be repeated for all grids in a loop
  
  for(j in 1:length(Grid)){
  # Filter the data for a particular grid
  grid=Grid[j]
  data2=data%>%
    filter(Grid==grid)
  
  #Select defunct data
  defunct=data2%>%
    filter(count=="DEFUNCT")
  # 1 is just a dummy character, entered to ensure that the code runs even when there are no defunct times
  defunct_time=c(defunct$timestamp)
  data2=data2%>%
    filter(count!="DEFUNCT")
  
  #calculate the duration of time the camera is on - i.e. since the deployment of camera to removal
  start = as.POSIXct(min(data2$timestamp))
  end = as.POSIXct(max(data2$timestamp))
  end_f = as.POSIXct(end+(24*60*60))
  duration = end - start
  
  #generate a vector with the specified time interval, to sample and match
  time_check = seq(start, end, by= sampling_interval)
  # Remove defunct duration from the total duration
  time_check = as.POSIXct(setdiff(as.character(time_check),defunct_time))
  
  # Prepare loop for obtaining species-wise parameters
  
for (i in 1:length(Species)) {
    
  #Select data of a particular species
    species=Species[i]
    r1 = data2%>%
      filter(Species==species)
    
    #match the vector with the dataset, and retain the matching rows
    r1_clean = r1[r1$timestamp %in% time_check,]
    y = r1_clean$timestamp - first(r1_clean$timestamp)
    #calculate detla between the matched records, and write matched to file, to make sure that the time difference is checked by user  
    r1_clean$delta = y
    
    #calculating the total animal count of the matched rows by the specified time interval from input data
    r1_clean$count=as.numeric(r1_clean$count)
    count = sum(r1_clean$count)
    #calculating the total duration of sampling = total days (in julian) of camera 
    #trapping * 24 hours* 60 minutes * 60 seconds, divided by sampling interval in seconds - changed to length timecheck as timecheck is the number of events after removing defunct interval
    events = length(time_check)
    #density is then calculated as the sum of counts divided by area (user defined, in sq km)* events  
    density_rem = count/(area*events)
    
    summary[(j-1)*length(Species)+i,"Grid"]=grid
    summary[(j-1)*length(Species)+i,"Species"]=species
    summary[(j-1)*length(Species)+i,"ER"]=density_rem
      }
  }
return(summary)
    }

ER=timeseries_vk(filename = filename,sampling_interval = 5,area=1)

write.csv(ER,"D:/WII - Thar LTEO/Camera trapping/REM/ER_summary.csv")

