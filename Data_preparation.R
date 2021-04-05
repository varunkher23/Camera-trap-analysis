###Camera trap distance sampling code

library(tidyverse)
library(lubridate)

#### Calculating effort for each camera trap

filename="D:/WII - Thar LTEO/Camera trapping/REM/all_sticks_csv_260321.csv"
effort= function(filename,sampling_interval)
{
  #Load packages
  library(dplyr)
  library(lubridate)
  
  data=read.csv(filename)
  data$timestamp=as.POSIXct(data$timestamp)
  
  #read the input data into the format required and assign data to appropriate variables
  Grid=unique(data$Grid)
  # Prepare a blank matrix to store the results.
  # The number of cells is equal to (Number of grids)x(No. of species) - this value is stated as nrow 
  effort=matrix(NA,nrow = length(Grid),ncol = 3)%>%
    as.data.frame()
  colnames(effort)=c("Grid","Effort","Defunct_time")
  
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
    
    #### Populate the output matrix
    effort[j,"Grid"]=grid
    effort[j,"Effort"]=length(time_check)
    effort[j,"Defunct_time"]=length(defunct_time)
  }
  return(effort)
}

effort=effort(filename,sampling_interval = 2)

##### Sample from camera trap images using the effort defined above


sample=function(filename,sampling_interval)
{
  data=read.csv(filename)
  data$timestamp=as.POSIXct(data$timestamp)
  
  #read the input data into the format required and assign data to appropriate variables
  Grid=unique(data$Grid)
  # Prepare a blank matrix to store the results.
  # The number of cells is equal to (Number of grids)x(No. of species) - this value is stated as nrow 
  sample=matrix(NA,ncol = 5)%>%
    as.data.frame()
  colnames(sample)=c("Grid","timestamp","Species","size","distance")
  
  # Loop for grid-wise analysis - same analysis will be repeated for all grids in a loop
  
  for(j in 1:length(Grid)){
    # Filter the data for a particular grid
    
    grid=Grid[j]
    data2=data%>%
      filter(Grid==grid)
    
    #### Select time available for sampling by removing defunct time
    defunct=data2%>%
      filter(count=="DEFUNCT")
    defunct_time=c(defunct$timestamp)
    data2=data2%>%
      filter(count!="DEFUNCT")
    start = as.POSIXct(min(data2$timestamp))
    end = as.POSIXct(max(data2$timestamp))
    end_f = as.POSIXct(end+(24*60*60))
    duration = end - start
    time_check = seq(start, end, by= sampling_interval)
    time_check = as.POSIXct(setdiff(as.character(time_check),defunct_time))
  
    #### Sample data
    data3=data2%>%
      filter(timestamp%in%time_check)%>%
      filter(Species!="")%>%
      group_by(timestamp,Species)%>%
      summarise(size=max(count),distance=mean(dist_midpt))%>%
      mutate(Grid=grid)
    
    sample=merge(data3,sample,all = T)%>%
      filter(!is.na(timestamp))
  }
  return(sample)
}

sample=sample(filename = filename, sampling_interval = 2)

##### Effort and sample together

trial=data%>%
  filter(Grid=="Chouhani Cha_10"|Grid=="Sudasri_out OUT 649")

camtrapdis=function(filename,sampling_interval)
{
  library(tidyverse)
  library(lubridate)
  
  data=read.csv(filename)
  
  data$timestamp=as.POSIXct(data$timestamp)
    #read the input data into the format required and assign data to appropriate variables
  Grid=unique(data$Grid)
  # Prepare a blank matrix to store the results.
  # The number of cells is equal to (Number of grids)x(No. of species) - this value is stated as nrow 
  sample=matrix(NA,ncol = 5)%>%
    as.data.frame()
  colnames(sample)=c("Grid","timestamp","Species","size","distance")
  effort=matrix(NA,nrow = length(Grid),ncol = 3)%>%
    as.data.frame()
  colnames(effort)=c("Grid","Effort","Defunct_time")
  camtrapdis=list(effort=effort,sample=sample)
  
  for(j in 1:length(Grid)){
    grid=Grid[j]
    data2=data%>%
      filter(Grid==grid)
    defunct=data2%>%
      filter(count=="DEFUNCT")
    defunct_time=c(defunct$timestamp)
    data2=data2%>%
      filter(count!="DEFUNCT")
    
    start = as.POSIXct(min(data2$timestamp))
    end = as.POSIXct(max(data2$timestamp))
    end_f = as.POSIXct(end+(24*60*60))
    duration = end - start
    time_check = seq(start, end, by= sampling_interval)
    time_check = as.POSIXct(setdiff(as.character(time_check),defunct_time))
    
    data3=data2%>%
      filter(timestamp%in%time_check)%>%
      filter(Species!="")%>%
      group_by(timestamp,Species)%>%
      summarise(size=max(count),distance=mean(dist_midpt))%>%
      mutate(Grid=grid)
    
    camtrapdis$effort[j,"Grid"]=grid
    camtrapdis$effort[j,"Effort"]=length(time_check)
    camtrapdis$effort[j,"Defunct_time"]=length(defunct_time)
    
    camtrapdis$sample=merge(data3,camtrapdis$sample,all = T)%>%
      filter(!is.na(timestamp))
    paste(grid,"processed")
  }
  return(camtrapdis)
}

filename="D:/WII - Thar LTEO/Camera trapping/REM/all_sticks_csv_260321.csv"

camtrapdist=camtrapdis(filename = filename,sampling_interval = 2)

flatfile=merge(camtrapdist$sample,camtrapdist$effort,all.y=T,by = "Grid")

write.csv(flatfile,"D:/WII - Thar LTEO/Camera trapping/REM/flatfile.csv")
