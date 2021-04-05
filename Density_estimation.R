library(astroFns)
library(activity)
library(tidyverse)
library(reshape2)
library(Distance)
library(photobiology)
library(lubridate)

flatfile=read.csv("D:/WII - Thar LTEO/Camera trapping/REM/Output_interim/flatfile.csv")%>%
  separate(Grid,c("Region.Label","Grid"),extra="merge",sep=" ")
flatfile$timestamp=as.POSIXct(flatfile$timestamp,tz = "Asia/Calcutta")
dn= day_night(date = flatfile$timestamp,tz=tz(flatfile$timestamp),geocode = data.frame(lat=27,lon=71),unit.out = "datetime")
timestamp_dn=cbind(timestamp,dn)%>%
  as.data.frame()
timestamp_dn=timestamp_dn%>%
  mutate(DN="NA")%>%
  mutate(DN=ifelse(timestamp>sunrise & timestamp<sunset,"Day",DN))%>%
  mutate(DN=ifelse(timestamp<sunrise | timestamp>sunset,"Night",DN))%>%
  select(timestamp,DN)

flatfile=merge(flatfile,timestamp_dn,by="timestamp")%>%
  unique.data.frame()

sum(!is.na(flatfile$distance))
View(table(flatfile$Grid))

traps=flatfile%>%
  select(Region.Label,Grid,Effort,Defunct_time)%>%
  unique.data.frame()
sample=flatfile%>%
  select(-Defunct_time,-Effort)

chinkara=sample%>%
  filter(Species=="CHINKARA")%>%
  merge(traps,by=c("Region.Label","Grid"),all = T)%>%
  rename(Sample.Label=Grid)%>%
  mutate(Area=as.numeric(1))%>%
  mutate(size=as.numeric(size))%>%
  mutate(timestamp=as.POSIXct(timestamp))%>%
  arrange(Sample.Label,timestamp)%>%
  group_by(Sample.Label)%>%
  mutate(diff = as.numeric(strptime(timestamp, "%Y-%m-%d %H:%M:%S") - lag(strptime(timestamp, "%Y-%m-%d %H:%M:%S"), 
                                                               default = strptime(timestamp, "%Y-%m-%d %H:%M:%S")[1])))%>%
  ungroup(Sample.Label)%>%
  as.data.frame()

activity_chinkara=chinkara%>%
  filter(!is.na(timestamp))%>%
  filter(diff>10)
activity_chinkara=fitact(hms2rad(strftime(activity_chinkara$timestamp,format="%H:%M:%S")),sample="data" )
    
Ifox=sample%>%
  filter(Species=="INDIAN FOX")%>%
  merge(traps,by=c("Region.Label","Grid"),all = T)%>%
  rename(Sample.Label=Grid)%>%
  mutate(Area=as.numeric(1))%>%
  mutate(size=as.numeric(size))%>%
  mutate(timestamp=as.POSIXct(timestamp))%>%
  arrange(Sample.Label,timestamp)%>%
  group_by(Sample.Label)%>%
  mutate(diff = as.numeric(strptime(timestamp, "%Y-%m-%d %H:%M:%S") - lag(strptime(timestamp, "%Y-%m-%d %H:%M:%S"), 
                                                                          default = strptime(timestamp, "%Y-%m-%d %H:%M:%S")[1])))%>%
  ungroup(Sample.Label)%>%
  as.data.frame()

activity_Ifox=Ifox%>%
  filter(!is.na(timestamp))%>%
  filter(diff>10)
activity_Ifox=fitact(hms2rad(strftime(activity_Ifox$timestamp,format="%H:%M:%S")),sample="data" )

Dfox=sample%>%
  filter(Species=="DESERT FOX")%>%
  merge(traps,by=c("Region.Label","Grid"),all = T)%>%
  rename(Sample.Label=Grid)%>%
  mutate(Area=as.numeric(1))%>%
  mutate(size=as.numeric(size))%>%
  mutate(timestamp=as.POSIXct(timestamp))%>%
  arrange(Sample.Label,timestamp)%>%
  group_by(Sample.Label)%>%
  mutate(diff = as.numeric(strptime(timestamp, "%Y-%m-%d %H:%M:%S") - lag(strptime(timestamp, "%Y-%m-%d %H:%M:%S"), 
                                                                          default = strptime(timestamp, "%Y-%m-%d %H:%M:%S")[1])))%>%
  ungroup(Sample.Label)%>%
  as.data.frame()

activity_Dfox=Dfox%>%
  filter(!is.na(timestamp))%>%
  filter(diff>10)
activity_Dfox=fitact(hms2rad(strftime(activity_Dfox$timestamp,format="%H:%M:%S")),sample="data" )

Dcat=sample%>%
  filter(Species=="DESERT CAT")%>%
  merge(traps,by=c("Region.Label","Grid"),all = T)%>%
  rename(Sample.Label=Grid)%>%
  mutate(Area=as.numeric(1))%>%
  mutate(size=as.numeric(size))%>%
  mutate(timestamp=as.POSIXct(timestamp))%>%
  arrange(Sample.Label,timestamp)%>%
  group_by(Sample.Label)%>%
  mutate(diff = as.numeric(strptime(timestamp, "%Y-%m-%d %H:%M:%S") - lag(strptime(timestamp, "%Y-%m-%d %H:%M:%S"), 
                                                                          default = strptime(timestamp, "%Y-%m-%d %H:%M:%S")[1])))%>%
  ungroup(Sample.Label)%>%
  as.data.frame()
plot(activity_Ifox)

activity_Dcat=Dcat%>%
  filter(!is.na(timestamp))%>%
  filter(diff>10)
activity_Dcat=fitact(hms2rad(strftime(activity_Dcat$timestamp,format="%H:%M:%S")),sample="data" )

Dog=sample%>%
  filter(Species=="DOG")%>%
  merge(traps,by=c("Region.Label","Grid"),all = T)%>%
  rename(Sample.Label=Grid)%>%
  mutate(Area=as.numeric(1))%>%
  mutate(size=as.numeric(size))%>%
  mutate(timestamp=as.POSIXct(timestamp))%>%
  arrange(Sample.Label,timestamp)%>%
  group_by(Sample.Label)%>%
  mutate(diff = as.numeric(strptime(timestamp, "%Y-%m-%d %H:%M:%S") - lag(strptime(timestamp, "%Y-%m-%d %H:%M:%S"), 
                                                                          default = strptime(timestamp, "%Y-%m-%d %H:%M:%S")[1])))%>%
  ungroup(Sample.Label)%>%
  as.data.frame()%>%
  filter(Sample.Label!="OUT 732")

activity_Dog=Dog%>%
  filter(!is.na(timestamp))%>%
  filter(diff>10)
activity_Dog=fitact(hms2rad(strftime(activity_Dog$timestamp,format="%H:%M:%S")),sample="data" )

GIB=sample%>%
  filter(Species=="GIB")%>%
  merge(traps,by=c("Region.Label","Grid"),all = T)%>%
  rename(Sample.Label=Grid)%>%
  mutate(Area=as.numeric(1))%>%
  mutate(size=as.numeric(size))%>%
  mutate(timestamp=as.POSIXct(timestamp))%>%
  arrange(Sample.Label,timestamp)%>%
  group_by(Sample.Label)%>%
  mutate(diff = as.numeric(strptime(timestamp, "%Y-%m-%d %H:%M:%S") - lag(strptime(timestamp, "%Y-%m-%d %H:%M:%S"), 
                                                                          default = strptime(timestamp, "%Y-%m-%d %H:%M:%S")[1])))%>%
  ungroup(Sample.Label)%>%
  as.data.frame()

activity_GIB=GIB%>%
  filter(!is.na(timestamp))%>%
  filter(diff>10)
activity_GIB=fitact(hms2rad(strftime(activity_GIB$timestamp,format="%H:%M:%S")),sample="data" )

breakpoints <- c(0,5,10,30)
### Indian fox  -> c(0,2.3,4.7,8) 
### Desert fox -> c(0,2.9,4.7,8)
### Chinkara -> c(0,2.7,7.8,14)
hist(GIB$distance,  main="Peak activity data set",
     xlab="Radial distance (m)")

conversion <- convert_units("meter", NULL, "square kilometer")
trunc.list <- list(left=0,right=30)
mybreaks <- breakpoints

hn01 <- ds(GIB, transect = "point", key="hn", adjustment = NULL,
          cutpoints = mybreaks,truncation = trunc.list)
gof_ds(hn01)
plot(hn01)

hn1 <- ds(chinkara, transect = "point", key="hn", adjustment = "herm",
          cutpoints = mybreaks,truncation = trunc.list)
uni1 <- ds(chinkara, transect = "point", key="unif", adjustment = "cos",
           order=1,
           cutpoints = mybreaks, truncation = trunc.list)
uni2 <- ds(chinkara, transect = "point", key="unif", adjustment = "cos",
           order=c(1,2),
           cutpoints = mybreaks, truncation = trunc.list)
hr0 <- ds(chinkara, transect = "point", key="hr", adjustment = NULL,
          cutpoints = mybreaks, truncation = trunc.list)
hr1 <- ds(chinkara, transect = "point", key="hr", adjustment = "cos",
          order=2,
          cutpoints = mybreaks, truncation = trunc.list)
hr2 <- ds(chinkara, transect = "point", key="hr", adjustment = "cos",
          order=c(2,3),
          cutpoints = mybreaks, truncation = trunc.list)

plot(hn0)
chat <- function(modobj) {
  #  computes c-hat for a dsmodel object using Method 1 of Howe et al. (2018)
  test <- gof_ds(modobj)
  num <- test$chisquare$chi1$chisq
  denom <- test$chisquare$chi1$df
  chat <- num/denom
  return(chat)
}

qaic <- function(modobj, chat) {
  #  computes QAIC for a dsmodel object given a c-hat
  value <- 2* modobj$ddf$ds$value/chat + 2 * (length(modobj$ddf$ds$pars)+1)
  return(value)
}

qaic.pass1 <- function(...) {
  #   Performs Pass 1 model selection based upon Method 1 of Howe et al. (2018)
  #   Arguments are dsmodel objects; assumed all based on same key function
  #    c-hat is computed for the most parameter-rich model in the group
  #    qaic is calculated for each model in group based upon this c-hat
  #   Result returned in the form of a data.frame with model name, npar, aic and qaic
  models <- list(...)
  num.models <- length(models)
  npar <- unlist(lapply(models, function(x) length(x$ddf$ds$par)))  
  modname <-  unlist(lapply(models, function(x) x$ddf$name.message))
  aic <-  unlist(lapply(models, function(x) x$ddf$criterion))
  chat.bigmod <- chat(models[[which.max(npar)]])
  qaic <- vector(mode="numeric", length = num.models)
  for (i in 1:num.models) {
    qaic[i] <- qaic(models[[i]], chat.bigmod)
  }
  nicetab <- data.frame(modname, npar, aic, qaic)
  return(nicetab)
}

knitr::kable(qaic.pass1(hn0, hn1), 
             caption="QAIC values for half normal key models.")

knitr::kable(qaic.pass1(uni1, uni2),
             caption="QAIC values for uniform key models.")

knitr::kable(qaic.pass1(hr0, hr1, hr2),
             caption="QAIC values for hazard rate key models.")

winners <- list(hn0, uni2,hr2)
chats <- unlist(lapply(winners, function(x) chat(x)))
modnames <- unlist(lapply(winners, function(x) x$ddf$name.message))
results <- data.frame(modnames, chats)
results.sort <- results[order(results$chats),]
knitr::kable(results.sort, digits=2, row.names = FALSE,
             caption="Compare with Table S5 of Howe et al. (2018)")

p_a <- hn0$ddf$fitted[1]
w <- 12
rho <- sqrt(p_a * w^2)

viewangle <- 42 # degrees
samfrac <- viewangle / 360
conversion <- convert_units("meter", NULL, "square kilometer")
chinkara.dens <- dht2(hn01, flatfile =chinkara, strat_formula = ~Region.Label,
                     sample_fraction = samfrac*activity_chinkara@act[["act"]], er_est = "P2", convert_units = conversion)
print(chinkara.dens, report="density")

Ifox.dens <- dht2(hn01, flatfile =Ifox, strat_formula = ~Region.Label,
                      sample_fraction = samfrac*activity_Ifox@act[["act"]], er_est = "P2", convert_units = conversion)
print(Ifox.dens, report="density")


Dfox.dens <- dht2(hn01, flatfile =Dfox, strat_formula = ~Region.Label,
                  sample_fraction = samfrac*activity_Dfox@act[["act"]], er_est = "P2", convert_units = conversion)
print(Dfox.dens, report="density")

Dog.dens <- dht2(hn01, flatfile =Dog, strat_formula = ~Region.Label,
                  sample_fraction = samfrac*activity_Dog@act[["act"]], er_est = "P2", convert_units = conversion)
print(Dog.dens, report="density")

GIB.dens <- dht2(hn01, flatfile =GIB, strat_formula = ~Region.Label,
                 sample_fraction = samfrac*activity_GIB@act[["act"]], er_est = "P2", convert_units = conversion)
print(GIB.dens, report="density")


mysummary <- function(ests, fit){
  return(data.frame(Dhat = ests$individuals$D$Estimate))
}

chinkara.boot.hr <- bootdht(model=hn01, flatfile=chinkara, resample_transects = TRUE,
                          nboot=400, summary_fun=mysummary, sample_fraction = samfrac,
                          convert.units = conversion)

GIB=flatfile%>%
  filter(Species=="GIB")

chinkara%>%
  group_by(DN)%>%
  summarise(mean=median(distance,na.rm=T))

unique(flatfile$timestamp)
