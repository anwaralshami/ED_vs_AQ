library(dplyr)
library(tidyr)
library(zoo)
AQdat<-read.csv("Beirut_09_10.csv")
HealthDat<-read.csv("Data 2009-2010.csv")

AQdat %>%
  spread(key = name, value = value ) ->AQdatWide

#AQdatWide$datettime<-as.POSIXct(paste0(as.character(AQdatWide$date)," ",as.character(AQdatWide$time)),format="%d/%m/%Y %H:%M:%S")
AQdatWide$datejoin<-as.POSIXct(paste0(as.character(AQdatWide$date)),format="%m/%d/%Y")
AQdatWide %>% 
  group_by(datejoin) %>%
  summarise_each(funs(mean))%>%
  mutate(pm2p5 = pm2p5*1e9)%>%
  mutate(pm2p5_5=rollmean(pm2p5,5,align='right',fill=NA))%>%
  mutate(pm2p5_10=rollmean(pm2p5,10,align='right',fill=NA))->AQdatWide_day
  

clipDate <- function(x){
  x<-paste0(x)
  return(paste0(substr(x,nchar(x)-1,nchar(x)),"/",
                substr(x,nchar(x)-3,nchar(x)-2),"/",
                if(nchar(substr(x,1,nchar(x)-4))==1){
                  paste0("0",substr(x,1,nchar(x)-4))
                }else(paste0(substr(x,1,nchar(x)-4))))
  )
  
}

clipTime <- function(x){
  x<-paste0(x)
  x<-paste0(paste(replicate(6-nchar(x),"0"),collapse = ""),x)
  #x<-paste0(rep("0",),x)
  return(paste0(substr(x,1,nchar(x)-4),":",
                substr(x,nchar(x)-3,nchar(x)-2),":",
                substr(x,nchar(x)-1,nchar(x))
  )
  
  )
  
}

HealthDat$ADMISSION.TIME<-unlist(lapply(HealthDat$ADMISSION.TIME, clipTime))
HealthDat$ADMISSION.DATE<-unlist(lapply(HealthDat$ADMISSION.DATE, clipDate))

#HealthDat$datettime<-as.POSIXct(paste0(HealthDat$ADMISSION.DATE," ",HealthDat$ADMISSION.TIME),format="%d/%m/%y %H:%M:%S")
HealthDat$datejoin<-as.POSIXct(paste0(HealthDat$ADMISSION.DATE),format="%d/%m/%y")

HealthDat %>% inner_join(select(AQdatWide_day,datejoin,pm2p5,pm2p5_5,pm2p5_10)) -> X
select(X,-datejoin) -> mergedData09
write.csv(mergedData09,"merge09.csv")

mergedData09 %>% 
  group_by(ICD9CM.1)%>%
  summarise(Age=mean(AGE),pm2p5=mean(pm2p5),pm2p5_5=mean(pm2p5_5),pm2p5_10=mean(pm2p5_10),n=n()) ->test2

write.csv(test2,"merge09Group.csv")

esquisse::esquisser(test2,viewer="browser")
