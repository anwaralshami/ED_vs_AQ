library(dplyr)
library(lubridate)

#Get CCS tagged admission data for both years
adm_08 <- read.csv("ER_adm_08.csv",stringsAsFactors = F)
adm_18 <- read.csv("ER_adm_18_CCS.csv",stringsAsFactors = F)

#Align and select relevant columns
adm_08 %>%
  rename(CCS = CCS.CATEGORY, CCS_des = CCS.CATEGORY.DESCRIPTION)%>%
  select(ADMISSION.DATE,BIRTHDATE,SEX,CCS,CCS_des)%>%
  filter(!is.na(CCS))->adm_08

adm_18 %>%
  rename(CCS = Beta_Version_CCS_Category, CCS_des = Beta_Version_CCS_Category_Description)%>%
  select(Arrived,Date.of.Birth,Gender,CCS,CCS_des)%>%
  filter(!is.na(CCS))->adm_18

colnames(adm_18)<-colnames(adm_08)

#Date technical correction
adm_18$ADMISSION.DATE<- date(ymd_hms(adm_18$ADMISSION.DATE))
adm_08$ADMISSION.DATE<-ymd(adm_08$ADMISSION.DATE)


