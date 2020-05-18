library(openxlsx)
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(tidyr)
library(plotly)
#source("funs.R")

##### Create Mapping and description#####
# ccs <- CCS::CCS_DX_mapping
# colnames(ccs)<-c("ccsCode", "icdCode", "icdCodeType")
# write.xlsx(ccs,file = "dat/ccs_icd_map.xlsx")
# ccsdesc<- CCS::CCS_DX_categories
# colnames(ccsdesc)<-c("chapter","ccsCode","L2code","ccsCodeDesc")
# write.xlsx(ccsdesc,file = "dat/ccs_code_desc.xlsx")


##### Load datasets ####
df2018_1 <- read.xlsx(xlsxFile = "dat/ED Visits-Nov 2018 to July 2019.xlsx",sheet = 1,detectDates = T)
df2018_2 <- read.xlsx(xlsxFile = "dat/Data set 2- 2018-2019.xlsx",sheet = 1,detectDates = T)
ccsMap <- read.xlsx(xlsxFile = "dat/ccs_icd_map.xlsx",sheet = 1)
ccsDesc <- read.xlsx("dat/ccs_code_desc.xlsx",sheet=1)
#df2009x <- read.xlsx(xlsxFile = "dat/FINAL coded 1.XLSX",sheet = 1)
df2009x <- read.csv("dat/emptyfilled.csv",stringsAsFactors = FALSE)
df2009y <- read.xlsx(xlsxFile = "dat/CCS Code.XLSX",sheet = 1)

##### process 2018 -2019 #####
##### Fix admission dates 
df2018_1$Arrived <- as_date(dmy_hm(df2018_1$Arrived))
df2018_2$Arrived <- as_date(as.Date(df2018_2$Arrived, origin = "1899-12-30"))

##### Fix age 
df2018_1$DateofBirth <- as_date(as.Date((df2018_1$DateofBirth),origin = "1899-12-30"))
df2018_2$Date.of.Birth <- as_date(ymd((df2018_2$Date.of.Birth)))
df2018_1$AGE <- as.duration(interval(df2018_1$DateofBirth,now())) %/% as.duration(years(1))
df2018_2$AGE <- as.duration(interval(df2018_2$Date.of.Birth,now())) %/% as.duration(years(1))

##### Fix Sex, select, and rename columns 
df2018_1 %>%
  mutate(SEX = if_else(Gender == "Male","M",
                        if_else(Gender == "Female","F","Other"))) %>%
  select(AGE,SEX,Arrived,Country,City,Diagnoses,EDDisposition)%>%
  rename(age = AGE, sex = SEX, admissionDate = Arrived, country = Country,address = City, 
         icdCode = Diagnoses, disposition = EDDisposition) %>%
  mutate(icdCodeType = "ICD10CM") -> df2018_1

df2018_2 %>%
  mutate(SEX = if_else(Gender == "Male","M",
                       if_else(Gender == "Female","F","Other"))) %>%
  select(AGE,SEX,Arrived,Country,City,Diagnoses,ED.Disposition)%>%
  rename(age = AGE, sex = SEX, admissionDate = Arrived, country = Country, address = City, 
         icdCode = Diagnoses, disposition = ED.Disposition) %>%
  mutate(icdCodeType = "ICD10CM") -> df2018_2

##### Clean icdCode, map ccsCode, and fix country
df2018 <- rbind(df2018_1,df2018_2)

df2018$icdCode<-unlist(
  lapply(df2018["icdCode"][[1]], function(y){
    
    s0<- strsplit(as.character(y)," ")[[1]][1]
    s1<- strsplit(s0,",")[[1]][1]
    s2<- strsplit(s1,";")[[1]][1]
    y<- gsub("[.]","",s2)
  })
)

df2018 %>%
  left_join(ccsMap,by = c("icdCode","icdCodeType"))%>%
  filter(!is.na(ccsCode)) %>%
  mutate(country = tolower(trimws(as.character(country))))%>%
  mutate(address = tolower(trimws(as.character(address)))) %>%
  mutate(address = if_else(address == "baalbeck-hermel","balbaak hermel",address))%>%
  mutate(address = if_else(address == "akaar","akkar",address))%>%
  filter(country == "lebanon")%>%
  select(admissionDate,age,sex,address,disposition,icdCode,icdCodeType,ccsCode)%>%
  left_join(select(ccsDesc,ccsCode ,ccsCodeDesc))->df2018

##### process 2009 -2010 ####
# df2009x %>%
#   select(CASENO, ADMISSION.DATE,MARITAL.STATUS, AGE, SEX, DISPOSITION, ADDRESS.1,ICD9CM.1)%>%
#   mutate(ADMISSION.DATE = as_date(ymd(paste0("20",df2009x$ADMISSION.DATE)))) %>%
#   mutate(ADDRESS.1 = tolower(trimws(as.character(ADDRESS.1))))%>%
#   mutate(ADDRESS.1 = if_else(ADDRESS.1 %in% c("balback hermil","balbaak"),"balbaak hermel",ADDRESS.1))%>%
#   mutate(ADDRESS.1 = if_else(ADDRESS.1 %in% c("beirut","beiurt"),"beirut",ADDRESS.1))%>%
#   mutate(ADDRESS.1 = na_if(ADDRESS.1, ""))%>%
#   mutate(ADDRESS.1 = if_else(ADDRESS.1 %in% c("akkar","akaar"),"akkar",ADDRESS.1))%>%
#   filter(ADDRESS.1 != "not lebanese")%>%
#   rename(age = AGE, sex = SEX, admissionDate = ADMISSION.DATE,
#          address = ADDRESS.1, icdCode = ICD9CM.1, disposition = DISPOSITION,
#          maritalStatus = MARITAL.STATUS)%>%
#   mutate(icdCode = trimws(icdCode))%>%
#   mutate(icdCodeType = "ICD9CM")%>%
#   left_join(ccsMap,by = c("icdCode","icdCodeType")) %>%
#   #filter(!is.na(ccsCode))%>%
#   select(admissionDate,age,sex,address,disposition,icdCode,icdCodeType,ccsCode)->df2009x1

df2009y %>% 
  select(CASE.NUMBER, ADMISSION.DATE,MARITAL.STATUS, AGE, SEX, DISPOSITION, ADDRESS.1,CCS)%>%
  mutate(ADMISSION.DATE = as_date(ymd(paste0("20",df2009y$ADMISSION.DATE)))) %>%
  mutate(ADDRESS.1 = tolower(trimws(as.character(ADDRESS.1))))%>%
  mutate(ADDRESS.1 = if_else(ADDRESS.1 %in% c("balback hermil","balbaak"),"balbaak hermel",ADDRESS.1))%>%
  mutate(ADDRESS.1 = if_else(ADDRESS.1 %in% c("beirut","beiurt"),"beirut",ADDRESS.1))%>%
  mutate(ADDRESS.1 = na_if(ADDRESS.1, ""))%>%
  mutate(ADDRESS.1 = if_else(ADDRESS.1 %in% c("akkar","akaar"),"akkar",ADDRESS.1))%>%
  filter(ADDRESS.1 != "not lebanese")%>%
  mutate(icdCodeType = NA)%>%
  rename(age = AGE, sex = SEX, admissionDate = ADMISSION.DATE,
         address = ADDRESS.1, disposition = DISPOSITION,
         maritalStatus = MARITAL.STATUS,ccsCode = CCS)%>%
  mutate(icdCode = NA)%>%
  mutate(admissionDate = ymd(admissionDate))%>%
  select(admissionDate,age,sex,address,disposition,icdCode,icdCodeType,ccsCode)->df2009y1

df2009x %>%
  mutate(ccsCode = as.character(ccsCode))%>%
  select(admissionDate,age,sex,address,disposition,icdCode,icdCodeType,ccsCode)%>%
  mutate(admissionDate = dmy(admissionDate))->df2009x1

rbind(df2009x1,df2009y1) %>%
  left_join(select(ccsDesc,ccsCode ,ccsCodeDesc)%>%mutate(ccsCode = as.character(ccsCode)))%>%
  mutate(ccsCode = as.character(ccsCode))->df2009
##### align dispositions, make age numeric #####
df2009%>%
  mutate(disposition = trimws(disposition))%>%
  mutate(disposition = trimws(disposition))%>%
  mutate(disposition = tolower(trimws(as.character(disposition))))%>%
  mutate(sex = if_else(sex == " ","Other",sex))%>%
  mutate(age = as.numeric(age))%>%
  mutate(disposition = if_else(disposition %in% c("death","death on arrival","deceased"),"death",disposition))%>%
  mutate(disposition = if_else(disposition %in% c("in-patient admission","in patient admission","admitted"),"admitted",disposition))%>%
  mutate(disposition = if_else(disposition %in% c("against medical advice","left againt medical advice","ama"),"ama",disposition))%>%
  mutate(disposition = if_else(disposition %in% c("transfer to another facility","clinics","other hospital/clinic"),"transfer",disposition))%>%
  mutate(disposition = if_else(disposition %in% c("cancelled/ incomplete service","lwbs","others"),"cancelled-incomplete",disposition))->df2009
df2018%>%
  mutate(disposition = trimws(disposition))%>%
  mutate(disposition = tolower(trimws(as.character(disposition))))%>%
  mutate(disposition = if_else(disposition %in% c("death","death on arrival","deceased"),"death",disposition))%>%
  mutate(disposition = if_else(disposition %in% c("in-patient admission","in patient admission","admitted"),"admitted",disposition))%>%
  mutate(disposition = if_else(disposition %in% c("against medical advice","left againt medical advice","ama"),"ama",disposition))%>%
  mutate(disposition = if_else(disposition %in% c("transfer to another facility","clinics","other hospital/clinic"),"transfer",disposition))%>%
  mutate(disposition = if_else(disposition %in% c("cancelled/ incomplete service","lwbs","others"),"cancelled-incomplete",disposition))%>%
  mutate(ccsCode = as.character(ccsCode))->df2018
         

##### export datasets####
save(df2009, file="EDdata/processedData/admissions_2009_2010_clean")
save(df2018, file="EDdata/processedData/admissions_2018_2019_clean")
rbind(df2018,df2009)%>%
  group_by(ccsCodeDesc)%>%
  summarize (count = n())%>%
  arrange(desc(count))->x
ordered_ccs_codes <- x$ccsCodeDesc
save(ordered_ccs_codes, file = "EDdata/processedData/ordered_ccs_codes")
address<-unique(rbind(df2018,df2009)$address)
disposition <- unique(rbind(df2018,df2009)$disposition)
save(address, file = "EDdata/processedData/address")
save(disposition, file = "EDdata/processedData/disposition")
