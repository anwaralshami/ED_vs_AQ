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

##### Load datasets #####
df2018_1 <- read.xlsx(xlsxFile = "dat/ED Visits-Nov 2018 to July 2019.xlsx",sheet = 1,detectDates = T)
df2018_2 <- read.xlsx(xlsxFile = "dat/Data set 2- 2018-2019.xlsx",sheet = 1,detectDates = T)
df2009 <- read.xlsx(xlsxFile = "dat/Data 2009-2010.XLSX",sheet = 1)
ccsMap <- read.xlsx(xlsxFile = "dat/ccs_icd_map.xlsx",sheet = 1)
ccsDesc <- read.xlsx("dat/ccs_code_desc.xlsx",sheet=1)

##### Fix admission dates #####
df2018_1$Arrived <- as_date(dmy_hm(df2018_1$Arrived))
df2018_2$Arrived <- as_date(as.Date(df2018_2$Arrived, origin = "1899-12-30"))
df2009$ADMISSION.DATE <- as_date(ymd(paste0("20",df2009$ADMISSION.DATE)))

##### Fix age #####
df2018_1$DateofBirth <- as_date(as.Date((df2018_1$DateofBirth),origin = "1899-12-30"))
df2018_2$Date.of.Birth <- as_date(ymd((df2018_2$Date.of.Birth)))
df2018_1$AGE <- as.duration(interval(df2018_1$DateofBirth,now())) %/% as.duration(years(1))
df2018_2$AGE <- as.duration(interval(df2018_2$Date.of.Birth,now())) %/% as.duration(years(1))

##### Fix Sex, select, and rename columns #####
df2018_1 %>%
  mutate(SEX = if_else(Gender == "Male","M",
                        if_else(Gender == "Female","F","Unknown"))) %>%
  select(AGE,SEX,Arrived,Country,City,Diagnoses)%>%
  rename(age = AGE, sex = SEX, admissionDate = Arrived, country = Country,address = City,  icdCode = Diagnoses) %>%
  mutate(icdCodeType = "ICD10CM") -> df2018_1

df2018_2 %>%
  mutate(SEX = if_else(Gender == "Male","M",
                       if_else(Gender == "Female","F","Unknown"))) %>%
  select(AGE,SEX,Arrived,Country,City,Diagnoses)%>%
  rename(age = AGE, sex = SEX, admissionDate = Arrived, country = Country, address = City, icdCode = Diagnoses) %>%
  mutate(icdCodeType = "ICD10CM") -> df2018_2

df2009 %>%
  select(AGE,SEX,ADMISSION.DATE,ADDRESS.1,ICD9CM.1)%>%
  rename(age = AGE, sex = SEX, admissionDate = ADMISSION.DATE, address = ADDRESS.1, icdCode = ICD9CM.1)%>%
  mutate(icdCodeType = "ICD9CM") -> df2009

##### Clean icdCode, map ccsCode, and fix country #####
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
  select(admissionDate,age,sex,country,address,icdCode,icdCodeType,ccsCode)%>%
  left_join(select(ccsDesc,ccsCode ,ccsCodeDesc))->df2018

df2009 %>%
  mutate(icdCode = trimws(icdCode))%>%
  left_join( ccsMap,by = c("icdCode","icdCodeType")) %>%
  filter(!is.na(ccsCode)) %>%
  mutate(address = tolower(trimws(as.character(address))))%>%
  mutate(address = if_else(address == "beiurt","beirut",address))%>%
  mutate(address = if_else(address == "uk,","uk",address))%>%
  mutate(country = if_else(address %in% c("beirut","mount lebanon","south lebanon","north lebanon","nabatiye","beqaa","balbaak hermel","akaar"),"lebanon",address))%>%
  select(admissionDate,age,sex,country,address,icdCode,icdCodeType,ccsCode)%>%
  left_join(select(ccsDesc,ccsCode ,ccsCodeDesc))->df2009

##### select only Lebanese ######
df2018%>%
  filter(country == "lebanon")->df2018

df2009%>%
  filter(country == "lebanon")->df2009

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
save(address, file = "EDdata/processedData/address")
