load(file="admissions_2009_2010_clean")
load(file="processedData/admissions_2018_2019_clean")
load(file = "processedData/ordered_ccs_codes")
load(file = "processedData/address")

source("funs.R")
###### Merge years and align dates for plotting ######

mindate09 <- ymd(20090630)
maxdate09 <- ymd(20100630)
mindate18 <- ymd(20181103)
maxdate18 <- ymd(20191104)
minage <-50
maxage <-60
addressList <- address
genderList <- c("M","F", "Other")
n <- 20

test <- df2009x
test %>%
  select(CASENO, ADMISSION.DATE,MARITAL.STATUS, AGE, SEX, DISPOSITION, ADDRESS.1,ICD9CM.1)%>%
  mutate(ADMISSION.DATE = as_date(ymd(paste0("20",df2009x$ADMISSION.DATE)))) %>%
  mutate(ADDRESS.1 = tolower(trimws(as.character(ADDRESS.1))))%>%
  mutate(ADDRESS.1 = if_else(ADDRESS.1 %in% c("balback hermil","balbaak"),"balbaak hermel",ADDRESS.1))%>%
  mutate(ADDRESS.1 = if_else(ADDRESS.1 %in% c("beirut","beiurt"),"beirut",ADDRESS.1))%>%
  mutate(ADDRESS.1 = na_if(ADDRESS.1, ""))%>%
  mutate(ADDRESS.1 = if_else(ADDRESS.1 %in% c("akkar","akaar"),"akkar",ADDRESS.1))%>%
  filter(ADDRESS.1 != "not lebanese")%>%
  rename(age = AGE, sex = SEX, admissionDate = ADMISSION.DATE,
         address = ADDRESS.1, icdCode = ICD9CM.1, disposition = DISPOSITION,
         maritalStatus = MARITAL.STATUS)%>%
  mutate(icdCode = trimws(icdCode))%>%
  mutate(icdCodeType = "ICD9CM")%>%
  select(CASENO,icdCode,icdCodeType)%>%
  left_join(ccsMap,by = c("icdCode","icdCodeType"))%>%
  filter(is.na(ccsCode))%>%
  distinct(icdCode)
test2 <- read.csv("dat/emptyfilled.csv",stringsAsFactors = FALSE)
test2 %>%
  mutate(ccsCode = as.character(ccsCode))%>%
  select(admissionDate,age,sex,address,disposition,icdCode,icdCodeType,ccsCode)->df2009x1

rbind(df2009x1,df2009y1)%>%
  filter(is.na(ccsCode))

df2009%>%
  filter(ccsCode == "7")

selectCount <- function(dtf,ageNum,gl){
  if (gl == "l"){
    return(
      dtf%>%
        filter(age < ageNum)%>%
        group_by(ccsCode,ccsCodeDesc)%>%
        summarize(count = n())
    )
  }else{
    return(
      dtf%>%
        filter(age > ageNum)%>%
        group_by(ccsCode,ccsCodeDesc)%>%
        summarize(count = n())
    )
  }
  
  
}

write.csv(selectCount(df2009,18,"l"),file = "processedData/2009pedCount.csv")
write.csv(selectCount(df2009,18,"g"),file = "processedData/2009adltCount.csv")
write.csv(selectCount(df2018,18,"l"),file = "processedData/2018pedCount.csv")
write.csv(selectCount(df2018,18,"g"),file = "processedData/2018adltCount.csv")
