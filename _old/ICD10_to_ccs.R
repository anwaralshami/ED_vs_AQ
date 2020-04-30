library(dplyr)
library(lubridate)
ER_adm_18 <- read.csv("ER_adm_18.csv")
ER_adm_18_0 <- read.csv("ER_adm_18_0.csv")
colnames(ER_adm_18_0)<-colnames(ER_adm_18)
ER_adm_18$Arrived <- mdy_hm(ER_adm_18$Arrived)
ER_adm_18_0$Arrived <- dmy_hm(ER_adm_18_0$Arrived)
ER_adm_18 <- rbind(ER_adm_18_0, ER_adm_18)
icd10_to_ccs <- read.csv("icd10_to_ccs.csv")

ER_adm_18$ICD.10.CM_Code<-unlist(
                  lapply(ER_adm_18["Diagnoses"][[1]], function(y){
                    
                    s0<- strsplit(as.character(y)," ")[[1]][1]
                    s1<- strsplit(s0,",")[[1]][1]
                    s2<- strsplit(s1,";")[[1]][1]
                    y<- gsub("[.]","",s2)
                  })
                )

write.csv(left_join(ER_adm_18, icd10_to_ccs)%>%
  filter(!is.na(Beta_Version_CCS_Category )) ,
  "ER_adm_18_CCS.csv"
)

write.csv(ER_adm_18 %>%
  filter(Diagnoses != ""),
  "ER_adm_18_completeCases.csv")

write.csv(left_join(ER_adm_18, icd10_to_ccs)%>%
  filter(!is.na(Beta_Version_CCS_Category ))%>%
  group_by(Beta_Version_CCS_Category, Beta_Version_CCS_Category_Description)%>%
  summarise(count = n()),
  "ER_adm_18_counts.csv")
