load(file="admissions_2009_2010_clean")
load(file="processedData/admissions_2018_2019_clean")
load(file = "processedData/ordered_ccs_codes")
load(file = "processedData/address")
load(file = "processedData/disposition")

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
dispositionList <- disposition
n <- 20
sortBY <- "count"
df2009 %>%
  filter(admissionDate>mindate09,
         admissionDate<maxdate09,
         age<maxage,
         age>minage,
         address %in% addressList,
         disposition %in% dispositionList,
         sex %in% genderList)%>%
  group_by(ccsCodeDesc)%>%
  summarize(count09 = n()) ->count09
n09<-sum(count09$count09)

df2018 %>%
  filter(admissionDate>mindate18,
         admissionDate<maxdate18,
         disposition %in% dispositionList,
         age<maxage,
         age>minage,
         address %in% addressList,
         sex %in% genderList)%>%
  group_by(ccsCodeDesc)%>%
  summarize(count18 = n()) ->count18
n18<-sum(count18$count18)
full_join(count18,count09)%>%
  replace_na (list(count09 = 0, count18 =0))%>%
  mutate(count = count09+count18,ccsCodeDesc = as.character(ccsCodeDesc))%>%
  arrange(desc(!!as.name(sortBY)))->counts
counts<-counts[1:n,]
ccsOrder <- as.character(counts$ccsCodeDesc)

counts %>%
  mutate(count18 = -as.numeric(count18))%>%
  mutate(count09 = 1000*as.numeric(count09)/n09,count18 = 1000*as.numeric(count18)/n18)%>%
  select(-count)%>%
  rename(`2009-2010` = count09, `2018-2019` = count18)%>%
  melt(id = "ccsCodeDesc")%>%
  mutate(ccsCodeDesc = as.factor(ccsCodeDesc))->counts#%>%
#filter(value >= n|value <= -n)->counts

counts$ccsCodeDesc <- factor(counts$ccsCodeDesc,levels = ccsOrder)

p<-ggplot(counts,aes(x=ccsCodeDesc, y=value, fill= variable, text=paste0(round(abs(value),2)," per 1000 cases of \n",ccsCodeDesc, " in ",variable)))+
  geom_bar(stat = "identity", position = "identity",width=0.6)+
  geom_text(aes(label=round(abs(value),2)),vjust = ifelse(counts$value >= 0, 0.5, 0.5),size=2.5) +
  scale_y_continuous(labels=abs)+
  scale_fill_discrete(name = "", labels = c("2018-2019", "2009-2010"))+
  theme_light()+
  xlab("CCS code")+
  ylab("count")+
  ggtitle("Occurrence per 1000 for selected filters")+
  theme(axis.text.x = element_blank(),
        axis.title = element_blank())+
  coord_flip()
p

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
