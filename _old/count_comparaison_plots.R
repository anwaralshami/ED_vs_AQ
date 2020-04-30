library(dplyr)
library(reshape2)
library(ggplot2)
count_08 <- read.csv("ER_adm_08_counts.csv",stringsAsFactors = F)
count_18 <- read.csv("ER_adm_18_counts.csv",stringsAsFactors = F)

count_18 %>%
  select(Beta_Version_CCS_Category,Beta_Version_CCS_Category_Description,count)%>%
  mutate(count = as.numeric(count))%>%
  rename(CCS_Category = Beta_Version_CCS_Category,count_18 = count) -> count_18


count_08<-count_08[complete.cases(count_08),]

counts <- left_join(count_18,count_08)

counts%>%
  select(Beta_Version_CCS_Category_Description,count_18,count_08)%>%
  mutate(count = count_08+count_18,Beta_Version_CCS_Category_Description = as.character(Beta_Version_CCS_Category_Description))%>%
  arrange(desc(count)) ->counts

order <- as.character(counts$Beta_Version_CCS_Category_Description)


counts %>%
  mutate(count_18 = -as.numeric(count_18))%>%
  mutate(count_08 = 1000*as.numeric(count_08)/38641,count_18 = 1000*as.numeric(count_18)/57774)%>%
  select(-count)%>%
  melt(id = "Beta_Version_CCS_Category_Description")%>%
  mutate(Beta_Version_CCS_Category_Description = as.factor(Beta_Version_CCS_Category_Description))%>%
  filter(value > 20|value < -20)->countsa
  
counts <- countsa
counts$Beta_Version_CCS_Category_Description <- factor(counts$Beta_Version_CCS_Category_Description,levels = order)
  #filter(!is.na(CCS_Category))%>%
  
p<-ggplot(counts,aes(x=Beta_Version_CCS_Category_Description, y=value, fill= variable))+
    geom_bar(stat = "identity", position = "identity",width=0.6)+
    geom_text(aes(label=round(abs(value),2)),vjust = ifelse(counts$value >= 0, 0.5, 0.5),size=2.5) +
    scale_y_continuous(labels=abs)+
    scale_fill_discrete(name = "ED adm year", labels = c("2018", "2008"))+
    theme_light()+
    xlab("CCS code")+
    ylab("count")+
    ggtitle("Occurrence per 1000 admissions")+
    theme(axis.text.x = element_blank(),
          axis.title = element_blank())+
    coord_flip()
p

ggsave("18vs08.jpg",plot = p,width = 10,height = 6)

sum(na.omit(counts$count_18))
sum(na.omit(counts$count_08))

