library(dplyr)
library(tidyr)
library(treemapify)

source("EDdata/funs.R")
load(file="EDdata/processedData/admissions_2009_2010_clean")
load(file="EDdata/processedData/admissions_2018_2019_clean")

df2009 %>% 
  as_tibble()%>%
  mutate(Year = 2009)%>%
  rbind(
    mutate(as_tibble(df2018),Year = 2018)
  )->df0918

df0918%>%
  #mutate(age_bin = binning(age,type = "equal",nbins = 7))%>%
  mutate(age_bin = ifelse(age>=0 & age <=5, "0-5",
                          ifelse(age>=6 & age <=12, "6-12",
                                 ifelse(age>=13 & age <=18, "13-18",
                                        ifelse( age>=19 & age <=39, "19-39",
                                                ifelse( age>=40 & age <=59, "40-59",
                                                        ifelse( age>=60 & age <=74, "60-74",
                                                                ifelse(age>=75, ">75",NA))))))))%>%
  group_by(age_bin,Year)%>%
  summarize(count = n())%>%
  spread(key = Year,value = count) %>%
  write.csv(file = "summaryTables/age.csv")

df0918%>%
  group_by(sex,Year)%>%
  summarize(count = n())%>%
  spread(key = Year,value = count)%>%
  write.csv(file = "summaryTables/sex.csv")

df0918%>%
  group_by(disposition,Year)%>%
  summarize(count = n())%>%
  spread(key = Year,value = count)%>%
  write.csv(file = "summaryTables/disposition.csv")

df0918%>%
  group_by(address,Year)%>%
  summarize(count = n())%>%
  spread(key = Year,value = count)%>%
  write.csv(file = "summaryTables/region.csv")

df0918 %>%
  mutate(age_bin = ifelse(age>=0 & age <=5, "0-5",
                          ifelse(age>=6 & age <=12, "6-12",
                                 ifelse(age>=13 & age <=18, "13-18",
                                        ifelse( age>=19 & age <=39, "19-39",
                                                ifelse( age>=40 & age <=59, "40-59",
                                                        ifelse( age>=60 & age <=74, "60-74",
                                                                ifelse(age>=75, ">75",NA))))))))%>%
  group_by(ccsCodeDesc,age_bin,Year)%>%
  summarize(count = n())%>%
  spread(key = Year,value = count)->topReasons

topReasons%>%
  select(-`2018`)%>%
  spread(key = age_bin,value = `2009`)%>%
  write.csv(file = "summaryTables/topReasons2009.csv")

topReasons%>%
  select(-`2009`)%>%
  spread(key = age_bin,value = `2018`)%>%
  write.csv(file = "summaryTables/topReasons2018.csv") 


ccsList <- c("Tuberculosis","Septicemia (except in labor)",
             "Bacterial infection; unspecified site","Mycoses",
             "HIV infection","Hepatitis","Viral infection",
             "Inflammation; infection of eye (except that caused by tuberculosis or sexually transmitteddisease)",
             "Other infections; including parasitic",
             "Sexually transmitted infections (not HIV or hepatitis)",
             "Immunizations and screening for infectious disease","Cancer of head and neck",
             "Cancer of esophagus","Cancer of stomach","Cancer of liver and intrahepatic bile duct",
             "Cancer of other GI organs; peritoneum","Cancer of bronchus; lung",
             "Cancer of bone and connective tissue","Other non-epithelial cancer of skin",
             "Cancer of breast")
ageVsCCS_facet <- function(df2018,df2009,ccsList){
  df2018 %>% 
    mutate(date = admissionDate) %>%
    #complete(date = seq.Date(min(date), max(date), by="day"))%>%
    mutate(year = "2018-2019")-> df2018p
  
  df2009 %>% 
    mutate(date = admissionDate+years(9)) %>%
    #complete(date = seq.Date(min(date), max(date), by="day"))%>%
    mutate(year = "2009-2010")-> df2009p
  
  dfp <- rbind(df2009p,df2018p) #merged data frame for plotting
  # dfp %>%
  #       ggplot(aes(age, color = year)) +
  #       geom_freqpoly(binwidth = 1)+
  #       theme_bw() +
  #       ggtitle(paste0("Admissions of "))+
  #       facet_wrap(~ChapterDesc)
  dfp %>%
    dplyr::filter(ccsCodeDesc %in% ccsList)%>%
    ggplot(aes(age, color = year)) +
    geom_density()+
    theme_bw() +
    facet_grid(ChapterDesc~ccsCodeDesc)->p
  return(p)
}


df2018%>%
  as_tibble()%>%
  mutate_if(sapply(df2009, is.character), as.factor)%>%
  rename(chapter = Chapter,chapterDesc = ChapterDesc)%>%
  group_by(chapter,ccsCode,ccsCodeDesc,chapterDesc)%>%
  summarize(count = n()) %>%
  as.data.frame()%>%
  mutate(chapter = fct_explicit_na(chapter),
         ccsCodeDesc = fct_explicit_na(ccsCodeDesc),
         ccsCode = fct_explicit_na(ccsCode)) %>%
  na.rm()%>%
  ggplot( ggplot2::aes(area = count, fill = chapter, label = ccsCodeDesc, subgroup = chapterDesc)) +
  geom_treemap(colour = "grey90",size=2)+
  geom_treemap_text(fontface = "italic", colour = "white",
                    grow = TRUE,place = "topleft", reflow = T)+
  geom_treemap_subgroup_border()+
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.4, colour =
                               "black", fontface = "italic", reflow = T, min.size = 0)+
  theme(legend.position = "none")->p18

df2009%>%
  as_tibble()%>%
  mutate_if(sapply(df2009, is.character), as.factor)%>%
  rename(chapter = Chapter,chapterDesc = ChapterDesc)%>%
  group_by(chapter,ccsCode,ccsCodeDesc,chapterDesc)%>%
  summarize(count = n()) %>%
  as.data.frame()%>%
  mutate(chapter = fct_explicit_na(chapter),
         ccsCodeDesc = fct_explicit_na(ccsCodeDesc),
         ccsCode = fct_explicit_na(ccsCode)) %>%
  na.rm()%>%
  ggplot( ggplot2::aes(area = count, fill = chapter, label = ccsCodeDesc, subgroup = chapterDesc)) +
  geom_treemap(colour = "grey90",size=2)+
  geom_treemap_text(fontface = "italic", colour = "white",
                    grow = TRUE,place = "topleft", reflow = T)+
  geom_treemap_subgroup_border()+
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.4, colour =
                               "black", fontface = "italic", reflow = T, min.size = 0)+
  theme(legend.position = "none")->p09
ggsave( "summaryTables/p18.jpg",p18, device = "jpeg",dpi = "retina",width = 4.74*5,height=2.28*5)  
ggsave( "summaryTables/p09.jpg",p09, device = "jpeg",dpi = "retina",width = 4.74*5,height=2.28*5)  
