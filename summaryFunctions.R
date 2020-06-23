library(dplyr)
library(tidyr)
library(treemapify)
library(ggpubr)

source("EDdata/funs.R")
load(file="EDdata/processedData/admissions_2009_2010_clean")
load(file="EDdata/processedData/admissions_2018_2019_clean")

##### make combined df for summarizing  ######
df2009 %>% 
  as_tibble()%>%
  mutate(Year = 2009)%>%
  rbind(
    mutate(as_tibble(df2018),Year = 2018)
  )->df0918


##### age table #####
df0918%>%
  #mutate(age_bin = binning(age,type = "equal",nbins = 7))%>%
  mutate(age_bin = ifelse(age>=0 & age <=5, "0-5",
                          ifelse(age>=6 & age <=12, "6-12",
                                 ifelse(age>=13 & age <=18, "13-18",
                                        ifelse( age>=19 & age <=44, "19-44",
                                                ifelse( age>=45 & age <=64, "45-64",
                                                        ifelse(age>=65, ">65",NA)))))))%>%
  group_by(age_bin,Year)%>%
  summarize(count = n())%>%
  spread(key = Year,value = count) %>%
  write.csv(file = "summaryTables/age.csv")

##### sex table #####
df0918%>%
  group_by(sex,Year)%>%
  summarize(count = n())%>%
  spread(key = Year,value = count)%>%
  write.csv(file = "summaryTables/sex.csv")

##### disposition table #####
df0918%>%
  group_by(disposition,Year)%>%
  summarize(count = n())%>%
  spread(key = Year,value = count)%>%
  write.csv(file = "summaryTables/disposition.csv")

##### region table #####
df0918%>%
  group_by(address,Year)%>%
  summarize(count = n())%>%
  spread(key = Year,value = count)%>%
  write.csv(file = "summaryTables/region.csv")

##### top reasons table #####
df0918 %>%
  mutate(age_bin = ifelse(age>=0 & age <=5, "0-5",
                          ifelse(age>=6 & age <=12, "6-12",
                                 ifelse(age>=13 & age <=18, "13-18",
                                        ifelse( age>=19 & age <=44, "19-44",
                                                ifelse( age>=45 & age <=64, "45-64",
                                                        ifelse(age>=65, ">65",NA)))))))%>%
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

##### facet plots #####

## shorten some ccs code Descriptions
df2009 %>%
  mutate(ccsCodeDesc = ifelse(ccsCodeDesc == "Pneumonia (except that caused by tuberculosis or sexually transmitted disease)",
                              "Pneumonia",
                              ifelse(ccsCodeDesc == "Chronic obstructive pulmonary disease and bronchiectasis",
                                     "COPD and bronchiectasis",
                                     ifelse(ccsCodeDesc == "Coronary atherosclerosis and other heart disease",
                                            "Coronary atherosclerosis",ccsCodeDesc))))->df2009y
df2018 %>%
  mutate(ccsCodeDesc = ifelse(ccsCodeDesc == "Pneumonia (except that caused by tuberculosis or sexually transmitted disease)",
                              "Pneumonia",
                              ifelse(ccsCodeDesc == "Chronic obstructive pulmonary disease and bronchiectasis",
                                     "COPD and bronchiectasis",
                                     ifelse(ccsCodeDesc == "Coronary atherosclerosis and other heart disease",
                                            "Coronary atherosclerosis",ccsCodeDesc))))->df2018y
# select ccs codes to plot, meke sure same number of plots per row for alignment
ccsList <- list()
ccsList[["Mental"]] <- c("Anxiety disorders", "Mood disorders", "Substance-related disorders","","","")
ccsList[["Certain"]] <- c("Viral infection", "Bacterial infection; unspecified site", "Other infections; including parasitic","","","")
ccsList[["Endocrine"]] <- c("Thyroid disorders", "Diabetes mellitus with complications", "Diabetes mellitus without complication","","","")
ccsList[["heart"]] <- c("Essential hypertension", "Coronary atherosclerosis", "Congestive heart failure; nonhypertensive", "Cardiac dysrhythmias", "Nonspecific chest pain","")
ccsList[["Respiratory"]] <- c("Other upper respiratory disease", "Other upper respiratory infections", "Other lower respiratory disease", "Pneumonia", "Asthma", "COPD and bronchiectasis")

ageDensity <- function(df2018,df2009,ccs,xTitle,lPos){
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
    dplyr::filter(ccsCodeDesc == ccs)%>%
    ggplot(aes(age, color = year)) +
    geom_density()+
    ggtitle(ccs)+
    xlim(0,100)+
    theme_minimal() +
    theme(legend.position = lPos,plot.title = element_text(size = 10),
          
          axis.title.x= element_blank(),
          axis.title.y = element_blank())->p
  if (xTitle == T){
    return(p)
  }
  if (xTitle == F){
    return(p+theme(axis.text.x = element_blank()))
  }
  
}

ageDensityPlotList <- function(df2018,df2009,ccsList,xTitle,lPos){
    xTitle <- xTitle
    lPos <- lPos
    pl<-sapply(ccsList, function(col) {
      ageDensity(df2018,df2009,col,xTitle,lPos)
    }, simplify=FALSE)
    
  return(pl)
}

#plot
pa <- ggarrange(ggarrange(plotlist = ageDensityPlotList(df2018y, df2009y, ccsList$Mental , F, "null"),nrow = 1),
                ggarrange(plotlist = ageDensityPlotList(df2018y, df2009y, ccsList$Certain , F, "null"),nrow = 1),
                ggarrange(plotlist = ageDensityPlotList(df2018y, df2009y, ccsList$Endocrine , F, "null"),nrow = 1),
                ggarrange(plotlist = ageDensityPlotList(df2018y, df2009y, ccsList$heart , F, "null"),nrow = 1),
                ggarrange(plotlist = ageDensityPlotList(df2018y, df2009y, ccsList$Respiratory , T, "null"),nrow = 1),
                nrow = 5)                

ggsave(plot = ageDensity(df2018,df2009,ccs = "Asthma",F,lPos = "right"),device = "png",filename = "l.png",dpi = 1000)

#save
ggexport( pa, filename = "y.png",width = 1000*10,height = 468*10,res = 600)

##### treemaps #####
mdcTree <- function (df, lp){
  df%>%
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
    ggplot( ggplot2::aes(area = count, fill = chapterDesc, label = ccsCodeDesc, subgroup = chapterDesc)) +
    geom_treemap(colour = "grey90",size=2)+
    geom_treemap_text(fontface = "italic", colour = "white",
                      grow = TRUE,place = "topleft", reflow = T)+
    geom_treemap_subgroup_border()+
    theme(text = element_text(size=20),
          legend.position = lp)->p
  return(p)
}
ggsave( "summaryTables/p18.jpg",mdcTree(df2018,"none"), device = "jpeg",dpi = "retina",width = 4.74*5,height=2.28*5)  
ggsave( "summaryTables/p09.jpg",mdcTree(df2009,"none"), device = "jpeg",dpi = "retina",width = 4.74*5,height=2.28*5)  
ggsave( "summaryTables/p18L.jpg",mdcTree(df2018,"right"), device = "jpeg",dpi = "retina",width = 4.74*5,height=2.28*5)  
ggsave( "summaryTables/p09L.jpg",mdcTree(df2009,"right"), device = "jpeg",dpi = "retina",width = 4.74*5,height=2.28*5)  
