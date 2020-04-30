library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(tidyr)
library(plotly)
library(reshape2)



timeSeriesCCS <- function(df2009,df2018,ccsCodeDescSelected, mindate09, maxdate09,
                          mindate18,maxdate18,minage,maxage,addressList,plotType){
  df2018 %>% 
    filter(ccsCodeDesc == ccsCodeDescSelected,
           admissionDate>mindate18,
           admissionDate<maxdate18,
           age<maxage,
           age>minage,
           address %in% addressList)%>%
    mutate(date = admissionDate) %>%
    #complete(date = seq.Date(min(date), max(date), by="day"))%>%
    mutate(year = "2018-2019")-> df2018p
  
  df2009 %>% 
    filter(ccsCodeDesc == ccsCodeDescSelected,
           admissionDate>mindate09,
           admissionDate<maxdate09,
           age<maxage,
           age>minage,
           address %in% addressList)%>%
    mutate(date = admissionDate+years(9)) %>%
    #complete(date = seq.Date(min(date), max(date), by="day"))%>%
    mutate(year = "2009-2010")-> df2009p
  
  dfp <- rbind(df2009p,df2018p) #merged data frame for plotting
  if (plotType == "Line"){
    dfp %>%
      mutate(date = as.character(date))%>%
      group_by(date,year,ccsCode)%>%
      summarise(count = n())->x
    as.data.frame(x)%>%mutate(date = ymd(date))->x
    x%>%
      #mutate(count = ifelse(is.na(ccsCode),0,count))%>%
      mutate(lab = paste0(count," admissions of CCS ", ccsCode))%>%
      ggplot(aes(text = paste(count," admissions of CCS ")))+
      geom_line(inherit.aes = F, aes(x = date, 
                                     y = count, colour = year, label = lab )) +
      scale_color_discrete(name="")+
      theme_bw() +
      ggtitle(paste0("Admissions of ",ccsCodeDescSelected))+
      theme(axis.title.x = element_blank(),legend.title = element_blank())+
      scale_x_date(labels = date_format("%b"))->p
  }else {
    dfp %>%
      mutate(date = as.character(date))%>%
      group_by(date,year,ccsCode)%>%
      summarise(count = n())->x
    as.data.frame(x)%>%mutate(date = ymd(date))->x
    x%>%mutate(lab = paste0(count," admissions of CCS ", ccsCode))%>%
      ggplot(aes(text = paste(count," admissions of CCS ")))+
      geom_smooth(inherit.aes = F, aes(x = date, 
                                       y = count, colour = year, label = lab )) +
      theme_bw() +
      ggtitle(paste0("Admissions of ",ccsCodeDescSelected))+
      theme(axis.title.x = element_blank())+
      scale_x_date(labels = date_format("%b"))->p
    
  }
  
  
  return(ggplotly(p, tooltip = c("label"))%>% config(displayModeBar = F))
  
  
}

pyramidPlot <- function(df2009,df2018,mindate09, maxdate09,
                        mindate18,maxdate18,minage,maxage,addressList,n){
  df2009 %>%
    filter(admissionDate>mindate09,
           admissionDate<maxdate09,
           age<maxage,
           age>minage,
           address %in% addressList)%>%
    group_by(ccsCodeDesc)%>%
    summarize(count09 = n()) ->count09
  n09<-sum(count09$count09)
  
  df2018 %>%
    filter(admissionDate>mindate18,
           admissionDate<maxdate18,
           age<maxage,
           age>minage,
           address %in% addressList)%>%
    group_by(ccsCodeDesc)%>%
    summarize(count18 = n()) ->count18
  n18<-sum(count18$count18)
  full_join(count18,count09)%>%
    replace_na (list(count09 = 0, count18 =0))%>%
    mutate(count = count09+count18,ccsCodeDesc = as.character(ccsCodeDesc))%>%
    arrange(desc(count))->counts
  counts<-counts[1:20,]
  ccsOrder <- as.character(counts$ccsCodeDesc)
  
  counts %>%
    mutate(count18 = -as.numeric(count18))%>%
    mutate(count09 = 1000*as.numeric(count09)/n09,count18 = 1000*as.numeric(count18)/n18)%>%
    select(-count)%>%
    rename(`2009-2010` = count09, `2018-2019` = count18)%>%
    melt(id = "ccsCodeDesc")%>%
    mutate(ccsCodeDesc = as.factor(ccsCodeDesc))%>%
    filter(value >= n|value <= -n)->counts
  
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
  
  return(ggplotly(p,tooltip = "text")%>% config(displayModeBar = F))
  
}


