library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(tidyr)
library(plotly)
library(reshape2)
library(forcats)
library(questionr)
library(epitools)


timeSeriesCCS <- function(df2009,df2018,ccsCodeDescSelected, mindate09, maxdate09,
                          mindate18,maxdate18,minage,maxage,addressList,genderList,dispositionList,plotType){
  df2018 %>% 
    filter(ccsCodeDesc == ccsCodeDescSelected,
           admissionDate>mindate18,
           admissionDate<maxdate18,
           age<maxage,
           age>minage,
           address %in% addressList,
           disposition %in% dispositionList,
           sex %in% genderList)%>%
    mutate(date = admissionDate) %>%
    #complete(date = seq.Date(min(date), max(date), by="day"))%>%
    mutate(year = "2018-2019")-> df2018p
  
  df2009 %>% 
    filter(ccsCodeDesc == ccsCodeDescSelected,
           admissionDate>mindate09,
           admissionDate<maxdate09,
           disposition %in% dispositionList,
           age<maxage,
           age>minage,
           address %in% addressList,
           sex %in% genderList)%>%
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
                        mindate18,maxdate18,minage,maxage,addressList,genderList,
                        dispositionList,n,sortBY){
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
    scale_y_continuous(labels=abs)+
    scale_fill_discrete(name = "", labels = c("2018-2019", "2009-2010"))+
    
    theme_light(base_size = 15)+
    geom_text(aes(label=round(abs(value),2)),vjust = ifelse(counts$value >= 0, 0.5, 0.5),
              size= 4) +
    xlab("CCS code")+
    ylab("count")+
    ggtitle("Occurrence per 1000 for selected filters")+
    theme(axis.text.x = element_blank(),
          axis.title = element_blank())+
    coord_flip()
  return(p)
  #return(ggplotly(p,tooltip = "text")%>% config(displayModeBar = F))
  
}

oddsRatioDat <- function(df2009,df2018, mindate09, maxdate09,
                          mindate18,maxdate18,minage,maxage,addressList,genderList,
                         dispositionList,min09,min18){
  df2009 %>%
    filter(admissionDate>mindate09,
           admissionDate<maxdate09,
           age<maxage,
           age>minage,
           address %in% addressList,
           disposition %in% dispositionList,
           sex %in% genderList)%>%
    group_by(ccsCodeDesc)%>%
    summarize(count09 = n())%>% 
    filter(count09 >= min09)->count09
  
  
  df2018 %>%
    filter(admissionDate>mindate18,
           admissionDate<maxdate18,
           disposition %in% dispositionList,
           age<maxage,
           age>minage,
           address %in% addressList,
           sex %in% genderList)%>%
    group_by(ccsCodeDesc)%>%
    summarize(count18 = n())%>% 
    filter(count18 >= min18) ->count18
  
  joinCounts <- inner_join(count09,count18)
  odds <- as.data.frame(select(joinCounts,-ccsCodeDesc))
  rownames(odds)<-joinCounts$ccsCodeDesc
  colnames(odds)<-c("2009-10","2018-19")
  totals <- data.frame(EDyear = c("2009-10","2018-19"),
                       EDcount = c(sum(odds$`2009-10`),sum(odds$`2018-19`))
  )
  oddsRatios <- generateOdds_5z(1,as.matrix(odds),totals,c("2009-10","2018-19"))                     
  
  for (i in 2:nrow(odds)){
    #print(i)
    oddsRatios<-rbind(oddsRatios,generateOdds_5z(i,as.matrix(odds),totals,c("2009-10","2018-19"))) 
  }
  
  oddsRatios %>% 
    filter(exposure == "2018-19")%>%
    select(disease,Case,Control,estimate,lower,upper,midp.exact)%>%
    filter(midp.exact <= 0.05)%>%
    mutate(disease = fct_reorder(disease, desc(estimate)))%>%
    arrange((estimate))->oddsRatios2
  
  return(oddsRatios2)
}

oddsRatioPlot <- function(oddsRatios2){
  
  
  p <- ggplot(oddsRatios2, aes(x = estimate, y = disease)) + 
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
    geom_errorbarh(aes(xmax = upper, xmin = lower), size = .5, height = 
                     .2, color = "gray50") +
    geom_point(size = 3.5, color = "orange") +
    scale_x_continuous(trans="log",breaks = c(0.1,0.5,1,2,5,10,20,50,100,200 )) +
    #coord_trans(x = 'log10') +
    theme_bw()+
    theme(panel.grid.minor = element_blank()) +
    ylab("") +
    xlab("Odds ratio (log scale)") +
    annotate(geom = "text", y =1.1, x = log10(0.5), 
             label = "Model p < 0.05", size = 3.5, hjust = 0)
  
  p
}

generateOdds_5z <- function(i,matDat,totals,expNames) {
  d0<-as.data.frame.matrix(matDat)[i,]
  dis<-rownames(d0)
  cases <- t(d0)
  colnames(cases)<-c("case")
  cases<- as.data.frame(cases)
  cases$batata <- as.numeric(totals[[2]])-cases[1]
  as.matrix(cases)
  epitools::oddsratio.midp(as.matrix(cases))
  
  tapw <- expNames
  outc <- c("Case", "Control")
  dat <- as.matrix(cases)
  dimnames(dat) <- list("exposure" = tapw, "Outcome" = outc)
  x<-epitools::oddsratio(dat, rev="c",)
  
  y<-as.data.frame(cbind(dat,x$measure,x$p.value))
  y$exposure <- rownames(y)
  y$disease <- dis
  return(y)
}    

set_shiny_plot_height_with_respects_to_width <- function(session, output_width_name){
  width <- function() { 
    session$clientData[[output_width_name]] 
  }
  # Do something with the width
  #round(as.numeric(width) *2)
  width
}

codingSystem <- function(df,codeSystem){
  if (codeSystem == "ccs"){
    return(
      df%>%
        select(-Chapter,-ChapterDesc)
    )
  }
  if (codeSystem == "chapter"){
    return(
      df%>%
        select(-ccsCode,-ccsCodeDesc)%>%
        rename(ccsCode = Chapter, ccsCodeDesc = ChapterDesc)%>%
        filter(!is.na(ccsCodeDesc))
    )
  }
}
