load(file="processedData/admissions_2009_2010_clean")
load(file="processedData/admissions_2018_2019_clean")
#setwd("~/genRepo/AUBMC_health_vs-AQ/EDdata")
source("funs.R")

 function(input, output, session) {
     
     output$plot <-renderPlotly({
         ccsCodeDescSelected <- input$ccsCodeDesc
         mindate09 <- input$dateRange09[1]
         maxdate09 <- input$dateRange09[2]
         mindate18 <- input$dateRange18[1]
         maxdate18 <- input$dateRange18[2]
         minage <- input$age[1]
         maxage <- input$age[2]
         addressList <- input$address
         genderList <- input$gender
         plotType <- input$plottype
         
         timeSeriesCCS(df2009,df2018,ccsCodeDescSelected, mindate09, maxdate09,
                       mindate18,maxdate18,minage,maxage,addressList,genderList,plotType)
         
                       
         
         })
     output$plot2 <-renderPlotly({
         #ccsCodeDescSelected <- input$ccsCodeDesc
         mindate09 <- input$dateRange09[1]
         maxdate09 <- input$dateRange09[2]
         mindate18 <- input$dateRange18[1]
         maxdate18 <- input$dateRange18[2]
         minage <- input$age[1]
         maxage <- input$age[2]
         addressList <- input$address
         genderList <- input$gender
         n <- 20
         #plotType <- input$plottype
         
         pyramidPlot(df2009,df2018,mindate09, maxdate09,
                     mindate18,maxdate18,minage,maxage,addressList,genderList,n)
         
         
         
     })
     output$textTS09 <- renderText({
             ccsCodeDescSelected <- input$ccsCodeDesc
             mindate09 <- input$dateRange09[1]
             maxdate09 <- input$dateRange09[2]
             mindate18 <- input$dateRange18[1]
             maxdate18 <- input$dateRange18[2]
             minage <- input$age[1]
             maxage <- input$age[2]
             addressList <- input$address
             genderList <- input$gender
             ncase09TS<-df2009 %>% 
                     filter(ccsCodeDesc == ccsCodeDescSelected,
                            admissionDate>mindate09,
                            admissionDate<maxdate09,
                            age<maxage,
                            age>minage,
                            address %in% addressList,
                            sex %in% genderList)%>%
                     nrow
             paste0(ncase09TS, " cases in 2009-2010 for selected filters") 
             
     })
     output$textTS18 <- renderText({
             ccsCodeDescSelected <- input$ccsCodeDesc
             mindate09 <- input$dateRange09[1]
             maxdate09 <- input$dateRange09[2]
             mindate18 <- input$dateRange18[1]
             maxdate18 <- input$dateRange18[2]
             minage <- input$age[1]
             maxage <- input$age[2]
             addressList <- input$address
             genderList <- input$gender
             ncase18TS<-df2018 %>% 
                     filter(ccsCodeDesc == ccsCodeDescSelected,
                            admissionDate>mindate18,
                            admissionDate<maxdate18,
                            age<maxage,
                            age>minage,
                            address %in% addressList,
                            sex %in% genderList)%>%
                     nrow
             paste0(ncase18TS, " cases in 2018-2019 for selected filters") 
             
     })
     output$textPR09 <- renderText({
             ccsCodeDescSelected <- input$ccsCodeDesc
             mindate09 <- input$dateRange09[1]
             maxdate09 <- input$dateRange09[2]
             mindate18 <- input$dateRange18[1]
             maxdate18 <- input$dateRange18[2]
             minage <- input$age[1]
             maxage <- input$age[2]
             genderList <- input$gender
             addressList <- input$address
             ncase09TS<-df2009 %>% 
                     filter(admissionDate>mindate09,
                            admissionDate<maxdate09,
                            age<maxage,
                            age>minage,
                            address %in% addressList,
                            sex %in% genderList)%>%
                     nrow
             paste0(ncase09TS, " cases in 2009-2010 for selected filters") 
             
     })
     output$textPR18 <- renderText({
             ccsCodeDescSelected <- input$ccsCodeDesc
             mindate09 <- input$dateRange09[1]
             maxdate09 <- input$dateRange09[2]
             mindate18 <- input$dateRange18[1]
             maxdate18 <- input$dateRange18[2]
             genderList <- input$gender
             minage <- input$age[1]
             maxage <- input$age[2]
             addressList <- input$address
             ncase18TS<-df2018 %>% 
                     filter(admissionDate>mindate18,
                            admissionDate<maxdate18,
                            age<maxage,
                            age>minage,
                            address %in% addressList,
                            sex %in% genderList)%>%
                     nrow
             paste0(ncase18TS, " cases in 2018-2019 for selected filters") 
             
     })
     rv <- reactiveValues(plotvals=NULL)
     observeEvent(input$run,{
             req(input$run)
             mindate09 <- input$dateRange09[1]
             maxdate09 <- input$dateRange09[2]
             mindate18 <- input$dateRange18[1]
             maxdate18 <- input$dateRange18[2]
             minage <- input$age[1]
             maxage <- input$age[2]
             addressList <- input$address
             genderList <- input$gender
             rv$plotvals <- oddsRatioDat(df2009,df2018, mindate09, maxdate09,
                                         mindate18,maxdate18,minage,maxage,addressList,genderList)
             })
 
     
     output$plot3 <- renderPlot({
             req(input$run)
             
             oddsRatioPlot(rv$plotvals)
     },height = function(){as.numeric(session$clientData$output_plot3_height)*nrow(rv$plotvals)/15 })
     
     output$textWidth <- renderText({
             #paste((session$clientData$output_plot3_width))
            x<-(as.numeric(session$clientData$output_plot3_height))
            paste(x)
             #set_shiny_plot_height_with_respects_to_width(session,"output_plot3_width")
     })
 }