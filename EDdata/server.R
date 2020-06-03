load(file="processedData/admissions_2009_2010_clean")
load(file="processedData/admissions_2018_2019_clean")
#setwd("~/genRepo/AUBMC_health_vs-AQ/EDdata")
source("funs.R")

 function(input, output, session) {
         df2009x <- reactive({
                 type <- input$type
                 #type <- "ccs"
                 codingSystem(df2009, type)
         })
         df2018x <- reactive({
                 type <- input$type
                 #type <- "ccs"
                 codingSystem(df2018, type)
         })
         
         observeEvent(
                 input$type,{
                         mindate09 <- input$dateRange09[1]
                         maxdate09 <- input$dateRange09[2]
                         mindate18 <- input$dateRange18[1]
                         maxdate18 <- input$dateRange18[2]
                         minage <- input$age[1]
                         maxage <- input$age[2]
                         addressList <- input$address
                         genderList <- input$gender
                         dispositionList <- input$disposition
                         plotType <- input$plottype
                         df2009x <- df2009x()
                         df2018x <- df2018x()
                         rbind(df2009x%>%
                                       filter(
                                               admissionDate>mindate09,
                                               admissionDate<maxdate09,
                                               age<maxage,
                                               age>minage,
                                               address %in% addressList,
                                               disposition %in% dispositionList,
                                               sex %in% genderList
                                       ),
                               df2018x%>%
                                       filter(
                                               admissionDate>mindate18,
                                               admissionDate<maxdate18,
                                               age<maxage,
                                               age>minage,
                                               address %in% addressList,
                                               disposition %in% dispositionList,
                                               sex %in% genderList
                                       ))%>%
                                 group_by(ccsCodeDesc)%>%
                                 summarize(count =n())%>%
                                 arrange(desc(count))%>%
                                 select(-count)%>%
                                 unique()%>%
                                 as.list()%>%
                                 unname()%>%
                                 unlist->t
                        updateSelectInput(session, "ccsCodeDesc", "Code",
                                   choices = t)
                        updateSelectInput(session, "ccsCodeDescAge", "Code",
                                          choices = t)
                 })
         
     
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
         dispositionList <- input$disposition
         plotType <- input$plottype
         df2009x <- df2009x()
         df2018x <- df2018x()
         
         
         timeSeriesCCS(df2009x,df2018x,ccsCodeDescSelected, mindate09, maxdate09,
                       mindate18,maxdate18,minage,maxage,addressList,genderList,dispositionList,plotType)
         
                       
         
         })
     
     output$plot2 <-renderPlot({
         #ccsCodeDescSelected <- input$ccsCodeDesc
         mindate09 <- input$dateRange09[1]
         maxdate09 <- input$dateRange09[2]
         mindate18 <- input$dateRange18[1]
         maxdate18 <- input$dateRange18[2]
         minage <- input$age[1]
         maxage <- input$age[2]
         addressList <- input$address
         genderList <- input$gender
         dispositionList <- input$disposition
         n <- input$n
         sortBY <- input$sortBY#"count"
         #plotType <- input$plottype
         df2009x <- df2009x()
         df2018x <- df2018x()
         
         pyramidPlot(df2009x,df2018x,mindate09, maxdate09,
                     mindate18,maxdate18,minage,maxage,addressList,genderList,dispositionList,n,sortBY)
         
         
         
     },height = function(){as.numeric(session$clientData$output_plot2_height)*input$n/20 })
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
             dispositionList <- input$disposition
             ncase09TS<-df2009x() %>% 
                     filter(ccsCodeDesc == ccsCodeDescSelected,
                            admissionDate>mindate09,
                            admissionDate<maxdate09,
                            age<maxage,
                            age>minage,
                            address %in% addressList,
                            disposition %in% dispositionList,
                            sex %in% genderList)%>%
                     nrow
             paste0(ncase09TS, " cases in 2009-2010 for selected filters") 
             
     })
     
     output$plotAge <-renderPlotly({
             ccsCodeDescSelected <- input$ccsCodeDescAge
             mindate09 <- input$dateRange09[1]
             maxdate09 <- input$dateRange09[2]
             mindate18 <- input$dateRange18[1]
             maxdate18 <- input$dateRange18[2]
             minage <- input$age[1]
             maxage <- input$age[2]
             addressList <- input$address
             genderList <- input$gender
             dispositionList <- input$disposition
             plotTypeAge <- input$plotTypeAge
             df2009x <- df2009x()
             df2018x <- df2018x()
             
             ageVsCCS(df2009x,df2018x,ccsCodeDescSelected, mindate09, maxdate09,
                                  mindate18,maxdate18,minage,maxage,addressList,genderList,
                      dispositionList,plotTypeAge)
                     
             
            
             
             
     })
     
     output$textTS18 <- renderText({
             ccsCodeDescSelected <- input$ccsCodeDesc
             mindate09 <- input$dateRange09[1]
             maxdate09 <- input$dateRange09[2]
             mindate18 <- input$dateRange18[1]
             maxdate18 <- input$dateRange18[2]
             dispositionList <- input$disposition
             minage <- input$age[1]
             maxage <- input$age[2]
             addressList <- input$address
             genderList <- input$gender
             ncase18TS<-df2018x() %>% 
                     filter(ccsCodeDesc == ccsCodeDescSelected,
                            admissionDate>mindate18,
                            admissionDate<maxdate18,
                            age<maxage,
                            age>minage,
                            address %in% addressList,
                            disposition %in% dispositionList,
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
             dispositionList <- input$disposition
             minage <- input$age[1]
             maxage <- input$age[2]
             genderList <- input$gender
             addressList <- input$address
             ncase09TS<-df2009x() %>% 
                     filter(admissionDate>mindate09,
                            admissionDate<maxdate09,
                            age<maxage,
                            age>minage,
                            disposition %in% dispositionList,
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
             dispositionList <- input$disposition
             minage <- input$age[1]
             maxage <- input$age[2]
             addressList <- input$address
             ncase18TS<-df2018x() %>% 
                     filter(admissionDate>mindate18,
                            admissionDate<maxdate18,
                            age<maxage,
                            age>minage,
                            address %in% addressList,
                            disposition %in% dispositionList,
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
             dispositionList <- input$disposition
             minage <- input$age[1]
             maxage <- input$age[2]
             min09 <- input$min09
             min18 <- input$min18
             df2009x <- df2009x()
             df2018x <- df2018x()
             addressList <- input$address
             genderList <- input$gender
             rv$plotvals <- oddsRatioDat(df2009x,df2018x, mindate09, maxdate09,
                                         mindate18,maxdate18,minage,maxage,addressList,
                                         genderList,dispositionList,min09,min18)
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