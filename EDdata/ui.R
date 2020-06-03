library(plotly)
library(lubridate)
load(file = "processedData/ordered_ccs_codes")
load(file = "processedData/address")
load(file = "processedData/disposition")

#For dropdown menu
actionLink <- function(inputId, ...) {
    tags$a(href='javascript:void',
           id=inputId,
           class='action-button',
           ...)
}

fluidPage(
    titlePanel("ED data explorer"),
    fluidRow(
        column(3,
               wellPanel(
                   h4("Filter"),
                   selectInput("type", label = 'Coding system', 
                               choices = list("CCS" = "ccs",
                                              "Chapters" = "chapter") 
                               ),
                   dateRangeInput('dateRange09',
                                  label = 'Date range 09-10: yyyy-mm-dd',
                                  start = ymd(20091103), end = ymd(20100630)
                   ),
                   dateRangeInput('dateRange18',
                                  label = 'Date range 18-19: yyyy-mm-dd',
                                  start = ymd(20181103), end = ymd(20190630)
                   ),
                   sliderInput("age", "Age at admission:",
                               min = 0, max = 100,
                               value = c(0,100)),
                   selectizeInput(
                       'gender', 'Gender:', 
                       choices = c("M","F","Other"), 
                       multiple = TRUE,
                       selected = c("M","F","Other")
                   ),
                   selectizeInput(
                       'disposition', 'Disposition:', 
                       choices = disposition, 
                       multiple = TRUE,
                       selected = disposition
                   ),
                   selectizeInput(
                       'address', 'Mohafaza:', 
                       choices = address, 
                       multiple = TRUE,
                       selected = address
                   )
               )

        ),
        column(9,
               tabsetPanel(type = "tabs",
                           tabPanel("Timeseries", 
                                    selectInput("ccsCodeDesc", "Code",
                                                ""),
                                    plotlyOutput("plot"),
                                    textOutput("textTS09"),
                                    textOutput("textTS18"),
                                    selectInput("plottype", "Timeseries plot type", 
                                                c("Line" = "Line", "local polynomial regression" = "Smooth"), selected = "Line")
                                    
                           ),
                           
                           tabPanel("Case comparaison",
                                    sliderInput("n", "",
                                                min = 10, max = 50,pre = "top ",post = " codes",
                                                value = 20,step = 10),
                                    selectInput("sortBY", label = h4("Sort by"), 
                                                choices = list("Cumulative" = "count",
                                                               "2009-2010" = "count09", 
                                                               "2018-2019" = "count18" 
                                                               ), 
                                                selected = 1),
                                    textOutput("textPR09"),
                                    textOutput("textPR18"),
                                    
                                    plotOutput("plot2")
                                    
                                    ),
                           tabPanel("Age Comparaison", 
                                    selectInput("ccsCodeDescAge", "Code",
                                                ""),
                                    selectInput("plotTypeAge", "Plot type", 
                                                c("Frequency" = "frequency", "Density" = "density"), selected = "frequency"),
                                    plotlyOutput("plotAge")
                           ),
                           tabPanel("Odds ratios",
                                    h4("Select the population you want to compute the odds ratios for using the the panel on the left"),
                                    h4("ORs are calculated with reference to 2009-2010, OR>1 means higher risk in 2018-2019, 
                                       while OR<1 means higher risk in 2009-2010"),
                                    sliderInput("min09", "cases in 2009-2010",
                                                min = 0, max = 500,pre = "more than ",post = " cases",
                                                value = 50,step = 10),
                                    sliderInput("min18", "cases in 2018-2019",
                                                min = 0, max = 500,pre = "more than ",post = " cases",
                                                value = 50,step = 10),
                                    tags$style(type="text/css",
                                               ".shiny-output-error { visibility: hidden; }",
                                               ".shiny-output-error:before { visibility: hidden; }"
                                    ),
                                    h4("Press the button to start computation for selected filters, only results with significant p-values will plot"),
                                    h4("Computation might take a few minutes..."),
                                    actionButton(inputId = 'run',label = 'Compute Odds Ratios'),
                                    plotOutput("plot3")#,height = "auto"),
                                    
                                    )
                           )
               )
              # ggvisOutput("plot1"),
               # wellPanel(
               #     span("Number of movies selected:",
               #          textOutput("n_movies")
               #     )
               # )
        
    )
    
)
