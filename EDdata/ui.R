library(plotly)
library(lubridate)
load(file = "processedData/ordered_ccs_codes")
load(file = "processedData/address")



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
                       'address', 'Mohafaza:', 
                       choices = address, 
                       multiple = TRUE,
                       selected = address
                   )
                   #,textOutput('test')   ## Uncommenting this line, it works.
               )

        ),
        column(9,
               tabsetPanel(type = "tabs",
                           tabPanel("Timeseries", 
                                    selectInput("ccsCodeDesc", "CCS category",
                                                ordered_ccs_codes),
                                    plotlyOutput("plot"),
                                    textOutput("textTS09"),
                                    textOutput("textTS18"),
                                    selectInput("plottype", "Timeseries plot type", 
                                                c("Line" = "Line", "local polynomial regression" = "Smooth"), selected = "Line")
                                    
                           ),
                           
                           tabPanel("Case comparaison",plotlyOutput("plot2"),
                                    textOutput("textPR09"),
                                    textOutput("textPR18")),
                           tabPanel("Odds ratios",
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
