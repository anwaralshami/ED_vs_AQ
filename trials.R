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
df2009x <- df2009
df2018x <- df2018

->a
->b
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


unlist(t)
as.character(t)
typeof(addressList)

ccsToChapter <- read.csv("dat/CCS to chapters.csv")
ccsToChapter%>%
  mutate(ccsCode = as.character(ccsCode),Chapter = as.character(Chapter)) ->ccsToChapter
x<-left_join(df2009,ccsToChapter)
x<-left_join(df2018,ccsToChapter)
x%>% filter(is.na(Chapter))

df <- df2009



xx<-codingSystem(df2009,"chapter")

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
df2009x <- df2009
df2018x <- df2018


