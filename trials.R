load(file="processedData/admissions_2009_2010_clean")
load(file="processedData/admissions_2018_2019_clean")
load(file = "processedData/ordered_ccs_codes")
load(file = "processedData/address")

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
n <- 20

x<-oddsRatioPlot(df2009,df2018, mindate09, maxdate09,
                          mindate18,maxdate18,minage,maxage,addressList,genderList)
x[[1]]
nrow(x[[1]])
x

