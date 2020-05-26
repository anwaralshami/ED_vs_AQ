library(dplyr)
load(file="processedData/admissions_2009_2010_clean")
load(file="processedData/admissions_2018_2019_clean")

selectCount <- function(dtf,ageNum,gl){
  if (gl == "l"){
    return(
      dtf%>%
        filter(age < ageNum)%>%
        group_by(ccsCode,ccsCodeDesc)%>%
        summarize(count = n())
    )
  }else{
    return(
      dtf%>%
        filter(age > ageNum)%>%
        group_by(ccsCode,ccsCodeDesc)%>%
        summarize(count = n())
    )
  }
  
  
}

write.csv(selectCount(df2009,18,"l"),file = "processedData/2009pedCount.csv")
write.csv(selectCount(df2009,18,"g"),file = "processedData/2009adltCount.csv")
write.csv(selectCount(df2018,18,"l"),file = "processedData/2018pedCount.csv")
write.csv(selectCount(df2018,18,"g"),file = "processedData/2018adltCount.csv")