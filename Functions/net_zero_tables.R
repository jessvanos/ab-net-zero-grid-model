## Zone Prices in a Table
Report_P <- function(case) {
  
  Years2Pivot <- c(2021,2022)    
  
  ZoneSum <- ZoneYr %>% 
    mutate(Time_Period = format(.$Time_Period, format="%Y")) %>%
    filter(Run_ID == case) %>%
    filter(Time_Period %in% Years2Pivot) #%>%
  # filter(Name== "WECC_Alberta")
  
  
  ZoneSum$Name <- factor(ZoneSum$Name,levels=c("WECC_Alberta","WECC_BritishColumbia","MRO_Saskatchewan"),ordered=TRUE)    
  
  pt <- PivotTable$new()
  pt$addData(ZoneSum)
  pt$addColumnDataGroups("Condition", addTotal=FALSE)
  pt$addRowDataGroups("Name", addTotal=FALSE) 
  pt$addColumnDataGroups("Time_Period", addTotal=FALSE) 
  pt$defineCalculation(calculationName="Price",summariseExpression=max("Price"),format="%#.2f")
  pt$renderPivot()
  
}