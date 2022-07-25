################################################################################
# TITLE: net_zero_tables
# DESCRIPTION: Functions To use for summarizing data

# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: July 22, 2022; LAST EDIT: July 25, 2022
#
################################################################################
###############################################################################  
## FUNCTION: Report_P 
## Report average zone prices in a table, organized by condition (average, peak, off-peak), 
## year, and optionally zone)
##
## INPUTS: 
##    case - Run_ID which you want to plot
##    Years2Pivot - Years to be displayed in table
## TABLES REQUIRED: 
##    Zone_Yr - Yearly zone information
################################################################################

Report_P <- function(Years2Pivot,case) {
  
  # Get zone year info and filter for case and years
  ZoneSum <- ZoneYr %>% 
    mutate(Time_Period = format(.$Time_Period, format="%Y")) %>%
    filter(Run_ID == case) %>%
    filter(Time_Period %in% Years2Pivot) #%>%
  # filter(Name== "WECC_Alberta")
  
  # Reorganize the zones. Do not need this if you only want to report AB
  ZoneSum$Name <- factor(ZoneSum$Name,levels=c("WECC_Alberta","WECC_BritishColumbia","MRO_Saskatchewan"),ordered=TRUE)    
  
  # Create Pivot table
  pt <- PivotTable$new() 
  {
    pt$addData(ZoneSum)
    pt$addColumnDataGroups("Condition", addTotal=FALSE)
    pt$addRowDataGroups("Name", addTotal=FALSE) 
    pt$addColumnDataGroups("Time_Period", addTotal=FALSE) 
    pt$defineCalculation(calculationName="Price",summariseExpression=max("Price"),format="%#.2f")
  pt$renderPivot() # Display in viewer
  } 
  
}