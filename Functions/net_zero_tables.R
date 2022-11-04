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
    pt$evaluatePivot()
  
  #Write the pivot table to an excel workbook
  wb <- createWorkbook()
  addWorksheet(wb, "Avg Price Data")
  pt$writeToExcelWorksheet(wb=wb, wsName="Avg Price Data", 
                           topRowNumber=1, leftMostColumnNumber=1, 
                           applyStyles=TRUE, mapStylesFromCSS=TRUE)
  
  saveWorkbook(wb, here('Figures (Local)',paste("ZonePrice_",case,"_",SourceDB,".xlsx")), overwrite = TRUE)
  
  
  pt$renderPivot() # Display in viewer
  
  } 
  
  
  
}

###############################################################################  
## FUNCTION: BuildMW_Totals
## Report the capacity built for each fuel type in the study by year
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    Zone_Yr - Yearly zone information
################################################################################

Build_Totals <- function(case) {
  
  # Bring in Resource Year Table and filter columns
  data <- ResYr%>%
    sim_filt3(.) %>% #Filter to rename fuels
    subset(., select=c(Name,Condition,Capacity,Peak_Capacity,End_Date,Beg_Date,Run_ID,Primary_Fuel,Time_Period,Capacity_Factor)) %>%
    filter(Run_ID == case) %>%
    filter(Condition == "Average") 
  
  
  # Set levels to each category in order specified
  data$Primary_Fuel <- factor(data$Primary_Fuel, levels=c("Coal-to-Gas", "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                                          "Blended  Simple Cycle","Blended  Combined Cycle",
                                                          "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                                                          "Hydro", "Other",
                                                          "Wind", "Solar", 
                                                          "Storage - Battery", "Storage - Compressed Air", "Storage - Pumped Hydro", 
                                                          "Cogeneration") )
  
  # Replace the capacity with the peak / actual capacity and not just what is available
  for (i in 1:length(data$Capacity)) {
    if (data[i,"Capacity"] < data[i,"Peak_Capacity"]) {
      data[i,"Capacity"] <- data[i,"Peak_Capacity"]
    }
  }
    
    #Get Year max for run and filter for end dates BEFORE this date
  MaxYr <- max(ResYr$YEAR)
  MinYr <- min(ResYr$YEAR)
  
  # Give current date so that already built are not listed as new additions
  CurDate <- as.Date("08/10/2022", 
                     format = "%m/%d/%Y")
  data$FiltDate  <- as.Date(data$Beg_Date, 
                            format = "%m/%d/%Y")
  
  data$Beg_Date  <- as.Date(data$Beg_Date, 
                            format = "%m/%d/%Y")
  data$Beg_Date <- format(data$Beg_Date,format="%Y")
  
  Builddata <- data %>%
    filter(.,Beg_Date <= MaxYr) %>%
    filter(.,FiltDate > CurDate) %>%
    filter(.,Capacity>0) %>%
    filter(Beg_Date==Time_Period) %>%
    select(., c("Name","Capacity","Primary_Fuel","Beg_Date"))
  
  #Now group everything together
  Builddata <- Builddata%>%
    group_by(Beg_Date,Primary_Fuel) %>%
    summarise(Capacity = sum(Capacity)) %>%
    ungroup() %>%
    mutate(Capacity=round(Capacity,digits = 0))
  
  # Send to a pivot table
  pt <- PivotTable$new() 
  {
    pt$addData(Builddata)
    pt$addColumnDataGroups("Primary_Fuel", addTotal=FALSE)
    pt$addRowDataGroups("Beg_Date", addTotal=FALSE) 
    pt$defineCalculation(calculationName="Capacity",summariseExpression=max("Capacity"))
    pt$evaluatePivot()
   
    pt$renderPivot() # Display in viewer
  }
}

###############################################################################  
## FUNCTION: BuildMW_A_Totals
## Report the capacity built for each fuel type in the study by year
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    Zone_Yr - Yearly zone information
################################################################################

Build_A_Totals <- function(case) {
  
  data <- Build %>%
    filter(Run_ID == case & LT_Iteration == max(LT_Iteration) & 
             Time_Period != "Study")%>%
    group_by(Fuel_Type, Time_Period) %>%
    summarise(Units = sum(Units_Built), Capacity = sum(Capacity_Built)) %>%
    sim_filt4(.) %>%
    ungroup() %>%
    mutate(Capacity=round(Capacity,digits = 0))
  
  levels(data$Fuel_Type) <- c("Hydrogen","Natual Gas and Hydrogen Blend","Natural Gas", 
                              "Hydro", "Other",
                              "Wind", "Solar", 
                              "Storage - Battery", "Storage - Compressed Air", "Storage - Pumped Hydro")
  
  
  # Send to a pivot table
  pt <- PivotTable$new() 
  {
    pt$addData(data)
    pt$addColumnDataGroups("Fuel_Type", addTotal=FALSE)
    pt$addRowDataGroups("Time_Period", addTotal=FALSE) 
    pt$defineCalculation(calculationName="Cap",summariseExpression=max("Capacity"))
    pt$evaluatePivot()
    
    pt$renderPivot() # Display in viewer
  }
}