################################################################################
# TITLE: Table_Functions
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
  ZoneSum <- ZoneHr_All %>% 
    mutate(Time_Period = format(.$date, format="%Y")) %>%
    filter(Run_ID == case) %>%
    filter(Time_Period %in% Years2Pivot) 
  
  # Reorganize the zones. Do not need this if you only want to report AB
  #ZoneSum$Name <- factor(ZoneSum$Name,levels=c("WECC_Alberta","WECC_BritishColumbia","MRO_Saskatchewan"),ordered=TRUE)    
  
  message("")
  # Create Pivot table
  pt <- PivotTable$new() 
  {
    pt$addData(ZoneSum)
    pt$addRowDataGroups("Condition", addTotal=FALSE)
    pt$addColumnDataGroups("Time_Period",addTotal=FALSE) 
    pt$defineCalculation(calculationName="Avg Price",summariseExpression="mean(Price,na.rm=TRUE)",format="%#.2f")
    pt$renderPivot()
  } 
  
  #Write the pivot table to an excel workbook
  wb <- createWorkbook()
  addWorksheet(wb, "Avg Price Data")
  pt$writeToExcelWorksheet(wb=wb, wsName="Avg Price Data",
                           topRowNumber=1, leftMostColumnNumber=1,
                           applyStyles=TRUE, mapStylesFromCSS=TRUE)

  # Folder Name
  FoldName <-paste('Data_',SourceDB)
  
  # Save to folder called Tables (Local) on computer. Allow overwrite is version already exists with this name
  saveWorkbook(wb, here('Tables (Local)',paste("ZonePrice_",SourceDB,".csv")), overwrite = TRUE)
  pt$renderPivot()
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
  
  
  # Add cap increases manual
  Capinc<-data.frame(Name=c("Base Plant (SCR1)"),
                     Capacity=c(800),
                     Primary_Fuel=c("Cogeneration"),
                     Beg_Date=c(2024))
  
  Builddata <-  rbind(Builddata,Capinc)
  
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
  
  #Write the pivot table to an excel workbook
  wb <- createWorkbook()
  addWorksheet(wb, "New Resources (MW)")
  pt$writeToExcelWorksheet(wb=wb, wsName="New Resources (MW)",
                           topRowNumber=1, leftMostColumnNumber=1,
                           applyStyles=TRUE, mapStylesFromCSS=TRUE)
  
  # Save to folder called Tables (Local) on computer. Allow overwrite is version already exists with this name
  saveWorkbook(wb, here('Tables (Local)',paste("New_Resources_Total_MW_",SourceDB,".csv")), overwrite = TRUE)
  
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

###############################################################################  
## FUNCTION: BuildMW_Totals
## Report the capacity built for each fuel type in the study by year
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    Zone_Yr - Yearly zone information
################################################################################

################################################################################
## FUNCTION: WriteAllTables
## Filters and writes all data to cvs files for excel input. 
##
## INPUTS: 
##    
## TABLES REQUIRED: 
##    Reshr - Hourly resouce tables
################################################################################

# Filter for resource hourly information over entire study period

# INDIVIDUAL RESOURCE OUTPUTS

  # Hourly Resource info
  DataHr <- ResHr %>%
    mutate(year = year(date),
           time = date) %>%
    filter(Run_ID == case,
           Condition == "Average",
           Zone == "WECC_Alberta") %>%
    sim_filt5(.) %>%
    mutate(Report_Year=as.numeric(Report_Year),
           Simulation_Name=paste(SourceDB)) %>%
    subset(.,select=c(ID,Name,Report_Year,date,Capability,Capacity,Dispatch_Cost,Output_MWH,Capacity_Factor,
                      Fuel_Usage,Primary_Fuel,
                      Net_Cost,Total_Cost_MWh,Fixed_Cost,Variable_OM_Cost,Total_Emission_Cost,Fuel_Cost,Startup_Cost,Build_Cost,
                      Revenue,Energy_Revenue_MWh,Value,Value_MWh,
                      Used_For_Op_Reserve, Forced_Outage,Maint_Outage,Total_Hours_Run,Beg_Date,End_Date,Simulation_Name))

gc()

  # Annual Resource info
  DataYr <- ResYr %>%
    filter(Run_ID == case,
           Condition == "Average",
           Zone == "WECC_Alberta") %>%
    sim_filt5(.) %>%
    mutate(Report_Year=as.numeric(YEAR),
           Simulation_Name=paste(SourceDB)) %>%
    filter(Capacity>0) %>%
    subset(.,select=c(Name,Report_Year,Capability,Capacity,Dispatch_Cost,Output_MWH,Capacity_Factor,
                      Fuel_Usage,Primary_Fuel,
                      Net_Cost,Total_Cost_MWh,Fixed_Cost,Variable_OM_Cost,Total_Emission_Cost,Fuel_Cost,Startup_Cost,Build_Cost,
                      Revenue,Energy_Revenue_MWh,Value,Value_MWh,
                      Used_For_Op_Reserve, Forced_Outage,Maint_Outage,Total_Hours_Run,Beg_Date,End_Date,Simulation_Name))
  
# RESOURCE GROUP OUTPUTS
  
  # Resource groups over entire year
  DataGrYr <- ResGroupYr%>%
    sim_filt(.) %>% #Filter to rename fuels
    filter(Run_ID == case) %>%
    filter(Condition == "Average") %>%
    mutate(Time_Period=as.numeric(Time_Period),
           Simulation_Name=paste(SourceDB)) %>%
    subset(., select=c(Name,ID,Time_Period,Condition,Output,Output_MWH,Capacity,
                       Dispatch_Cost,Net_Cost,Fixed_Cost,Variable_OM_Cost,Variable_OM_Cost_Base,Fixed_Cost_Aux1,
                       Fuel_Usage,Percent_Marginal,Percent_Committed,Revenue,Energy_Revenue,
                       Total_Fuel_Cost,Value,Forced_Outage,Maint_Outage,Spin_Reserve,Total_Hours_Run,
                       Capacity_Factor,Storage_Charging_Cost,Simulation_Name)) 
    
  
# EMISSIONS OUTPUTS
  
# ZONE OUTPUTS
  
# TRADE OUTPUTS
  