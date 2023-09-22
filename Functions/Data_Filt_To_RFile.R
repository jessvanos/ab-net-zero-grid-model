################################################################################
## FUNCTION: AnnualDataR
## Writes all relevant annual data to an seperate r file to combine with other cases later.
## INPUTS: 
##    ScenarioName - Name of scenario (EX: "BAU:)
##    case - Filter by run case
## TABLES REQUIRED: 
##    ResGroupEmYr
##    ResGroupYr
##    ResYr
##    FuelYr
##    ZoneYr
################################################################################

AnnualDataR<- function(ScenarioName,case){
  
################################################################################
## ANNUAL RESOURCE GROUP OUTPUT 
## Resource Group annual Information: 2022-MaxYr Gives info on resource group 
## costs, outputs, and emission costs. 
################################################################################
  { #First get annual emissions data
    DataGrEmYr<-ResGroupEmYr%>%
      # Filter to rename fuels
      sim_filt5(.) %>% 
      # Filter for case
      filter(Run_ID == case,
             Condition == "Average",
             Type=="CO2") %>%
      # Make cost in dollars, and convert Ton to tonne
      mutate(Year=year(Time_Period),
             Emissions_Cost=Cost*1000,
             Emissions_Tonne=Amount*0.90718474) %>%
      # Filter out dates after max year
      filter(Year<=MaxYrStudy) %>%
      # Chose what to keep
      subset(., select=c(ID,Year,Type,Emissions_Tonne,Emissions_Cost))%>%
      rename(Plant_Type=ID)
    
    # Resource groups over entire year
    DataGrYr <- ResGroupYr%>%
      sim_filt5(.) %>% #Filter to rename fuels
      filter(Run_ID == case) %>%
      filter(Condition == "Average") %>%
      # Convert all costs to dollars from $000
      mutate(Year=year(Time_Period),
             Sim_Name=paste(SourceDB),
             Revenue=Revenue*1000,
             Total_Fuel_Cost=Total_Fuel_Cost*1000,
             Fixed_Cost=Fixed_Cost*1000,
             Fixed_Cost_Base=Fixed_Cost_Base*1000,
             Variable_OM_Cost=Variable_OM_Cost*1000,
             Value=Value*1000,
             Total_Cost=Value-Revenue,
             Storage_Charging_Cost*1000,
             Capacity=round(Capacity,digits=0)) %>%
      # Filter out dates after MaxYr
      filter(Year<=MaxYrStudy) %>%
      # Choose what to keep
      subset(., select=c(ID,Year,Output_MWH,Capacity,
                         Dispatch_Cost,Fuel_Usage,
                         Total_Fuel_Cost,Fixed_Cost,Fixed_Cost_Base,Variable_OM_Cost,
                         Total_Cost,Revenue,Value,
                         Total_Hours_Run,Percent_Marginal,
                         Capacity_Factor,Sim_Name)) %>%
      rename(Plant_Type=ID,
             Capacity_MW=Capacity,
             Avg_Dispatch_Cost=Dispatch_Cost,
             Fuel_Usage_GJ=Fuel_Usage)
    
    # Combine the Emissions information with the annual resource information 
    AllDataGrYr1<-merge(DataGrYr,DataGrEmYr,by=c("Plant_Type","Year"), all.x = TRUE)
    
    # Re-sort the columns
    AllDataGrYr <- AllDataGrYr1 %>%
      subset(., select=c(Plant_Type,Year,Output_MWH,Capacity_MW,
                         Avg_Dispatch_Cost,Fuel_Usage_GJ,
                         Total_Fuel_Cost,Fixed_Cost_Base,Fixed_Cost,Variable_OM_Cost,
                         Emissions_Tonne,Emissions_Cost,
                         Total_Cost,Revenue,Value,
                         Total_Hours_Run,Percent_Marginal,
                         Capacity_Factor,Sim_Name)) %>%
      rename("Fixed_O&M"=Fixed_Cost_Base,
             "Variable_O&M"=Variable_OM_Cost)
    
    # Save R file
    saveRDS(AllDataGrYr, here("Data Files","Case Specific R Files",paste("ResGrYr_",ScenarioName, sep = "")))
  }
################################################################################
## EMISSION PERFORMANCE CREDIT VALUES
################################################################################
  # Here we get value of carbon credits and cost
  EPC_Values <- AllDataGrYr1 %>%
     filter(Plant_Type %in% c("Wind","Solar","Hydro")) %>%
    mutate(Emissions_Cost=Emissions_Cost*-1,
          Credit=Emissions_Cost/Output_MWH,
           EPC_Perc_of_Rev=abs(100*Emissions_Cost/(Revenue+Emissions_Cost))) %>%
    arrange(Year) %>%
     subset(select=c(Plant_Type,Year,Output_MWH,Emissions_Cost,Revenue,Value,
                     Credit,EPC_Perc_of_Rev,Sim_Name)) %>%
     rename("EPC_Revenue"=Emissions_Cost,
            "EPC_Value_$/MWh"=Credit,
            "Other_Revenue"=Revenue)
  
  # Save R file
  saveRDS(EPC_Values, here("Data Files","Case Specific R Files",paste("EPC_Values_",ScenarioName, sep = "")))
  
################################################################################
## ANNUAL CAPACITY CHANGE
## Resource Group annual Information: 2022-MaxYr Gives info on resource group 
## costs, outputs, and emission costs. 
################################################################################
  {
    # Bring in Resource Year Table and filter for relevant data. Format date columns
    Add_Ret_data <- ResYr%>%
      sim_filt6(.) %>% #Filter to rename fuels
      subset(., select=c(Name,Condition,Capacity,Nameplate_Capacity,End_Date,Beg_Date,
                         Run_ID,Primary_Fuel,Time_Period,Capacity_Factor)) %>%
      filter(Run_ID == case)%>%
      mutate(Time_Period=as.numeric(Time_Period),
             End_Date=as.Date(End_Date,format = "%m/%d/%Y"),
             End_Year=year(End_Date),
             Beg_Date=as.Date(Beg_Date,format = "%m/%d/%Y"),
             Beg_Year=year(Beg_Date))%>%
      filter(Condition == "Average",
             Time_Period<=MaxYrStudy) 
    
    # Set levels to each category in order specified
    Add_Ret_data$Primary_Fuel <- factor(Add_Ret_data$Primary_Fuel, 
                                        levels=c("Coal","Coal-to-Gas", "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                                  #"Blended  Simple Cycle","Blended  Combined Cycle",
                                                  "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle CCS Retrofit",
                                                  "Natural Gas Combined Cycle", 
                                                  "Hydro", "Other",
                                                  "Wind", "Solar", 
                                                  "Storage - Battery", "Storage - Compressed Air", "Storage - Pumped Hydro", 
                                                  "Cogeneration"))
    
    # FILTER CAP RETIREMENTS
    #Further filter peak capacity >0 (it is not yet retired), and end date = time period (to ensure you dont get doubles)
    Retdata <- Add_Ret_data%>%
      group_by(Name)%>%
      mutate(In_Cap=max(Nameplate_Capacity))%>%
      ungroup()%>%
      filter(End_Year==Time_Period)%>%
      subset(select=c("Name","In_Cap","Primary_Fuel","Capacity_Factor","Beg_Date","Beg_Year","End_Date","End_Year"))%>%
      mutate(Type="Retirement")%>%
      arrange(.,End_Date)
    
    # FILTER CAP ADDITIONS
    Builddata <- Add_Ret_data %>%
      filter(Beg_Year >= 2022,
             Beg_Year==Time_Period) %>%
      group_by(Name)%>%
      mutate(In_Cap=max(Nameplate_Capacity))%>%
      ungroup()%>%
      select(., c("Name","In_Cap","Primary_Fuel","Capacity_Factor","Beg_Date","Beg_Year","End_Date","End_Year")) %>%
      mutate(Type="Addition")%>%
      arrange(.,Beg_Date)
    
    # Add cap increases manual
    Capinc<-data.frame(Name=c("Base Plant (SCR1)"),
                       In_Cap=c(800),
                       Primary_Fuel=c("Cogeneration"),
                       Capacity_Factor=NA,
                       Beg_Date=c(as.Date("07/01/2024", 
                                          format = "%m/%d/%Y")),
                       Beg_Year=c(2024),
                       End_Date=NA,
                       End_Year=NA,
                       Type="Addition")
    
    # Add the manual plant to the rest
    Builddata <-  rbind(Builddata,Capinc)%>%
      arrange(.,Beg_Date)
    
    # Gets specific plants
    Indv_Change <-rbind(Retdata,Builddata) %>%
      mutate(Sim_Name=paste(SourceDB),
             Capacity_Factor=Capacity_Factor) %>%
      rename("Installed_Cap"=In_Cap,
             "Plant_Type"=Primary_Fuel)
    
    # NOW PUT IT ALL TOGETHER TO GET TOTALS BY RESOURCE TYPE
    BuilddataTot <- Builddata%>%
      group_by(Primary_Fuel, Beg_Year) %>%
      summarise(Capacity_Added = sum(In_Cap))%>%
      mutate(Year=Beg_Year,
             Capacity_Added=Capacity_Added)%>%
      subset(select=c(Primary_Fuel,Year,Capacity_Added))
    
    RetdatadataTot <- Retdata%>%
      group_by(Primary_Fuel, End_Year) %>%
      summarise(Capacity_Retired = sum(In_Cap))%>%
      mutate(Year=End_Year,
             Capacity_Retired=Capacity_Retired)%>%
      subset(select=c(Primary_Fuel,Year,Capacity_Retired))
    
    # Get summary for each year!
    Tot_Change<-merge(BuilddataTot,RetdatadataTot,by=c("Primary_Fuel","Year"), all.x = TRUE, all.y = TRUE)
    
    # Replace NA values with 0
    Tot_Change[is.na(Tot_Change)]=0
    
    # Find the capacity difference in given year
    AnnualCap_Change <- Tot_Change %>%
      mutate("Difference_MW"=Capacity_Added-Capacity_Retired,
             Sim_Name=paste(SourceDB)) %>%
      arrange(Year)%>%
      rename("Plant_Type"=Primary_Fuel)
    
    # Save R file
    saveRDS(AnnualCap_Change, here("Data Files","Case Specific R Files",paste("Ann_Cap_",ScenarioName, sep = "")))
  }
  
################################################################################
## TOTAL CAPACITY CHANGE
## Resource Group annual Information: 2022-MaxYr Gives info on resource group 
## costs, outputs, and emission costs. 
################################################################################
  
  # FIND TOTAL CAPACITY CHANGES OVERALL
  # Added in Manually:
  # Define 2023 additions automatically
  AESO_2024<-Tot_Change %>%
    filter(Year<2025)%>%
    group_by(Primary_Fuel)%>%
    summarize(AESO_Add=sum(Capacity_Added))
  
  # Get Canyon
  Canyon_PS3<- Tot_Change %>%
    filter(Year<2027,
           Primary_Fuel=="Storage - Pumped Hydro")%>%
    group_by(Primary_Fuel)%>%
    summarize(AESO_Add=sum(Capacity_Added))
  
  AESO_Add <-rbind(AESO_2024,Canyon_PS3)
  
  # Aurora 
  # Get additions from 2025-MaxYr
  AllCap_Changes1a <- Tot_Change %>%
    filter(Year>2024)%>%
    group_by(Primary_Fuel)%>%
    summarize(Capacity_Added=sum(Capacity_Added))%>%
    ungroup()
  
  # Get retirements 2022-MaxYr
  AllCap_Changes1b <- Tot_Change %>%
    group_by(Primary_Fuel)%>%
    summarize(Capacity_Retired=sum(Capacity_Retired))%>%
    ungroup()
  
  AllCap_Changes1c <-merge(AllCap_Changes1a,AllCap_Changes1b,by=c("Primary_Fuel"), all.x = TRUE, all.y = TRUE)
  
  # Put it all together
  AllCap_Changes2 <-merge(AESO_Add,AllCap_Changes1c,by=c("Primary_Fuel"), all.x = TRUE, all.y = TRUE)
  # Replace NA values with 0
  AllCap_Changes2[is.na(AllCap_Changes2)]=0
  
  # Now get final values
  AllCap_Changes2 <- AllCap_Changes2 %>%  
    mutate(Total_Added=AESO_Add+Capacity_Added,
           Scenario=SourceDB)%>%
    subset(select=c(Primary_Fuel,AESO_Add,Capacity_Added,Total_Added,Capacity_Retired,Scenario)) %>%
    rename("Plant_Type"=Primary_Fuel,
           "New_Cap_Total"=Total_Added,
           "New_Cap_AESO"=AESO_Add,
           "New_Cap_AURORA"=Capacity_Added,
           "Cap_Retired"=Capacity_Retired)
  
  # Save R file
  saveRDS(AllCap_Changes2, here("Data Files","Case Specific R Files",paste("Tot_Cap_",ScenarioName, sep = "")))
  
################################################################################
## ANNUAL AVERAGE HEAT RATES
## Resource Group annual Information: 2022-MaxYr Gives info on resource group 
## costs, outputs, and emission costs. 
################################################################################
  {
    # Bring in Resource Year Table and filter for relevant data. Format date columns
    AVG_HeatRates <- ResYr%>%
      sim_filt6(.) %>% #Filter to rename fuels
      subset(., select=c(Name,Condition,Capacity,Peak_Capacity,
                         Full_Load_Heat_Rate,Net_Heat_Rate,Incr_Heat_Rate,
                         Run_ID,Primary_Fuel,Time_Period,Capacity_Factor)) %>%
      filter(Run_ID == case)%>%
      mutate(Time_Period=as.numeric(Time_Period))%>%
      filter(Condition == "Average",
             Capacity>0,
             Time_Period<=MaxYrStudy,
             !Primary_Fuel %in% c("Wind","Solar","Hydro",
                                  "Storage - Battery", "Storage - Compressed Air", "Storage - Pumped Hydro")) %>%
      group_by(Primary_Fuel,Time_Period)%>%
      summarise("Full_Load_Heat_Rate"=mean(Full_Load_Heat_Rate),
                "Net_Heat_Rate"=mean(Net_Heat_Rate),
                "Incr_Heat_Rate"=mean(Incr_Heat_Rate),
                "Avg_CF"=mean(Capacity_Factor)) %>%
      rename("Plant_Type"=Primary_Fuel)%>%
      mutate(Scenario=SourceDB)
    
    # Save R file
    saveRDS(AVG_HeatRates, here("Data Files","Case Specific R Files",paste("Ann_HR_",ScenarioName, sep = "")))
    
  }
  
################################################################################
## ANNUAL FUEL TABLES
################################################################################
  {
    FuelData <- FuelYr %>%
      # Filter for case
      filter(Run_ID == case,
             Condition == "Average") %>%
      sim_filtFuel(.) %>%
      mutate(Cost=Cost*1000,
             Year=year(Time_Period),
             Year<=MaxYrStudy,
             Scenario=SourceDB) %>%
      subset(select=c(ID,Year,Price,Cost,Usage)) %>%
      rename("Usage_GJ"=Usage,
             "Price_$/GJ"=Price)
    
    # Save R file
    saveRDS(FuelData, here("Data Files","Case Specific R Files",paste("Ann_Fuel_",ScenarioName, sep = "")))
  }
  
################################################################################
## ANNUAL ZONE INFO
################################################################################
  {
    # First we need the total energy output for this year!
    AnnualOut <-AllDataGrYr %>%
      group_by(Year)%>%
      summarise(Total_Output=sum(Output_MWH))
    
    ZnData <- ZoneYr %>%
      mutate(Year = year(Time_Period),
             time = Time_Period) %>%
      filter(Run_ID == case,
             Condition == "Average",
             Name == "WECC_Alberta",
             Year<=MaxYrStudy) %>%
      mutate(Report_Year=as.numeric(Report_Year),
             Scenario=SourceDB,
             Production_Cost_Total=1000*Production_Cost_Total,
             Fixed_Cost_Total=1000*Fixed_Cost_Total)%>%
      subset(.,select=c(Name,Year,Price, 
                        Demand_Total,Net_Load_Total,
                        Production_Cost_Total,Fixed_Cost_Total,Imports_Total,Exports_Total,Scenario))
    
    # Get the emissions in each year
    DataAnnualEm <- DataGrEmYr %>%
      filter(Plant_Type!="Cogeneration")%>%
      group_by(Year)%>%
      summarise(NonCogen_Emissions=sum(Emissions_Tonne)/1000000)%>%
      ungroup()
    
    # Combine the total output in MWh with zone info
    AllZoneData1a<-merge(ZnData,AnnualOut,by=c("Year"), all.x = TRUE)
    AllZoneData1b<-merge(AllZoneData1a,DataAnnualEm,by=c("Year"), all.x = TRUE)
    
    # Now, rename some columns and get unit prices
    AllZoneData <-AllZoneData1b %>%
      mutate(Unit_Prod=round(Production_Cost_Total/Total_Output,digits=2),
             Unit_Fix=round(Fixed_Cost_Total/Total_Output,digits=2),
             Price=round(Price,digits=2),
             Imports_Total=round(Imports_Total,digits=0),
             Exports_Total=round(Exports_Total,digits=0),
             Production_Cost_Total=Production_Cost_Total/1000000000,
             Fixed_Cost_Total=Fixed_Cost_Total/1000000000,
             Total_Costs=Production_Cost_Total+Fixed_Cost_Total) %>%
      subset(.,select=c(Name,Year,Price, 
                        Demand_Total,Net_Load_Total,Total_Output,
                        Production_Cost_Total,Unit_Prod,Fixed_Cost_Total,Unit_Fix,Total_Costs,
                        Imports_Total,Exports_Total,NonCogen_Emissions,
                        Scenario)) %>%
      rename("Avg_Price"=Price)
    
    # Save R file
    saveRDS(AllZoneData, here("Data Files","Case Specific R Files",paste("Zone_",ScenarioName, sep = "")))
  }
}
 