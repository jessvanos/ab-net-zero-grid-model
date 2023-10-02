################################################################################
## FUNCTION: AnnaulDataExcel
## Writes all relevant annual data to an excel file on different sheets.
## INPUTS: 
##    ScenarioName (CaseName) - Name of scenario (EX: "BAU:)
##    case - Filter by run case
## TABLES REQUIRED: 
##    ResGroupEmYr
##    ResGroupYr
##    ResYr
##    FuelYr
##    ZoneYr
################################################################################

AnnualDataExcel<- function(ScenarioName,NameShort,case){

################################################################################
## ANNUAL RESOURCE GROUP OUTPUT TABLES
## Resource Group annual Information: 2022-MaxYr Gives info on resource group 
## costs, outputs, and emission costs. 
################################################################################
  {
    # FILTER OUT EMISSIONS
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
             Emissions_Tonne=Amount*0.90718474,
             Emissions_Mt=Emissions_Tonne/1000000) %>%
      # Filter out dates after MaxYr
      filter(Year<=MaxYrStudy) %>%
      # Chose what to keep
      subset(., select=c(ID,Year,Type,Emissions_Tonne,Emissions_Mt,Emissions_Cost))%>%
      rename(Plant_Type=ID)
    
    # ANNUAL RESOURCE GROUP DATA
    DataGrYr <- ResGroupYr%>%
      sim_filt5(.) %>% #Filter to rename fuels
      filter(Run_ID == case) %>%
      filter(Condition == "Average") %>%
      # Convert all costs to dollars from $000
      mutate(Year=year(Time_Period),
             Sim_Name=paste(SourceDB),
             Revenue=Revenue*1000,
             Total_Fuel_Cost=Total_Fuel_Cost*1000,
             Fixed_OM_Cost=Fixed_Cost_Base*1000,
             CAPEX=Fixed_Cost_Aux1*1000,
             Variable_OM_Cost=Variable_OM_Cost*1000,
             Value=Value*1000,
             Storage_Charging_Cost*1000,
             Total_Cost=Revenue-Value, 
             Capacity=round(Capacity,digits=0)) %>%
      # Filter out dates after MaxYr
      filter(Year<=MaxYrStudy) %>%
      # Choose what to keep
      subset(., select=c(ID,Year,Output_MWH,Capacity,
                         Dispatch_Cost,Fuel_Usage,
                         Total_Fuel_Cost,Fixed_OM_Cost,Variable_OM_Cost,CAPEX,
                         Revenue,Value,Total_Cost,
                         Total_Hours_Run,Percent_Marginal,Capacity_Factor,
                         Storage_Charging_Cost,Sim_Name)) %>%
      rename(Plant_Type=ID,
             Capacity_MW=Capacity,
             Avg_Dispatch_Cost=Dispatch_Cost,
             Fuel_Usage_GJ=Fuel_Usage)
    
    # Combine the Emissions information with the annual resource information 
    AllDataGrYr<-merge(DataGrYr,DataGrEmYr,by=c("Plant_Type","Year"), all.x = TRUE)
    
    # CARBON CREDIT DATA
    AllRenewData <- AllDataGrYr %>%
      filter(Plant_Type %in% c("Wind","Solar")) %>%
      mutate(Emissions_Cost=Emissions_Cost*-1,
             Credit=round(Emissions_Cost/Output_MWH,digits=2),
             PercRev=round(abs(100*Emissions_Cost/(Revenue+Emissions_Cost)),digits=2)) %>%
      arrange(Year) %>%
      subset(select=c(Plant_Type,Year,Output_MWH,Emissions_Cost,Revenue,
                      PercRev,
                      Credit)) %>%
      rename("Plant Type"=Plant_Type,
             "Emissions Revenue"=Emissions_Cost,
             "Output (MWh)"=Output_MWH,
             "Offset Value ($/MWh)"=Credit,
             "Total Revenue"=Revenue,
             "Percent of Total Revenue from Emission Credits (%)"=PercRev
      )
    
    # SORT ANNUAL DATA FOR RESOURCE GROUPS
    AllDataGrYr <- AllDataGrYr %>%
      subset(., select=c(Plant_Type,Year,Output_MWH,Capacity_MW,
                         Avg_Dispatch_Cost,Fuel_Usage_GJ,
                         Total_Fuel_Cost,Fixed_OM_Cost,Variable_OM_Cost,CAPEX,
                         Emissions_Mt,Emissions_Cost,
                         Revenue,Value,Total_Cost,
                         Total_Hours_Run,Percent_Marginal,
                         Capacity_Factor,Sim_Name)) %>%
      # Round some values
      mutate(Avg_Dispatch_Cost=round(Avg_Dispatch_Cost,digits=2),
             Percent_Marginal=round(Percent_Marginal,digits=2),
             Capacity_Factor=round(Capacity_Factor,digits=2)) %>%
      rename("Plant Type"=Plant_Type,
             "Output (MWh)"=Output_MWH,
             "Capacity (MW)"=Capacity_MW,
             "Average Dispatch Cost ($/MWh)"=Avg_Dispatch_Cost,
             "Fuel Usage (GJ)"=Fuel_Usage_GJ,
             "Fuel Cost"=Total_Fuel_Cost,
             "Fixed O&M Cost"=Fixed_OM_Cost,
             "Variable O&M Cost"=Variable_OM_Cost,
             "Emissions (M-tonnes)"=Emissions_Mt,
             "Emissions Cost"=Emissions_Cost,
             "Total Hours Run"=Total_Hours_Run,
             "Total Cost"=Total_Cost,
             "Capital Costs"=CAPEX,
             "Percent Marginal"=Percent_Marginal,
             "Average Capacity Factor"=Capacity_Factor)
  }

################################################################################
## ANNUAL RESOURCE ADDITIONS AND RETIREMENT TABLES
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
    
    # PUT ALL TOGETHER!
    Indv_Change <-rbind(Retdata,Builddata) %>%
      mutate(Sim_Name=paste(SourceDB),
             In_Cap=round(In_Cap,digits=1),
             Capacity_Factor=round(Capacity_Factor,digits=2)) %>%
      rename("Installed Capacity (MW)"=In_Cap,
             "Plant Type"=Primary_Fuel,
             "Average Capacity Factor"=Capacity_Factor,
             "End Date"=End_Date,
             "End Year"=End_Year,
             "Start Date"=Beg_Date,
             "Start Year"=Beg_Year)
    
    # NOW PUT IT ALL TOGETHER TO GET TOTALS BY RESOURCE TYPE
    BuilddataTot <- Builddata%>%
      group_by(Primary_Fuel, Beg_Year) %>%
      summarise(Capacity_Added = sum(In_Cap))%>%
      mutate(Year=Beg_Year,
             Capacity_Added=round(Capacity_Added,digits=1))%>%
      subset(select=c(Primary_Fuel,Year,Capacity_Added))
    
    RetdatadataTot <- Retdata%>%
      group_by(Primary_Fuel, End_Year) %>%
      summarise(Capacity_Retired = sum(In_Cap))%>%
      mutate(Year=End_Year,
             Capacity_Retired=round(Capacity_Retired,digits=1))%>%
      subset(select=c(Primary_Fuel,Year,Capacity_Retired))
    
    # Get summary for each year!
    Tot_Change<-merge(BuilddataTot,RetdatadataTot,by=c("Primary_Fuel","Year"), all.x = TRUE, all.y = TRUE)
    
    # Replace NA values with 0
    Tot_Change[is.na(Tot_Change)]=0
    
    # Find the capacity difference in given year
    AnnualCap_Change <- Tot_Change %>%
      mutate("Difference (MW)"=Capacity_Added-Capacity_Retired,
             Sim_Name=paste(SourceDB)) %>%
      arrange(Year)%>%
      rename("Plant Type"=Primary_Fuel,
             "Capacity Added (MW)"=Capacity_Added,
             "Capacity Retired (MW)"=Capacity_Retired)
    
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
      mutate(Total_Added=AESO_Add+Capacity_Added)%>%
      subset(select=c(Primary_Fuel,AESO_Add,Capacity_Added,Total_Added,Capacity_Retired)) %>%
      rename("Plant Type"=Primary_Fuel,
             "New Capacity Total (MW)"=Total_Added,
             "New Capacity Manual (MW)"=AESO_Add,
             "New Capacity Aurora post 2025 (MW)"=Capacity_Added,
             "Capacity Retired (MW)"=Capacity_Retired)
    
  }
  
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
      summarise("Average Full Load Heat Rate (Btu/kWh)"=round(mean(Full_Load_Heat_Rate),digits=0),
                "Average Net Heat Rate (Btu/kWh)"=round(mean(Net_Heat_Rate),digits=0),
                "Average Inc Heat Rate (Btu/kWh)"=round(mean(Incr_Heat_Rate),digits=0),
                "Average Capacity Factor"=round(mean(Capacity_Factor),digits=2)) %>%
      rename("Plant Type"=Primary_Fuel)
    
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
         Year<=MaxYrStudy) %>%
  subset(select=c(ID,Year,Price,Cost,Usage)) %>%
  rename("Usage (GJ)"=Usage,
         "Price ($/GJ)"=Price,
         "Cost ($)"=Cost)
}

################################################################################
## ANNUAL ZONE INFO
################################################################################
  {  
    # First we need the total energy output for this year!
    AnnualOut <-AllDataGrYr %>%
      group_by(Year)%>%
      summarise(Total_Output=sum(`Output (MWh)`))
    
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
      rename("Average Price ($/MWh)"=Price,
             "Demand (MWh)"=Demand_Total,
             "Net Load (MWh)"=Net_Load_Total,
             "Total Production Costs ($B)"=Production_Cost_Total,
             "Total Fixed Costs ($B)"=Fixed_Cost_Total,
             "Total Costs ($B)"=Total_Costs,
             "Total Output (MWh)"=Total_Output,
             "Unit Production Cost ($/MWh)"=Unit_Prod,
             "Unit Fixed Cost ($/MWh)"=Unit_Fix,
             "Imports (MWh)"=Imports_Total,
             "Exports (MWh)"=Exports_Total,
             "Non-Cogen Emissions (Mt)"=NonCogen_Emissions)
  }

################################################################################
## SEND ALL TO ONE EXCEL FILE
################################################################################

dataset_names <-list('1 Annual Resource Group Data'=AllDataGrYr,
                     '2 Retirements and Additions'=Indv_Change,
                     '3 Cap Changes by Plant Type'=Tot_Change,
                     '4 Annual Cap Changes'=AnnualCap_Change,
                     '5 Full Study Results' =AllCap_Changes2,
                     '6 Annual System Data'=AllZoneData,
                     '7 Annual Fuel Data'=FuelData,
                     '8 Avg Heat Rates by Plant Type'=AVG_HeatRates,
                     '9 Offset Value'=AllRenewData)


filename <-paste("Annual_Data_",ScenarioName,"_",SourceDB,".xlsx")

write_xlsx(dataset_names, path = here("Data Files","Result Files",NameShort,filename),
                     col_names = TRUE, format_headers = TRUE)

}

################################################################################
## FUNCTION: HourlyDataExcel
## Writes all relevant annual data to an excel file on different sheets.
## INPUTS: 
##    ScenarioName - Name of scenario (EX: "BAU:)
##    case - Filter by run case
## TABLES REQUIRED: 
##    ResGroupHr
##    ResHr
##    ZoneHr
################################################################################

HourlyDataExcel<- function(ScenarioName,NameShort,case){
  
################################################################################
## HOURLY RESOURCE GROUP OUTPUT TABLES
## Resource Group annaul Information: 2022-MaxYr. Gives info on resource group 
## costs, outputs, and emission costs. 
################################################################################
 ZoneHrData <- ZoneHr_Avg %>%
   mutate(Year = year(date),
          Month=month(date),
          Day=day(date),
          Hour=hour(date),
          Date=as.Date(date),
          Time=format(date,"%H:%M:%S"),
          Scenario=SourceDB) %>%
   filter(Year<=MaxYrStudy) %>%
   arrange(.,date) %>%
   subset(select=c(Date,Time,Price,Demand,Net_Load,
                   Marginal_Resource,Imports,Exports,Year,Month,Day,Scenario)) %>%
   rename("Price ($/MWh)"=Price,
          "Demand (MW)"=Demand,
          "Net Load (MW)"=Net_Load,
          "Marginal Resource"=Marginal_Resource)
 
  # Clear un-used 
  gc()
################################################################################
## HOURLY ZONE INFO
################################################################################
 
 # Get emissions for eay type to start
 DataGrEmHr<-ResGroupEmHr%>%
   # Filter to rename fuels
   sim_filt5(.) %>% 
   # Filter for case
   filter(Run_ID == case,
          Condition == "Average",
          Type=="CO2") %>%
   # Make cost in dollars, and convert Ton to tonne
   mutate(Year=year(date),
          Month=month(date),
          Day=day(date),
          Hour=hour(date),
          Date=as.Date(date),
          Time=format(date,"%H:%M:%S"),
          Scenario=SourceDB,
          Emissions_Cost=Cost*1000,
          Emissions_Tonne=Amount*0.90718474) %>%
   # Filter out dates after MaxYr
   filter(Year<=MaxYrStudy) %>%
    arrange(.,date) %>%
   # Chose what to keep
   subset(., select=c(Date,Time,ID,Emissions_Tonne,Emissions_Cost,Year,Month,Day,Scenario))%>%
   rename(Plant_Type=ID)
 
 # Resource groups over entire year
 DataGrHr <- ResGroupHr%>%
   sim_filt5(.) %>% #Filter to rename fuels
   filter(Run_ID == case) %>%
   filter(Condition == "Average") %>%
   # Convert all costs to dollars from $000
 mutate(Year=year(date),
        Month=month(date),
        Day=day(date),
        Hour=hour(date),
        Date=as.Date(date),
        Time=format(date,"%H:%M:%S"),
        Scenario=paste(SourceDB),
        Revenue=Revenue*1000,
        Total_Fuel_Cost=Total_Fuel_Cost*1000,
        Fixed_Cost=Fixed_Cost*1000,
        Variable_OM_Cost=Variable_OM_Cost*1000,
        Value=Value*1000,
        Storage_Charging_Cost*1000,
        Capacity=round(Capacity,digits=0)) %>%
   # Filter out dates after MaxYr
   filter(Year<=MaxYrStudy) %>%
   arrange(.,date) %>%
   # Choose what to keep
   subset(., select=c(Date,Time,ID,Output_MWH,Capacity,
                      Dispatch_Cost,
                      Total_Fuel_Cost,Fixed_Cost,Variable_OM_Cost,
                      Revenue,Value,Capacity_Factor,
                      Year,Month,Day,Scenario)) %>%
     rename(Plant_Type=ID)%>%
   rename("Plant Type"=Plant_Type,
          "Output (MWh)"=Output_MWH,
          "Capacity (MW)"=Capacity,
          "Average Dispatch Cost ($MWh)"=Dispatch_Cost,
          "Fuel Cost"=Total_Fuel_Cost,
          "Fixed Cost"=Fixed_Cost,
          "Variable O&M Cost"=Variable_OM_Cost,
          "Capacity Factor"=Capacity_Factor)

   # Clear un-used 
   gc()

    
################################################################################
## SEND ALL TO ONE EXCEL FILE
################################################################################
   
   dataset_names <-list('1 Hourly Resource Group Data'=DataGrHr,
                        '2 Hourly Emission Data'=DataGrEmHr,
                        '3 Hourly System Data'=ZoneHrData)
   
   filename <-paste("Hourly_Data_",ScenarioName,"_",SourceDB,".xlsx")
   
   write_xlsx(dataset_names, path = here("Data Files","Result Files",NameShort,filename),
              col_names = TRUE, format_headers = TRUE)
}

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
             Emissions_Cost=Cost,
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
             Revenue=Revenue,
             Total_Fuel_Cost=Total_Fuel_Cost,
             Variable_OM_Cost=Variable_OM_Cost,
             Fixed_OM_Cost=Fixed_Cost_Base,
             CAPEX=Fixed_Cost_Aux1,
             Value=Value,
             Total_Cost=Revenue-Value, 
             Storage_Charging_Cost,
             Capacity=round(Capacity,digits=0)) %>%
      # Filter out dates after MaxYr
      filter(Year<=MaxYrStudy) %>%
      # Choose what to keep
      subset(., select=c(ID,Year,Output_MWH,Capacity,
                         Dispatch_Cost,Fuel_Usage,
                         Total_Fuel_Cost,Variable_OM_Cost,Fixed_OM_Cost,CAPEX,
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
      mutate(Misc_Costs=as.numeric(Total_Cost-(Total_Fuel_Cost+Variable_OM_Cost+Fixed_OM_Cost+CAPEX+Emissions_Cost)),
             OPEX=Emissions_Cost+Total_Fuel_Cost+Misc_Costs+Variable_OM_Cost+Fixed_OM_Cost)%>%
      subset(., select=c(Plant_Type,Year,Output_MWH,Capacity_MW,
                         Avg_Dispatch_Cost,Fuel_Usage_GJ,Emissions_Tonne,
                         Emissions_Cost,Total_Fuel_Cost,Misc_Costs,Variable_OM_Cost,Fixed_OM_Cost,
                         CAPEX,OPEX,
                         Total_Cost,Revenue,Value,
                         Total_Hours_Run,Percent_Marginal,
                         Capacity_Factor,Sim_Name)) 
    
    # Save R file in folder
    SaveR_Loc(AllDataGrYr,ScenarioName,"ResGrYr_")
    
  }
################################################################################
## EMISSION PERFORMANCE CREDIT VALUES
################################################################################
  # Here we get value of carbon credits and cost
  EPC_Values <- AllDataGrYr1 %>%
    filter(Plant_Type %in% c("Wind","Solar","Hydro")) %>%
    mutate(Emissions_Cost=Emissions_Cost*-1000,
           Credit=Emissions_Cost/Output_MWH,
           EPC_Perc_of_Rev=abs(100*Emissions_Cost/(Revenue+Emissions_Cost))) %>%
    arrange(Year) %>%
    subset(select=c(Plant_Type,Year,Output_MWH,Emissions_Cost,Revenue,Value,
                    Credit,EPC_Perc_of_Rev,Sim_Name)) %>%
    rename("EPC_Revenue"=Emissions_Cost,
           "EPC_Value_$/MWh"=Credit,
           "Other_Revenue"=Revenue)
  
  # Save R file
  SaveR_Loc(EPC_Values,ScenarioName,"EPC_Values_")
  
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
    SaveR_Loc(AnnualCap_Change,ScenarioName,"Ann_Cap_")
    
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
  
  # Get pumped storage (separate since added post 2025)
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
  
  New_PS3<- Tot_Change %>%
    filter(Year>2026,
           Primary_Fuel=="Storage - Pumped Hydro")%>%
    group_by(Primary_Fuel)%>%
    summarize(AESO_Add=sum(Capacity_Added))
  
  AllCap_Changes1a<-rbind(AllCap_Changes1a,New_PS3)
  
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
  SaveR_Loc(AllCap_Changes2,ScenarioName,"Tot_Cap_")
  
  
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
    SaveR_Loc(AVG_HeatRates,ScenarioName,"Ann_HR_")
    
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
      subset(select=c(ID,Year,Price,Cost,Usage,Scenario)) %>%
      rename("Usage_GJ"=Usage,
             "Price_$/GJ"=Price)
    
    # Save R file
    SaveR_Loc(FuelData,ScenarioName,"Ann_Fuel_")
    
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
             Production_Cost_Total=Production_Cost_Total,
             Fixed_Cost_Total=Fixed_Cost_Total)%>%
      subset(.,select=c(Name,Year,Price, 
                        Demand_Total,Net_Load_Total,
                        Production_Cost_Total,Fixed_Cost_Total,Imports_Total,Exports_Total,Scenario))
    
    # Get the non-cogen emissions in each year
    DataAnnualEm1 <- DataGrEmYr %>%
      filter(Plant_Type!="Cogeneration")%>%
      group_by(Year)%>%
      summarise(NonCogen_Emissions=sum(Emissions_Tonne)/1000000)%>%
      ungroup()
    
    # Get the total emissions in each year
    DataAnnualEm2 <- DataGrEmYr %>%
      group_by(Year)%>%
      summarise(Emissions=sum(Emissions_Tonne)/1000000)%>%
      ungroup()
    
    # Combine the total output in MWh with zone info
    AllZoneData1a<-merge(ZnData,AnnualOut,by=c("Year"), all.x = TRUE)
    AllZoneData1b<-merge(AllZoneData1a,DataAnnualEm1,by=c("Year"), all.x = TRUE)
    AllZoneData1c<-merge(AllZoneData1b,DataAnnualEm2,by=c("Year"), all.x = TRUE)
    
    
    # Now, rename some columns and get unit prices
    AllZoneData <-AllZoneData1c %>%
      mutate(Unit_Prod=round(Production_Cost_Total/Total_Output,digits=2),
             Unit_Fix=round(Fixed_Cost_Total/Total_Output,digits=2),
             Price=round(Price,digits=2),
             Imports_Total=round(Imports_Total,digits=0),
             Exports_Total=round(Exports_Total,digits=0),
             Production_Cost_Total=Production_Cost_Total/1000000,
             Fixed_Cost_Total=Fixed_Cost_Total/1000000,
             Total_Costs=Production_Cost_Total+Fixed_Cost_Total) %>%
      subset(.,select=c(Name,Year,Price, 
                        Demand_Total,Net_Load_Total,Total_Output,
                        Production_Cost_Total,Unit_Prod,Fixed_Cost_Total,Unit_Fix,Total_Costs,
                        Imports_Total,Exports_Total,NonCogen_Emissions,
                        Scenario)) %>%
      rename("Avg_Price"=Price)
    
    # Save R file
    SaveR_Loc(AllZoneData,ScenarioName,"Zone_")
    
  }
}

################################################################################
## FUNCTION: CombineFilesR
## Combine all relevant annual data from to files, use after AnnualDataR.
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

# Load files from each scenario and combine into one R file with new name
CombineFilesR<-function(ScenarioName1,ScenarioName2,CScenarioName){
  
  # Annual Resource Group Capacity
  Ann_Cap1<-readRDS(here("Data Files","Result Files",ScenarioName1,paste("Ann_Cap_",ScenarioName1, sep = "")))
  Ann_Cap2<-readRDS(here("Data Files","Result Files",ScenarioName2,paste("Ann_Cap_",ScenarioName2, sep = "")))
  # Combine into new R file and save combined file in new folder
  Ann_Cap<-rbind(Ann_Cap1,Ann_Cap2)
  Ann_Cap[is.na(Ann_Cap)]<-0
  rm(Ann_Cap1,Ann_Cap2)
  SaveR_Loc(Ann_Cap,CScenarioName,"Ann_Cap_")
  
  # Annual Fuel Usage
  Ann_Fuel1<-readRDS(here("Data Files","Result Files",ScenarioName1,paste("Ann_Fuel_",ScenarioName1, sep = "")))
  Ann_Fuel2<-readRDS(here("Data Files","Result Files",ScenarioName2,paste("Ann_Fuel_",ScenarioName2, sep = "")))
  # Combine into new R file and save combined file in new folder
  Ann_Fuel<-rbind(Ann_Fuel1,Ann_Fuel2)
  Ann_Fuel[is.na(Ann_Fuel)]<-0
  rm(Ann_Fuel1,Ann_Fuel2)
  SaveR_Loc(Ann_Fuel,CScenarioName,"Ann_Fuel_")
  
  
  # Annual Heat Rates
  Ann_HR1<-readRDS(here("Data Files","Result Files",ScenarioName1,paste("Ann_HR_",ScenarioName1, sep = "")))
  Ann_HR2<-readRDS(here("Data Files","Result Files",ScenarioName2,paste("Ann_HR_",ScenarioName2, sep = "")))
  # Combine into new R file and save combined file in new folder
  Ann_HR<-rbind(Ann_HR1,Ann_HR2)
  Ann_HR[is.na(Ann_HR)]<-0
  rm(Ann_HR1,Ann_HR2)
  SaveR_Loc(Ann_HR,CScenarioName,"Ann_HR_")
  
  # Annual EPC Values
  EPC_Values1<-readRDS(here("Data Files","Result Files",ScenarioName1,paste("EPC_Values_",ScenarioName1, sep = "")))
  EPC_Values2<-readRDS(here("Data Files","Result Files",ScenarioName2,paste("EPC_Values_",ScenarioName2, sep = "")))
  # Combine into new R file and save combined file in new folder
  EPC_Values<-rbind(EPC_Values1,EPC_Values2)
  EPC_Values[is.na(EPC_Values)]<-0
  rm(EPC_Values1,EPC_Values2)
  SaveR_Loc(EPC_Values,CScenarioName,"EPC_Values_")
  
  # Annual Resource Group Data
  ResGrYr1<-readRDS(here("Data Files","Result Files",ScenarioName1,paste("ResGrYr_",ScenarioName1, sep = "")))
  ResGrYr2<-readRDS(here("Data Files","Result Files",ScenarioName2,paste("ResGrYr_",ScenarioName2, sep = "")))
  # Combine into new R file and save in new folder
  ResGrYr<-rbind(ResGrYr1,ResGrYr2)
  ResGrYr[is.na(ResGrYr)]<-0
  rm(ResGrYr1,ResGrYr2)
  SaveR_Loc(ResGrYr,CScenarioName,"ResGrYr_")
  
  
  # Total Capacity Changes (Study)
  Tot_Cap1<-readRDS(here("Data Files","Result Files",ScenarioName1,paste("Tot_Cap_",ScenarioName1, sep = "")))
  Tot_Cap2<-readRDS(here("Data Files","Result Files",ScenarioName2,paste("Tot_Cap_",ScenarioName2, sep = "")))
  # Combine into new R file and save combined file in new folder
  Tot_Cap<-rbind(Tot_Cap1,Tot_Cap2)
  Tot_Cap[is.na(Tot_Cap)]<-0
  rm(Tot_Cap1,Tot_Cap2)
  SaveR_Loc(Tot_Cap,CScenarioName,"Tot_Cap_")
  
  # Annual Zone Data
  Zone1<-readRDS(here("Data Files","Result Files",ScenarioName1,paste("Zone_",ScenarioName1, sep = "")))
  Zone2<-readRDS(here("Data Files","Result Files",ScenarioName2,paste("Zone_",ScenarioName2, sep = "")))
  # Combine into new R file and save combined file in new folder
  Zone<-rbind(Zone1,Zone2)
  Zone[is.na(Zone)]<-0
  rm(Zone1,Zone2)
  SaveR_Loc(Zone,CScenarioName,"Zone_")
  
################################################################################
## REFORMAT FILES AS DESIRED FOR EXCEL SHEET ONLY
################################################################################
  {
    # General info Tab - work this into the original filter eventually
    Sim_Info<-ResGrYr %>%
      group_by(Sim_Name)%>%
      summarize(MinYr=min(Year),
                MaxYr=max(Year))%>%
      rotate_df()
    
    # Sheet info
    ContentsDF=data.frame(
      V1=c("",
           'FORMATED RESULTS',
           '1 Capacity by Tech',
           '2 Percent Capacity',
           '3 AURORA Capaity Added',
           '4 Total Capacity Changes',
           '5 Annual Cap Additions',
           '6 Annual Cap Ret',
           '7 Total Cap Ret',
           '8 Generation by Tech',
           '9 Percent Generation',
           '10 Emissions by Tech',
           '11 Emissions Cost by Tech',
           '12 Emissions Total',
           '13 Cost by Tech',
           '14 Total Costs',
           '15 Zone Costs',
           '16  Average Pool Price',
           '17 Imports_Exports',
           '18 Net Imports',
           '19 Fuel Usage',
           '20 Fuel Price',
           "",
           'COMPILED RESULTS',
           'A All Groups Data',
           'B Annual Zone',
           'C Annual Capacity Changes',
           'D EPC Values',
           'E Annual Fuel Data',
           'F Average Heat Rates'))
    ContentsDF$V2=""
    
    Sim_Info<-rbind(Sim_Info,ContentsDF)
    Sim_Info<-janitor::row_to_names(Sim_Info,1)
    
    # Available model capacity for each year by resource group
    Cap1<-ResGrYr %>%
      group_by(Plant_Type,Sim_Name,Year)%>%
      summarize(Avail_Cap=sum(Capacity_MW))
    
    Cap2<-ResGrYr %>%
      group_by(Sim_Name,Year)%>%
      summarise(Avail_Cap=sum(Capacity_MW))%>%
      mutate(Plant_Type="Total")%>%
      subset(.,select=c(Plant_Type,Sim_Name,Year,Avail_Cap))
    
    Cap_ByGroup<-rbind(Cap1,Cap2)%>%
      mutate(Sim_Name=paste(Sim_Name," Capacity_MW", sep="\n"))%>%
      pivot_wider(names_from=Sim_Name,values_from =Avail_Cap)%>%
      arrange(.,(Year))
    
    # Available model capacity by percentage
    Perc_Cap<-ResGrYr %>%
      group_by(Sim_Name,Year)%>%
      mutate(TotalCap=sum(Capacity_MW),
             PercGen=Capacity_MW/TotalCap)%>%
      group_by(Plant_Type,Sim_Name,Year)%>%
      summarize(Percent_Cap=paste(round(100*PercGen,2),'%'))%>%
      mutate(Sim_Name=paste(Sim_Name," % Capacity", sep="\n"))%>%
      pivot_wider(names_from=Sim_Name,values_from =Percent_Cap)%>%
      arrange(.,(Year)) 
    
    # Total Capacity added annually by model
    Ann_Add<-Ann_Cap %>%
      group_by(Plant_Type,Sim_Name,Year)%>%
      summarize(Cap=round(sum(Capacity_Added),1))%>%
      mutate(Sim_Name=paste(Sim_Name," Model_Capacity_Added_MW", sep="\n"))%>%
      pivot_wider(names_from=Sim_Name,values_from =Cap)%>%
      arrange(.,(Year))
    
    # Total Capacity added by model
    Auroraadd<-Tot_Cap %>%
      group_by(Plant_Type,Scenario)%>%
      summarize(Cap=round(sum(New_Cap_AURORA),1))%>%
      mutate(Scenario=paste(Scenario," AURORA_Added_MW", sep="\n"))%>%
      pivot_wider(names_from=Scenario,values_from =Cap)
    
    # Total Capacity added
    Totaladd<-Tot_Cap %>%
      group_by(Plant_Type,Scenario)%>%
      summarize(Cap=round(sum(New_Cap_Total),1))%>%
      mutate(Scenario=paste(Scenario," Total_Capacity_Added_MW", sep="\n"))%>%
      pivot_wider(names_from=Scenario,values_from =Cap)
    
    # Total Capacity retied annually by model
    Ann_Ret<-Ann_Cap %>%
      group_by(Plant_Type,Sim_Name,Year)%>%
      summarize(Cap=sum(Capacity_Retired))%>%
      mutate(Sim_Name=paste(Sim_Name," Model_Capacity_Retired_MW", sep="\n"))%>%
      pivot_wider(names_from=Sim_Name,values_from =Cap)%>%
      arrange(.,(Year))
    
    # Total Capacity retired
    TotalRet<-Tot_Cap %>%
      group_by(Plant_Type,Scenario)%>%
      summarize(Cap=round(sum(Cap_Retired),1))%>%
      mutate(Scenario=paste(Scenario," Total_Capacity_Retired_MW", sep="\n"))%>%
      pivot_wider(names_from=Scenario,values_from =Cap)
    
    # Total generation 
    Gen_ByGroup<-ResGrYr %>%
      group_by(Plant_Type,Sim_Name,Year)%>%
      summarize(TotalGen=sum(Output_MWH)/1000000)%>%
      mutate(Sim_Name=paste(Sim_Name," Generation TWh", sep="\n"))%>%
      pivot_wider(names_from=Sim_Name,values_from =TotalGen)%>%
      arrange(.,(Year))
    
    # Percent generation 
    Perc_Gen<-ResGrYr %>%
      group_by(Sim_Name,Year)%>%
      mutate(TotalGen=sum(Output_MWH),
             PercGen=Output_MWH/TotalGen)%>%
      group_by(Plant_Type,Sim_Name,Year)%>%
      summarize(Percent_Gen=paste(round(100*PercGen,2),'%'))%>%
      mutate(Sim_Name=paste(Sim_Name," % Generation", sep="\n"))%>%
      pivot_wider(names_from=Sim_Name,values_from =Percent_Gen)%>%
      arrange(.,(Year))  
    
    # Emissions 
    Em_ByGroup<-ResGrYr %>%
      group_by(Plant_Type,Sim_Name,Year)%>%
      summarize(TotalEm=sum(Emissions_Tonne)/1000000)%>%
      mutate(Sim_Name=paste(Sim_Name," Emissions Mtonne", sep="\n"))%>%
      filter(TotalEm>0)%>%
      pivot_wider(names_from=Sim_Name,values_from =TotalEm)%>%
      arrange(.,(Year))
    
    # Emissions Cost
    EmCost_ByGroup<-ResGrYr %>%
      group_by(Plant_Type,Sim_Name,Year)%>%
      summarize(TotalEm=sum(Emissions_Cost)/1000)%>%
      mutate(Sim_Name=paste(Sim_Name," Emissions Cost ($M)", sep="\n"))%>%
      pivot_wider(names_from=Sim_Name,values_from =TotalEm)%>%
      arrange(.,(Year))
    
    # Emissions Total
    #   Filters out cogen - may want to add back in later
    Em_Total<-ResGrYr %>%
      filter(Emissions_Tonne>0,
             !Plant_Type %in% c("Cogeneration"))%>%
      group_by(Sim_Name,Year)%>%
      summarize(TotalEm=sum(Emissions_Tonne)/1000000)%>%
      mutate(Sim_Name=paste(Sim_Name," Non-Cogen Emissions (Mt)", sep="\n"))%>%
      pivot_wider(names_from=Sim_Name,values_from =TotalEm)%>%
      arrange(.,(Year))
    
    # Resource Group Costs 
    OPEX_G<-ResGrYr %>%
      group_by(Plant_Type,Sim_Name,Year)%>%
      summarize(Cost_Tot=sum(OPEX)/1000)%>%
      mutate(Cost_Type="OPEX ($M)")
    
    CAPEX_G<-ResGrYr %>%
      group_by(Plant_Type,Sim_Name,Year)%>%
      summarize(Cost_Tot=sum(CAPEX)/1000)%>%
      mutate(Cost_Type="CAPEX ($M)")
    
    Rev_G<-ResGrYr %>%
      group_by(Plant_Type,Sim_Name,Year)%>%
      summarize(Cost_Tot=sum(Revenue)/1000)%>%
      mutate(Cost_Type="Revenue ($M)")
    
    Value_G<-ResGrYr %>%
      group_by(Plant_Type,Sim_Name,Year)%>%
      summarize(Cost_Tot=sum(Value)/1000)%>%
      mutate(Cost_Type="Value ($M)")
    
    Costs_G<-bind_rows(Rev_G,OPEX_G,CAPEX_G,Value_G)%>%
      pivot_wider(names_from=Sim_Name,values_from =Cost_Tot)%>%
      arrange(.,Cost_Type,Year)
    
    # Zone Costs
    OPEX_T<-ResGrYr %>%
      group_by(Sim_Name,Year)%>%
      summarize(Cost_Tot=sum(OPEX)/1000000)%>%
      mutate(Cost_Type="OPEX ($B)")
    
    CAPEX_T<-ResGrYr %>%
      group_by(Sim_Name,Year)%>%
      summarize(Cost_Tot=sum(CAPEX)/1000000)%>%
      mutate(Cost_Type="CAPEX ($B)")
    
    All_C_T<-ResGrYr %>%
      group_by(Sim_Name,Year)%>%
      summarize(Cost_Tot=sum(CAPEX+OPEX)/1000000)%>%
      mutate(Cost_Type="Total Cost ($B)")
    
    Rev_T<-ResGrYr %>%
      group_by(Sim_Name,Year)%>%
      summarize(Cost_Tot=sum(Revenue)/1000000)%>%
      mutate(Cost_Type="Revenue ($B)")
    
    Value_T<-ResGrYr %>%
      group_by(Sim_Name,Year)%>%
      summarize(Cost_Tot=sum(Value)/1000000)%>%
      mutate(Cost_Type="Value ($B)")
    
    Costs_T<-bind_rows(Rev_T,OPEX_T,CAPEX_T,All_C_T,Value_T)%>%
      pivot_wider(names_from=Sim_Name,values_from =Cost_Tot)%>%
      arrange(.,Cost_Type,Year)
    
    # Zone Costs   
    ZCosts1<-Zone%>%
      group_by(Scenario,Year)%>%
      summarize(ZoneCosts=sum(Production_Cost_Total))%>%
      mutate(Cost_Type="Production Costs ($B)")
    
    ZCosts2<-Zone%>%
      group_by(Scenario,Year)%>%
      summarize(ZoneCosts=sum(Fixed_Cost_Total))%>%
      mutate(Cost_Type="Fixed Costs ($B)")
    
    ZCosts3<-Zone%>%
      group_by(Scenario,Year)%>%
      summarize(ZoneCosts=(Production_Cost_Total+Fixed_Cost_Total))%>%
      mutate(Cost_Type="Total Costs ($B)")
    
    ZCosts<-bind_rows(ZCosts1,ZCosts2,ZCosts3)%>%
      pivot_wider(names_from=Scenario,values_from =ZoneCosts)%>%
      arrange(.,Cost_Type,Year)
    
    
    # Pool Price
    PoolPrice<-Zone %>%
      group_by(Scenario,Year)%>%
      summarize(Avg_P=sum(Avg_Price))%>%
      mutate(Scenario=paste(Scenario," Average Price ($/MWh)", sep="\n"))%>%
      pivot_wider(names_from=Scenario,values_from =Avg_P)%>%
      arrange(.,(Year)) 
    
    # Import/Export
    # Import/Export
    Z_Imp<-Zone %>%
      group_by(Scenario,Year)%>%
      summarize(Amount=(Imports_Total)/1000000)%>%
      mutate(Type="Import")
    
    Z_Exp<-Zone %>%
      group_by(Scenario,Year)%>%
      summarize(Amount=(Exports_Total)/1000000)%>%
      mutate(Type="Export")
    
    Imp_Exp<-rbind(Z_Imp,Z_Exp)%>%
      pivot_wider(names_from=Scenario,values_from =Amount)%>%
      arrange(.,Type,Year)
    
    
    # Net Import
    Net_Imp<-Zone %>%
      group_by(Scenario,Year)%>%
      summarize(Net_Imp=(Imports_Total-Exports_Total)/1000000)%>%
      mutate(Scenario=paste(Scenario," Net Import (TWh)", sep="\n"))%>%
      pivot_wider(names_from=Scenario,values_from =Net_Imp)%>%
      arrange(.,(Year)) 
    
    # Fuel Usage
    Fuel_Used<-Ann_Fuel %>%
      group_by(ID,Scenario,Year)%>%
      summarize(Burn=sum(Usage_GJ))%>%
      mutate(Scenario=paste(Scenario," Fuel Used (GJ)", sep="\n"))%>%
      pivot_wider(names_from=Scenario,values_from =Burn)%>%
      arrange(.,(Year))
    
    Fuel_P<-Ann_Fuel %>%
      group_by(ID,Scenario,Year)%>%
      summarize(Burn=sum(`Price_$/GJ`))%>%
      mutate(Scenario=paste(Scenario," Fuel Price ($/GJ)", sep="\n"))%>%
      pivot_wider(names_from=Scenario,values_from =Burn)%>%
      arrange(.,(Year))
    
  }  
################################################################################
## SEND ALL TO ONE EXCEL FILE
################################################################################
  
  dataset_names <-list('Info'=Sim_Info,
                       '1 Capacity by Tech'=Cap_ByGroup,
                       '2 Percent Capacity'=Perc_Cap,
                       '3 AURORA Capaity Added'=Auroraadd,
                       '4 Total Capacity Changes'=Totaladd,
                       '5 Annual Cap Additions'=Ann_Add,
                       '6 Annual Cap Ret'=Ann_Ret,
                       '7 Total Cap Ret'=TotalRet,
                       '8 Generation by Tech'=Gen_ByGroup,
                       '9 Percent Generation'=Perc_Gen,
                       '10 Emissions by Tech'=Em_ByGroup,
                       '11 Emissions Cost by Tech'=EmCost_ByGroup,
                       '12 Emissions Total'=Em_Total,
                       '13 Cost by Tech'=Costs_G,
                       '14 Total Resource Costs'=Costs_T,
                       '15 Zone Costs'=ZCosts,
                       '16  Average Pool Price'=PoolPrice,
                       '17 Imports_Exports'=Imp_Exp,
                       '18 Net Imports'=Net_Imp,
                       '19 Fuel Usage'=Fuel_Used,
                       '20 Fuel Price'=Fuel_P,
                       
                       'A All Groups Data'=ResGrYr,
                       'B Annual Zone'=Zone,
                       'C Annual Capacity Changes' =Ann_Cap,
                       'D EPC Values'=EPC_Values,
                       'E Annual Fuel Data'=Ann_Fuel,
                       'F Average Heat Rates'=Ann_HR)
  
  filename <-paste("Compare_",CScenarioName,".xlsx", sep = "")
  
  # Save the excel sheet to folder
  write_xlsx(dataset_names, path = here("Data Files","Result Files",CScenarioName,filename),
             col_names = TRUE, format_headers = TRUE) 
  
}
