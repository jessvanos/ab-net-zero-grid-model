################################################################################
## FUNCTION: AnnaulDataExcel
## Writes all relevant annual data to an excel file on different sheets.
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

AnnaulDataExcel<- function(ScenarioName,case){

################################################################################
## ANNUAL RESOURCE GROUP OUTPUT TABLES
## Resource Group annaul Information: 2022-2035. Gives info on resource group 
## costs, outputs, and emission costs. 
################################################################################
{
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
    # Filter out dates after 2035
    filter(Year<=2035) %>%
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
           Variable_OM_Cost=Variable_OM_Cost*1000,
           Value=Value*1000,
           Storage_Charging_Cost*1000,
           Capacity=round(Capacity,digits=0)) %>%
    # Filter out dates after 2035
    filter(Year<=2035) %>%
    # Choose what to keep
    subset(., select=c(ID,Year,Output_MWH,Capacity,
                       Dispatch_Cost,Fuel_Usage,
                       Total_Fuel_Cost,Fixed_Cost,Variable_OM_Cost,
                       Revenue,Value,
                       Total_Hours_Run,Percent_Marginal,
                       Capacity_Factor,Sim_Name)) %>%
    rename(Plant_Type=ID,
           Capacity_MW=Capacity,
           Avg_Dispatch_Cost=Dispatch_Cost,
           Fuel_Usage_GJ=Fuel_Usage)
  
  # Combine the Emissions information with the annual resource information 
  AllDataGrYr<-merge(DataGrYr,DataGrEmYr,by=c("Plant_Type","Year"), all.x = TRUE)
  
  # Here we get value of carbon credits
  AllRenewData <- AllDataGrYr %>%
    filter(Plant_Type %in% c("Wind","Solar")) %>%
    mutate(Credit=round(Emissions_Cost/Output_MWH,digits=2),
           PercRev=round(abs(100*Emissions_Cost/(Revenue+(Emissions_Cost*-1))),digits=2),
           Emissions_Cost=Emissions_Cost*-1) %>%
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
    
  # Re-sort the columns
  AllDataGrYr <- AllDataGrYr %>%
    subset(., select=c(Plant_Type,Year,Output_MWH,Capacity_MW,
                       Avg_Dispatch_Cost,Fuel_Usage_GJ,
                       Total_Fuel_Cost,Fixed_Cost,Variable_OM_Cost,
                       Emissions_Tonne,Emissions_Cost,
                       Revenue,Value,
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
           "Fixed Cost"=Fixed_Cost,
           "Variable O&M Cost"=Variable_OM_Cost,
           "Emissions (tonnes)"=Emissions_Tonne,
           "Emissions Cost"=Emissions_Cost,
           "Total Hours Run"=Total_Hours_Run,
           "Percent Marginal"=Percent_Marginal,
           "Average Capacity Factor"=Capacity_Factor)
}

################################################################################
## ANNUAL RESOURCE ADDITIONS AND RETIREMENT TABLES
## Resource Group annual Information: 2022-2035. Gives info on resource group 
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
           Time_Period<=2035) 
  
  # Set levels to each category in order specified
  Add_Ret_data$Primary_Fuel <- factor(Add_Ret_data$Primary_Fuel, 
                                      levels=c("Coal","Coal-to-Gas", "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                               "Blended  Simple Cycle","Blended  Combined Cycle",
                                               "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
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
           Capacity_Added=round(Capacity_Added,digits=0))%>%
    subset(select=c(Primary_Fuel,Year,Capacity_Added))
  
  RetdatadataTot <- Retdata%>%
    group_by(Primary_Fuel, End_Year) %>%
    summarise(Capacity_Retired = sum(In_Cap))%>%
    mutate(Year=End_Year,
           Capacity_Retired=round(Capacity_Retired,digits=0))%>%
    subset(select=c(Primary_Fuel,Year,Capacity_Retired))
  
  # Get summary for each year!
  Tot_Change<-merge(BuilddataTot,RetdatadataTot,by=c("Primary_Fuel","Year"), all.x = TRUE, all.y = TRUE)
  
  # Replace NA values with 0
  Tot_Change[is.na(Tot_Change)]=0
  
  # Find the capacity difference in given year
  Tot_Change <- Tot_Change %>%
    mutate("Difference (MW)"=Capacity_Added-Capacity_Retired,
           Sim_Name=paste(SourceDB)) %>%
    rename("Plant Type"=Primary_Fuel,
           "Capacity Added (MW)"=Capacity_Added,
           "Capacity Retired (MW)"=Capacity_Retired)

}

################################################################################
## ANNUAL AVERAGE HEAT RATES
## Resource Group annual Information: 2022-2035. Gives info on resource group 
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
             Time_Period<=2035,
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
         Year<=2035) %>%
  subset(select=c(ID,Year,Price,Cost,Usage)) %>%
  rename("Usage (GJ)"=Usage,
         "Price ($/GJ)"=Price,
         "Cost ($)"=Cost)
}

################################################################################
## ANNUAL ZONE INFO
################################################################################

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
         Year<=2035) %>%
  mutate(Report_Year=as.numeric(Report_Year),
         Scenario=SourceDB,
         Production_Cost_Total=1000*Production_Cost_Total,
         Fixed_Cost_Total=1000*Fixed_Cost_Total)%>%
  subset(.,select=c(Name,Year,Price, 
                    Demand_Total,Net_Load_Total,
                    Production_Cost_Total,Fixed_Cost_Total,Imports_Total,Exports_Total,Scenario))

  # Combine the total output in MWh with zone info
  AllZoneData1<-merge(ZnData,AnnualOut,by=c("Year"), all.x = TRUE)
  
  # Now, rename some columns and get unit prices
  AllZoneData <-AllZoneData1 %>%
    mutate(Unit_Prod=round(Production_Cost_Total/Total_Output,digits=2),
           Unit_Fix=round(Fixed_Cost_Total/Total_Output,digits=2),
           Price=round(Price,digits=2),
           Imports_Total=round(Imports_Total,digits=0),
           Exports_Total=round(Exports_Total,digits=0)) %>%
    subset(.,select=c(Name,Year,Price, 
                      Demand_Total,Net_Load_Total,Total_Output,
                      Production_Cost_Total,Unit_Prod,Fixed_Cost_Total,Unit_Fix,
                      Imports_Total,Exports_Total,
                      Scenario)) %>%
    rename("Average Price ($/MWh)"=Price,
           "Demand (MWh)"=Demand_Total,
           "Net Load (MWh)"=Net_Load_Total,
           "Total Production Costs"=Production_Cost_Total,
           "Total Fixed Costs"=Fixed_Cost_Total,
           "Total Output (MWh)"=Total_Output,
           "Unit Production Cost ($/MWh)"=Unit_Prod,
           "Unit Fixed Cost ($/MWh)"=Unit_Fix,
           "Imports (MWh)"=Imports_Total,
           "Exports (MWh)"=Exports_Total)


################################################################################
## SEND ALL TO ONE EXCEL FILE
################################################################################

dataset_names <-list('1 Annual Resource Group Data'=AllDataGrYr,
                     '2 Retirements and Additions'=Indv_Change,
                     '3 Cap Changes by Plant Type'=Tot_Change,
                     '4 Annual System Data'=AllZoneData,
                     '5 Annual Fuel Data'=FuelData,
                     '6 Avg Heat Rates by Plant Type'=AVG_HeatRates,
                     '7 Offset Value'=AllRenewData)

filename <-paste("Annual_Data_",ScenarioName,"_",SourceDB,".xlsx")

write_xlsx(dataset_names, path = here("Data Files",filename),
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

HourlyDataExcel<- function(ScenarioName,case){
  
################################################################################
## HOURLY RESOURCE GROUP OUTPUT TABLES
## Resource Group annaul Information: 2022-2035. Gives info on resource group 
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
   filter(Year<=2035) %>%
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
   # Filter out dates after 2035
   filter(Year<=2035) %>%
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
   # Filter out dates after 2035
   filter(Year<=2035) %>%
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
   
   write_xlsx(dataset_names, path = here("Data Files",filename),
              col_names = TRUE, format_headers = TRUE)
}