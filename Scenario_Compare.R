################################################################################
# TITLE: Scenario_Compare
# DESCRIPTION:  Compare filtered .R datafiles between scenarios.
#
#
# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: September 2023
#
# NOTES: Make sure the project file is open first or "here" commands wont work right.
#        Before running, create folder called "Data Files" inside project directory and populate it with 
#        any data you want to access. 
################################################################################

################################################################################
## LOAD REQUIRED PACKAGES AND SOURCE FUNCTIONS
################################################################################

{ # Must load the here package in order to make sure internal project directories work
  library(here)
  
  # Import functions from other R files, take from the functions folder in R project
  source(here('Functions','Other_Functions.R'))       # Other functions used in plotting functions
  source(here('Functions','Data_Filt_To_Table.R'))    # Functions that filter data and export it to excel sehets
  source(here('Functions','Data_Filt_To_RFile.R'))    # 
  
  # Packages required
  packs_to_load = c("tidyverse","scales","grid","gtable","gridExtra","ggpubr","extrafont",
                    "lubridate","cowplot","scales","dplyr","reshape2","zoo",
                    "ggpattern","here","showtext","DescTools",
                    "openxlsx","timeDate","writexl","viridis","ggnewscale")
  # Function to check for packages, install if not present, and load
  packs_check(packs_to_load)
  
}

################################################################################
## DEFINE SCENARIOS TO COMBINE
################################################################################
  # Specify the scenario names to combine. Can combine an already combined file with new one 
  # (ex: If R file already contains case1, case2, case3, we can use the "ScenarioName" for the 
  # combined file and merge it with a new scenario)
  ScenarioName1<-"AugCase"
  ScenarioName2<-"TestCase"
  
  # This is the name for the new combined R files
  CScenarioName <-"Sep_Aug"

################################################################################
## LOAD SCENARIOS, COMBINE, AND SAVE
################################################################################
  # Load files from each scenario and combine into one R file with new name
  {
  # Annual Resource Group Capacity
  Ann_Cap1<-readRDS(here("Data Files","Case Specific R Files",ScenarioName1,paste("Ann_Cap_",ScenarioName1, sep = "")))
  Ann_Cap2<-readRDS(here("Data Files","Case Specific R Files",ScenarioName2,paste("Ann_Cap_",ScenarioName2, sep = "")))
    # Combine into new R file and save combined file in new folder
      Ann_Cap<-rbind(Ann_Cap1,Ann_Cap2)
        rm(Ann_Cap1,Ann_Cap2)
      SaveR_Loc(Ann_Cap,CScenarioName,"Ann_Cap_")
  
  # Annual Fuel Usage
  Ann_Fuel1<-readRDS(here("Data Files","Case Specific R Files",ScenarioName1,paste("Ann_Fuel_",ScenarioName1, sep = "")))
  Ann_Fuel2<-readRDS(here("Data Files","Case Specific R Files",ScenarioName2,paste("Ann_Fuel_",ScenarioName2, sep = "")))
    # Combine into new R file and save combined file in new folder
      Ann_Fuel<-rbind(Ann_Fuel1,Ann_Fuel2)
        rm(Ann_Fuel1,Ann_Fuel2)
      SaveR_Loc(Ann_Fuel,CScenarioName,"Ann_Fuel_")
  
  
  # Annual Heat Rates
  Ann_HR1<-readRDS(here("Data Files","Case Specific R Files",ScenarioName1,paste("Ann_HR_",ScenarioName1, sep = "")))
  Ann_HR2<-readRDS(here("Data Files","Case Specific R Files",ScenarioName2,paste("Ann_HR_",ScenarioName2, sep = "")))
    # Combine into new R file and save combined file in new folder
      Ann_HR<-rbind(Ann_HR1,Ann_HR2)
        rm(Ann_HR1,Ann_HR2)
      SaveR_Loc(Ann_HR,CScenarioName,"Ann_HR_")
  
  # Annual EPC Values
  EPC_Values1<-readRDS(here("Data Files","Case Specific R Files",ScenarioName1,paste("EPC_Values_",ScenarioName1, sep = "")))
  EPC_Values2<-readRDS(here("Data Files","Case Specific R Files",ScenarioName2,paste("EPC_Values_",ScenarioName2, sep = "")))
    # Combine into new R file and save combined file in new folder
      EPC_Values<-rbind(EPC_Values1,EPC_Values2)
        rm(EPC_Values1,EPC_Values2)
      SaveR_Loc(EPC_Values,CScenarioName,"EPC_Values_")
  
  # Annual Resource Group Data
  ResGrYr1<-readRDS(here("Data Files","Case Specific R Files",ScenarioName1,paste("ResGrYr_",ScenarioName1, sep = "")))
  ResGrYr2<-readRDS(here("Data Files","Case Specific R Files",ScenarioName2,paste("ResGrYr_",ScenarioName2, sep = "")))
    # Combine into new R file and save in new folder
      ResGrYr<-rbind(ResGrYr1,ResGrYr2)
        rm(ResGrYr1,ResGrYr2)
      SaveR_Loc(ResGrYr,CScenarioName,"ResGrYr_")
  
  
  # Total Capacity Changes (Study)
  Tot_Cap1<-readRDS(here("Data Files","Case Specific R Files",ScenarioName1,paste("Tot_Cap_",ScenarioName1, sep = "")))
  Tot_Cap2<-readRDS(here("Data Files","Case Specific R Files",ScenarioName2,paste("Tot_Cap_",ScenarioName2, sep = "")))
    # Combine into new R file and save combined file in new folder
      Tot_Cap<-rbind(Tot_Cap1,Tot_Cap2)
        rm(Tot_Cap1,Tot_Cap2)
      SaveR_Loc(Tot_Cap,CScenarioName,"Tot_Cap_")
  
  # Annual Zone Data
  Zone1<-readRDS(here("Data Files","Case Specific R Files",ScenarioName1,paste("Zone_",ScenarioName1, sep = "")))
  Zone2<-readRDS(here("Data Files","Case Specific R Files",ScenarioName2,paste("Zone_",ScenarioName2, sep = "")))
    # Combine into new R file and save combined file in new folder
      Zone<-rbind(Zone1,Zone2)
      rm(Zone1,Zone2)
      SaveR_Loc(Zone,CScenarioName,"Zone_")
  }
  
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
         '2 Model Capaity Added',
         '3 Total Capacity Changes',
         '4 Percent Capacity',
         '5 Generation by Tech',
         '6 Percent Generation',
         '7 Emissions by Tech',
         '8 Emissions Cost by Tech',
         '9 Emissions Total',
         '10 Cost by Tech',
         '11 Total Costs',
         '12  Average Pool Price',
         '13 Imports / Exports',
         '14 Net Imports',
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
    mutate(Sim_Name=paste(Sim_Name,"Capacity_MW", sep="\n"))%>%
    pivot_wider(names_from=Sim_Name,values_from =Avail_Cap)%>%
    arrange(.,(Year))
    
  
# Available model capacity by percentage
  Perc_Cap<-ResGrYr %>%
    group_by(Sim_Name,Year)%>%
    mutate(TotalCap=sum(Capacity_MW),
           PercGen=Capacity_MW/TotalCap)%>%
    group_by(Plant_Type,Sim_Name,Year)%>%
    summarize(Percent_Cap=paste(round(100*PercGen,2),'%'))%>%
    mutate(Sim_Name=paste(Sim_Name,"% Capacity", sep="\n"))%>%
    pivot_wider(names_from=Sim_Name,values_from =Percent_Cap)%>%
    arrange(.,(Year)) 
  
# Total Capacity added by model
  Auroraadd<-Tot_Cap %>%
    group_by(Plant_Type,Scenario)%>%
    summarize(Cap=sum(New_Cap_AURORA))%>%
    mutate(Scenario=paste(Scenario,"Model_Capacity_Added_MW", sep="\n"))%>%
    pivot_wider(names_from=Scenario,values_from =Cap)
  
  # Total Capacity added
 Totaladd<-Tot_Cap %>%
    group_by(Plant_Type,Scenario)%>%
    summarize(Cap=sum(New_Cap_Total))%>%
    mutate(Scenario=paste(Scenario,"Total_Capacity_Added_MW", sep="\n"))%>%
    pivot_wider(names_from=Scenario,values_from =Cap)
  
# Total generation 
  Gen_ByGroup<-ResGrYr %>%
    group_by(Plant_Type,Sim_Name,Year)%>%
    summarize(TotalGen=sum(Output_MWH)/1000000)%>%
    mutate(Sim_Name=paste(Sim_Name,"Generation TWh", sep="\n"))%>%
    pivot_wider(names_from=Sim_Name,values_from =TotalGen)%>%
    arrange(.,(Year))
    
# Percent generation 
  Perc_Gen<-ResGrYr %>%
    group_by(Sim_Name,Year)%>%
    mutate(TotalGen=sum(Output_MWH),
           PercGen=Output_MWH/TotalGen)%>%
    group_by(Plant_Type,Sim_Name,Year)%>%
    summarize(Percent_Gen=paste(round(100*PercGen,2),'%'))%>%
    mutate(Sim_Name=paste(Sim_Name,"% Generation", sep="\n"))%>%
    pivot_wider(names_from=Sim_Name,values_from =Percent_Gen)%>%
    arrange(.,(Year))  
  
# Emissions 
  Em_ByGroup<-ResGrYr %>%
    group_by(Plant_Type,Sim_Name,Year)%>%
    summarize(TotalEm=sum(Emissions_Tonne)/1000000)%>%
    mutate(Sim_Name=paste(Sim_Name,"Emissions Mtonne", sep="\n"))%>%
    filter(TotalEm>0)%>%
    pivot_wider(names_from=Sim_Name,values_from =TotalEm)%>%
    arrange(.,(Year))
  
  # Emissions Cost
  EmCost_ByGroup<-ResGrYr %>%
    group_by(Plant_Type,Sim_Name,Year)%>%
    summarize(TotalEm=sum(Emissions_Cost)/1000000)%>%
    mutate(Sim_Name=paste(Sim_Name,"Emissions Cost ($M)", sep="\n"))%>%
    pivot_wider(names_from=Sim_Name,values_from =TotalEm)%>%
    arrange(.,(Year))
  
  # Emissions Total
  #   Filters out cogen - may want to add back in later
  Em_Total<-ResGrYr %>%
    filter(Emissions_Tonne>0,
           !Plant_Type %in% c("Cogeneration"))%>%
    group_by(Sim_Name,Year)%>%
    summarize(TotalEm=sum(Emissions_Tonne)/1000000)%>%
    mutate(Sim_Name=paste(Sim_Name,"Non-Cogen Emissions (Mt)", sep="\n"))%>%
    pivot_wider(names_from=Sim_Name,values_from =TotalEm)%>%
    arrange(.,(Year))
  
  # Resource Group Costs 
  OPEX_G<-ResGrYr %>%
    group_by(Plant_Type,Sim_Name,Year)%>%
    summarize(Cost_Tot=sum(OPEX)/1000000)%>%
    mutate(Cost_Type="OPEX ($M)")
  
  CAPEX_G<-ResGrYr %>%
    group_by(Plant_Type,Sim_Name,Year)%>%
    summarize(Cost_Tot=sum(CAPEX)/1000000)%>%
    mutate(Cost_Type="CAPEX ($M)")
  
  Rev_G<-ResGrYr %>%
    group_by(Plant_Type,Sim_Name,Year)%>%
    summarize(Cost_Tot=sum(Revenue)/1000000)%>%
    mutate(Cost_Type="Revenue ($M)")
  
  Value_G<-ResGrYr %>%
    group_by(Plant_Type,Sim_Name,Year)%>%
    summarize(Cost_Tot=sum(Value)/1000000)%>%
    mutate(Cost_Type="Value ($M)")
  
  Costs_G<-bind_rows(Rev_G,OPEX_G,CAPEX_G,Value_G)%>%
    pivot_wider(names_from=Sim_Name,values_from =Cost_Tot)%>%
    arrange(.,Cost_Type,Year)
  
  # Zone Costs
  OPEX_T<-ResGrYr %>%
    group_by(Sim_Name,Year)%>%
    summarize(Cost_Tot=sum(OPEX)/1000000000)%>%
    mutate(Cost_Type="OPEX ($B)")
  
  CAPEX_T<-ResGrYr %>%
    group_by(Sim_Name,Year)%>%
    summarize(Cost_Tot=sum(CAPEX)/1000000000)%>%
    mutate(Cost_Type="CAPEX ($B)")
  
  Rev_T<-ResGrYr %>%
    group_by(Sim_Name,Year)%>%
    summarize(Cost_Tot=sum(Revenue)/1000000000)%>%
    mutate(Cost_Type="Revenue ($B)")
  
  Value_T<-ResGrYr %>%
    group_by(Sim_Name,Year)%>%
    summarize(Cost_Tot=sum(Value)/1000000000)%>%
    mutate(Cost_Type="Value ($B)")
  
  Costs_T<-bind_rows(
    Rev_T,
    #OPEX_T,CAPEX_T,
    Value_T)%>%
    pivot_wider(names_from=Sim_Name,values_from =Cost_Tot)%>%
    arrange(.,Cost_Type,Year)
    
 # Pool Price
  PoolPrice<-Zone %>%
    group_by(Scenario,Year)%>%
    summarize(Avg_P=sum(Avg_Price))%>%
    mutate(Scenario=paste(Scenario,"Average Price ($/MWh)", sep="\n"))%>%
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
    mutate(Scenario=paste(Scenario,"Net Import (TWh)", sep="\n"))%>%
    pivot_wider(names_from=Scenario,values_from =Net_Imp)%>%
    arrange(.,(Year)) 
  
  # Fuel Usage
  Fuel_Used<-Ann_Fuel %>%
    group_by(ID,Scenario,Year)%>%
    summarize(Burn=sum(Usage_GJ))%>%
    mutate(Scenario=paste(Scenario,"Fuel Used (GJ)", sep="\n"))%>%
    pivot_wider(names_from=Scenario,values_from =Burn)%>%
    arrange(.,(Year))
  
  Fuel_P<-Ann_Fuel %>%
    group_by(ID,Scenario,Year)%>%
    summarize(Burn=sum(`Price_$/GJ`))%>%
    mutate(Scenario=paste(Scenario,"Fuel Price ($/GJ)", sep="\n"))%>%
    pivot_wider(names_from=Scenario,values_from =Burn)%>%
    arrange(.,(Year))
  
}  
################################################################################
## SEND ALL TO ONE EXCEL FILE
################################################################################
  
  dataset_names <-list('Info'=Sim_Info,
                       '1 Capacity by Tech'=Cap_ByGroup,
                       '2 Model Capaity Added'=Auroraadd,
                       '3 Total Capacity Changes'=Totaladd,
                       '4 Percent Capacity'=Perc_Cap,
                       '5 Generation by Tech'=Gen_ByGroup,
                       '6 Percent Generation'=Perc_Gen,
                       '7 Emissions by Tech'=Em_ByGroup,
                       '8 Emissions Cost by Tech'=EmCost_ByGroup,
                       '9 Emissions Total'=Em_Total,
                       '10 Cost by Tech'=Costs_G,
                       '11 Total Costs'=Costs_T,
                       '12  Average Pool Price'=PoolPrice,
                       '13 Imports_Exports'=Imp_Exp,
                       '14 Net Imports'=Net_Imp,
                       '15 Fuel Usage'=Fuel_Used,
                       '16 Fuel Price'=Fuel_P,
                       
                       'A All Groups Data'=ResGrYr,
                       'B Annual Zone'=Zone,
                       'C Annual Capacity Changes' =Ann_Cap,
                       'D EPC Values'=EPC_Values,
                       'E Annual Fuel Data'=Ann_Fuel,
                       'F Average Heat Rates'=Ann_HR)
  
  filename <-paste(CScenarioName,".xlsx", sep = "")
  
  # Save the excel sheet to folder
  write_xlsx(dataset_names, path = here("Data Files","Scenario Compare",filename),
             col_names = TRUE, format_headers = TRUE)  
  
  
  








