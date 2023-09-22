################################################################################
# TITLE: Scenario_Compare
# DESCRIPTION:  Compare filtered .R datafiles between scnearios
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
  packs_to_load = c("tidyverse","ggplot2","scales","grid","gtable","gridExtra","ggpubr","extrafont",
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
## SEND ALL TO ONE EXCEL FILE
################################################################################
  
  dataset_names <-list('1 Reource Groups'=ResGrYr,
                       '2 Annual Zone'=Zone,
                       '3 Annual Capacity Changes' =Ann_Cap,
                       '4 Total Capacity Changes'=Tot_Cap,
                       '5 EPC Values'=EPC_Values,
                       '6 Annual Fuel Data'=Ann_Fuel,
                       '7 Average Heat Rates'=Ann_HR)
  
  filename <-paste(CScenarioName,".xlsx", sep = "")
  
  write_xlsx(dataset_names, path = here("Data Files",filename),
             col_names = TRUE, format_headers = TRUE)  
  
  
  
  
  
  








