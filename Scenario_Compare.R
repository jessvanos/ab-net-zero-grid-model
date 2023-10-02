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
  source(here('Functions','Data_Filt_To_File.R'))     # Functions that filter data and export it to excel sheets

  # Packages required
  packs_to_load = c("tidyverse","scales","grid","gtable","gridExtra","ggpubr","extrafont",
                    "lubridate","cowplot","scales","dplyr","reshape2","zoo",
                    "ggpattern","here","showtext","DescTools",
                    "openxlsx","timeDate","writexl","viridis","ggnewscale","janitor","sjmisc")
  # Function to check for packages, install if not present, and load
  packs_check(packs_to_load)
  
}

################################################################################
## DEFINE SCENARIOS TO COMBINE
################################################################################
  # Specify the scenario names to combine. Can combine an already combined file with new one 
  # (ex: If R file already contains case1, case2, case3, we can use the "ScenarioName" for the 
  # combined file and merge it with a new scenario)

  # EXISTING SCENARIOS
  #   "Sep18"         September case following major revisions to file.
  #   "Sep21"         September case following CCS retrofit option inclusion (forced here).
  #   "Sep28"         Normal LZ run 2022-2040, RM=0%
  #   "Sep29"         Normal LZ run 2022-2040, RM=15%
  #   "Sep_28_29"     Combined Sept 28 and 29 runs to one file



{
  # Define cases here
  ScenarioName1<-"Sep28"
  ScenarioName2<-"Sep29"
  
  # This is the name for the new combined R files and excel sheet
  CScenarioName <-"Sep_28_29"
}

################################################################################
## LOAD SCENARIOS, COMBINE, AND SAVE
################################################################################
  
# Function to combine two scenarios. Annual results data
CombineFilesR(ScenarioName1,ScenarioName2,CScenarioName)

 
  
  
  








