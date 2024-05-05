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
  source(here('Functions','Other_Functions.R'))                # Other functions used in plotting functions
  source(here('Functions','Data_Filt_To_File.R'))              # Functions that filter data and export it to excel sheets
  source(here('Functions','Scenario_Compare_Functions.R'))     # Functions that filter data and export it to excel sheets
  source(here('Functions','Group_PlotSave.R'))                 # Saving functions import
  

  # Packages required
  packs_to_load = c("tidyverse","scales","grid","gtable","gridExtra","ggpubr","extrafont",
                    "lubridate","cowplot","scales","dplyr","reshape2","zoo",
                    "ggpattern","here","showtext","DescTools",
                    "openxlsx","timeDate","writexl","viridis","ggnewscale","janitor","sjmisc","treemapify","waterfalls")
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
  #   'CP_12Apr''CP_04Apr'       Current policy (TX mod, normal)
  #   'CER_14Apr''CER_02Apr'     Draft CER (TX mod, normal)
  #   'EL_19Apr''EL_06Apr'       Emission limit (TX mod, normal)
  #   'CP_noITC_07Apr'           CP no investment tax credits
  #   'CER_noITC_08Apr'          CER no investment tax credits
  #   'CP_noEPC_10Apr'           CP no emission performance credits
  #   'TIER2050_18Apr'           CP with TIER limit to zero by 2050
  #   'TIER2035_11Apr'           CP with TIER limit to zero by 2035 
  #   'CP_noEPC_10Apr'          CP with EPCs values at 0% Cprice
  #   'CP_50EPCs_22Apr'          CP with EPCs values at 50% Cprice

  # COMBINED SCENARIOS
  #   'Main_3'                   CP_04Apr, CER_02Apr, EL_06Apr   
  #   'Main_3_BCmod'             CP_12Apr, CER_14Apr, EL_19Apr   
  #   'ITC_4'                    CP_04Apr, CER_02Apr, CP_noITC_07Apr, CER_noITC_08Apr
{
  # Define cases here
  ScenarioName1<-"TIER_2"
  ScenarioName2<-"TIER2035_11Apr"
  
  # This is the name for the new combined R files and excel sheet. Adds compare to name automatically!
  CScenarioName <-"TIER_3"
}

################################################################################
## LOAD SCENARIOS, COMBINE, AND SAVE
################################################################################
  
  # Function to combine two scenarios. Annual results data
  CombineFilesR(ScenarioName1,ScenarioName2,CScenarioName)

################################################################################
## READ R FILES WITH SCENARIO DATA (AFTER COMBINE) AND PLOT
################################################################################

# Folder name & color selection
CaseName <- "TIER_3" # Match 'CScenarioName' from combine files (ie: folder name)
COL_choice = 3

# Read R-Data files for compare scenario
{
  # Annual Resource Group Capacity
  Ann_Cap<-readRDS(here("Data Files","Result Files",CaseName,paste("Ann_Cap_",CaseName, sep = "")))
  # Annual Fuel Usage
  Ann_Fuel<-readRDS(here("Data Files","Result Files",CaseName,paste("Ann_Fuel_",CaseName, sep = "")))
  # Annual Heat Rates
  Ann_HR<-readRDS(here("Data Files","Result Files",CaseName,paste("Ann_HR_",CaseName, sep = "")))
  # Annual EPC Values
  EPC_Values<-readRDS(here("Data Files","Result Files",CaseName,paste("EPC_Values_",CaseName, sep = "")))
  # Annual Resource Group Data
  ResGrYr<-readRDS(here("Data Files","Result Files",CaseName,paste("ResGrYr_",CaseName, sep = "")))
  # Total Capacity Changes (Study)
  Tot_Cap<-readRDS(here("Data Files","Result Files",CaseName,paste("Tot_Cap_",CaseName, sep = "")))
  # Annual Zone Data
  Zone<-readRDS(here("Data Files","Result Files",CaseName,paste("Zone_",CaseName, sep = "")))
}  

################################################################################
## PLOT SETTINGS
################################################################################

{ # Available Fonts for plotting, can choose different one and change Plot_Text if needed
  # Uses local computer font files (search font in search bar to confirm font names)
  
  font_add(family="Times",regular="times.ttf")
  font_add("times_bf", regular = "timesbd.ttf")
  Plot_Text <- "Times"
  Plot_Text_bf <- "times_bf"
  showtext_auto()
  
  # Gives years to summarize info from 
  Years2Disp <- c(2023,2025,2030,2035,2040,2045) # Years to show in figures

  # Set default size for plot features to be constant. All based on general text size
  { GenText_Sz =46 # GGsave
    #GenText_Sz = 20 # Windows save 
    Tit_Sz = GenText_Sz-2
    XTit_Sz = GenText_Sz+6
    YTit_Sz = GenText_Sz+6
    Leg_Sz=GenText_Sz-6
    Overall_Sz=GenText_Sz}
  
  
  # Set legend color schemes for constancy
  { 
    if (COL_choice == 1){
      # Normal Color 
      # Import/Export
      cOL_IMPORT <- "#F8B660"  
      cOL_EXPORT <- "burlywood4"
      
      # Coal/Cogen
      cOL_NUCLEAR <- "midnightblue"
      cOL_COAL <- "black"
      cOL_COGEN <- "gray30"
      
      # H2 groups (blues)
      cOL_SCGT_H2 <- "#7e4e90ff"
      cOL_NGCC_H2 <- "darkorchid4"
      COL_H2 <- cOL_NGCC_H2  
      #cOL_SCGT_Blend <- "#7FABD3"
      #cOL_NGCC_Blend <- "#3573B9"
      #COL_Blend <- cOL_NGCC_Blend 
      
      # Gas Groups (Purples)
      cOL_COal2Gas <-  "gray65"
      cOL_NGConv <- cOL_COal2Gas
      cOL_SCGT <- "gray50"
      cOL_NGCC <- "gray85"
      COL_NatGas <-cOL_NGCC 
      cOL_NGCC_CCS <-"#A79FE1"
      
      # Renewables and Other
      cOL_OTHER <- "steelblue1"
      cOL_HYDRO <- "royalblue3"
      cOL_SOLAR <- "#FDE725FF"#'darkgoldenrod1'#
      cOL_WIND <- "#73D055FF" #"#238b45"#
      col_WIND_CURT <-"#1d632d"
      col_SOLAR_CURT <-'goldenrod1'
      
      # Storage Groups
      cOL_STORAGE <- "rosybrown1" 
      COL_Battery <-"rosybrown1" 
      COL_CompAir <-"coral3"
      COL_Pumped <-"firebrick4"
      
    }else if (COL_choice == 2){
      # Gray-scale safe colors
      # Renewables and Other
      cOL_SOLAR <- "#BDBDBD"
      cOL_WIND <-  "#565656"
      col_WIND_CURT <-cOL_WIND
      col_SOLAR_CURT <-cOL_SOLAR
      cOL_HYDRO <- "#717171"
      cOL_OTHER <- #7b7b7b"
        
        # H2 groups (blues)
        cOL_SCGT_H2 <- "#272727"
      cOL_NGCC_H2 <- "#7e7e7e"
      COL_H2 <- cOL_SCGT_H2 
      
      # Gas Groups (Purples)
      cOL_NGCC_CCS <-"#464646"
      cOL_SCGT <-"#9B9B9B"
      cOL_NGCC <- "#DDDDDD"
      cOL_COal2Gas <- "#BDBDBD"
      cOL_NGConv <- cOL_COal2Gas
      COL_NatGas <-cOL_NGCC
      
      # Coal/Cogen
      cOL_COAL <- "#282828"
      cOL_COGEN <-"#0A0A0A"
      cOL_NUCLEAR <- "midnightblue"
      
      # Import/Export
      cOL_IMPORT <- "white" 
      cOL_EXPORT <- "white"
      
      # Storage Groups
      COL_Battery <-"#E7E7E7"
      COL_CompAir <-"#EFEFEF"
      COL_Pumped <-"#F5F5F5"
      cOL_STORAGE <- COL_Battery
      
    }else{
      
      # Gray-scale safe colors
      cOL_SOLAR <- "#D9D012"
      cOL_WIND <-  "#237636"
      col_WIND_CURT <-cOL_WIND
      col_SOLAR_CURT <-cOL_SOLAR
      cOL_HYDRO <- "#5965F8"
      cOL_OTHER <- "#3EA836"
      
      # H2 groups (blues)
      cOL_SCGT_H2 <- "#0E3239"
      cOL_NGCC_H2 <- "#7e7e7e"
      COL_H2 <- cOL_SCGT_H2 
      
      # Gas Groups (Purples)
      cOL_NGCC_CCS <-"#464646"
      cOL_SCGT <-"#9B9B9B"
      cOL_NGCC <- "#DDDDDD"
      cOL_COal2Gas <- "#C4AEFC"
      cOL_NGConv <- cOL_COal2Gas
      COL_NatGas <-cOL_NGCC
      
      # Coal/Cogen
      cOL_COAL <- "#460E65"
      cOL_COGEN <-"#0A0A0A"
      cOL_NUCLEAR <- "midnightblue"
      
      # Import/Export
      cOL_IMPORT <- "white" 
      cOL_EXPORT <- "white"
      
      # Storage Groups
      COL_Battery <-"#F4EEA0" 
      COL_CompAir <-"#EFEFEF"
      COL_Pumped <-"#F5F5F5"
      cOL_STORAGE <- COL_Battery
      
    }
    # Set plot color transparacny 
    Plot_Trans<-1
    
    ## Now Define Lists to assign legends and colors in plots
    colours1=c("Trade"= cOL_EXPORT, "Coal"=cOL_COAL, "Cogeneration"=cOL_COGEN, 
               "Coal-to-Gas"=cOL_NGConv,"Hydrogen Simple Cycle"=cOL_SCGT_H2,"Hydrogen Combined Cycle"=cOL_NGCC_H2,
               #"Blended  Simple Cycle"=cOL_SCGT_Blend,"Blended  Combined Cycle"=cOL_NGCC_Blend,
               "Natural Gas Combined Cycle + CCS"=cOL_NGCC_CCS,
               "Natural Gas Simple Cycle"=cOL_SCGT, "Natural Gas Combined Cycle"=cOL_NGCC, 
               "Hydro"=cOL_HYDRO, "Other"=cOL_OTHER, "Wind"=cOL_WIND, 
               "Solar"=cOL_SOLAR, "Storage"=cOL_STORAGE)
    
    colours1b=c("Trade"= cOL_EXPORT, "Coal"=cOL_COAL, "Cogeneration"=cOL_COGEN, 
                "Coal-to-Gas"=cOL_NGConv,"Hydrogen Simple Cycle"=cOL_SCGT_H2,"Hydrogen Combined Cycle"=cOL_NGCC_H2,
                #"Blended  Simple Cycle"=cOL_SCGT_Blend,"Blended  Combined Cycle"=cOL_NGCC_Blend,
                "Natural Gas Combined Cycle + CCS"=cOL_NGCC_CCS,
                "Natural Gas Simple Cycle"=cOL_SCGT, "Natural Gas Combined Cycle"=cOL_NGCC, 
                "Hydro"=cOL_HYDRO, "Other"=cOL_OTHER, "Wind"=cOL_WIND, 
                "Solar"=cOL_SOLAR, "Storage"=cOL_STORAGE,"Demand Curtailment"="black")
    
    # Used in Day2
    colours1c=c("Import"= cOL_IMPORT, "Coal"=cOL_COAL, "Cogeneration"=cOL_COGEN, 
                "Coal-to-Gas"=cOL_NGConv,"H2SC"=cOL_SCGT_H2,"H2CC"=cOL_NGCC_H2,
                #"Blended  Simple Cycle"=cOL_SCGT_Blend,"Blended  Combined Cycle"=cOL_NGCC_Blend,
                "NGCC+CCS"=cOL_NGCC_CCS,
                "SCCT"=cOL_SCGT, "NGCC"=cOL_NGCC, 
                "Hydro"=cOL_HYDRO, "Other"=cOL_OTHER, "Wind"=cOL_WIND, 
                "Solar"=cOL_SOLAR, "Storage"=cOL_STORAGE)
    
    colours2 = c("Coal"= cOL_COAL, "Coal-to-Gas"=cOL_COal2Gas, "Cogen"=cOL_COGEN,
                 "Natural Gas"=COL_NatGas,"Natural Gas + CCS"=cOL_NGCC_CCS,"Hydrogen"=COL_H2,
                 #"Natual Gas and Hydrogen Blend"=COL_Blend,
                 "Hydro"=cOL_HYDRO, "Other"=cOL_OTHER, 
                 "Wind"=cOL_WIND, "Solar"=cOL_SOLAR, "Storage"=cOL_STORAGE,"Nuclear"=cOL_NUCLEAR)
    
    colours3 = c("Coal"=cOL_COAL, "Cogeneration"=cOL_COGEN, 
                 "Coal-to-Gas"=cOL_NGConv,"Hydrogen Simple Cycle"=cOL_SCGT_H2,"Hydrogen Combined Cycle"=cOL_NGCC_H2,
                 #"Blended  Simple Cycle"=cOL_SCGT_Blend,"Blended  Combined Cycle"=cOL_NGCC_Blend,
                 "Natural Gas Combined Cycle + CCS"=cOL_NGCC_CCS,
                 "Natural Gas Simple Cycle"=cOL_SCGT, "Natural Gas Combined Cycle"=cOL_NGCC, 
                 "Hydro"=cOL_HYDRO, "Other"=cOL_OTHER, "Wind"=cOL_WIND, 
                 "Solar"=cOL_SOLAR, "Storage"=cOL_STORAGE)
    
    colours3b = c("Net Imports"=cOL_IMPORT,"Coal"=cOL_COAL, "Cogeneration"=cOL_COGEN, 
                 "Coal-to-Gas"=cOL_NGConv,"Hydrogen Simple Cycle"=cOL_SCGT_H2,"Hydrogen Combined Cycle"=cOL_NGCC_H2,
                 #"Blended  Simple Cycle"=cOL_SCGT_Blend,"Blended  Combined Cycle"=cOL_NGCC_Blend,
                 "Natural Gas Combined Cycle + CCS"=cOL_NGCC_CCS,
                 "Natural Gas Simple Cycle"=cOL_SCGT, "Natural Gas Combined Cycle"=cOL_NGCC, 
                 "Hydro"=cOL_HYDRO, "Other"=cOL_OTHER, "Wind"=cOL_WIND, 
                 "Solar"=cOL_SOLAR, "Storage"=cOL_STORAGE)
    
    Patterns3 = c("Coal"="none", "Cogeneration"="none", 
                  "Coal-to-Gas"="stripe","Hydrogen Simple Cycle"="none","Hydrogen Combined Cycle"="none",
                  #"Blended  Simple Cycle"="stripe","Blended  Combined Cycle"="stripe",
                  "Natural Gas Combined Cycle + CCS"="none",
                  "Natural Gas Simple Cycle"="none", "Natural Gas Combined Cycle"="none", 
                  "Hydro"="none", "Other"="none", "Wind"="none", 
                  "Solar"="none", "Storage"="none")
    
    colours4=c("Import"= cOL_IMPORT, "Coal-to-Gas"=cOL_COal2Gas, "Coal"=cOL_COAL,"Cogen"=cOL_COGEN, 
               "Natural Gas"=COL_NatGas,"Natural Gas + CCS"=cOL_NGCC_CCS,"Hydrogen"=COL_H2,
               #"Natual Gas and Hydrogen Blend"=COL_Blend,
               "Hydro"=cOL_HYDRO, "Other"=cOL_OTHER, "Nuclear"=cOL_NUCLEAR,
               "Wind"=cOL_WIND, "Solar"=cOL_SOLAR, "Storage"=cOL_STORAGE)
    
    colours5 = c("Cogeneration"=cOL_COGEN, 
                 "Coal-to-Gas"=cOL_NGConv,"Hydrogen Simple Cycle"=cOL_SCGT_H2,"Hydrogen Combined Cycle"=cOL_NGCC_H2,
                 #"Blended  Simple Cycle"=cOL_SCGT_Blend,"Blended  Combined Cycle"=cOL_NGCC_Blend,
                 "Natural Gas Combined Cycle + CCS"=cOL_NGCC_CCS,"Natural Gas Combined Cycle CCS Retrofit"=cOL_NGCC_CCS,
                 "Natural Gas Simple Cycle"=cOL_SCGT, "Natural Gas Combined Cycle"=cOL_NGCC, 
                 "Hydro"=cOL_HYDRO, "Other"=cOL_OTHER, "Wind"=cOL_WIND, 
                 "Solar"=cOL_SOLAR,  "Storage - Battery"=COL_Battery, 
                 "Storage - Compressed Air"=COL_CompAir, "Storage - Pumped Hydro"=COL_Pumped,
                 "Nuclear"=cOL_NUCLEAR
    )
    
    Patterns5 = c("Cogeneration"="none", 
                  "Coal-to-Gas"="stripe","Hydrogen Simple Cycle"="none","Hydrogen Combined Cycle"="none",
                  #"Blended  Simple Cycle"="none","Blended  Combined Cycle"="none",
                  "Natural Gas Combined Cycle + CCS"="none","Natural Gas Combined Cycle CCS Retrofit"="stripe",
                  "Natural Gas Simple Cycle"="none", "Natural Gas Combined Cycle"="none", 
                  "Hydro"="none", "Other"="none", "Wind"="none", 
                  "Solar"="none",  "Storage - Battery"="none", 
                  "Storage - Compressed Air"="none", "Storage - Pumped Hydro"="none",
                  "Nuclear"="none"
    )
    
    colours6=c("Natural Gas"=COL_NatGas,"Hydrogen"=COL_H2,
               #"Natual Gas and Hydrogen Blend"=COL_Blend,
               "Hydro"=cOL_HYDRO, "Other"=cOL_OTHER, 
               "Wind"=cOL_WIND, "Solar"=cOL_SOLAR,  "Storage - Battery"=COL_Battery, 
               "Storage - Compressed Air"=COL_CompAir, "Storage - Pumped Hydro"=COL_Pumped)
    
    colours7=c("Total Emissions"="black","Coal"=cOL_COAL, "Cogeneration"=cOL_COGEN,"NAICS 221112 Cogeneration"= cOL_COGEN,
               "Coal-to-Gas"=cOL_NGConv, 
               #"Blended  Simple Cycle"=cOL_SCGT_Blend,"Blended  Combined Cycle"=cOL_NGCC_Blend,
               "Natural Gas Combined Cycle + CCS"=cOL_NGCC_CCS,
               "Natural Gas Simple Cycle"=cOL_SCGT, "Natural Gas Combined Cycle"=cOL_NGCC, "Other"=cOL_OTHER)
    
    Lines7=c("Total Emissions"=1,"Coal"=1, "Cogeneration"=6, 
             "Coal-to-Gas"=2, 
             #"Blended  Simple Cycle"=5,"Blended  Combined Cycle"=6,
             "Natural Gas Combined Cycle + CCS"=2,
             "Natural Gas Simple Cycle"=5, "Natural Gas Combined Cycle"=3, "Other"=4)
    
    # EVERYTHING
    colours8 = c("Coal"=cOL_COAL,"Coal-to-Gas"=cOL_NGConv,
                 "Natural Gas Combined Cycle"=cOL_NGCC,"Natural Gas Simple Cycle"=cOL_SCGT, 
                 "Natural Gas Combined Cycle + CCS"=cOL_NGCC_CCS,"Natural Gas Combined Cycle CCS Retrofit"=cOL_NGCC_CCS,
                 "Hydrogen Simple Cycle"=cOL_SCGT_H2,"Hydrogen Combined Cycle"=cOL_NGCC_H2,
                 #"Blended  Simple Cycle"=cOL_SCGT_Blend,"Blended  Combined Cycle"=cOL_NGCC_Blend,
                 "Hydro"=cOL_HYDRO, "Other"=cOL_OTHER, "Wind"=cOL_WIND, 
                 "Solar"=cOL_SOLAR,  "Storage - Battery"=COL_Battery, 
                 "Storage - Compressed Air"=COL_CompAir, "Storage - Pumped Hydro"=COL_Pumped,"Cogeneration"=cOL_COGEN)
    
    Patterns8 = c("Coal"="none","Coal-to-Gas"="stripe",
                  "Natural Gas Combined Cycle"="none","Natural Gas Simple Cycle"="none", 
                  "Natural Gas Combined Cycle + CCS"="none","Natural Gas Combined Cycle CCS Retrofit"="stripe",
                  "Hydrogen Simple Cycle"="none","Hydrogen Combined Cycle"="none",
                  #"Blended  Simple Cycle"=cOL_SCGT_Blend,"Blended  Combined Cycle"=cOL_NGCC_Blend,
                  "Hydro"="none", "Other"="none", "Wind"="none", 
                  "Solar"="none",  "Storage - Battery"="none", 
                  "Storage - Compressed Air"="none", "Storage - Pumped Hydro"="none","Cogeneration"="none")

    
    colorsgroup_1 = c("Abated Natural Gas"="#001933",Hydrogen="#4472C4","Natural Gas"='#515151',"Other"='#767171',             
                     "Renewables"="#238b45","Storage"='#cc79a7','Coal'="black")
    
    AESO_colours <- c("goldenrod1", "gray60", "yellowgreen", "cornflowerblue",
                      "#001933")
    
  # Map scenario colors  
  COL_CP ="#4472C4"
  COL_CER =  '#A6A6A6'
  COL_EL ='gray85'
  
  COL_TIER2050 = "#238b45"
  COL_TIER2035 = "yellowgreen"
  
  COL_noITC = '#515151'
  COL_noITC_CER = '#cc79a7'
  
  COL_Az = "goldenrod1"
  COL_AZ_noH2 = "goldenrod4"

  COL_noEPC = "#440154FF"
  COL_50EPC = "#472D7BFF"
  
  COL_CP2x = "#001933"
  
  COL_CP_update='#32538E'
  COL_CER_update='#262626'
  COL_EL_update='#717171'
  
  # Scenario colors with historic
   sn_colors_l <-c("Draft CER"=COL_CER,
                   "Current Policy"=COL_CP,
                   "Emissions Limit"=COL_EL,
                   "TIER 2050" = COL_TIER2050,
                   "TIER 2035" = COL_TIER2035,
                   "No ITCs"= COL_noITC,
                   "No ITCs with CER"=COL_noITC_CER,
                   "Absolute Zero" = COL_Az,
                   "No H2 Absolute Zero" = COL_AZ_noH2,
                   "No Emission Credits" = COL_noEPC,
                   "50% EPC Value" = COL_50EPC,
                   "CP_txmod" = COL_CP_update,
                   "CER_txmod" =COL_CER_update,
                   "EL_txmod" = COL_EL_update,
                   "Increased Transmission" = COL_CP2x,
                   'Historic'='black')
   
   sn_line_l <-c("Draft CER"=1,
                 "Current Policy"=1,
                 "Emissions Limit"=1,
                 "TIER 2050" = 1,
                 "TIER 2035" = 1,
                 "No ITCs"= 1,
                 "No ITCs with CER"=1,
                 "Absolute Zero" = 1,
                 "No H2 Absolute Zero"=1,
                 "No Emission Credits"=1,
                 "50% EPC Value" = 1,
                 "CP_txmod" = 1,
                 "CER_txmod" =1,
                 "EL_txmod" = 1,
                 "Increased Transmission" = 1,
                 'Historic'=1)
   
   sn_colors_s <-c("CER"=COL_CER,
                   "CP"=COL_CP,
                   "EL"=COL_EL,
                   "TIER2050"=COL_TIER2050,
                   "TIER2035"=COL_TIER2035,
                   "noITCs"=COL_noITC,
                   "CERnoITCs"=COL_noITC_CER,
                   "AZ" = COL_Az,
                   "AZ_noH2"= COL_AZ_noH2,
                   "noEPCs" = COL_noEPC,
                   "50EPC" = COL_50EPC,
                   "CP_txmod" = COL_CP_update,
                   "CER_txmod" =COL_CER_update,
                   "EL_txmod" = COL_EL_update,
                   "CP_2TX" = COL_CP2x,
                   'Historic'='black')
   
   sn_line_s <-c("CER"=1,
                 "CP"=1,
                 "EL"=1,
                 "TIER2050"=1,
                 "TIER2035"=1,
                 "noITCs"=1,
                 "CERnoITCs"=1,
                 "AZ"=1,
                 "AZstrict"=1,
                 "noEPCs" =1,
                 "50EPC" = 1,
                 "CP_txmod" = 1,
                 "CER_txmod" =1,
                 "EL_txmod" = 1,
                 "CP_2TX" = 1,
                 'Historic'=1)
   
   # Scenario colors no historic
   sn_colors2_l <-c("Draft CER"=COL_CER,
                   "Current Policy"=COL_CP,
                   "Emissions Limit"=COL_EL,
                   "TIER 2050" = COL_TIER2050,
                   "TIER 2035" = COL_TIER2035,
                   "No ITCs"=COL_noITC,
                   "No ITCs with CER"=COL_noITC_CER,
                   "Absolute Zero" = COL_Az,
                   "No H2 Absolute Zero" = COL_AZ_noH2,
                   "No Emission Credits"=COL_noEPC,
                   "50% EPC Value" = COL_50EPC,
                   "CP_txmod" = COL_CP_update,
                   "EL_txmod" = COL_EL_update,
                   "CER_txmod" =COL_CER_update,
                   "Increased Transmission" = COL_CP2x
                   )
   sn_line2_l <-c("Draft CER"=1,
                 "Current Policy"=1,
                 "Emissions Limit"=1,
                 "TIER 2050" = 1,
                 "TIER 2035" = 1,
                 "No ITCs"=1,
                 "No ITCs with CER"=1,
                 "Absolute Zero" = 1,
                 "No H2 Absolute Zero" = 1,
                 "No Emission Credits" = 1,
                 "50% EPC Value" = 1,
                 "CP_txmod" = 1,
                 "CER_txmod" =1,
                 "EL_txmod" = 1,
                 "Increased Transmission" = 1
                 )
   sn_colors2_s <-c("CER"=COL_CER,
                   "CP"=COL_CP,
                   "EL"=COL_EL,
                   "TIER2050"= COL_TIER2050,
                   "TIER2035" = COL_TIER2035,
                   "noITCs"=COL_noITC,
                   "CERnoITCs"=COL_noITC_CER,
                   "AZ" = COL_Az,
                   "AZstrict"=COL_AZ_noH2,
                   "noEPCs" =COL_noEPC,
                   "50EPC" = COL_50EPC,
                   "CP_txmod" = COL_CP_update,
                   "CER_txmod" =COL_CER_update,
                   "EL_txmod" = COL_EL_update,
                   "CP_2TX" = COL_CP2x
                  )
   sn_line2_s <-c("CER"=1,
                 "CP"=1,
                 "EL"=1,
                 "TIER2050"=1,
                 "TIER2035"=1,
                 "noITCs"=1,
                 "CERnoITCs"=1,
                 "AZ"=1,
                 "AZstrict"=1,
                 "noEPCs" =1,
                 "50EPC" = 1,
                 "CP_txmod" = 1,
                 "CER_txmod" =1,
                 "EL_txmod" = 1,
                 "CP_2TX" = 1
                 )
   
  }
}
  
################################################################################
## CREATE COMPARE PLOTS
################################################################################
{
# Compare pool prices
GGSave_Loc_custom(CaseName,"Annual Pool Price Compare",AvgYr_price_COMPARE("l","Y"),12,8)
GGSave_Loc_custom(CaseName,"Annual Pool Price Compare2",AvgYr_price_COMPARE2("l","Y"),6,8)
  

# Compare Emissions
GGSave_Loc_custom(CaseName,"Annual Emissions Compare",AnnualEm_COMPARE("l", "Y"),12,8)
GGSave_Loc_custom(CaseName,"Annual Emissions Compare noncogen2",AnnualEm_COMPARE("l", "n"),12,4)
GGSave_Loc_custom(CaseName,"Annual Emissions Compare noncogen",AnnualEm_COMPARE("l", "n"),12,8)
GGSave_Loc_custom(CaseName,"Cummulative Emissions Compare noncogen",AnnualEm_Cum_COMPARE("l", "n"),12,8)

# Capacity changes
GGSave_Loc_custom(CaseName,"Total Capacity Added",Total_Cap_Add_COMPARE("l"),12,8)
GGSave_Loc_custom(CaseName,"Total Capacity Retired",Total_Cap_Ret_COMPARE("l"),12,8)
GGSave_Loc_custom(CaseName,"Capacity Relative to CP",Cap_Relative_COMPARE("l"),12,8)
GGSave_Loc_custom(CaseName,"2045 Capacity Relative to CP",Cap_Year_Relative_COMPARE("l",2045),12,8)
GGSave_Loc_custom(CaseName,"Annual Capacity Added Aggregated",Annual_Cap_Add_COMPARE("s"),14,6)
GGSave_Loc_custom(CaseName,"Annual Capacity Years",Annual_Cap_COMPARE("s"),14,6)

# Total Gen annual
GGSave_Loc_custom(CaseName,"Annual Generation Aggregated",Annual_Gen_COMPARE("s"),14,6)
GGSave_Loc_custom(CaseName,"Year Gen Study 2023",Year_Gen_COMPARE("l",2023,show_neg="N"),12,8)
GGSave_Loc_custom(CaseName,"Year Gen Study 2045",Year_Gen_COMPARE("l",2045,show_neg="N"),12,8)
GGSave_Loc_custom(CaseName,"Year Gen Study 2045 with neg",Year_Gen_COMPARE("l",2045,show_neg="Y"),12,8)

# Total Gen Cummulative
GGSave_Loc_custom(CaseName,"Total Gen Study",Total_Gen_COMPARE("l"),12,8)
GGSave_Loc_custom(CaseName,"Total Gen relative to CP2",Total_Gen_Relative_COMPARE("l"),12,8)
GGSave_Loc_custom(CaseName,"Study Gen perc with Cogen",Total_Gen_Treemap_COMPARE("l",cogen_include="Y"),12,8)
GGSave_Loc_custom(CaseName,"Study Gen perc no Cogen",Total_Gen_Treemap_COMPARE("l",cogen_include="n"),12,8)

# Costs
GGSave_Loc_custom(CaseName,"Total Cost Breakdown",Cost_Cum_COMPARE("l"),12,8)
GGSave_Loc_custom(CaseName,"Cummulative Annual Cost",AnnualCost_Cum_COMPARE("l",emissions_include="Y"),12,8)
GGSave_Loc_custom(CaseName,"Cummulative Annual Cost rel",AnnualCost_Cum_rel_COMPARE("l",emissions_include="Y"),12,8)
GGSave_Loc_custom(CaseName,"Total Cost Breakdown rel",Cost_Cum_rel_COMPARE("l"),12,8)
GGSave_Loc_custom(CaseName,"Total Cost Breakdown norm",AnnualCost_Cum_COMPARE_norm("l"),12,8)
GGSave_Loc_custom(CaseName,"Total Value Breakdown norm",AnnualValue_Cum_norm("l"),12,8)

}

# PLOTS FOR YEAR BY YEAR, BREAK UP BY GROUP
{
  
  # Annual capacity by group
  all_groups <- c("Coal", "Cogeneration","Coal-to-Gas","Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                  "Natural Gas Combined Cycle + CCS", "Natural Gas Simple Cycle", "Natural Gas Combined Cycle", 
                  "Hydro", "Other", "Wind", "Solar", "Storage")
  all_groups_noStor <- c("Coal", "Cogeneration","Coal-to-Gas","Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                  "Natural Gas Combined Cycle + CCS", "Natural Gas Simple Cycle", "Natural Gas Combined Cycle", 
                  "Hydro", "Other", "Wind", "Solar")
  renew <- c("Hydro", "Other", "Wind", "Solar")
  all_fossil <- c("Coal", "Coal-to-Gas","Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                  "Natural Gas Combined Cycle + CCS", "Natural Gas Simple Cycle", "Natural Gas Combined Cycle")
  unabated_fossil <- c("Coal", "Coal-to-Gas","Natural Gas Simple Cycle", "Natural Gas Combined Cycle")
  h2_ccs <- c("Hydrogen Simple Cycle","Hydrogen Combined Cycle","Natural Gas Combined Cycle + CCS")
  em_groups <-  c("Coal", "Coal-to-Gas","Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                 "Natural Gas Combined Cycle + CCS", "Natural Gas Simple Cycle", "Natural Gas Combined Cycle","Other")
  em_groups_noOT <-  c("Coal", "Coal-to-Gas",
                  "Natural Gas Combined Cycle + CCS", "Natural Gas Simple Cycle", "Natural Gas Combined Cycle")
  
  GGSave_Loc_custom(CaseName,"Annual Capacity All",Annual_Cap_group(name_type="l",list_groups=all_groups),14,6)
  GGSave_Loc_custom(CaseName,"Annual Capacity Renewable",Annual_Cap_group(name_type="l",list_groups=renew),14,6)
  GGSave_Loc_custom(CaseName,"Annual Capacity All Fossil",Annual_Cap_group(name_type="l",list_groups=all_fossil),14,6)
  GGSave_Loc_custom(CaseName,"Annual Capacity Unabated Fossil",Annual_Cap_group(name_type="l",list_groups=unabated_fossil),14,6)
  GGSave_Loc_custom(CaseName,"Annual Capacity H2 and CCS",Annual_Cap_group(name_type="l",list_groups=h2_ccs),14,6)
  GGSave_Loc_custom(CaseName,"Annual Capacity CCNG",Annual_Cap_group(name_type="l",list_groups=c("Natural Gas Combined Cycle + CCS", "Natural Gas Combined Cycle")),14,6)
  GGSave_Loc_custom(CaseName,"Annual Capacity Wind",Annual_Cap_group(name_type="l",list_groups=c("Wind")),14,6)
  GGSave_Loc_custom(CaseName,"Annual Capacity Solar",Annual_Cap_group(name_type="l",list_groups=c("Solar")),14,6)
  GGSave_Loc_custom(CaseName,"Annual Capacity Storage",Annual_Cap_group(name_type="l",list_groups=c("Storage")),14,6)
  
  GGSave_Loc_custom(CaseName,"Annual Generation All",Annual_Gen_group(name_type="l",list_groups=all_groups),14,6)
  GGSave_Loc_custom(CaseName,"Annual Generation Renewable",Annual_Gen_group(name_type="l",list_groups=renew),14,6)
  GGSave_Loc_custom(CaseName,"Annual Generation All Fossil",Annual_Gen_group(name_type="l",list_groups=all_fossil),14,6)
  GGSave_Loc_custom(CaseName,"Annual Generation Unabated Fossil",Annual_Gen_group(name_type="l",list_groups=unabated_fossil),14,6)
  GGSave_Loc_custom(CaseName,"Annual Generation H2 and CCS",Annual_Gen_group(name_type="l",list_groups=h2_ccs),14,6)
  GGSave_Loc_custom(CaseName,"Annual Generation Wind",Annual_Gen_group(name_type="l",list_groups=c("Wind")),14,6)
  GGSave_Loc_custom(CaseName,"Annual Generation Solar",Annual_Gen_group(name_type="l",list_groups=c("Solar")),14,6)
  
  GGSave_Loc_custom(CaseName,"Annual Emissions non-cogen",Annual_Em_group(name_type="l",list_groups=em_groups),14,6)
  GGSave_Loc_custom(CaseName,"Annual Emissions non-cogen no other",Annual_Em_group(name_type="l",list_groups=em_groups_noOT),14,6)
  GGSave_Loc_custom(CaseName,"Annual Emissions Combined-Cycle",Annual_Em_group(name_type="l",list_groups=c("Natural Gas Combined Cycle + CCS", "Natural Gas Combined Cycle")),14,6)
  GGSave_Loc_custom(CaseName,"Annual Emissions Simple-Cycle and Coal-to-gas",Annual_Em_group(name_type="l",list_groups=c("Natural Gas Simple Cycle","Coal-to-Gas")),14,6)
  
  
  # STACKED AREAS
  GGSave_Loc_custom(CaseName,"Annual Generation All Area",Annual_Gen_group_area(name_type="l",list_groups=all_groups,nrg_include=FALSE),14,6)
  GGSave_Loc_custom(CaseName,"Annual Generation All Perc",Annual_Gen_group_perc(name_type="l",list_groups=all_groups_noStor,nrg_include=FALSE),14,6)
  GGSave_Loc_custom(CaseName,"Annual Capacity All Area",Annual_Cap_group_area(name_type="l",list_groups=all_groups,nrg_include=FALSE),14,6)
  
}

# COMPARE METRICS
  base_case=c("CP","Current Policy")
  em_diff_groups <- c("Coal", "Cogeneration","Coal-to-Gas","Natural Gas Combined Cycle + CCS", 
                      "Natural Gas Simple Cycle", "Natural Gas Combined Cycle", 
                     "Other","Cogeneration")
  # Ind metrics
  GGSave_Loc_custom(CaseName,"2045 Capacity Diff from CP",Cap_Year_Diff_COMPARE("l",2045,base_case),12,8)
  GGSave_Loc_custom(CaseName,"Total Gen Diff from CP",Gen_Diff_COMPARE("l",base_case),12,8)
  GGSave_Loc_custom(CaseName,"Total Emissions Diff from CP",Em_Diff_COMPARE("l",base_case,em_diff_groups),12,8)
  GGSave_Loc_custom(CaseName,"Total Cost Diff from CP",Cost_Diff_COMPARE("l",base_case,0.05),12,8)
  
  
  # plot of metrics (low quality)
  SourceDB=CScenarioName
  windows(20,12,buffered=FALSE)
  compare_metrics("l",2045,base_case,em_diff_groups,0.1,12)
  SaveRun_Loc(CaseName,"Compare Metrics")
################################################################################
## CREATE COMPARE PLOTS WITH NRG STREAM DATA
################################################################################
  date_filt<-"2014-01-1"
  nrg_file_name <- "nrgstream_gen_corrected03Mar2023.RData"
  demand_file_name <-"nrgstream_demand03Mar2023.RData"
  
  # Load the data
  df1a <- Load_NRG_hourly(date_filt,2022,nrg_file_name,reformat_names=TRUE)
  Actdemand <- Load_NRG_demand(date_filt,demand_file_name)

  GGSave_Loc_custom(CaseName,"Annual Generation All Area with old",Annual_Gen_group_area(name_type="l",list_groups=all_groups,nrg_include=TRUE),14,6)
  GGSave_Loc_custom(CaseName,"Annual Generation All Perc with old",Annual_Gen_group_perc(name_type="l",list_groups=all_groups_noStor,nrg_include=TRUE),14,6)
  GGSave_Loc_custom(CaseName,"Annual Capacity All Area with old",Annual_Cap_group_area(name_type="l",list_groups=all_groups,nrg_include=TRUE),14,6)
  
################################################################################
## GENERATE PLOTS TO LOOK - NOT SAVE :)
##  Copy/paste below
################################################################################
GenText_Sz <-20

# Average price, use "Y" to include AESO historic prices
AvgYr_price_COMPARE(name_type="l", AESO_include="Y")

# Clear all plots from window
dev.off(dev.list()["RStudioGD"])
