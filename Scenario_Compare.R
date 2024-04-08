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
  #   'CP_11Feb'                 CP with solar outate update
  #   'CER_12Feb'                CER with solar outage update
  #   'EL_25Feb"
  #   'TIER2050_23Feb'
  #   'TIER2035_29Feb'
  #   'CP_noITC_04Mar'
  #   'CER_noITCs_Mar'
  #
  #   'TIER2035_29_Feb_2024'
  # COMBINED SCENARIOS
  #   'CP_CER'                   CP_11Feb, CER_12Feb
  #   'Main_3'                   CP_11Feb, CER_12Feb, EL_25Feb   
  #   'TIER_3'                   CP_11Feb, TIER2050_23Feb, TIER2035L_29Feb 
  #   'All_5'                    CP_11Feb, CER_12Feb, EL_25Feb, TIER2050_23Feb, TIER2035L_29Feb 
{
  # Define cases here
  ScenarioName1<-"CP_04Apr"
  ScenarioName2<-"EL_06Apr"
  
  # This is the name for the new combined R files and excel sheet. Adds compare to name automatically!
  CScenarioName <-"Test"
}

################################################################################
## LOAD SCENARIOS, COMBINE, AND SAVE
################################################################################
  
  # Function to combine two scenarios. Annual results data
  CombineFilesR(ScenarioName1,ScenarioName2,CScenarioName)

################################################################################
## READ R FILES WITH SCENARIO DATA (AFTER COMBINE)
################################################################################

# Read R-Data files for compare scenario
{
  # Annual Resource Group Capacity
  Ann_Cap<-readRDS(here("Data Files","Result Files",CScenarioName,paste("Ann_Cap_",CScenarioName, sep = "")))
  # Annual Fuel Usage
  Ann_Fuel<-readRDS(here("Data Files","Result Files",CScenarioName,paste("Ann_Fuel_",CScenarioName, sep = "")))
  # Annual Heat Rates
  Ann_HR<-readRDS(here("Data Files","Result Files",CScenarioName,paste("Ann_HR_",CScenarioName, sep = "")))
  # Annual EPC Values
  EPC_Values<-readRDS(here("Data Files","Result Files",CScenarioName,paste("EPC_Values_",CScenarioName, sep = "")))
  # Annual Resource Group Data
  ResGrYr<-readRDS(here("Data Files","Result Files",CScenarioName,paste("ResGrYr_",CScenarioName, sep = "")))
  # Total Capacity Changes (Study)
  Tot_Cap<-readRDS(here("Data Files","Result Files",CScenarioName,paste("Tot_Cap_",CScenarioName, sep = "")))
  # Annual Zone Data
  Zone<-readRDS(here("Data Files","Result Files",CScenarioName,paste("Zone_",CScenarioName, sep = "")))
}  

################################################################################
## PLOT SETTINGS
################################################################################
# Folder name
CaseName <- "Main_3"
#CaseName <- "TIER_3"

{ # Available Fonts for plotting, can choose different one and change Plot_Text if needed
  # Uses local computer font files (search font in search bar to confirm font names)
  
  font_add(family="Times",regular="times.ttf")
  Plot_Text <- "Times"
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
    cOL_SOLAR <- "#FDE725FF"
    cOL_WIND <- "#73D055FF"
    
    # Storage Groups
    cOL_STORAGE <- "rosybrown1" 
    COL_Battery <-"rosybrown1" 
    COL_CompAir <-"coral3"
    COL_Pumped <-"firebrick4"
    
    # Gray-scale colors
    cOL_IMPORTg<- "gray95"
    
    # Gas Groups (Purples)
    cOL_COal2Gasg<-"gray60"
    cOL_NGConvg <- cOL_COal2Gasg
    cOL_SCGTg <- "gray45"
    cOL_NGCCg <- "gray80"
    COL_NatGasg <-"gray45"
    cOL_NGCC_CCSg<-"gray20"
    
    cOL_OTHERg<-"gray70"
    cOL_HYDROg<-"steelblue3"
    cOL_STORAGEg<-"steelblue1"
    cOL_SOLARg<-"lightsteelblue"
    cOL_WINDg<-"steelblue4"
    
    cOL_COALg<-"black"
    cOL_COGENg<-"gray35"
    
    cOL_NUCLEARg<-"midnightblue"
    COL_H2g<-"gray89"
    
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
    
    colours4g=c("Import"= cOL_IMPORTg, "Coal-to-Gas"=cOL_COal2Gasg, "Coal"=cOL_COALg,"Cogen"=cOL_COGENg,"Nuclear"=cOL_NUCLEARg, 
                "Natural Gas"=COL_NatGasg,"Natural Gas + CCS"=cOL_NGCC_CCSg,"Hydrogen"=COL_H2g,
                #"Natual Gas and Hydrogen Blend"=COL_Blend,
                "Hydro"=cOL_HYDROg, "Other"=cOL_OTHERg, "Nuclear"=cOL_NUCLEARg,
                "Wind"=cOL_WINDg, "Solar"=cOL_SOLARg, "Storage"=cOL_STORAGEg)
    
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
    
    
  COL_CP ="#4472C4"
  COL_CER =  '#A6A6A6'
  COL_EL ='gray85'
  COL_TIER2050 = "#238b45"
  COL_TIER2035 = "yellowgreen"
  COL_noITC = '#515151'
  COL_noITC_CER = '#cc79a7'
  COL_Az = "goldenrod1"
  COL_AZstrict = "goldenrod4"
  
  # Scenario colors with historic
   sn_colors_l <-c("Draft CER"=COL_CER,
                   "Current Policy"=COL_CP,
                   "Emissions Limit"=COL_EL,
                   "TIER 2050" = COL_TIER2050,
                   "TIER 2035" = COL_TIER2035,
                   "No ITCs"= COL_noITC,
                   "No ITCs with CER"=COL_noITC_CER,
                   "Absolute Zero" = COL_Az,
                   "No H2 Absolute Zero" = COL_AZstrict,
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
                 'Historic'=1)
   
   sn_colors_s <-c("CER"=COL_CER,
                   "CP"=COL_CP,
                   "EL"=COL_EL,
                   "TIER2050"=COL_TIER2050,
                   "TIER2035"=COL_TIER2035,
                   "noITCs"=COL_noITC,
                   "CERnoITCs"=COL_noITC_CER,
                   "AZ" = COL_Az,
                   "AZstrict"= COL_AZstrict,
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
                   "No H2 Absolute Zero" = COL_AZstrict
                   )
   sn_line2_l <-c("Draft CER"=1,
                 "Current Policy"=1,
                 "Emissions Limit"=1,
                 "TIER 2050" = 1,
                 "TIER 2035" = 1,
                 "No ITCs"=1,
                 "No ITCs with CER"=1,
                 "Absolute Zero" = 1,
                 "No H2 Absolute Zero" = 1
                 )
   sn_colors2_s <-c("CER"=COL_CER,
                   "CP"=COL_CP,
                   "EL"=COL_EL,
                   "TIER2050"= COL_TIER2050,
                   "TIER2035" = COL_TIER2035,
                   "noITCs"=COL_noITC,
                   "CERnoITCs"=COL_noITC_CER,
                   "AZ" = COL_Az,
                   "AZstrict"=COL_AZstrict
                  )
   sn_line2_s <-c("CER"=1,
                 "CP"=1,
                 "EL"=1,
                 "TIER2050"=1,
                 "TIER2035"=1,
                 "noITCs"=1,
                 "CERnoITCs"=1,
                 "AZ"=1,
                 "AZstrict"=1
                 )
   
  }
  
################################################################################
## CREATE COMPARE PLOTS
################################################################################
{
# Compare pool prices
GGSave_Loc_custom(CaseName,"Annual Pool Price Compare",AvgYr_price_COMPARE("l","Y"),12,8)

# Compare Emissions
GGSave_Loc_custom(CaseName,"Annual Emissions Compare",AnnualEm_COMPARE("l", "Y"),12,8)
GGSave_Loc_custom(CaseName,"Annual Emissions Compare noncogen",AnnualEm_COMPARE("l", "n"),12,8)
GGSave_Loc_custom(CaseName,"Cummulative Emissions Compare noncogen",AnnualEm_Cum_COMPARE("l", "n"),12,8)

# Capacity changes
GGSave_Loc_custom(CaseName,"Total Capacity Added",Total_Cap_Add_COMPARE("l"),12,8)
GGSave_Loc_custom(CaseName,"Total Capacity Retired",Total_Cap_Ret_COMPARE("l"),12,8)
GGSave_Loc_custom(CaseName,"Capacity Relative to CP",Cap_Relative_COMPARE("l"),12,8)
GGSave_Loc_custom(CaseName,"2045 Capacity Relative to CP",Cap_Year_Relative_COMPARE("l",2045),12,8)
GGSave_Loc_custom(CaseName,"Annual Capacity Aggregated",Annual_Cap_COMPARE("s"),14,6)

# Total Gen annual
GGSave_Loc_custom(CaseName,"Annual Generation Aggregated",Annual_Gen_COMPARE("s"),14,6)
GGSave_Loc_custom(CaseName,"Year Gen Study 2023",Year_Gen_COMPARE("l",2023,show_neg="N"),12,8)
GGSave_Loc_custom(CaseName,"Year Gen Study 2045",Year_Gen_COMPARE("l",2045,show_neg="N"),12,8)
GGSave_Loc_custom(CaseName,"Year Gen Study 2045 with neg",Year_Gen_COMPARE("l",2045,show_neg="Y"),12,8)

# Total Gen Cummulative
GGSave_Loc_custom(CaseName,"Total Gen Study",Total_Gen_COMPARE("l"),12,8)
GGSave_Loc_custom(CaseName,"Study Gen perc with Cogen",Total_Gen_Treemap_COMPARE("l",cogen_include="Y"),12,8)
GGSave_Loc_custom(CaseName,"Study Gen perc no Cogen",Total_Gen_Treemap_COMPARE("l",cogen_include="n"),12,8)

# Costs
GGSave_Loc_custom(CaseName,"Total Cost Breakdown",Cost_Cum_COMPARE("l"),12,8)
GGSave_Loc_custom(CaseName,"Cummulative Annual Cost",AnnualCost_Cum_COMPARE("l",emissions_include="Y"),12,8)
GGSave_Loc_custom(CaseName,"Cummulative Annual Cost rel",AnnualCost_Cum_rel_COMPARE("l",emissions_include="Y"),12,8)
GGSave_Loc_custom(CaseName,"Total Cost Breakdown rel",Cost_Cum_rel_COMPARE("l"),12,8)
GGSave_Loc_custom(CaseName,"Total Cost Breakdown norm",AnnualCost_Cum_COMPARE_norm("l"),12,8)

}


################################################################################
## GENERATE PLOTS TO LOOK - NOT SAVE :)
################################################################################
GenText_Sz <-40

# Average price, use "Y" to include AESO historic prices
AvgYr_price_COMPARE(name_type="l", AESO_include="Y")

# Annual Emissions
AnnualEm_COMPARE(name_type="l", cogen_include="Y")

# Total Additions
Total_Cap_Add_COMPARE(name_type="l")

# Annual gen
Year_Gen_COMPARE("l",year_look=2045,show_neg="N")

# Total Gen
Total_Gen_Treemap_COMPARE(name_type="l",cogen_include="Y")
################################################################################
## OTHER USEFUL STUFF
################################################################################
# Clear all
dev.off(dev.list()["RStudioGD"])

Total_Gen_COMPARE("l")
