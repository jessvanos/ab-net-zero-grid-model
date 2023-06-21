################################################################################
# TITLE: Database_Loading
# DESCRIPTION:  Script loads database from Microsoft SQL Server, it then imports other data from files, 
# and calls functions to display plots and table.
#
#
# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: May 2022; LAST EDIT: February 28, 2023
#
# NOTES: Make sure the project file is open first or "here" commands wont work right.
#        Before running, create folder called "Data Files" inside project directory and populate it with 
#        any data you want to access. 
#        Once this file is run through completion, can call any functions with environment that is loaded 
#        (ie: you do not need to run it all again).
################################################################################

################################################################################
## LOAD REQUIRED PACKAGES AND SOURCE FUNCTIONS
################################################################################
 {# Package Info
    # tidyverse: Data science package
    # ggplot: Used for graphical packages and aesthetic
    # grid: Used for plotting, adds grid to the plot
    # scales: Use to re-format plots 
    # gtable: Grob tables, more tools
    # gridExtra: User functions for grid graphics
    # odbc: Driver for Database Loading
    # ggpubr: Used to reformat plots from ggplot
    # DBI: Package for interface between database and R
    # lubridate: Allow time and data manipulation
    # cowplot: Quality features for ggplots
    # scales: Graphical mapping stuff
    # dplyr: Data manipulation package
    # reshape2: Restructure data
    # zoo: Used for time series indexing
    # ggpattern: Geoms for ggplot2
    # here: Package to set filepaths inside R project
    # beepr: Allows sound to paly when code is done
    # showtext: Allows fonts changes in ggplot
    # DescTools: Stats tools 
    # pivottabler: Allows pivot tables to easily be created in R
    # openxlsx: Used to interact with xlsx files from R environment
    # timeDate: Used for time zone information and holidays.
    # writexl: Easy way to write dataframes to excel files with multiple pages
    # viridis: A color pallete for plots
    # ggnewscale: Allow multiple color scales in one plot
  }

{ # Must load the here package in order to make sure internal project directories work
  library(here)
  
  # Import functions from other R files, take from the functions folder in R project
  source(here('Functions','Output_Gen_Functions.R'))  # Output and generation plots as well as other misc plots
  source(here('Functions','Emission_Functions.R'))    # Emission plots
  source(here('Functions','Price_Functions.R'))       # Plots related to prices
  source(here('Functions','Build_Retire_Functions.R'))# Plots on new and retired resources
  source(here('Functions','Intertie_Functions.R'))    # Plots on trade information and BC/MT/SK information
  source(here('Functions','Table_Functions.R'))       # Summary pivot tables
  source(here('Functions','Res_Filter_Functions.R'))  # Filtering by resource type, required for plots
  source(here('Functions','Other_Functions.R'))       # Other functions used in plotting functions
  #source(here('Functions','Developing_Functions.R'))  # Under construction functions
  source(here('Functions','Data_Filt_To_Table.R'))    # Functions that filter data and export it to excel sehets
  source(here('Functions','aeso_eval_1.R'))           #
  source(here('Functions','aseo_sim_comp_1.R'))       #
  
  # Packages required
  packs_to_load = c("tidyverse","ggplot2","scales","grid","gtable","gridExtra","odbc","ggpubr",
                   "DBI","lubridate","cowplot","scales","dplyr","reshape2","zoo",
                   "ggpattern","here","beepr","showtext","DescTools","pivottabler",
                   "openxlsx","sqldf","timeDate","writexl","viridis","ggnewscale")
  # Function to check for packages, install if not present, and load
  packs_check(packs_to_load)
 
}

################################################################################
## CONNECT TO MICROSOFT SQL SERVER
################################################################################


{ #Input Database Name below:
  SourceDB<-"LZ_June_20_2023"
  
  #Connect to database specified (via server, user, and password)
  con <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = rstudioapi::askForPassword("Server(IP_Address,Port)"),
                   Database = SourceDB,
                   UID = rstudioapi::askForPassword("User Name"),
                   PWD = rstudioapi::askForPassword("Password"))  
}

################################################################################
## DEFINE CASES TO STUDY (IE: RUN ID)
## Value can be found in the "Run_Id" column of any AURORA table
################################################################################

{ BC <- "Base Case" 
  C2 <- "Case2"
}

################################################################################
## READ TABLES FROM DATABASE INTO ENVIRONMENT
## Can edit to select required tables only, DOUBLE CHECK all tables are in database
## It will just skip tables that are not there and all the ones after
################################################################################

{ # Fuel tables
   FuelYr <- dbReadTable(con,'FuelYear1')
#  FuelMn <- dbReadTable(con,'FuelMonth1')
  
  # Resource Tables
  ResYr <- dbReadTable(con,'ResourceYear1')
#  ResMn <- dbReadTable(con,'ResourceMonth1')
  ResHr <- dbReadTable(con,'ResourceHour1') # This one takes eons
  ResSt <-dbReadTable(con,'ResourceStudy1')
  ResEmYr <-dbReadTable(con,'ResourceEmissionsYear1')
  
  # Resource Group Tables
  ResGroupYr <- dbReadTable(con,'ResourceGroupYear1')
  ResGroupMn <- dbReadTable(con,'ResourceGroupMonth1')
  ResGroupHr <- dbReadTable(con,'ResourceGroupHour1')
  ResGroupEmYr <- dbReadTable(con,'ResourceGroupEmissionsYear1')
  ResGroupEmHr <- dbReadTable(con,'ResourceGroupEmissionsHour1')
#  ResGroupEmSt <- dbReadTable(con,'ResourceGroupEmissionsStudy1')
  
  # Other Tables
  ResStackYr <- dbReadTable(con,'ResourceStackYear1')
#  ResStackHr  <- dbReadTable(con,'ResourceStackHour1')
  LinkYr <- dbReadTable(con,'LinkYear1')
# LinkMn <- dbReadTable(con,'LinkMonth1')
#  LinkHr <- dbReadTable(con,'LinkHour1')
  
#  CC <- dbReadTable(con,'CustomConstraint1')
  ZoneYr <- dbReadTable(con,'ZoneYear1')
  ZoneMn <- dbReadTable(con,'ZoneMonth1')
  ZoneHr <- dbReadTable(con,'ZoneHour1')
  #LTRes <- dbReadTable(con,'LTResValue1')
  Build <- dbReadTable(con,'LTBuildReport1')
  Study <- dbReadTable(con,'LTStudyLog1')
  
  # Get rid of Unused R memory to keep speed up
  
  gc()   
} 

################################################################################
## REFLECT PROPER TIME PERIODS
## Adds a column which is formated in a way that R can understand for dates and times
################################################################################

{  # Fuel Tables
  FuelYr$Time_Period <- as.Date(as.character(FuelYr$Time_Period), 
                       format = "%Y")
#  FuelMn$Time_Period <- ym(FuelMn$Time_Period)
  
  # Resource Tables
  ResYr$YEAR  <- as.POSIXct(as.character(ResYr$Time_Period), 
                        format = "%Y")
  ResYr$YEAR <- format(ResYr$YEAR,format="%Y") # Reformat for year only
  
#  ResMn$Time_Period <- ym(ResMn$Time_Period)
  
  ResHr$date <- as.POSIXct(as.character(ymd_h(gsub(" Hr ", "_",ResHr$Time_Period))), 
                        tz = "MST")-(60*60)

  ResEmYr$YEAR  <- as.POSIXct(as.character(ResEmYr$Time_Period), 
                            format = "%Y")
  
  # Resource Group Tables
  ResGroupHr$date <- as.POSIXct(as.character(ymd_h(gsub(" Hr ", "_",ResGroupHr$Time_Period))), 
                        tz = "MST")-(60*60)
  
  ResGroupMn$Time_Period <- ym(ResGroupMn$Time_Period)
  
  ResGroupYr$Time_Period  <- as.Date(as.character(ResGroupYr$Time_Period), 
                        format = "%Y")
  ResGroupEmYr$Time_Period  <- as.Date(as.character(ResGroupEmYr$Time_Period), 
                        format = "%Y")
  ResGroupEmHr$date <- as.POSIXct(as.character(ymd_h(gsub(" Hr ", "_",ResGroupEmHr$Time_Period))), 
                                tz = "MST")-(60*60)
  
  # Other Tables
  ResStackYr$Time_Period  <- as.Date(as.character(ResStackYr$Time_Period), 
                        format = "%Y")
 # ResStackHr$date <- as.POSIXct(as.character(ymd_h(gsub(" Hr ", "_",ResStackHr$Time_Period))), 
 #                       tz = "MST")-(60*60)
  
   LinkYr$Time_Period  <- as.Date(as.character(LinkYr$Time_Period), 
                        format = "%Y")
 #  LinkMn$Time_Period <- ym(LinkMn$Time_Period)
   # LinkHr$Time_Period <- as.POSIXct(as.character(ymd_h(gsub(" Hr ", "_",LinkHr$Time_Period))), 
   #                           tz = "MST")-(60*60) 
   
   
   ZoneYr$Time_Period  <- as.Date(as.character(ZoneYr$Time_Period), 
                       format = "%Y")
  ZoneMn$Time_Period <- ym(ZoneMn$Time_Period)
  ZoneHr$date <- as.POSIXct(as.character(ymd_h(gsub(" Hr ", "_",ZoneHr$Time_Period))), 
                         tz = "MST")-(60*60)   
  gc()
}

################################################################################
## FILTER TABLES TO GET RID OF COLUMNS I DONT CARE ABOUT & PULL OUT IMPORT/EXPORT
################################################################################

{
  # HOURLY DATA
  # Resourse Group Hourly Tables, choose specific columns
  ResGroupHr_sub <- ResGroupHr %>%
    subset(., select = c(ID, date, Report_Year, Output_MWH, Run_ID, Capacity_Factor))
  
  # Zone Hourly Tables, for average hourly results
  ZoneHr_Avg <- ZoneHr %>%
    filter(Name == "WECC_Alberta") %>%
    filter(Condition == "Average") %>%
    subset(., select = c(date, Price, Baseline_Demand, Demand, Demand_Total,
                         Net_Load, Net_Load_Total, Marginal_Resource, 
                         Smp_Max_Date_Time, Smp_Max_Demand, Smp_Max_Capacity,Demand_Side_Output, 
                         Run_ID, Imports, Exports))
  
  # Zone Hourly for Everything
  ZoneHr_All <- ZoneHr %>%
    filter(Name == "WECC_Alberta") %>%
    subset(., select = c(date, Condition, Price, Demand, Marginal_Resource, 
                         Name,Report_Year, Report_Month,Demand_Side_Output,
                         Run_ID))
  
  # #Hourly individual resources in BC or SK
  # ResHr_Int <- ResHr
  #   filter(Zone == -c("WECC_Alberta")) %>%
  #     subset(., select = c(ID, Name, Beg_Date, End_Date, date, Capability, Capacity, 
  #                          Dispatch_Cost, Incr_Cost, Fixed_Cost, Fuel_Cost, 
  #                          Output_MWH, Percent_Marginal, Percent_Committed,
  #                          Revenue, Variable_OM_Cost, Capacity_Factor, 
  #                          Total_Emission_Cost, Total_Hours_Run, Condition, 
  #                          Report_Year, Run_ID, Peak_Capacity, 
  #                          Primary_Fuel,Zone))  
  
  #Hourly For individual resources in AB
  ResHr <- ResHr %>%
    filter(Zone == "WECC_Alberta") %>%
    subset(., select = c(ID, Name, Beg_Date, End_Date, date, Capability, Capacity, 
                         Dispatch_Cost, Incr_Cost, Fixed_Cost, Fuel_Cost,Fuel_Usage, 
                         Output_MWH, Percent_Marginal, Percent_Committed,
                         Revenue,Energy_Revenue_MWh,Value,Value_MWh, 
                         Net_Cost,Total_Cost_MWh,Fixed_Cost,Variable_OM_Cost,Startup_Cost,Build_Cost,Capacity_Factor, 
                         Total_Emission_Cost, Total_Hours_Run, Condition, 
                         Report_Year, Run_ID, Peak_Capacity, 
                         Used_For_Op_Reserve, Forced_Outage,Maint_Outage,
                         Primary_Fuel,Zone))   
  
  
  #IMPORT/EXPORT 
  # Select the Import/Export data Hourly
  Import <- ZoneHr_Avg %>%
    subset(., select = c(date, Imports, Run_ID)) %>%
    'colnames<-'(c("date", "Output_MWH", "Run_ID")) %>%
    add_column(ID = "Import")
  
  Export <- ZoneHr_Avg %>%
    subset(., select = c(date, Exports, Run_ID)) %>%
    'colnames<-'(c("date", "Output_MWH", "Run_ID")) %>%
    add_column(ID = "Export")
  
  # Select Import/Export data Yearly
  Import_Yr <- ZoneYr %>%
    filter(Condition == "Average") %>%
    subset(., select = c(Time_Period, Imports_Total, Run_ID,Name)) %>%
    'colnames<-'(c("Time_Period", "Output_MWH", "Run_ID","Name")) %>%
    add_column(ID = "Import")
  
  Export_Yr <- ZoneYr %>%
    filter(Condition == "Average") %>%
    subset(., select = c(Time_Period, Exports_Total, Run_ID,Name)) %>%
    'colnames<-'(c("Time_Period", "Output_MWH", "Run_ID","Name")) %>%
    add_column(ID = "Export")
  
  #Fix Aurora sign convention if needed
  if (min(Export$Output_MWH) < 0) {
    Export$Output_MWH <- Export$Output_MWH * -1   }
  
  gc()
}

################################################################################
## LOAD AESO TRADE INFO FROM R FILE INTO WORKSPACE (OPTIONAL)
################################################################################

# HRcalc <- readRDS(here("Data Files","HRcalc.RData")) 
# 
# { HRcalc$date <- as.POSIXct(HRcalc$Date_Begin_Local,tz="",format="%Y-%m-%d %H:%M")
#   
#   HRcalc<- HRcalc %>%
#     select(.,-c("DAY_AHEAD_POOL_PRICE")) 
# 
# #Replace all NA values with zero
# HRcalc[HRcalc==0] <- NA
# 
# #Reformat Day as day of year
# HRcalc$Day <- format(HRcalc$date,"%j")
# HRcalc$Week <- format(HRcalc$date,"%W") 
# HRcalc$Month2 <- format(HRcalc$date,"%b")
# }

################################################################################
## SAVE OTHER DATA AS R FILE AND MODIFY (OPTIONAL)
## Only need to run if new files are available
################################################################################
# 
# # Load merit data
# {
#   merit <- read_csv(here("Data Files","Alberta Data","student_data_2023_Mar_03_21_55.csv.gz"))
#     # Save as R file
#     saveRDS(merit, here("Data Files","Alberta Data","Leach_MeritData03Mar2023.RData"))
#     # Remove from workspace
#      rm(merit)
#   }
# 
# # Load NRG data and rename time column
# {
#      load(here("Data Files","Alberta Data","nrgstream_gen03Mar2023.RData"))
#      nrgstream_gen <- nrgstream_gen %>%
#        rename(time=Time)
# 
#      # Remove NA values
#      nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$gen),]
#      nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$time),]
# 
#      # Apply data corrections
#      corrected <- nrgstream_gen %>%
#          filter(is.na(Latitude)) %>%
#          mutate(Latitude=case_when(grepl("BRD1",ID) ~ 49.842735,
#                                    grepl("BUR1",ID) ~ 49.814877,
#                                    grepl("CLR",ID) ~ 50.032911,
#                                    grepl("CLY",ID) ~ 49.840967,
#                                    grepl("CHP1",ID) ~ 50.22189,
#                                    grepl("COL1",ID) ~ 49.833218,
#                                    grepl("CRD",ID) ~ 49.807,
#                                    grepl("CRR2",ID) ~ 49.55891,
#                                    grepl("FMG1",ID) ~ 49.66334,
#                                    grepl("KKP",ID) ~ 53.469986,
#                                    grepl("MON1",ID) ~ 49.833144,
#                                    grepl("NMK1",ID) ~ 51.026118,
#                                    grepl("RIV1",ID) ~ 49.53245,
#                                    grepl("STR",ID) ~ 51.033273,
#                                    grepl("TVS1",ID) ~ 50.27324,
#                                    grepl("VCN1",ID) ~ 50.0975,
#                                    grepl("VXH1",ID) ~ 50.095223,
#                                    grepl("WEF1",ID) ~ 49.65405,
#                                    grepl("WHT",ID) ~ 49.64029),
#                 Longitude=case_when(grepl("BRD1",ID) ~ -111.537891,
#                                     grepl("BUR1",ID) ~ -111.543323,
#                                     grepl("CHP1",ID) ~ -110.437106,
#                                     grepl("CLR",ID) ~ -113.484369,
#                                     grepl("CLY",ID) ~ -110.356864,
#                                     grepl("COL1",ID) ~ -112.97448,
#                                     grepl("CRD",ID) ~ -112.578,
#                                     grepl("CRR2",ID) ~ -113.983,
#                                     grepl("FMG1",ID) ~ -111.122,
#                                     grepl("KKP",ID) ~ -113.61337,
#                                     grepl("MON1",ID) ~ -112.974231,
#                                     grepl("NMK1",ID) ~ -113.163017,
#                                     grepl("RIV1",ID) ~ -113.977,
#                                     grepl("STR",ID) ~ -113.371296,
#                                     grepl("TVS1",ID) ~ -112.73059,
#                                     grepl("VCN1",ID) ~ -112.84841,
#                                     grepl("VXH1",ID) ~ -112.149936,
#                                     grepl("WEF1",ID) ~ -111.515812,
#                                     grepl("WHT",ID) ~ -111.291),
#                 Installation_Year=case_when(grepl("CRR2",ID)~2019,
#                                             grepl("CYP",ID)~2022,
#                                             #grepl("CYP2",ID)~"post2019",
#                                             grepl("FMG1",ID)~2022,
#                                             grepl("GDP1",ID)~2022,
#                                             grepl("GRZ1",ID)~2022,
#                                             grepl("HHW1",ID)~2022,
#                                             grepl("HLD1",ID)~2022,
#                                             grepl("JNR",ID)~2022,
#                                             grepl("RIV1",ID)~2019,
#                                             grepl("RTL1",ID)~2021,
#                                             grepl("WHE1",ID)~2022,
#                                             grepl("WHT1",ID)~2019,
#                                             grepl("WHT2",ID)~2021,
#                                             grepl("WRW1",ID)~2021),
#                 Installation_Year=case_when(is.na(Installation_Year)~"pre2019",
#                                               TRUE~"post2019"))
# 
#      # Get non-corrected and remove Latitude
#      nocorrection <- nrgstream_gen %>%
#          filter(!is.na(Latitude))%>%
#        mutate(Installation_Year="")
# 
#      # put back together and remove old files
#      nrgstream_gen <- rbind(corrected,nocorrection)
#         rm(corrected,nocorrection)
# 
#      # Save new file
#      saveRDS(nrgstream_gen,here("Data Files","Alberta Data","nrgstream_gen_corrected03Mar2023.RData"))
# 
#      # Make separate file for demand and save
#      Actdemand <- nrgstream_gen %>%
#          group_by(time) %>%
#          summarise(Demand = median(Demand),
#                    Price = median(Price),
#                    AIL = median(AIL))
# 
#      # Save the demand
#      saveRDS(Actdemand, here("Data Files","Alberta Data","nrgstream_demand03Mar2023.RData"))
#         rm(Actdemand,nrgstream_gen)
# }

################################################################################
## BRING IN DATA FROM AESO FILES & FORMAT 
################################################################################
{ 
  # Load Leach Merit Data - Hourly resource info for Alberta (similar to ResHr and StackHr)
  merit <- readRDS(here("Data Files","Alberta Data","Leach_MeritData03Mar2023.RData"))
  
    #Filter Data to relevant dates & remove old data
    merit_filt <- filter(merit,date >= as.Date("2015-01-1"))
    rm(merit)
  
  # Load nrgstream_gen - Load and demand info, plus a whole ton more
    nrgstream_gen <- readRDS(here("Data Files","Alberta Data","nrgstream_gen_corrected03Mar2023.RData")) 
    Actdemand <- readRDS(here("Data Files","Alberta Data","nrgstream_demand03Mar2023.RData"))
  
    # Take out dates I don't care about and remove the old table
    sub_samp<-filter(nrgstream_gen, time >= as.Date("2015-01-1"))

    # Create a list to describe Import/Exports
    trade_excl<-c("AB - WECC Imp Hr Avg MW", 
                  "AB - WECC Exp Hr Avg MW",
                  "AB - WECC Imp/Exp Hr Avg MW")
    
    # Create Dataframe, only select rows where the Nat resource group is in the defined groups (ie trading)
    # then grouped by plant type 
    df1 <- sub_samp %>% 
      filter(! NRG_Stream %in% trade_excl)%>% 
      group_by(Plant_Type,time) %>% 
      summarise(meancap = mean(Cap_Fac),
                total_gen=sum(gen,na.rm = T),
                total_rev=sum(Revenue,na.rm = T),
                price_mean=mean(Price),
                heatrt_mean=mean(Heat.Rate)) %>% 
      ungroup()
    
        #Reformat the dates
        df1$Day <- date(df1$time)
        df1$Year <- as.factor(year(df1$time))
    
  {  # ORGANIZE RESOURCES
     #Make a resource type list
     plant_types<-c("COAL","NGCONV","COGEN","HYDRO","NGCC", "OTHER", "SCGT","SOLAR","IMPORT","EXPORT","WIND","STORAGE")
    
     # Create a new dataframe with plant types specified only, 
     # Then filter AESO data to exclude dates without information (till end of 2022)
     df1a <- df1 %>%
     filter(Plant_Type %in% plant_types,
            year(time)<2023)
      
         # Put in desired order: Coal, Cogen, NGCC, SCGT, Other, Hydro, Wind, Solar, Import, Export
         df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "OTHER",after=Inf)
         df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "HYDRO",after=Inf)
         df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "WIND",after=Inf)
         df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "SOLAR",after=Inf)
         df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "IMPORT",after=Inf)
         df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "EXPORT",after=Inf)
         gc()   
   }
}

################################################################################
## PLOT SETTINGS
################################################################################

{ # Available Fonts for plotting, can choose different one and change Plot_Text if needed
  # Uses local computer font files (search font in search bar to confirm font names)
  
    font_add(family="Times",regular="times.ttf")
    Plot_Text <- 'Times'
    
    # font_add(family="Cambrai",regular="CAMBRIA.ttc")
    # Plot_Text <- 'Cambrai'
    
    showtext_auto()
    
  # Set size for plot features to be constant. All based on general text size
  { GenText_Sz =15
    Tit_Sz = GenText_Sz+5
    XTit_Sz = GenText_Sz+2
    YTit_Sz = GenText_Sz+2
    Leg_Sz=GenText_Sz-2
    Overall_Sz=GenText_Sz}
    
    
  { # Define fuel types for new builds
    solar <- "SUN"
    wind <- "WND"
    other <- "OT"
    storage <- "PS"
    gas1 <- "Gas1"
    gas0 <- "Gas0"
  }  

  # Set legend color schemes for constancy
    ## Can change here
    # To see a bunch of options, use this: 
        # library("colorspace")
        # hcl_palettes(plot = TRUE)
        #sequential_hcl(5,palette="oranges)
    { 
            
      # Color Fill info
          # Import/Export
          cOL_IMPORT <- "#F8B660" 
          cOL_EXPORT <- "#DDCC77"
          
          # Coal/Cogen
          cOL_NUCLEAR <- "black"
          cOL_COAL <- "gray80"
          cOL_COGEN <- "gray40"
          
          # H2 groups (blues)
          cOL_SCGT_H2 <- "#C1DBEC"
          cOL_NGCC_H2 <- "#042333b2"
          COL_H2 <- cOL_NGCC_H2  
              #cOL_SCGT_Blend <- "#7FABD3"
              #cOL_NGCC_Blend <- "#3573B9"
              #COL_Blend <- cOL_NGCC_Blend 

          
          # Gas Groups (Purples)
          cOL_COal2Gas <-  "#440154FF" #"mediumorchid4"
          cOL_NGConv <- cOL_COal2Gas
          cOL_SCGT <- "#7e4e90ff"
          cOL_NGCC <- "#A79FE1"
          COL_NatGas <-cOL_NGCC 
          cOL_NGCC_CCS <-"#403891b2"
          
          # Renewables and Other
          cOL_HYDRO <- "royalblue2"
          cOL_OTHER <- "#1F968BFF"
          cOL_WIND <- "#73D055FF"
          cOL_SOLAR <- "#FDE725FF"
          
          
          # Storage Groups
          cOL_STORAGE <- "coral3" 
          COL_Battery <-"coral3"
          COL_CompAir <-"rosybrown1"
          COL_Pumped <-"firebrick4"
          
          # Special Groups
          cOL_DSM <-"grey10"
          
          # Set plot color transparacny 
          Plot_Trans<-0.8
              
   ## Now Define Lists to assign legends and colors in plots
     colours1=c("Import"= cOL_IMPORT, "Coal"=cOL_COAL, "Cogeneration"=cOL_COGEN, 
                "Coal-to-Gas"=cOL_NGConv,"Hydrogen Simple Cycle"=cOL_SCGT_H2,"Hydrogen Combined Cycle"=cOL_NGCC_H2,
                #"Blended  Simple Cycle"=cOL_SCGT_Blend,"Blended  Combined Cycle"=cOL_NGCC_Blend,
                "Natural Gas Combined Cycle + CCS"=cOL_NGCC_CCS,
                "Natural Gas Simple Cycle"=cOL_SCGT, "Natural Gas Combined Cycle"=cOL_NGCC, 
                "Hydro"=cOL_HYDRO, "Other"=cOL_OTHER, "Wind"=cOL_WIND, 
                "Solar"=cOL_SOLAR, "Storage"=cOL_STORAGE)
      
      colours1b=c("Import"= cOL_IMPORT, "Coal"=cOL_COAL, "Cogeneration"=cOL_COGEN, 
                 "Coal-to-Gas"=cOL_NGConv,"Hydrogen Simple Cycle"=cOL_SCGT_H2,"Hydrogen Combined Cycle"=cOL_NGCC_H2,
                 #"Blended  Simple Cycle"=cOL_SCGT_Blend,"Blended  Combined Cycle"=cOL_NGCC_Blend,
                 "Natural Gas Combined Cycle + CCS"=cOL_NGCC_CCS,
                 "Natural Gas Simple Cycle"=cOL_SCGT, "Natural Gas Combined Cycle"=cOL_NGCC, 
                 "Hydro"=cOL_HYDRO, "Other"=cOL_OTHER, "Wind"=cOL_WIND, 
                 "Solar"=cOL_SOLAR, "Storage"=cOL_STORAGE,"Demand Curtailment"=cOL_DSM)
      
      colours2 = c("Coal"= cOL_COAL, "Coal-to-Gas"=cOL_COal2Gas, "Cogen"=cOL_COGEN,
                   "Natural Gas"=COL_NatGas,"Natural Gas + CCS"=cOL_NGCC_CCS,"Hydrogen"=COL_H2,
                   #"Natual Gas and Hydrogen Blend"=COL_Blend,

                   "Hydro"=cOL_HYDRO, "Other"=cOL_OTHER, 
                   "Wind"=cOL_WIND, "Solar"=cOL_SOLAR, "Storage"=cOL_STORAGE)

      colours3 = c("Coal"=cOL_COAL, "Cogeneration"=cOL_COGEN, 
                   "Coal-to-Gas"=cOL_NGConv,"Hydrogen Simple Cycle"=cOL_SCGT_H2,"Hydrogen Combined Cycle"=cOL_NGCC_H2,
                   #"Blended  Simple Cycle"=cOL_SCGT_Blend,"Blended  Combined Cycle"=cOL_NGCC_Blend,
                   "Natural Gas Combined Cycle + CCS"=cOL_NGCC_CCS,
                   "Natural Gas Simple Cycle"=cOL_SCGT, "Natural Gas Combined Cycle"=cOL_NGCC, 
                   "Hydro"=cOL_HYDRO, "Other"=cOL_OTHER, "Wind"=cOL_WIND, 
                   "Solar"=cOL_SOLAR, "Storage"=cOL_STORAGE)

      
      colours4=c("Import"= cOL_IMPORT, "Coal-to-Gas"=cOL_COal2Gas, "Coal"=cOL_COAL,"Cogen"=cOL_COGEN, 
                 "Natural Gas"=COL_NatGas,"Natural Gas + CCS"=cOL_NGCC_CCS,"Hydrogen"=COL_H2,
                 #"Natual Gas and Hydrogen Blend"=COL_Blend,
                 "Hydro"=cOL_HYDRO, "Other"=cOL_OTHER, 
                 "Wind"=cOL_WIND, "Solar"=cOL_SOLAR, "Storage"=cOL_STORAGE)

      colours5 = c("Cogeneration"=cOL_COGEN, 
                   "Coal-to-Gas"=cOL_NGConv,"Hydrogen Simple Cycle"=cOL_SCGT_H2,"Hydrogen Combined Cycle"=cOL_NGCC_H2,
                   #"Blended  Simple Cycle"=cOL_SCGT_Blend,"Blended  Combined Cycle"=cOL_NGCC_Blend,
                   "Natural Gas Combined Cycle + CCS"=cOL_NGCC_CCS,
                   "Natural Gas Simple Cycle"=cOL_SCGT, "Natural Gas Combined Cycle"=cOL_NGCC, 
                   "Hydro"=cOL_HYDRO, "Other"=cOL_OTHER, "Wind"=cOL_WIND, 
                   "Solar"=cOL_SOLAR,  "Storage - Battery"=COL_Battery, 
                   "Storage - Compressed Air"=COL_CompAir, "Storage - Pumped Hydro"=COL_Pumped)
      
      colours6=c("Natural Gas"=COL_NatGas,"Hydrogen"=COL_H2,
                 #"Natual Gas and Hydrogen Blend"=COL_Blend,
                 "Hydro"=cOL_HYDRO, "Other"=cOL_OTHER, 
                 "Wind"=cOL_WIND, "Solar"=cOL_SOLAR,  "Storage - Battery"=COL_Battery, 
                 "Storage - Compressed Air"=COL_CompAir, "Storage - Pumped Hydro"=COL_Pumped)

      colours7=c("Total Emissions"="black","Coal"=cOL_COAL, "Cogeneration"=cOL_COGEN, 
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
      colours8 = c("Cogeneration"=cOL_COGEN, "Coal"=cOL_COAL,
                   "Coal-to-Gas"=cOL_NGConv,
                   "Natural Gas Combined Cycle"=cOL_NGCC,"Natural Gas Simple Cycle"=cOL_SCGT, 
                   "Natural Gas Combined Cycle + CCS"=cOL_NGCC_CCS,
                   "Hydrogen Simple Cycle"=cOL_SCGT_H2,"Hydrogen Combined Cycle"=cOL_NGCC_H2,
                   "Blended  Simple Cycle"=cOL_SCGT_Blend,"Blended  Combined Cycle"=cOL_NGCC_Blend,
                   "Hydro"=cOL_HYDRO, "Other"=cOL_OTHER, "Wind"=cOL_WIND, 
                   "Solar"=cOL_SOLAR,  "Storage - Battery"=COL_Battery, 
                   "Storage - Compressed Air"=COL_CompAir, "Storage - Pumped Hydro"=COL_Pumped)
      
      AESO_colours <- c("goldenrod1", "gray60", "yellowgreen", "cornflowerblue",
                        "#001933")
    }

  # Gives years to summarize info from 
  Years2Disp <- c(2022,2025,2030,2035) # Years to show in figures
  Years2Pivot <- c(2022,2025,2030,2035)  # Years to display in tables
  
  # Get max year to display
  MaxYrStudy <-max(as.numeric(ResYr$Time_Period))-5

  #For fun, make the code beep when its all done
  beep(3)
}

### DELTE LATER. ##
# Plot all colors used
Legend_PlotAll(0.7)

# Plot main colors used
Legend_PlotMain(0.7)
####

# Create folder name to save as
CaseName <- "Limit to Zero - New"

################################################################################
## THE MOST USEFULL FUNCTIONS, AND SAVING OPTIONS
################################################################################
  
  # GENERATION
      # Grid of weekly output - need to edit for more than one week of data
      year_weeks(2022,BC)
      SaveRun_Loc(CaseName,"2022 Hourly Generation for One Week (Stacked Area)")
          
          year_weeks(2025,BC)
          SaveRun_Loc(CaseName,"2025 Hourly Generation for One Week (Stacked Area)")
          
          year_weeks(2030,BC)
          SaveRun_Loc(CaseName,"2030 Hourly Generation for One Week (Stacked Area)")
          
          year_weeks(2035,BC)
          SaveRun_Loc(CaseName,"2035 Hourly Generation for One Week (Stacked Area)")
      
          year_weeks(2040,BC)
          SaveRun_Loc(CaseName,"2040 Hourly Generation for One Week (Stacked Area)")
          
      # One year of weeks for storage output and pool price
      year_stor(2035,BC)
      SaveRun_Loc(CaseName,"2035 Storage Output with Pool Price")
          
          year_stor(2023,BC)
          SaveRun_Loc(CaseName,"2023 Storage Output with Pool Price")
      
      # Save all full size images
      windows(14,10,buffered=FALSE)
      
      # Yearly Output
      Evalyr(BC)
      SaveRun_Loc(CaseName,"Annual Generation (Stacked Area)")
      
      # Yearly Capacity
      Evalcap(BC)
      SaveRun_Loc(CaseName,"Annual Capacity")
      
      # Yearly percentage of generation
      EvalPerc(BC)
      SaveRun_Loc(CaseName,"Annual Generation (Percent)")
    
      # Bar chart showing each resource groups yearly output
      Output_Comp(BC)
      SaveRun_Loc(CaseName,"Annual Generation (Bar Chart)")
      
      # capacity factors for 2 years
      CFcompare(2022,2035,BC)
      SaveRun_Loc(CaseName,"Capacity Factors 2022 and 2023")
      
      # Annual average capacity factors for all resource types
      CF_Annual(BC)
      SaveRun_Loc(CaseName,"Annual Capacity Factors")
      
      # Wind duration curve with output as is
      Wind_Dur(BC)
      SaveRun_Loc(CaseName,"Wind Load Duration Curve")
      
      # Wind duration curve with Output normalized
      Wind_DurNorm(BC)
      SaveRun_Loc(CaseName,"Wind Capacity Factor Duration Curve")
      
      # Tell R the files are done and close window
      dev.off()
      
  # LTCE RESULTING BUILD/RETIRE
      # New window
      windows(14,6,buffered=FALSE)
      
      # Retirements by capacity (grouped by fuel type)
      RetireMW(BC)
      SaveRun_Loc(CaseName,"Retirements")
       
      # Capacity built by Aurora over study period
      # Build_A_MW(BC)
    
      # All new capacity
      BuildMW(BC)
      SaveRun_Loc(CaseName,"Additions")
      Build_Totals(BC)
  
      # Tell R the files are done and close window
      dev.off()
      
      # New window
      windows(14,10,buffered=FALSE)
      
      # Difference in capacity
      Eval_diffcap(BC)
      SaveRun_Loc(CaseName,"Capacity Changes")
  
  # PRICES AND COSTS
      # Shows Prices for simulation duration
      Sim_dur(BC)
      SaveRun_Loc(CaseName,"Price Duration Curve")
      
      # Shows production costs and fixed costs for full system
      System_Cost(BC)
      SaveRun_Loc(CaseName,"Total System Cost")
      
      # Price Table
      Report_P(Years2Pivot,BC)
  
      # Average monthly prices over full period
      AvgMn_price(BC)
      SaveRun_Loc(CaseName,"Monthly Pool Prices")
      
      # Average annual pool price
      AvgYr_poolprice(BC)
      SaveRun_Loc(CaseName,"Average Annual Pool Prices")
      
  # EMISSIONS
      # Annual emissions in stacked area chart
      AnnualEmStackCol(BC)
      SaveRun_Loc(CaseName,"Annual Emissions (Bar)")
      
      # Annual emissions in individual lines
      AnnualEmLine(BC)
      SaveRun_Loc(CaseName,"Annual Emissions (Line)")
      
  # OTHER STUFF
      # Annual import and export from AB as a bar chart
      Imp_Exp1(BC)
      SaveRun_Loc(CaseName,"Annual Imports and Exports")
      
      # Import and export for full year from AB
      Imp_Exp2(2025,BC)
      
      #Full Year output
      T_month_all_Sim(2022,BC)
  
      # Shows demand in AB 
      AnnualDemand(ZoneMn,BC)
      SaveRun_Loc(CaseName,"Annual Demand")
  
  # COMPARING TO AESO
      # Compare wind duration curves between AESO from 2025 to max year of sim
      AESO_Sim_WindDur(2024,BC)
      SaveRun_Loc(CaseName,"Wind Load Duration Curve Compared to AESO")
      
      # Compare wind duration curves between AESO from 2025 to max year of sim, normalize by capacity
      AESO_Sim_WindDurNorm(2026,BC)
      SaveRun_Loc(CaseName,"Wind Capacity Factor Duration Curve Compared to AESO")
                  
      # Tell R the files are done and close window
      dev.off()
   
   # NEW STUFF (NOT DONE)
      # New window
      windows(14,10,buffered=FALSE)
      
      # Shows new resource value each year 
      # 1 - wind, 2 - Solar, 3 - Storage, 4 - Natural gas, 5 - Hydrogen and Natural gas blend, 
      # 6 - Hydrogen, 7 - All rest (other, hydro, cogen, cola-to-gas)
      ResValue_Annual(1,BC)
      SaveRun_Loc(CaseName,"Annual Nomminal Value New Wind")
      
      # Shows new resource value added up to be cumulative (nominal values to each year)
      ResValue_Total(4,BC)
      SaveRun_Loc(CaseName,"Cummulative nominal value")
      
      # Net present value of all plants in resource group
      ResValue_NPV(1,BC)
      SaveRun_Loc(CaseName,"2023$ NPV Wind")
      
  # WRITE TO EXCEL
      # Annual data
      AnnualDataExcel(CaseName,BC)
      
      # Hourly data
      HourlyDataExcel(CaseName,BC)
      
      # Misc Annual Data for sheets
      CompareDataExcel(CaseName,BC)

################################################################################  
## BUT THERE ARE MORE ... HERE ARE ALL THE AVAILABLE FUNCTIONS!
################################################################################

################################################################################
## Output and Generation Functions (Output_Gen_Functions)
################################################################################
  
    # Gives stacked area chart for single week
    Week1(2035,12,08,BC)
      SaveRun_Loc(CaseName,"December 2035 Normal")
      
      WeekTest(2035,12,08,BC)
      SaveRun_Loc(CaseName,"December 2035 Adjusted")
    #Gives stacked area chart for a single day, output (MWh vs Date), grouped by resource
    day1(2022,11,07,BC)

    # Gives weekly storage function
    Stor1(2035,01,08,BC)
    
    # Gives weekly storage function with pool price
    Stor2(2023,02,08,BC)
    
    # Gives overall picture of Output over time period
    Evalyr(BC)
    
    # Gives overall picture of capacity over time period
    Evalcap(BC)
    
    # Gives all as % of total generation being met
    EvalPerc(BC)

    # Capacity of resource groups for selected years as a bar chart
    Output_Comp(BC)
    
    # Just shows the average demand
    AnnualDemand(ZoneYr,BC) 
    AnnualDemand(ZoneMn,BC) 
    
    # Shows the capacity factor for selected technologies in 2 different years (side by side bars)
    CFcompare(2022,2035,BC)
    
    # Average annaul capacity factors for all technolgoies as lines over study
    CF_Annual(BC)
    
    # Shows a year of week outputs corresponding to chosen year
    year_weeks(2022,BC)
    
    # Shows pool price over a week of resource group outputs
    PrOt(2035,01,08,BC)
    
    # Shows pool price over a week of resource group outputs, includes storage utilization
    PrOut(2035,01,08,BC)
    
    # Wind duration curve with output as is
    Wind_Dur(BC)
    
    # Wind duration curve with Output normalized
    Wind_DurNorm(BC)
    
    # One year of weeks for storage output and pool price
    year_stor(2035,BC)
################################################################################    
## Price Functions (Price_Functions)
################################################################################
    
    # Average Pool Price for one week
    week_price(2022,10,08,BC)
    
    #Shows Prices for simulation duration
    Sim_dur(BC)  
  
    # Average monthly pool price with a two-tailed 90th percentile range
    AvgMn_price(BC)
    
    # Shows the average monthly pool price for 2 years
    poolprice_2year(2022,2023,BC)
    
    # Average annual pool price for on and off peak conditions
    AvgYr_poolprice(BC)
    
    # Shows production costs and fixed costs for full system
    System_Cost(BC)

    # Shows new resrouce value each year 
    # 1 - wind, 2 - Solar, 3 - Storage, 4 - Natural gas, 5 - Hydrogen and Natural gas blend, 
    # 6 - Hydrogen, 7 - All rest (other, hydro, cogen, cola-to-gas)
    ResValue_Annual(1,BC)
    
    # Shows new resource value added up to be cumulative (nominal values to each year)
    ResValue_Total(1,BC)
    
################################################################################
## Build and Reture Functions (Build_Retire_Functions)
################################################################################
    
    # Number of units retired by fuel type
    Retirecol(BC)
    
    # Retirements by capacity (grouped by fuel type)
    RetireMW(BC)
    
    # Units Built over study period
    Builtcol(BC)
    
    # Capacity built by Aurora over study period
    Build_A_MW(BC)
    
    # All new capacity
    BuildMW(BC)
    
    # Shows capacity changes from previous year
    Eval_diffcap(BC)
    
    # Lets you get where units were built 
    Units(BC,wind)
    
    # Lets you get where units could have been built 
    Slack(BC,wind)

    #Compare available units and built units
    BuildUnits(BC, wind)
    
################################################################################
## Emission Functions (Emission_Functions)
################################################################################
    
    # Annual emissions in stacked area chart, outputs total annaul emissions in console
    AnnualEmLine(case)
    
    # Annual emissions in individual lines
    AnnualEmLine(case)
    
################################################################################
## Intertie Functions (Intertie_Functions)
################################################################################
    
    #Annual import and export from AB 
    Imp_Exp1(BC)
    
    # Hourly import and exports for a single year for AB
    Imp_Exp2(2022,BC)
    
    # Imports and exports from BC and SK
    BC_SK_IE(2022,BC)
    
    # Shows pool price and trade for a single month
    MN_Trade_Price(2022,09,BC)
    
    # Shows imports and exports for a single month
    MN_TradeOnly(2023,04,BC)
    
    # Show all trade for a singe year
    T_month_all_Sim(2035,BC)
    
    # AB imports and exports from AESO for a month
    Trade_Mn_AESO(2022,03,Imp_Exp)
    
    # AB imports and exports for a month with pool price, AESO
    TradeOnly_Mn_AESO(2022,03,Imp_Exp)
    
    # AB imports and exports for a year, AESO
    Trade_Yr_AESO(2022,Imp_Exp)
    
    # AESO price duration curve
    Duration_AESO(Years2See)
    
    # All trade for a year, AESO
    T_month_all(2022,Imp_Exp)
    
################################################################################
## Table Functions (Table_Functions)
################################################################################
    
    # Report the average annual pool prices for years specified
    Report_P(Years2Pivot,BC)
   
    # Total capacity added by resource group type
    Build_Totals(BC)
    
    # Capacity added by Aurora only
    Build_A_Totals(BC)
    
################################################################################
##  AESO and Sim Compare Functions
################################################################################
    
    # Prices and Output for noth AESO and Sim
    AESO_SimOP(2022,04,08,BC)
    
    # output compare only
    AESO_SimO(2022,01,08,BC)
    
    # Price compare only
    AESO_SimP(2022,04,08,BC)
    
    #Plot the full year pool price
    AESO_SimP2(2021,BC)
    
    #Show difference between pool price for year in graph
    year_comp(2021,BC)
    
    # Pool price diff as bar chart with labels
    year_dif(2021,BC)
    
    # Monthly average prices, sim vs act
    year_avg(2021,BC)
    
    #Plot Average Monthly Pool Price vs AESO
    year_pool(2021,2022,BC)
    
    # Compare duration of pool price from year 1 to year 2
    comp_dur(2021,2022,BC)
    
    # Compare duration of hourly load from year 1 to year 2
    load_dur(2025,2030,BC)
    
    # Plot capacity factors 
    tech_cap(2021,2022,BC)
    
    margin(2021,2022,BC)
    
    tot_cap(2021,2022,BC)
    
    AESOSim(2021,2022,BC)
    
    # Compare wind duration curves between AESO from 2025 to max year of sim
    AESO_Sim_WindDur(2025,BC)
    
################################################################################
## Data summarize and put in table  (Data_Filt_To_Table)
################################################################################
    # Annual data
    AnnualDataExcel("BAU",BC)
    
    # Hourly data
    HourlyDataExcel("BAU",BC)
    
    # Misc Annual Data for sheets
    CompareDataExcel("BAU",BC)
    
################################################################################
## Developing Functions (Developing_Functions)
################################################################################
    
    # Show a single day output for a full year
    YearOfDays(2022,3,BC,"ALL",14000)
    
    # Show a single day output for a full year, one resource group
    YearOfDays(2030,3,BC,"WIND",10000)
    
    # Single day output
    day1(2032,01,09,BC)
    
    # Single day output for single resource group
    day2(2022,01,11,BC,"WIND",10000)  
    
################################################################################
## OTHER FUNCTIONS
################################################################################    
    # Plot all colors used
    Legend_PlotAll(0.7)
    
    # Plot main colors used
    Legend_PlotMain(0.7)
    
################################################################################
## AESO FUNCTIONS
################################################################################
    
    #AESO Output
    Week_act(2020,01,08)
    
    #AESO Week Price
    wkPrice(2021,10,08)
    
    # AESO Week Price and output in one
    AESO_PrOt(2021,01,08)
    
    # Wind duration curve with output as is
    Wind_Dur_AESO(BC)
    
    # Wind duration curve with Output normalized
    Wind_DurNorm_AESO(BC)
    
################################################################################
## THESE ARE JUST SOME WINDOW SIZES AND STUFF
################################################################################
      
    #Clears all plots from R window
    dev.off(dev.list()["RStudioGD"]) 
      
    # Different window sizes for figures
    windows(12,8)
    windows(14,8)
    windows(10,8)
    windows(18,12)
    windows(16,12)

####
    # Random stuff, delete later
    ZnData <- ZoneYr %>%
      mutate(Year = year(Time_Period),
             time = Time_Period) %>%
      filter(Run_ID == case,
             Condition == "Average",
             Year<=2035) %>%
      mutate(Report_Year=as.numeric(Report_Year),
             Scenario=SourceDB,
             Production_Cost_Total=1000*Production_Cost_Total,
             Fixed_Cost_Total=1000*Fixed_Cost_Total)%>%
      subset(.,select=c(Name,Year,Price, 
                        Demand_Total,Net_Load_Total,
                        Production_Cost_Total,Fixed_Cost_Total,Scenario))
    
    
    checkthis <- sub_samp
    
    checkthis$Day <- date(checkthis$time)
    checkthis$Year <- as.factor(year(checkthis$time))
    
    checkthis2 <- checkthis %>%
      filter(Year==2021) %>%
      group_by(AESO_Name,ID) %>%
      summarise(Generation=sum(gen),Cap=mean(Capacity),Emissions=sum(co2_est))%>%
      filter(ID %in% c("TC02","TC01","RL1","MKRC","MKR1","COD1","BCRK","BCR2","ALS1"))
    