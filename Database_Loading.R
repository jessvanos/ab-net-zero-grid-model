################################################################################
# TITLE: Database_Loading
# DESCRIPTION:  Script loads database from Microsoft SQL Server, it then imports other data from files, 
# and calls functions to display plots and table.
#
#
# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: May 2022
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
    # ggpattern: Geoms for ggplot2, allow pattern fills (like stripes)
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
    # extrafont: Add windows font options to R environment.
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
  source(here('Functions','Daily_Output_Functions.R'))# Filtering by resource type, required for plots
  source(here('Functions','Other_Functions.R'))       # Other functions used in plotting functions
  #source(here('Functions','Developing_Functions.R')) # Under construction functions
  source(here('Functions','Data_Filt_To_File.R'))     # Functions that filter data and export it to excel sheets
  source(here('Functions','aeso_gen.R'))           #
  source(here('Functions','aseo_sim_comp_1.R'))       #
  source(here('Functions','Group_PlotSave.R'))          #
  
  
  # Packages required
  packs_to_load = c("tidyverse","ggplot2","scales","grid","gtable","gridExtra","odbc","ggpubr","extrafont",
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
  SourceDB<-"CP_02_Feb_2024"
  
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

BC <- "Base Case" 

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
  ResMn <- dbReadTable(con,'ResourceMonth1')
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

  # LT Tables
  LTRes <- dbReadTable(con,'LTResValue1')
  Build <- dbReadTable(con,'LTBuildReport1')
  Study <- dbReadTable(con,'LTStudyLog1')
  #CReport <- dbReadTable(con,'LTConstraintReport1')
  
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
  
  ResMn$Time_Period <- ym(ResMn$Time_Period)
  
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
## SAVE OTHER DATA AS R FILE AND MODIFY (OPTIONAL)
## Only need to run if new files are available
################################################################################
# 
# # Load merit data
# {
# merit <- read_csv(here("Data Files","Alberta Data","student_data_2023_Aug_15_16_56.csv.gz"))
#   # Save as R file
#   saveRDS(merit, here("Data Files","Alberta Data","Leach_MeritData15Aug2023.RData"))
#   # Remove from workspace
#    rm(merit)
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
## BRING IN DATA FROM AESO FILES & FORMAT  (OPTIONAL)
################################################################################
{ 
  date_filt<-"2005-01-1"
  
  # Load Leach Merit Data - Hourly resource info for Alberta (similar to ResHr and StackHr)
  merit <- readRDS(here("Data Files","Alberta Data","Leach_MeritData15Aug2023.RData"))
  
    #Filter Data to relevant dates & remove old data
    merit_filt <- filter(merit,date >= as.Date(date_filt))
    rm(merit)
  
  # Load nrgstream_gen - Load and demand info, plus a whole ton more
    nrgstream_gen <- readRDS(here("Data Files","Alberta Data","nrgstream_gen_corrected03Mar2023.RData")) 
    Actdemand <- readRDS(here("Data Files","Alberta Data","nrgstream_demand03Mar2023.RData"))
  
    #Reformat the dates
    Actdemand$Day <- date(Actdemand$time)
    
    # Take out dates I don't care about and remove the old table
    sub_samp<-filter(nrgstream_gen, time >= as.Date(date_filt))
    Actdemand<-filter(Actdemand, time >= as.Date(date_filt))
    
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
                capacity =sum(Capacity),
                total_gen=sum(gen,na.rm = T),
                total_rev=sum(Revenue,na.rm = T),
                price_mean=mean(Price),
                heatrt_mean=mean(Heat.Rate)) %>% 
      ungroup()
    
        #Reformat the dates
        df1$Day <- date(df1$time)
        df1$Year <- as.factor(year(df1$time))
        df1$Hour <-hour(df1$time)
    
  {  # ORGANIZE RESOURCES
     #Make a resource type list
     plant_types<-c("COAL","NGCONV","COGEN","HYDRO","NGCC", "OTHER", "SCGT","SOLAR","IMPORT","EXPORT","WIND","STORAGE")
    
     # Create a new dataframe with plant types specified only, 
     # Then filter AESO data to exclude dates without information (till end of 2022)
     df1a <- df1 %>%
     filter(Plant_Type %in% plant_types,
            year(time)<2024)
      
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
  
    #font_import()
    #loadfonts(dev="win")
    font_add(family="Times",regular="times.ttf")
    Plot_Text <- "Times"
    
    # Make ggplot show text changes
        showtext_auto()
    # font_add(family="Cambrai",regular="CAMBRIA.ttc")
    # Plot_Text <- 'Cambrai'
    
    
    
  # Set default size for plot features to be constant. All based on general text size
  { GenText_Sz =46 # GGsave
    #GenText_Sz = 20 # Windows save 
    Tit_Sz = GenText_Sz-2
    XTit_Sz = GenText_Sz+6
    YTit_Sz = GenText_Sz+6
    Leg_Sz=GenText_Sz-6
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
      
       
      AESO_colours <- c("goldenrod1", "gray60", "yellowgreen", "cornflowerblue",
                        "#001933")
    }

  # Gives years to summarize info from 
  Years2Disp <- c(2023,2025,2030,2035,2040,2045) # Years to show in figures
  Years2Pivot <- c(2023,2025,2030,2035,2040,2045)  # Years to display in tables
  
  # Get max year to display
  MaxYrStudy<-2045
  #MaxYrStudy <-max(as.numeric(ResYr$Time_Period))-5
  
  # Adjust capacity manually for 2025 (Manual add vs Aurora)
  AESO_SUN_2025=242.5
  AESO_WND_2025=400
  AESO_PS_2025=29
  AESO_COGEN_2025=25

  #For fun, make the code beep when its all done
  beep(3)
}

### CAN DELTE LATER. ##
# Plot all colors used
Legend_PlotAll(0.7)

# Plot main colors used
Legend_PlotMain(0.7)
# The gray colors used
Legend_PlotGray(1)


# Create folder name to save as 
#   Casename is long description for figures/files
#   NameShort is short name for later reference in r files
CaseName <- "CP Renewable Analysis"
NameShort<-'CER_05Feb'

################################################################################
## OUTPUT PLOTS AND DATA TO FOLDERS:
##  Data: Data Files > Result Files
##  Figures: Figures (Local)
################################################################################

# SAVE PLOTS AND FIGURES  
  # ANALYSIS 
    # Normal analysis
    Analysis_saveall(CaseName)
    # Detailed generation plots
    Detail_Gen_save(CaseName)

  # ADDITIONAL ANALYSIS
    # Value plots
      Value_saveall(CaseName)
    # Slack plots
      Slack_saveall(CaseName)
      
  # CER ANALYSIS 
      CER_saveall(CaseName)
      
# SAVE DATA
  # WRITE TO EXCEL
    # Annual data ('long name','short name',case)
      AnnualDataExcel(CaseName,NameShort,BC)
      
    # Hourly data ('long name','short name',case)
      HourlyDataExcel(CaseName,NameShort,BC)
      
  # GENERATE R FILES TO COMPARE LATER ('short name',case) -  skip if this is not needed
    AnnualDataR(NameShort,BC)
      
# OPTIONAL 
    # Estimate hourly wind and solar weighted data -> this takes a LONG TIME to run
    ResGrouphr_R8760 <- ResGrouphr_Renew8760()
    
################################################################################
## COMMON INDIVIDUAL PLOT SAVING OPTIONS
################################################################################
  
    GenText_Sz = 20 # Better for viewing individual plots :)
    
  # HOURLY GENERATION
      # Grid of weekly output - need to edit for more than one week of data
      year_weeks(2043,BC)
      SaveRun_Loc(CaseName,"2023 Hourly Generation for One Week (Stacked Area)")

      # Four months of generation and pool price
      FourMonthSummary(2040,01,04,07,10,BC) ### Redo without trade included
      SaveRun_Loc(CaseName,"2040 Output, Trade, Price") 
          
  # GENERATION
      # Save all full size images
      windows(14,10,buffered=FALSE)
      
      # RUN TO CHECK CURTAIL OR SPECIFIC RESOURCE
          # One week
          week12_Curt(2043,08,08,BC)
          SaveRun_Loc(CaseName,"Week 2043 Aug")
          
          # One week each resource ("free_y", "fixed")
          EachResWeek(2030,02,08,BC,"free_y")
          SaveRun_Loc(CaseName,"Indv Week Oct 2040")
      
          # capacity factors for 2 years
          CFcompare(2023,2040,BC)
          SaveRun_Loc(CaseName,"Capacity Factors 2022 and 2045")
          
      # Yearly Output
      Evalyr(BC,"n")
      SaveRun_Loc(CaseName,"Annual Generation (Stacked Area)")
      
      # Yearly Capacity
      Evalcap(BC,"n")
      SaveRun_Loc(CaseName,"Annual Capacity")
      
      # Yearly percentage of generation
      EvalPerc(BC,"n")
      SaveRun_Loc(CaseName,"Annual Generation (Percent)")
    
      # Bar chart showing each resource groups yearly output
      Output_Comp(BC)
      SaveRun_Loc(CaseName,"Annual Generation (Bar Chart)")
      
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
      Build_A_MW(BC)
    
      # All new capacity
      BuildMW(BC)
      SaveRun_Loc(CaseName,"Additions")
      
      Build_Totals(BC)
  
      # Tell R the files are done and close window
      dev.off()
      
      # New window
      windows(14,10,buffered=FALSE)
      
      # Difference in capacity
      TotalCapChange(BC)
      SaveRun_Loc(CaseName,"Capacity Changes")
      
      # Net difference in capacity
      Eval_CapChange(BC)
      SaveRun_Loc(CaseName,"Net Capacity Changes")
  

      # Combined cycle study fate by resource
      CC_Fate_study(BC)
        CC_Fate_year(BC)
      SaveRun_Loc(CaseName,"Combined Cycle Gas Fate")
      
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
      
      #Capture Prices
      capture_p(2023,2030,BC)
      SaveRun_Loc(CaseName,"Capture Prices")
      
      # Relative capture prices
      Relcapture_p(2023,2035,BC)
      SaveRun_Loc(CaseName,"Relative Capture Prices")
      
      # Premeium to pool price
      ach_poolprem(BC)
      SaveRun_Loc(CaseName,"Achived Premium to Pool Price")
      
      
  # EMISSIONS
      # Annual emissions in stacked area chart
      AnnualEmStackCol(BC,"NAICS")
      SaveRun_Loc(CaseName,"Annual Emissions (Bar)")
      
      # Annual emissions in individual lines
      AnnualEmLine(BC,"ALL")
      SaveRun_Loc(CaseName,"Annual Emissions (Line)")
      
  # OTHER STUFF
      # Annual import and export from AB as a bar chart
      Imp_Exp1(BC)
      SaveRun_Loc(CaseName,"Annual Imports and Exports")
      
      # Import and export for full year from AB
      Imp_Exp2(2043,BC)
  
  # DAILY OUTPUT FUNCTIONS
    CompDay_Season(2040,14,BC)  
    SaveRun_Loc(CaseName,"Daily Output - Season 2040")
      
    CompDay_Wind(2040,BC)
    SaveRun_Loc(CaseName,"Daily Output - Max Wind 2040")
    
    CompDay_Solar(2040,BC)
    SaveRun_Loc(CaseName,"Daily Output - Max Solar 2040")
    
    CompDay_Years(2023,2043,11,10,BC)
    SaveRun_Loc(CaseName,"Daily Output Nov- Years")
    
    CompDay_AESO(2023,2,14,BC)
    SaveRun_Loc(CaseName,"Daily Output Compared to AESO Feb Day")
    
      
  # COMPARING TO AESO
      # Compare wind duration curves between AESO from 2025 to max year of sim
      AESO_Sim_WindDur(2024,BC)
      SaveRun_Loc(CaseName,"Wind Load Duration Curve Compared to AESO")
      
      # Compare wind duration curves between AESO from 2025 to max year of sim, normalize by capacity
      AESO_Sim_WindDurNorm(2023,2045,3,BC)
      SaveRun_Loc(CaseName,"Wind Capacity Factor Duration Curve Compared to AESO")
      
      # Capacity factor ridgeline
      Resource_Ridge("LTO_Wind",2023,2045,5,BC)
      Resource_Ridge("LTO_Solar",2023,2045,5,BC)
                  
      # Tell R the files are done and close window
      dev.off()
  
################################################################################  
## BUT THERE ARE MORE ... HERE ARE ALL THE AVAILABLE FUNCTIONS!
################################################################################

################################################################################
## Output and Generation Functions (Output_Gen_Functions)
################################################################################
  
    # Gives stacked area chart for single week
    Week1(2023,02,08,BC)
      SaveRun_Loc(CaseName,"Feb 2035 Output")
      
    #Gives stacked area chart for a single day, output (MWh vs Date), grouped by resource
    Day1(2043,08,13,BC)

    # Gives weekly storage function
    Stor1(2035,01,08,BC)
    
    # Gives weekly storage function with pool price
    Stor2(2023,02,08,BC)
    
    # Gives overall picture of Output over time period
    Evalyr(BC,"n")
    
    # Gives overall picture of capacity over time period
    Evalcap(BC,"n")
    
    # Gives all as % of total generation being met
    EvalPerc(BC,"n")

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
    PrOt(2022,01,08,BC)
    
    # Shows pool price over a week of resource group outputs, includes storage utilization
    PrOut(2022,01,08,BC)
    
    # Show capacity factor for individual resources included in CER
    CF_CER_Res(BC)
    
    # Show hours opperated for individual units included in CER
    Hours_CER_Res(BC)
    
    # Show capacity factor based on when CER applies by group
    CF_CER_groups(BC)
    
    # Wind duration curve with output as is
    Wind_Dur(BC)
    
    # Wind duration curve with Output normalized
    Wind_DurNorm(BC)
    
    # Max curtailment that occured
    MaxCurtail(BC)
    SaveRun_Loc(CaseName,"Max Curtailment")
    
    # One year of weeks for storage output and pool price
    year_stor(2035,BC)
    SaveRun_Loc(CaseName,"2035 Storage Output with Pool Price")
    
    # Four months of generation, intertie, and pool price
    FourMonthSummary(2022,BC)
    
    # Number of startups by plant type
    Num_Startups(case)
    
    # Hours run and emissions for CER resources
    CER_EM_hour_Res(case)
    
    # Capacity factor and emissions for CER resources grouped by year applied
    CER_EM_hour_group(case)
    
    # Ridgeline capacity factor pot
    Resource_Ridge("LTO_Wind",2023,2045,2,BC)
    Resource_Ridge("LTO_Solar",2023,2045,5,BC)
    
    
    # Renewable curtailment 
    Renew_Curtail_MWa(BC)
    Renew_Curtail_perc(BC)
    
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

    # Shows new resource value each year 
    # 1 wind, 2- Solar, 3 - Storage, 4 - Unabated natural gas, 5- Abated natural gas, 6 - Hydrogen
    # 7 - Hydro, 8 - Other, 9 - Cogen
    ResValue_Annual(1,1800,BC)
    SaveRun_Loc(CaseName,"Annual Value Wind")
    
    ResValue_Annual_MWh(5,1800,BC)
    SaveRun_Loc(CaseName,"Annual Value Wind MWh")
    
    # Show capture price relative to mean price
    Relcapture_p(2019,2030,BC)
    
    # Show capture price 
    capture_p(2023,2043,BC)
    
    # Show achived pool price premium
    ach_poolprem(BC)
    
################################################################################
## Build and Retire Functions (Build_Retire_Functions)
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
    
    # Shows net capacity changes from previous year
    Eval_CapChange(BC)
    
    # Shows total capacity changes from previous year
    TotalCapChange(BC)
    
    # Lets you get where units were built 
    Units(BC,wind)
    
    # Lets you get where units could have been built 
    Slack(BC,wind)

    #Compare available units and built units ("WND", "SUN","GasCCS","BIO","Gas1","Gas2","H2","UR","PS")
    BuildUnits(BC, "Gas2")
    SaveRun_Loc(CaseName,"Res Built UR")

    # CCS retrofits - shows year of retrofit and year of retirement
    Build_CCSRet(BC)
    Build_CCSRet2(BC)
    SaveRun_Loc(CaseName,"CCS Retrofit Dates")
    
    # Original units CCS retorift end date
    Build_CCSRet2(BC)
    SaveRun_Loc(CaseName,"CC Retrofit Retire Date")
    
    # Combined cycle study fate by resource
    CC_Fate_study(BC)
    
    # Combined cycle annual capacity changes and fate
    CC_Fate_year(BC)
    SaveRun_Loc(CaseName,"CC Fate Year")
################################################################################
## Emission Functions (Emission_Functions)
################################################################################
    
    # Annual emissions in stacked area chart, outputs total annual emissions in console
    AnnualEmStackCol(case)
    
    # Annual emissions in individual lines
    AnnualEmLine(case)
    
    # CER resource emissions ind plants
    Emissions_CER_Res(BC)
    
    # CER resource emissions grouped
    Emissions_CER_group(BC)
    
################################################################################
## Intertie Functions (Intertie_Functions)
################################################################################
    
    #Annual import and export from AB 
    Imp_Exp1(BC)
    
    # Hourly import and exports for a single year for AB
    Imp_Exp2(2022,BC)
    
    # Sinlge week import and export
    Imp_ExpWk(2022,12,08,BC)
    
    # Imports and exports from BC and SK
    BC_SK_IE(2022,BC)
    
    # Shows pool price and trade for a single month
    MN_Trade_Price(2022,09,BC)
    
    # Shows imports and exports for a single month
    MN_TradeOnly(2023,04,BC)
    
    # Show all trade for a singe year
    T_month_all_Sim(2035,BC)
    
    # SPECIAL ONES
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
## DAILY OUTPUT FUNCTIONS
################################################################################
    
    # Plot seasonal typical days for a given year
    CompDay_Season(2022,08,BC)  

    # Plot max and min wind days for a given year
    CompDay_Wind(2022,BC)

    # Plot max and min solar days for a given year
    CompDay_Solar(2022,BC)
    
    # Compare same days for two seperate years
    CompDay_Years(2022,2034,08,13,BC)
    
    # Plot and compare actual data to sim data for a given year
    CompDay_AESO(2022,08,08,BC)
    
################################################################################
##  AESO and Sim Compare Functions
################################################################################
    
    # Prices and Output for noth AESO and Sim
    AESO_SimOP(2022,04,08,BC)
    
    # output compare only
    AESO_SimO(2022,06,08,BC)
    
    # Price compare only
    AESO_SimP(2022,04,08,BC)
    
    #Plot the full year pool price
    AESO_SimP2(2022,BC)
    
    #Show difference between pool price for year in graph
    year_comp(2022,BC)
    
    # Pool price diff as bar chart with labels
    year_dif(2043,BC)
    
    # Monthly average prices, sim vs act
    year_avg(2022,BC)
    
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
    
    # Compare ridgelines
    AESO_Sim_RidgeCF("SOLAR",2022,2023,1,BC)
    SaveRun_Loc(CaseName,"Solar 2022 and 2023 Ridgelines")
    
################################################################################
## Data summarize and put in table  (Data_Filt_To_Table)
################################################################################
    # Annual data
    AnnualDataExcel("BAU",BC)
    
    # Hourly data
    HourlyDataExcel("BAU",BC)

################################################################################
## AESO FUNCTIONS
################################################################################
    
    #AESO Output
    Week_act(2020,04,08)
    
    #AESO Week Price
    wkPrice(2021,10,08)
    
    # AESO Week Price and output in one
    AESO_PrOt(2021,01,08)
    
    # Wind duration curve with output as is
    Wind_Dur_AESO(BC)
    
    # Wind duration curve with Output normalized
    Wind_DurNorm_AESO(BC)
  
    # Historical Gen (year min plot, seperate)
    Evalyr_AESO(2010,"y")
    
    # Capacity AESO
    Evalcap_AESO(2010,"n")
    
    # AESO solar nad wind ridgelines
    Resource_Ridge_AESO("WIND")
    Resource_Ridge_AESO("SOLAR")
    
    SourceDB<-"NRG"
    GGSave_Loc("Hist Figs","hist_cap_AB_byplant",Evalcap_AESO2(2010,"n"),300)
################################################################################
## Developing Functions (Developing_Functions)
################################################################################
    
    # Show a single day output for a full year
    YearOfDays(2022,3,BC,"ALL",14000)
    
    # Show a single day output for a full year, one resource group
    YearOfDays(2030,3,BC,"WIND",10000)
    
    # Single day output for single resource group
    day2(2022,01,11,BC,"WIND",10000)  
    
      
################################################################################
## OTHER FUNCTIONS
################################################################################    
    # Plot all colors used
    Legend_PlotAll(0.7)
    
    # Plot main colors used
    Legend_PlotMain(0.7)
    
    # Plot main colors used
    Legend_PlotGray(1)
    
    
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

    
################################################################################    

    
    
    
    
    
    
    
    
    
             