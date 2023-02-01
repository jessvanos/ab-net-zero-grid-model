################################################################################
# TITLE: Database_Loading
# DESCRIPTION:  Script loads database from Microsoft SQL Server, it then imports other data from files, 
# and calls functions to display plots and table.
#
#
# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: May 2022; LAST EDIT: January 6, 2023
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
    # ggplot: Used for graphical packages and aestheticc
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
                   "openxlsx","sqldf","timeDate","writexl")
  # Function to check for packages, install if not present, and load
  packs_check(packs_to_load)
 
}

################################################################################
## CONNECT TO MICROSOFT SQL SERVER
################################################################################

{ #Input Database Name below:
  SourceDB<-"BAU_Jan_31_2023"
  
  #Connect to database specified (via server, user, and password)
  con <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = rstudioapi::askForPassword("Server(IP_Addess,Port)"),
                   Database = SourceDB,
                   UID = rstudioapi::askForPassword("User Name"),
                   PWD = rstudioapi::askForPassword("Password"))  
}

################################################################################
## DEFINE CASES TO STUDY (IE: RUN ID)
## Value can be found in the "Run_Id" column of any AURORA table
################################################################################

{ BC <- "Base Case" 
  NZ <- "Net Zero"
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
                         Smp_Max_Date_Time, Smp_Max_Demand, Smp_Max_Capacity, 
                         Run_ID, Imports, Exports))
  
  # Zone Hourly for Everything
  ZoneHr_All <- ZoneHr %>%
    filter(Name == "WECC_Alberta") %>%
    subset(., select = c(date, Condition, Price, Demand, Marginal_Resource, 
                         Name,Report_Year, Report_Month,
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

HRcalc <- readRDS(here("Data Files","HRcalc.RData")) 

{ HRcalc$date <- as.POSIXct(HRcalc$Date_Begin_Local,tz="",format="%Y-%m-%d %H:%M")
  
  HRcalc<- HRcalc %>%
    select(.,-c("DAY_AHEAD_POOL_PRICE")) 

#Replace all NA values with zero
HRcalc[HRcalc==0] <- NA

#Reformat Day as day of year
HRcalc$Day <- format(HRcalc$date,"%j")
HRcalc$Week <- format(HRcalc$date,"%W") 
HRcalc$Month2 <- format(HRcalc$date,"%b")
}
################################################################################
## BRING IN OTHER DATA FROM AESO FILES & FORMAT (OPTIONAL)
################################################################################

{ # Load Leach Merit Data - Hourly resource info for Alberta (similar to ResHr and StackHr)
  merit <- readRDS(here("Data Files","Leach_MeritData.RData"))
  
    #Filter Data to relevant dates & remove old data
    merit_filt <- filter(merit, 
                         date >= as.Date("2020-01-1"))
    rm(merit)
  
  # Load nrgstream_gen - Load and demand info, plus a whole ton more
  load(here("Data Files","nrgstream_gen.RData")) 
  
    # Rename the time
    nrgstream_gen <- nrgstream_gen %>% rename(time=Time)
    
    # Get rid of rows with blanks
    nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$gen),] 
    nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$time),]   
}

################################################################################
## FURTHER FORMAT AND MANIPULATE NRG DATA (OPTIONAL)
################################################################################

{   # Create a demand table. Summarize takes median of Demand, AIL, and Price for each time period 
    demand <- nrgstream_gen %>%
      group_by(time) %>%
      summarise(Demand = median(Demand), 
                Price = median(Price),
                AIL = median(AIL))
    
    #Take out dates I dont care about and remove the old table
    sub_samp<-filter(nrgstream_gen, time >= as.Date("2020-01-1"))
    rm(nrgstream_gen)

    # Create a list to describe Import/Exports
    trade_excl<-c("AB - WECC Imp Hr Avg MW", 
                  "AB - WECC Exp Hr Avg MW",
                  "AB - WECC Imp/Exp Hr Avg MW")
    
    # Create Dataframe, ony select rows where the Nat resource group is in the defined groups (ie trading)
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
     plant_types<-c("COAL","COGEN","HYDRO","NGCC", "OTHER", "SCGT","SOLAR","IMPORT","EXPORT","WIND")
    
     # Create a new dataframe with plant types specified only, 
     # Then filter AESO data to exclude dates without information (after April 2022)
     df1a <- df1 %>%
     filter(Plant_Type %in% plant_types,month(time)<05)
      
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
            
      # Colour Fill info
          # Basic Groups
            cOL_IMPORT <- "hotpink" 
            cOL_EXPORT <- "firebrick4"
            cOL_COAL <- "snow3"
            cOL_COGEN <- "gray47"
            cOL_HYDRO <- "lightskyblue"
            cOL_OTHER <- "darkgreen"
            cOL_WIND <- "green3"
            cOL_SOLAR <- "gold"
            cOL_NUCLEAR <- "black"
            
          # H2 groups (blues)
            cOL_SCGT_H2 <- "#273871"
            cOL_NGCC_H2 <- "#3573B9"
            cOL_SCGT_Blend <- "#7FABD3"
            cOL_NGCC_Blend <- "#C1DBEC"
            COL_Blend <- "#C1DBEC" # may edit
            COL_H2 <- "#3573B9"  # may edit
            
          # Gas Groups (Purples)
            cOL_COal2Gas <-  "#611163" #"mediumorchid4"
            cOL_NGConv <- "#611163" #"mediumorchid4"
            cOL_SCGT <- "#3C2692"
            cOL_NGCC <- "#6D60BB"
            cOL_NGCC_CCS <- "#A79FE1"
            COL_NatGas <-"#6D60BB" #Maybe edit
            
          # Storage Groups
            cOL_STORAGE <- "yellow4" 
              COL_Battery <-"#F7921E"
              COL_CompAir <-"#D35000"
              COL_Pumped <-"#802A07"

   ## Now Define Lists to assign legends and colors in plots
      colours1=c("Import"= cOL_IMPORT, "Coal"=cOL_COAL, "Cogeneration"=cOL_COGEN, 
                  "Coal-to-Gas"=cOL_NGConv,"Hydrogen Simple Cycle"=cOL_SCGT_H2,"Hydrogen Combined Cycle"=cOL_NGCC_H2,
                 "Blended  Simple Cycle"=cOL_SCGT_Blend,"Blended  Combined Cycle"=cOL_NGCC_Blend,
                 "Natural Gas Combined Cycle + CCS"=cOL_NGCC_CCS,
                 "Natural Gas Simple Cycle"=cOL_SCGT, "Natural Gas Combined Cycle"=cOL_NGCC, 
                 "Hydro"=cOL_HYDRO, "Other"=cOL_OTHER, "Wind"=cOL_WIND, 
                 "Solar"=cOL_SOLAR, "Storage"=cOL_STORAGE)
      
      
      colours2 = c("Coal"= cOL_COAL, "Coal-to-Gas"=cOL_COal2Gas, "Cogen"=cOL_COGEN, 
                   "Natural Gas"=COL_NatGas,"Natural Gas + CCS"=cOL_NGCC_CCS,"Natual Gas and Hydrogen Blend"=COL_Blend,
                   "Hydrogen"=COL_H2,
                   "Hydro"=cOL_HYDRO, "Other"=cOL_OTHER, 
                   "Wind"=cOL_WIND, "Solar"=cOL_SOLAR, "Storage"=cOL_STORAGE)

      colours3 = c("Coal"=cOL_COAL, "Cogeneration"=cOL_COGEN, 
                   "Coal-to-Gas"=cOL_NGConv,"Hydrogen Simple Cycle"=cOL_SCGT_H2,"Hydrogen Combined Cycle"=cOL_NGCC_H2,
                   "Blended  Simple Cycle"=cOL_SCGT_Blend,"Blended  Combined Cycle"=cOL_NGCC_Blend,
                   "Natural Gas Combined Cycle + CCS"=cOL_NGCC_CCS,
                   "Natural Gas Simple Cycle"=cOL_SCGT, "Natural Gas Combined Cycle"=cOL_NGCC, 
                   "Hydro"=cOL_HYDRO, "Other"=cOL_OTHER, "Wind"=cOL_WIND, 
                   "Solar"=cOL_SOLAR, "Storage"=cOL_STORAGE)

      
      colours4=c("Import"= cOL_IMPORT, "Coal-to-Gas"=cOL_COal2Gas, "Coal"=cOL_COAL,"Cogen"=cOL_COGEN, 
                 "Natural Gas"=COL_NatGas,"Natural Gas + CCS"=cOL_NGCC_CCS,
                 "Natual Gas and Hydrogen Blend"=COL_Blend,"Hydrogen"=COL_H2,
                 "Hydro"=cOL_HYDRO, "Other"=cOL_OTHER, 
                 "Wind"=cOL_WIND, "Solar"=cOL_SOLAR, "Storage"=cOL_STORAGE)

      colours5 = c("Cogeneration"=cOL_COGEN, 
                   "Coal-to-Gas"=cOL_NGConv,"Hydrogen Simple Cycle"=cOL_SCGT_H2,"Hydrogen Combined Cycle"=cOL_NGCC_H2,
                   "Blended  Simple Cycle"=cOL_SCGT_Blend,"Blended  Combined Cycle"=cOL_NGCC_Blend,
                   "Natural Gas Combined Cycle + CCS"=cOL_NGCC_CCS,
                   "Natural Gas Simple Cycle"=cOL_SCGT, "Natural Gas Combined Cycle"=cOL_NGCC, 
                   "Hydro"=cOL_HYDRO, "Other"=cOL_OTHER, "Wind"=cOL_WIND, 
                   "Solar"=cOL_SOLAR,  "Storage - Battery"=COL_Battery, 
                   "Storage - Compressed Air"=COL_CompAir, "Storage - Pumped Hydro"=COL_Pumped)
      
      colours6=c("Natural Gas"=COL_NatGas,"Natual Gas and Hydrogen Blend"=COL_Blend,"Hydrogen"=COL_H2,
                 "Hydro"=cOL_HYDRO, "Other"=cOL_OTHER, 
                 "Wind"=cOL_WIND, "Solar"=cOL_SOLAR,  "Storage - Battery"=COL_Battery, 
                 "Storage - Compressed Air"=COL_CompAir, "Storage - Pumped Hydro"=COL_Pumped)

      colours7=c("Total Emissions"="black","Coal"=cOL_COAL, "Cogeneration"=cOL_COGEN, 
                 "Coal-to-Gas"=cOL_NGConv, "Blended  Simple Cycle"=cOL_SCGT_Blend,
                 "Blended  Combined Cycle"=cOL_NGCC_Blend,
                 "Natural Gas Combined Cycle + CCS"=cOL_NGCC_CCS,
                 "Natural Gas Simple Cycle"=cOL_SCGT, "Natural Gas Combined Cycle"=cOL_NGCC, "Other"=cOL_OTHER)
      
      Lines7=c("Total Emissions"=1,"Coal"=1, "Cogeneration"=6, 
               "Coal-to-Gas"=2, "Blended  Simple Cycle"=5,
               "Blended  Combined Cycle"=6,
               "Natural Gas Combined Cycle + CCS"=2,
               "Natural Gas Simple Cycle"=5, "Natural Gas Combined Cycle"=3, "Other"=4)
      
      # EVERYTHING
      colours8 = c("Cogeneration"=cOL_COGEN, "Coal"=cOL_COAL,
                   "Coal-to-Gas"=cOL_NGConv,"Hydrogen Simple Cycle"=cOL_SCGT_H2,"Hydrogen Combined Cycle"=cOL_NGCC_H2,
                   "Blended  Simple Cycle"=cOL_SCGT_Blend,"Blended  Combined Cycle"=cOL_NGCC_Blend,
                   "Natural Gas Simple Cycle"=cOL_SCGT, "Natural Gas Combined Cycle"=cOL_NGCC, 
                   "Natural Gas Combined Cycle + CCS"=cOL_NGCC_CCS,
                   "Hydro"=cOL_HYDRO, "Other"=cOL_OTHER, "Wind"=cOL_WIND, 
                   "Solar"=cOL_SOLAR,  "Storage - Battery"=COL_Battery, 
                   "Storage - Compressed Air"=COL_CompAir, "Storage - Pumped Hydro"=COL_Pumped,"Nuclear"=cOL_NUCLEAR)
      
      AESO_colours <- c("goldenrod1", "gray60", "yellowgreen", "cornflowerblue",
                        "#001933")
    }

  # Gives years to summarize info from 
  Years2Disp <- c(2022,2025,2030,2035) # Years to show in figures
  Years2Pivot <- c(2022,2025,2030,2035)  # Years to display in tables

  #For fun, make the code beep when its all done
  beep(3)
}

# Create folder name to save as
CaseName <- "Buisness as Usual"

################################################################################
## THE MOST USEFULL FUNCTIONS, AND SAVING OPTIONS
################################################################################
  
  # Save all full size images
  windows(14,10,buffered=FALSE)
  
  # GENERATION
      # Grid of weekly output - need to edit for more than one week of data
      year_weeks(2034,BC)
    
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
  
      # Tell R the files are done and close window
      dev.off()
   
   # NEW STUFF (NOT DONE)
      # Shows new resource value each year 
      # 1 - wind, 2 - Solar, 3 - Storage, 4 - Natural gas, 5 - Hydrogen and Natural gas blend, 
      # 6 - Hydrogen, 7 - All rest (other, hydro, cogen, cola-to-gas)
      ResValue_Annual(1,BC)
      SaveRun_Loc(CaseName,"Annual Nomminal Value")
      
      # Shows new resource value added up to be cumulative (nominal values to each year)
      ResValue_Total(1,BC)
      SaveRun_Loc(CaseName,"Cummulative nominal value")
      
      # Net present value of all plants in resource group
      ResValue_NPV(1,BC)
      SaveRun_Loc(CaseName,"NPV Wind")
      
  # WRITE TO EXCEL
      # Annual data
      AnnaulDataExcel(CaseName,BC)
      
      # Hourly data
      HourlyDataExcel(CaseName,BC)
      

################################################################################  
## BUT THERE ARE MORE ... HERE ARE ALL THE AVAILABLE FUNCTIONS!
################################################################################

################################################################################
## Output and Generation Functions (Output_Gen_Functions)
################################################################################
  
    # Gives stacked area chart for single week
    Week1(2021,01,08,BC)
  
    #Gives stacked area chart for a single day, output (MWh vs Date), grouped by resource
    day1(2022,11,07,BC)

    # Gives weekly storage function
    Stor1(2021,01,08,BC)
    
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
    PrOt(2022,09,09,BC)
    
    # Shows pool price over a week of resource group outputs, includes storage utilization
    PrOut(2022,09,09,BC)
    
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
    
################################################################################
## Data summarize and put in table  (Data_Filt_To_Table)
################################################################################
    # Annual data
    AnnaulDataExcel("FreeO&M PS SUN",BC)
    
    # Hourly data
    HourlyDataExcel("BAU",BC)
    
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
## AESO FUNCTIONS
################################################################################
    
    #AESO Output
    Week_act(2020,01,08)
    
    #AESO Week Price
    wkPrice(2021,10,08)
    
    # AESO Week Price and output in one
    AESO_PrOt(2021,01,08)
    
    
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


    