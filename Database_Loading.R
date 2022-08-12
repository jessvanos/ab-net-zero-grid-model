################################################################################
# TITLE: Database_Loading
# DESCRIPTION:  Script loads database from Microsoft SQL Server and imports tables, it then imports AESO data


# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: May 2022; LAST EDIT: July 25, 2022

# NOTES: Make sure the project file is open first or "here" commands wont work right.
#        Before running, create folder called "Data Files" withen project directory and populate it with AESO data. 
#        Once this file is run through completion, can call any functions with environment that is loaded.

################################################################################
## LOAD REQUIRED PACKAGES AND SOURCE FUNCTIONS
 {# Package Info
    # tidyverse: Data science package
    # ggplot: Used for graphical packages and aestheticc
    # grid: Used for plotting, adds grid to the plot
    # gtable: Grob tables, more tools
    # gridExtra: User functions for grid graphics
    # odbc: Driver for Database Loading
    # ggpubr:
    # DBI: Package for interface between database and R
    # lubridate: Allow time and data manipulation
    # cowplot: Quality features for ggplots
    # scales: Graphical mapping stuff
    # dplyr: Data manipulation package
    # reshape2:
    # zoo: Used for time series indexing
    # ggpattern: Geoms for ggplot2
    # here: Package to set filepaths inside R project
    # beepr: Allows sound to paly when code is done
    # showtext: Allows fonts changes in ggplot
  }

{ # Must load the here package in order to make sure internal project directories work
  library(here)
  
  # Import functions from files, take from the functions folder in R project
  source(here('Functions','other_functions.R'))
  source(here('Functions','sim_eval_1.R'))
  source(here('Functions','aeso_eval_1.R'))
  source(here('Functions','aseo_sim_comp_1.R')) 
  source(here('Functions','Net_Zero_eval.R'))
  source(here('Functions','intertie_info.R'))
  source(here('Functions','net_zero_tables.R'))
  
  # Packages required
  packs_to_load = c("tidyverse","ggplot2","grid","gtable","gridExtra","odbc","ggpubr",
                   "DBI","lubridate","cowplot","scales","dplyr","reshape2","zoo",
                   "ggpattern","here","beepr","showtext","DescTools","pivottabler",
                   "openxlsx")
  # Function to check for packages, install if not present, and load
  packs_check(packs_to_load)
 
}

################################################################################
## CONNECT TO MICROSOFT SQL SERVER

{ #Input Database Name below:
  SourceDB<-"TestRun_Aug_4_2022_d"
  
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

{ BC <- "Base Case" 
  BAU <- "BAU" #Buisness as usualcase
  CC <- "CarbonCredits"
  BC1 <- "BC1"
  BC2 <- "BC2"
}

################################################################################
## READ TABLES FROM DATABASE INTO ENVIRONMENT
## Can edit to select required tables only, DOUBLE CHECK all tables are in databse
## It will just skip tables that are not there and all the ones after

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
#  ResGroupEmSt <- dbReadTable(con,'ResourceGroupEmissionsStudy1')
  
  # Other Tables
  ResStackYr <- dbReadTable(con,'ResourceStackYear1')
  ResStackHr  <- dbReadTable(con,'ResourceStackHour1')
  LinkYr <- dbReadTable(con,'LinkYear1')
# LinkMn <- dbReadTable(con,'LinkMonth1')
  LinkHr <- dbReadTable(con,'LinkHour1')
  
#  CC <- dbReadTable(con,'CustomConstraint1')
  ZoneYr <- dbReadTable(con,'ZoneYear1')
#  ZoneMn <- dbReadTable(con,'ZoneMonth1')
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

{  # Fuel Tables
#  FuelYr$Time_Period <- as.Date(as.character(FuelYr$Time_Period), 
#                       format = "%Y")
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
  
  # Other Tables
  ResStackYr$Time_Period  <- as.Date(as.character(ResStackYr$Time_Period), 
                        format = "%Y")
  ResStackHr$date <- as.POSIXct(as.character(ymd_h(gsub(" Hr ", "_",ResStackHr$Time_Period))), 
                        tz = "MST")-(60*60)
  
   LinkYr$Time_Period  <- as.Date(as.character(LinkYr$Time_Period), 
                        format = "%Y")
   LinkMn$Time_Period <- ym(LinkMn$Time_Period)
   LinkHr$Time_Period <- as.POSIXct(as.character(ymd_h(gsub(" Hr ", "_",LinkHr$Time_Period))), 
                             tz = "MST")-(60*60) 
   
   
   ZoneYr$Time_Period  <- as.Date(as.character(ZoneYr$Time_Period), 
                       format = "%Y")
#  ZoneMn$Time_Period <- ym(ZoneMn$Time_Period)
  ZoneHr$date <- as.POSIXct(as.character(ymd_h(gsub(" Hr ", "_",ZoneHr$Time_Period))), 
                         tz = "MST")-(60*60)   
  
}

################################################################################
## FILTER TABLES TO GET RID OF COLUMNS I DONT CARE ABOUT & PULL OUT IMPORT/EXPORT
{ 
  { # HOURLY DATA
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
                           Dispatch_Cost, Incr_Cost, Fixed_Cost, Fuel_Cost, 
                           Output_MWH, Percent_Marginal, Percent_Committed,
                           Revenue, Variable_OM_Cost, Capacity_Factor, 
                           Total_Emission_Cost, Total_Hours_Run, Condition, 
                           Report_Year, Run_ID, Peak_Capacity, 
                           Primary_Fuel,Zone))   
  }
  
  { #IMPORT/EXPORT 
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
  }
}  
################################################################################
## LOAD AESO TRADE INFO FROM R FILE INTO WORKSPACE

HRcalc <- readRDS(here("Data Files","HRcalc.RData")) 

{ HRcalc$date <- as.POSIXct(HRcalc$Date_Begin_Local,tz="",format="%Y-%m-%d %H:%M")
  
  HRcalc<- HRcalc %>%
    select(.,-c("DAY_AHEAD_POOL_PRICE")) }

#Replace all NA values with zero
HRcalc[HRcalc==0] <- NA

#Reformat Day as day of year
HRcalc$Day <- format(HRcalc$date,"%j")
HRcalc$Week <- format(HRcalc$date,"%W") 
HRcalc$Month2 <- format(HRcalc$date,"%b")

################################################################################
## BRING IN OTHER DATA FROM AESO FILES & FORMAT

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
## FURTHER FORMAT AND MANIPULATE NRG DATA

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

{ # Available Fonts for plotting, can choose different one and change Plot_Text if needed
  # Uses local computer font files (search font in search bar to confirm font names)
  
    font_add(family="Times",regular="times.ttf")
    Plot_Text <- 'Times'
    
    # font_add(family="Cambrai",regular="CAMBRIA.ttc")
    # Plot_Text <- 'Cambrai'
    
    showtext_auto()
    
  # Set size for plot features to be consitant
  { Tit_Sz = 15
    XTit_Sz = 15
    YTit_Sz = 15
    Overall_Sz =15 }
    
    
  { # Define fuel types for new builds
    solar <- "SUN"
    wind <- "WND"
    other <- "OT"
    storage <- "PS"
    gas1 <- "Gas1"
    gas0 <- "Gas0"
  }  

  # Set legend color schemes for contistancy
    ## Can change here
    { # Colours Outline Info
      OUT_IMPORT <- "hotpink3"
      OUT_COAL <- "snow4"
      OUT_NGConv <- "mediumorchid4"
      OUT_COGEN <- "gray27"
      OUT_SCGT <- "midnightblue"
      OUT_NGCC <- "dodgerblue4"
      OUT_OTHER <- "darkgreen"
      OUT_HYDRO <- "deepskyblue"
      OUT_WIND <- "green4"
      OUT_SOLAR <- "gold3"
      OUT_STORAGE <- "yellow4"
      OUT_COal2Gas <- "mediumorchid4"
      
      # Colour Fill info
      cOL_IMPORT <- "hotpink" #"darkorchid1"
      cOL_COAL <- "snow3"
      cOL_NGConv <- "mediumorchid4"
      cOL_COGEN <- "gray47"
      cOL_SCGT <- "navy"
      cOL_NGCC <- "dodgerblue3"
      cOL_HYDRO <- "lightskyblue"
      cOL_OTHER <- "darkgreen"
      cOL_WIND <- "green3"
      cOL_SOLAR <- "gold"
      cOL_STORAGE <- "yellow4" 
      cOL_COal2Gas <- "mediumorchid4"
      cOL_EXPORT <- "firebrick4"
      
      cOL_Gas <- "slateblue"
      COL_Gas1 <- "dodgerblue3"
      COL_Gas2 <- "navy"

      # Now Define Lists
      colours1=c("Import"= cOL_IMPORT, "Coal"=cOL_COAL, "Cogen"=cOL_COGEN, 
                  "Coal-to-Gas"=cOL_NGConv,"SCGT"=cOL_SCGT, "NGCC"=cOL_NGCC, 
                 "Hydro"=cOL_HYDRO, "Other"=cOL_OTHER, "Wind"=cOL_WIND, 
                 "Solar"=cOL_SOLAR, "Storage"=cOL_STORAGE)
      
      Outline1 = c("Import"= OUT_IMPORT, "Coal"= OUT_COAL,"Cogen"=OUT_COGEN,
                   "Coal-to-Gas"=OUT_NGConv,"SCGT"=OUT_SCGT, "NGCC"=OUT_NGCC, 
                   "Hydro"=OUT_HYDRO, "Other"=OUT_OTHER, "Wind"=OUT_WIND, 
                   "Solar"=OUT_SOLAR, "Storage"=OUT_STORAGE)
      
      colours2 = c("Coal"= cOL_COAL, "Coal-to-Gas"=cOL_COal2Gas, "Cogen"=cOL_COGEN, 
                   "Natural Gas"=cOL_NGCC,"Hydro"=cOL_HYDRO, "Other"=cOL_OTHER, 
                   "Wind"=cOL_WIND, "Solar"=cOL_SOLAR, "Storage"=cOL_STORAGE)
      
      Outline2 = c("Coal"= OUT_COAL, "Coal-to-Gas"=OUT_COal2Gas, "Cogen"=OUT_COGEN, 
                   "Natural Gas"=OUT_NGCC, "Hydro"=OUT_HYDRO, "Other"=OUT_OTHER, 
                   "Wind"=OUT_WIND, "Solar"=OUT_SOLAR, "Storage"=OUT_STORAGE)
      
      colours3 = c("CCCT gas/oil"=cOL_NGCC, "SCCT"=cOL_SCGT,"Other"=cOL_OTHER,
                    "Wind"=cOL_WIND, "Solar"=cOL_SOLAR,"Storage"=cOL_STORAGE)

      
      colours4=c("Import"= cOL_IMPORT, "Coal"=cOL_COAL, "Coal-to-Gas"=cOL_COal2Gas,
                 "Cogen"=cOL_COGEN,"Natural Gas"=cOL_NGCC, 
                 "Hydro"=cOL_HYDRO, "Other"=cOL_OTHER, "Wind"=cOL_WIND, 
                 "Solar"=cOL_SOLAR, "Storage"=cOL_STORAGE)
      
      Outline4 = c("Import"= OUT_IMPORT, "Coal"=OUT_COAL, "Coal-to-Gas"=OUT_COal2Gas,
                   "Cogen"=OUT_COGEN,  "Natural Gas"=OUT_NGCC, 
                   "Hydro"=OUT_HYDRO, "Other"=OUT_OTHER, "Wind"=OUT_WIND, 
                   "Solar"=OUT_SOLAR, "Storage"=OUT_STORAGE)
      
      colours5 = c(cOL_COAL, cOL_COGEN, cOL_Gas, COL_Gas1, COL_Gas2,
                   cOL_HYDRO, cOL_SOLAR, cOL_WIND, cOL_STORAGE,cOL_OTHER)
      
      AESO_colours <- c("goldenrod1", "gray60", "yellowgreen", "cornflowerblue",
                        "#001933")
    }
}

  # Gives years to summarize info from 
  Years2Disp <- c(2021,2022,2023,2024,2025) # Years to show in figures
  Years2Pivot <- c(2021,2022,2023,2024,2025)  # Years to display in tables

  #For fun, make the code beep when its all done
  beep(3)
  
################################################################################
## SET UP FOR PLOTTING & CALL FUNCTIONS
  windows(12,8)
  
## THE MOST USEFULL FUNCTIONS
  # Gives stacked area chart for single week
  Week1(2022,01,08,BC)
  
  # Grid of weekly output
  year_weeks(2021,BC)
  
  # Yearly Output
  Evalyr(ResGroupYr,BC)
  
  # Yearly Capacity
  Evalcap(ResGroupYr,BC)
  
  # Yearly percentage of generation
  EvalPerc(ResGroupYr,BC)
  
  # Retiremtns by capacity (grouped by fuel type)
  RetireMW(BC)
  
  # Capacity built by Aurora over study period
  Build_A_MW(BC)
  
  # All new capacity
  BuildMW(BC)
  
  # Bar chart showing each resource groups yearly output
  Output_Comp(BC)
  
  # Shows Prices for simulation duration
  Sim_dur(BC)
  
  # Annual import and export from AB as a bar chart
  Imp_Exp1(BC)
  
  # Import and export for full year from AB
  Imp_Exp2(2021,BC)
  
  # Price and Trade monthly
  MN_Trade_Price(2021,02,BC)
      Trade_Mn_AESO(2022,02,HRcalc)
  
  #Imports and exports from BC and SK yearly totals
  BC_SK_IE(year,BC)
  
  # Month Compare Trade
  mn_Trade_Comp(2022,05,BC,HRcalc)

################################################################################  
## BUT THERE ARE MORE ...
  
## SIM FUNCTIONS (sim_eval_1)
{  #Gives stacked area chart for a single day, output (MWh vs Date), grouped by resource
    day1(2029,01,08,BC)
    
    # Gives stacked area chart for single week
    Week1(2021,01,08,BC)
    
    # Gives weekly storage function
    Stor1(2021,01,08,BC)
    
    # Average Pool Price for one week
    week_price(2022,10,08,BC)
    
    # Gives overall picture of Output over time period
    Eval(ResGroupMn,BC)
    Evalyr(ResGroupYr,BC)
    
    # Gives overall picture of capacity over time period
    Evalcap(ResGroupMn,BC)
    Evalcap(ResGroupYr,BC)
    
    # Gives all as % of total generation being met
    EvalPerc(ResGroupYr,BC)

    # Lets you get where units were built 
    Units(BC,wind)
    
    # Lets you get where units could have been built 
    Slack(BC,wind)
    
    #Shows Prices for simulation duration
    Sim_dur(BC)
}
## COMBINED SIM FUNCTIONS
    
    # Plot over 4 years defined in plotting section (2x2 grid)
    Week4(01,08,BAU)
    
    #Plot Pool price and output for a week 
    PrOt(2023,10,08,BAU)

    # New units, new capacity, and output for the duration
    EvalOut(ResGroupMn,BAU)
    
    #Compare available units and built units
    BuildUnits(BAU, wind)
    
    
## AESO FUNCTIONS
    #AESO Output
    Week_act(2020,01,08)
    
    #AESO Week Price
    wkPrice(2021,10,08)
    
    # AESO Week Price and output in one
    AESO_PrOt(2021,01,08)
    
##  AESO SIM COMPARE FUNCTIONS

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

## NET ZERO FUNCTIONS (Net_Zero_eval)
    
    # Number of units retired by fuel type
    Retirecol(BC)
    
    # Retiremtns by capacity (grouped by fuel type)
    RetireMW(BC)
    
    # Units Built over study period
    Builtcol(BC)
    
    # Capacity built by Aurora over study period
    Build_A_MW(BC)
    
    # All new capacity
    BuildMW(BC)
    
    # Bar chart showing each resource groups yearly output
    Output_Comp(BC)
    
    # Shows demand in AB 
    AnnualDemand(ZoneMn,BC)
    
    Imp_Exp2(BC)
    
## INTERTIE FUNCTIONS
    #Annual import and export from AB 
    Imp_Exp(BC)
    
    #Imports and exports from BC adn SK
    BC_SK_IE(BC)
    
## Table Functions
    Report_P(Years2Pivot,BC)
    
################################################################################
## THESE ARE JUST SOME WINDOW SIZES AND STUFF
      
    dev.off(dev.list()["RStudioGD"]) #Clears all plots
      
    windows(12,8)
    windows(14,8)
    windows(10,8)
    windows(18,12)
    
    windows(16,12)

######

    
 
    
    
    