################################################################################
# TITLE: Database_Loading
# DESCRIPTION: Load database from Microsoft SQL Server and import tables


# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# OTHER CONTRIBUTORS:
# CREATED: May 2022; LAST EDIT: June 14, 2022

################################################################################
##gc LOAD REQUIRED PACKAGES AND SOURCE FUNCTIONS
{
  # Packages
  library(odbc)          # Driver for Database Loading
  library(DBI)           # Package for interface between database and R
  library(ggplot2)       # Package for graphics and plot aesthetics
  library(dplyr)         # Data manipulation package
  library(ggplot2)       # Used for graphical packages and aestheticc
  library(tidyverse)     # Data science package
  library(lubridate)     # Allow time and data manipulation
  library(here)          # Package to set filepaths inside R project
  
  # Functions
  source("plot_functions.R")
}

################################################################################
## CONNECT TO MICROSOFT SQL SERVER
{
  #Input Database Name below:
  SourceDB<-"Comp_June13"
  
  #Connect to database specified (via server, user, and password)
  con <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = rstudioapi::askForPassword("Server"),
                   Database = SourceDB,
                   UID = rstudioapi::askForPassword("User Name"),
                   PWD = rstudioapi::askForPassword("Password"))
}

################################################################################
## DEFINE CASES TO STUDY (IE: RUN ID)
## This is only needed when running multiple cases in a simulation

AsIs <- "LTCE Shorter Run Time"
BC <- "Base Case"
NZC1 <- "Net Zero Case 1"

################################################################################
## READ TABLES FROM DATABASE INTO ENVIRONMENT
## Can edit to select required tables only
{
  # Fuel tables
#  FuelYr <- dbReadTable(con,'FuelYear1')
#  FuelMn <- dbReadTable(con,'FuelMonth1')
  
  # Resource Tables
  ResYr <- dbReadTable(con,'ResourceYear1')
#  ResMn <- dbReadTable(con,'ResourceMonth1')
  ResHr <- dbReadTable(con,'ResourceHour1')
  
  # Resource Group Tables
  ResGroupYr <- dbReadTable(con,'ResourceGroupYear1')
  ResGroupMn <- dbReadTable(con,'ResourceGroupMonth1')
  ResGroupHr <- dbReadTable(con,'ResourceGroupHour1')
  ResGroupEmYr <- dbReadTable(con,'ResourceGroupEmissionsYear1')
#  ResGroupEmSt <- dbReadTable(con,'ResourceGroupEmissionsStudy1')
  
  # Other Tables
  ResStackYr <- dbReadTable(con,'ResourceStackYear1')
  ResStackHr  <- dbReadTable(con,'ResourceStackHour1')
  Link <- dbReadTable(con,'LinkYear1')
#  CC <- dbReadTable(con,'CustomConstraint1')
#  ZoneYr <- dbReadTable(con,'ZoneYear1')
#  ZoneMn <- dbReadTable(con,'ZoneMonth1')
  ZoneHr <- dbReadTable(con,'ZoneHour1')
#  LTRes <- dbReadTable(con,'LTResValue1')
#  BuildReport <- dbReadTable(con,'LTBuildReport1')
#  Study <- dbReadTable(con,'StudyLog1')
  
  # Get rid of Unused R memory
  gc() 
}

################################################################################
## CHANGE DATA TYPE TO REFLECT PROPER TIME PERIODS
{
  # Fuel Tables
#  FuelYr$Time_Period  <- as.Date(as.character(FuelYr$Time_Period), 
#                                 format = "%Y")
#  FuelMn$Time_Period <- ym(FuelMn$Time_Period)
  
  # Resource Tables
  ResYr$Time_Period  <- as.Date(as.character(ResYr$Time_Period), 
                                format = "%Y")
#  ResMn$Time_Period <- ym(ResMn$Time_Period)
  ResHr$date <- as.POSIXct(as.character(ymd_h(gsub(" Hr ", "_",ResHr$Time_Period))), 
                           tz = "MST")-(60*60)

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
#  Link$Time_Period  <- as.Date(as.character(Link$Time_Period), 
#                               format = "%Y")
#  ZoneYr$Time_Period  <- as.Date(as.character(ZoneYr$Time_Period), 
#                                 format = "%Y")
#  ZoneMn$Time_Period <- ym(ZoneMn$Time_Period)
  ZoneHr$date <- as.POSIXct(as.character(ymd_h(gsub(" Hr ", "_",ZoneHr$Time_Period))), 
                              tz = "MST")-(60*60)
}

################################################################################
## FILTER TABLES TO GET RID OF COLUMNS I DONT CARE ABOUT & PULL OUT IMPORT/EXPORT
{ 
  {
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
    
    #Hourly For individual resources
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
  {
    # Select the Import/Export data
    Import <- ZoneHr_Avg %>%
      subset(., select = c(date, Imports, Run_ID)) %>%
      'colnames<-'(c("date", "Output_MWH", "Run_ID")) %>%
      add_column(ID = "Import")
    
    Export <- ZoneHr_Avg %>%
      subset(., select = c(date, Exports, Run_ID)) %>%
      'colnames<-'(c("date", "Output_MWH", "Run_ID")) %>%
      add_column(ID = "Export")
    
    #Fix Aurora sign convention
    Export$Output_MWH <- Export$Output_MWH * -1
    }
}  

################################################################################
## BRING IN OTHER DATA FROM AESO FILES & FORMAT
{
  # Load Leach Merit Data - Hourly resource info for Alberta (similar to ResHr and StackHr)
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

## FURTHER FORMAT AND MANIPULATE NRG DATA
{
    # Create a demand table. Summarise takes median of Demand, AIL, and Price for each time period 
    demand <- nrgstream_gen %>%
      group_by(time) %>%
      summarise(Demand = median(Demand), 
                Price = median(Price),
                AIL = median(AIL))
    
    #Take out dates I dont care about
    sub_samp<-filter(nrgstream_gen, time >= as.Date("2020-01-1"))
    rm(nrgstream_gen)

    # Create a list to describe Import/Exports
    trade_excl<-c("AB - WECC Imp Hr Avg MW", 
                  "AB - WECC Exp Hr Avg MW",
                  "AB - WECC Imp/Exp Hr Avg MW")
    
    # Create Dataframe, ony select rows where the Nat resource group is in the defined groups (ie trading), then grouped by plant type 
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
    
    {
      #Resource type list
      plant_types<-c("COAL","COGEN","HYDRO","NGCC", "OTHER", "SCGT","SOLAR","IMPORT","EXPORT","WIND")
    
      #Create a new dataframe with plant types only
      df1a <- df1 %>%
        filter(Plant_Type %in% plant_types,month(time)<05)
      
      # Put in desired order: Coal, Cogen, NGCC, SCGT, Other, Hydro, Wind, Solar, Import, Export
      df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "OTHER",after=Inf)
      df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "HYDRO",after=Inf)
      df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "WIND",after=Inf)
      df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "SOLAR",after=Inf)
      df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "IMPORT",after=Inf)
      df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "EXPORT",after=Inf)
    }
}
################################################################################
## SET UP FOR PLOTTING & CALL FUNCTIONS
  

source(here::here('Extra Code','sim_eval.R'))
source(here::here('Extra Code','aeso_eval.R'))
source(here::here('Extra Code','aeso_sim_comp_eval.R'))  
  
  
  