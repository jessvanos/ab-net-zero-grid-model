################################################################################
# TITLE: Database_Loading
# DESCRIPTION: Load database from Microsoft SQL Server and import tables


# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# OTHER CONTRIBUTORS:
# CREATED: May 2022; LAST EDIT: June 13, 2022

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
  SourceDB<-"AuroraNetZeroCase1_Jess_May30"
  
  #Connect to database specified (via server, user, and password)
  con <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = rstudioapi::askForPassword("Server"),
                   Database = SourceDB,
                   UID = rstudioapi::askForPassword("User Name"),
                   PWD = rstudioapi::askForPassword("Password"))
}

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
## PULL OUT REQUIRED COLUMNS AND TIME PERIODS

Hr$date <- as.POSIXct(as.character(ymd_h(gsub(" Hr ", "_",Hr$Time_Period))), 
                      tz = "MST")-(60*60)


################################################################################
## BRING IN OTHER DATA FROM AESO FILES

  # Load Leach Merit Data
  merit <- readRDS(here("Data Files","Leach_MeritData.RData"))
  
    #Filter Data to relevant dates & remove old data
    merit_filt <- filter(merit, 
                         date >= as.Date("2018-01-1"))
    rm(merit)
  
  # Load nrgstream_gen
  load(here("Data Files","nrgstream_gen.RData")) 
  
    # Rename the time
    nrgstream_gen <- nrgstream_gen %>% rename(time=Time)
    
    # Get rid of rows with blanks
    nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$gen),] 
    nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$time),] 
    
    # Organize the data
    demand <- nrgstream_gen %>%
      group_by(time) %>%
      summarise(Demand = median(Demand), 
                Price = median(Price),
                AIL = median(AIL))
    sub_samp<-filter(nrgstream_gen, time >= as.Date("2018-01-1"))
    rm(nrgstream_gen)

################################################################################
## SET UP FOR PLOTTING
  
  # Set limits for plots to be consistent
  ylimit <- max(Hr$Output_MWH) + max(ZoneHour$Imports)


  
  
  
  