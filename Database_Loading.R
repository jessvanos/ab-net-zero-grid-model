################################################################################
# TITLE: Database_Loading
# DESCRIPTION: Load database from Microsoft SQL Server and import tables


# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# OTHER CONTRIBUTORS:
# CREATED: May 2022; LAST EDIT: June 13, 2022

################################################################################
# LOAD & ATTACH REQUIRED PACKAGES
{
  library(odbc)          # Driver for Database Loading
  library(DBI)           # Package for interface between database and R
  library(ggplot2)       # Package for graphics and plot aesthetics
  library(here)          # Package to set filepaths inside R project
}

################################################################################
# READ TABLES FROM DATABASE INTO ENVIRONMENT
# Can edit to select required tables only
{
  # Fuel tables
  FuelYr <- dbReadTable(con,'FuelYear1')
  FuelMn <- dbReadTable(con,'FuelMonth1')
  
  # Resource Tables
  ResYr <- dbReadTable(con,'ResourceYear1')
  ResMn <- dbReadTable(con,'ResourceMonth1')
  ResHr <- dbReadTable(con,'ResourceHour1')
  
  # Resource Group Tables
  ResGroupYr <- dbReadTable(con,'ResourceGroupYear1')
  ResGroupMn <- dbReadTable(con,'ResourceGroupMonth1')
  ResGroupHr <- dbReadTable(con,'ResourceGroupHour1')
  ResGroupEmYr <- dbReadTable(con,'ResourceGroupEmissionsYear1')
  ResGroupEmSt <- dbReadTable(con,'ResourceGroupEmissionsStudy1')
  
  # Other Tables
  ResStackYr <- dbReadTable(con,'ResourceStackYear1')
  ResStackHr  <- dbReadTable(con,'ResourceStackHour1')
  Link <- dbReadTable(con,'LinkYear1')
#  CC <- dbReadTable(con,'CustomConstraint1')
  ZoneYr <- dbReadTable(con,'ZoneYear1')
  ZoneMn <- dbReadTable(con,'ZoneMonth1')
  ZoneHr <- dbReadTable(con,'ZoneHour1')
#  LTRes <- dbReadTable(con,'LTResValue1')
#  BuildReport <- dbReadTable(con,'LTBuildReport1')
#  Study <- dbReadTable(con,'StudyLog1')
  
  # Get rid of Unused R memory
  gc() 
}
################################################################################
# BRING IN OTHER DATA FROM FILES

# Load Leach Merit Data
merit <- readRDS(here("Data Files","Leach_MeritData.RData"))

  #Filter Data to relevant dates & remove old
  merit_filt <- filter(merit, 
                       date >= as.Date("2020-01-1"))
  rm(merit)

# Load nrgstream_gen
load(here("Data Files","nrgstream_gen.RData")) 

  # Rename the time
  nrgstream_gen <- nrgstream_gen %>% rename(time=Time)
  
  # Get rid of blanks
  nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$gen),] 
  nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$time),] 
  
  # Organize the data
  demand <- nrgstream_gen %>%
    group_by(time) %>%
    summarise(Demand = median(Demand), 
              Price = median(Price),
              AIL = median(AIL))
  sub_samp<-filter(nrgstream_gen, time >= as.Date("2017-01-1"))
  rm(nrgstream_gen)


