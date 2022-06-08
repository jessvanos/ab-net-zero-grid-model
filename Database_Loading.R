################################################################################
# TITLE: Database_Loading
# DESCRIPTION: Load database from Microsoft SQL Server and import tables


# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# OTHER CONTRIBUTORS:
# LAST EDIT: June 2022


################################################################################
# LOAD & ATTACH REQUIRED PACKAGES

library(odbc)          # Driver for Database Loading
library(DBI)           # Package for interface between database and R
library(ggplot2)       # Package for graphics and plot aesthetics

################################################################################
# CONNECT TO MICROSOFT SQL SERVER

#Input Database Name below:
SourceDB<-"AuroraNetZeroCase1_Jess_May30"

#Connect to database specified (via server, user, and password)
con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = rstudioapi::askForPassword("Server"),
                 Database = SourceDB,
                 UID = rstudioapi::askForPassword("User Name"),
                 PWD = rstudioapi::askForPassword("Password"))

################################################################################
# READ IN TABLES FROM DATABASE

Link <- dbReadTable(con,'LinkStudy1')

