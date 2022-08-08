################################################################################
# TITLE: AESO_Analysis
# DESCRIPTION:  Script imports data and analyses intertie behavior


# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: August 2022; LAST EDIT: August 4, 2022

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
  
  # Packages required
  packs_to_load = c("tidyverse","ggplot2","grid","gtable","gridExtra","odbc","ggpubr",
                    "DBI","lubridate","cowplot","scales","dplyr","reshape2","zoo",
                    "ggpattern","here","beepr","showtext","DescTools","pivottabler",
                    "openxlsx")
  # Function to check for packages, install if not present, and load
  packs_check(packs_to_load)
  
}

################################################################################
## LOAD FROM EXCELL SHEET AND WRITE TO .R FILE
  # ImpExp <- read_csv("Hourly_Metered_Volumes_and_Pool_Price_and_AIL.csv")
  # saveRDS(ImpExp, file = "AESO_IMP_EXP.RData")

################################################################################
## LOAD FROM >R FILE INTO WORKSPACE
  Imp_Exp <- readRDS(here("Data Files","AESO_IMP_EXP.RData"))
  
  #Replease all NA values with zero
  Imp_Exp[is.na(Imp_Exp)] <- 0

################################################################################
## SEASONAL DATA
  IE_Winter <- Imp_Exp %>%
    filter(Season=="Winter")%>%
    select(.,-c("Date_Begin_GMT","DAY_AHEAD_POOL_PRICE"))
  
  IE_Spring <- Imp_Exp %>%
    filter(Season=="Spring")%>%
    select(.,-c("Date_Begin_GMT","DAY_AHEAD_POOL_PRICE"))
  
  IE_Summer <- Imp_Exp %>%
    filter(Season=="Summer")%>%
    select(.,-c("Date_Begin_GMT","DAY_AHEAD_POOL_PRICE"))
  
  IE_Fall <- Imp_Exp %>%
    filter(Season=="Fall")%>%
    select(.,-c("Date_Begin_GMT","DAY_AHEAD_POOL_PRICE"))
  
  
  
################################################################################
## MONTHLY DATA
  
  
################################################################################
## PRICE BASED DATA


################################################################################
## PLOT ACTUAL DURATION CURVES
  tot <- Imp_Exp%>%
    group_by(Year)%>%
    mutate(perc = 1-ecdf(ACTUAL_POOL_PRICE)(ACTUAL_POOL_PRICE))
  
  tot$Year <- as.factor(tot$Year)
  
  tot <- tot %>%
    filter(Year %in% c(2012,2014,2016,2018,2020,2022))
  
  ggplot() +
    geom_line(data = tot, 
              aes(x = perc, y = ACTUAL_POOL_PRICE, colour = Year), size = 1) +
    
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),
          panel.spacing = unit(2, "lines"),
          axis.title.x = element_text(size = 15,face="bold"),
          axis.title.y = element_text(size = 15,face="bold"),
          text = element_text(size = 15),
          legend.title = element_blank(),
          legend.position = "right",
          panel.grid.major.y = element_line(size=0.25,linetype=5,color = "gray70")) +
    
    labs(y = "Hourly Pool Price ($/MWh)", x = "Percentage of Time",colour = "Year",
         caption = "Hourly Metered Volumes and Pool Price and AIL data 2010 to 2022") +
    
    scale_color_brewer(palette= "Dark2") +
    
    scale_x_continuous(expand=c(0,0), 
                       limits = c(0,1),
                       labels = percent) +
    
    scale_y_continuous(expand=c(0,0),limits = c(0,1000),breaks = pretty_breaks(5))  




