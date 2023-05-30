################################################################################
# TITLE: Misc_Data_Visuals
#
# DESCRIPTION:  Plots AESO planning area data and more
#
#
# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: May 2023; LAST EDIT: May 30, 2023
#
# NOTES: Make sure the project file is open first or "here" commands wont work right.
#        Before running, create folder called "Data Files" inside project directory and populate it with 
#        any data you want to access. 
#        Once this file is run through completion, can call any functions with environment that is loaded 
#        (ie: you do not need to run it all again).
################################################################################

#AB_Planning_Plotting
################################################################################
## LOAD REQUIRED PACKAGES AND SOURCE FUNCTIONS
################################################################################
library(here)
source(here('Functions','Other_Functions.R'))       # Other functions used in plotting functions

# Packages required
packs_to_load = c("tidyverse","ggplot2","scales","grid","gtable","gridExtra","odbc","ggpubr",
                  "DBI","lubridate","cowplot","scales","dplyr","reshape2","zoo",
                  "ggpattern","here","beepr","showtext","DescTools","pivottabler",
                  "openxlsx","sqldf","timeDate","writexl","viridis","ggnewscale","sf","broom")

# Function to check for packages, install if not present, and load
packs_check(packs_to_load)

################################################################################
## BRING IN DATA
################################################################################
# Read Map Data from AESO 
# Data source:https://www.aeso.ca/market/market-and-system-reporting/data-requests/planning-area-boundary-data/
PlanningMapData<-st_read(dsn= here("Data Files","AESO_Planning_Areas.shp"))

# Data from excel to merge with rest
# Data Source: https://www.aeso.ca/market/market-and-system-reporting/long-term-adequacy-metrics/
    # Save new data
    # saveRDS(May_2023_AESO_planning_analytics,file=here("Data Files","May_2023_AESO_Planning_Data.RData"))

# Read in the saved data
May2023Projects<-readRDS(here("Data Files","May_2023_AESO_Planning_Data.RData"))%>%
  subset(., select = c("Area_ID","Solar_Capacity","Wind_Capacity","Storage_Capacity","Gas_Capacity"))

# Merge together
PlanMapDataCombined<-merge(PlanningMapData,May2023Projects,by="Area_ID")

################################################################################
## PLOT IT
################################################################################
# Create folder name to save as
  CaseName <- "May 2023 Generation Projects"
  
# Make Window
# Save all full size images
  windows(10,12,buffered=FALSE)

# Plot Solar
  ggplot(data=PlanMapDataCombined) +
    geom_sf(mapping = aes(fill = Solar_Capacity),color="black") +
    theme_bw() +
    theme(axis.line = element_blank(),
          axis.ticks =  element_blank(),
          axis.text = element_blank(),
          axis.title=element_blank(),
          plot.caption =element_text(face = "italic",color="grey50",size=6,hjust = 1.35),
          plot.title=element_text(size=12,hjust=0.5),
          plot.subtitle=element_text(size=8,hjust=0.5),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.text=element_text(size=8,format(1000000, big.mark = ",", scientific = FALSE)),
          legend.title = element_text(size=10),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = 'transparent')) +
    
    scale_fill_gradient(low="white", high="#7D5E02")+
    
    labs(fill="Capacity (MW)",
         title="Alberta Solar Generation Projects at Various Stages of Development",
         subtitle="Includes projects classified as under construction, recieved AUC approval, and announced",
         caption ="Figure by Jessica Van Os, Data from AESO LTA report and connection project list (May 2023)") +
    
    geom_sf_text (aes(label = Area_ID),size=2,color="black")

  SaveIm_Loc(CaseName,"May 2023 Solar Projects")
  
# Plot Wind
  ggplot(data=PlanMapDataCombined) +
    geom_sf(mapping = aes(fill = Wind_Capacity),color="black") +
    theme_bw() +
    theme(axis.line = element_blank(),
          axis.ticks =  element_blank(),
          axis.text = element_blank(),
          axis.title=element_blank(),
          plot.caption =element_text(face = "italic",color="grey50",size=6,hjust = 1.35),
          plot.title=element_text(size=12,hjust=0.5),
          plot.subtitle=element_text(size=8,hjust=0.5),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.text=element_text(size=8,format(1000000, big.mark = ",", scientific = FALSE)),
          legend.title = element_text(size=10),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = 'transparent')) +
    
    scale_fill_gradient(low="white", high="#277E3E")+
    
    labs(fill="Capacity (MW)",
         title="Alberta Wind Generation Projects at Various Stages of Development",
         subtitle="Includes projects classified as under construction, recieved AUC approval, and announced",
         caption ="Figure by Jessica Van Os, Data from AESO LTA report and connection project list (May 2023)") +
    
    geom_sf_text (aes(label = Area_ID),size=2,color="black")

  SaveIm_Loc(CaseName,"May 2023 Wind Projects")
  
  
# Plot Storage
  ggplot(data=PlanMapDataCombined) +
    geom_sf(mapping = aes(fill = Storage_Capacity),color="black") +
    theme_bw() +
    theme(axis.line = element_blank(),
          axis.ticks =  element_blank(),
          axis.text = element_blank(),
          axis.title=element_blank(),
          plot.caption =element_text(face = "italic",color="grey50",size=6,hjust = 1.35),
          plot.title=element_text(size=12,hjust=0.5),
          plot.subtitle=element_text(size=8,hjust=0.5),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.text=element_text(size=8,format(1000000, big.mark = ",", scientific = FALSE)),
          legend.title = element_text(size=10),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = 'transparent')) +
    
    scale_fill_gradient(low="white", high="#C05200")+
    
    labs(fill="Capacity (MW)",
         title="Alberta Storage Generation Projects at Various Stages of Development",
         subtitle="Includes projects classified as under construction, recieved AUC approval, and announced",
         caption ="Figure by Jessica Van Os, Data from AESO LTA report and connection project list (May 2023)")+
    
    geom_sf_text (aes(label = Area_ID),size=2,color="black")

  SaveIm_Loc(CaseName,"May 2023 Storage Projects")
  
  
# Plot Gas
  ggplot(data=PlanMapDataCombined) +
    geom_sf(mapping = aes(fill = Gas_Capacity),color="black") +
    theme_bw() +
    theme(axis.line = element_blank(),
          axis.ticks =  element_blank(),
          axis.text = element_blank(),
          axis.title=element_blank(),
          plot.caption =element_text(face = "italic",color="grey50",size=6,hjust = 1.35),
          plot.title=element_text(size=12,hjust=0.5),
          plot.subtitle=element_text(size=8,hjust=0.5),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.text=element_text(size=8,format(1000000, big.mark = ",", scientific = FALSE)),
          legend.title = element_text(size=10),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = 'transparent')) +
    
    scale_fill_gradient(low="white", high="#595959")+
    
    labs(fill="Capacity (MW)",
         title="Alberta Gas Generation Projects at Various Stages of Development",
         subtitle="Includes projects classified as under construction, recieved AUC approval, and announced
                   Encompasses coal-to-gas transitions, cogeneration, and other gas generation",
         caption ="Figure by Jessica Van Os, Data from AESO LTA report and connection project list (May 2023)") +
    
    geom_sf_text (aes(label = Area_ID),size=2,color="black")

  SaveIm_Loc(CaseName,"May 2023 Gas Projects")
  