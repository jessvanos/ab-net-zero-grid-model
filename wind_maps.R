################################################################################
# TITLE: wind_maps
# DESCRIPTION: Create maps showing simulation wind farm locations
#
# AUTHOR: Jessica Van Os (adapted from Taylor Pawlenchuk, 2022)
# CONTACT: jvanos@ualberta.ca
# CREATED: March 2024
#
# NOTES: Make sure the project file is open first or "here" commands wont work right.
#        Before running, create folder called "Data Files" inside project directory and populate it with 
#        any data you want to access. 
################################################################################

################################################################################
## LOAD REQUIRED PACKAGES AND SOURCE FUNCTIONS
################################################################################
{ # Must load the here package in order to make sure internal project directories work
  library(here)
  
  # Import functions from other R files, take from the functions folder in R project
  source(here('Functions','Other_Functions.R'))                # Other functions used in plotting functions
  source(here('Functions','Group_PlotSave.R'))                 # Saving functions import
  
  
  # Packages required
  packs_to_load = c("terra","sf","ggplot2","ggpubr","cowplot","tidyverse","dplyr",
                    "lubridate","readxl","colorRamps","scales",
                    "ggmap","sp","geodata",'ggspatial','dplyr','nasapower')
  # Function to check for packages, install if not present, and load
  packs_check(packs_to_load)
  
}

################################################################################
# LOAD DATA
# Wind Speeds: Canada Wind Atlas 
# http://www.windatlas.ca/nav-en.php?no=46&field=EU&height=80&season=ANU
#
# Wind farm names and locations: input excel
################################################################################
  wind_profile <- readRDS(here("Data Files","Wind Data","WindAtlas_Data00_0.05"))
  colnames(wind_profile) <- c('Latitude', 'Longitude', 'Wind')
  
  wind_Aurora <- read_excel(here("Data Files","Wind Data","Aurora_Wind.xlsx"))
  
  # New options
  new_prj <- wind_Aurora %>%
    filter(Status %in% c("Potential","NR_queue","NR_built"))
  
  # Existing and Construction
  existing_prj = wind_Aurora %>%
    filter(Status %in% c("Active","Queue"))

################################################################################
# GET MAP
################################################################################

  # Map or world bounds.Level 1 shows provincial bounds. Level 2 is electoral bounds
  can_level1 = geodata::gadm("Canada", path=here("Data Files","Wind Data"),level = 1, version="latest")
  can_level2 = geodata::gadm("Canada", path=here("Data Files","Wind Data"),level = 2, version="latest")
  #alberta_level1 = can_level1[which(can_level1$NAME_1 == "Alberta"),]
  
  # Stretch to to more realistic shape
  WGS84 <- rast(crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  canada_level1_ellipsoid = project(can_level1, WGS84)
  canada_level2_ellipsoid = project(can_level2, WGS84)
  
  # Get AB!
  alberta_ellip1 = canada_level1_ellipsoid[which(canada_level1_ellipsoid$NAME_1 == "Alberta"),]
  AB_sf1 = st_as_sf(alberta_ellip1)
  alberta_ellip2 = canada_level2_ellipsoid[which(canada_level2_ellipsoid$NAME_1 == "Alberta"),]
  AB_sf2 = st_as_sf(alberta_ellip2)
  
  # Coordinated from object
  alberta_coordinates1 = data.frame(st_coordinates(AB_sf1)[,1:2])

################################################################################
# WIND SPEED MAP
################################################################################

  ggplot() + 
    geom_raster(data = wind_profile, 
                aes(x = Longitude, y = Latitude, fill = Wind)) +
    geom_sf(data = AB_sf1, 
                 aes(group = NAME_1), 
                 fill = "transparent", colour = "black") +
    # https://colorspace.r-forge.r-project.org/reference/rainbow_hcl.html
    scale_fill_gradientn(colours = colorspace::rainbow_hcl(100),
                         #colours = colorspace::diverging_hcl(100,"Blue-Yellow 2"),
                         #colours = matlab.like(100),
                         limits=c(2,10),oob=squish, 
                         breaks=seq(2,10,by=2),
                         name = "Mean annual\nwind speed\nat 80m height \n(m/s)") +
    
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          #legend.key.height = unit(2,'cm'),
          legend.box.background = element_rect(fill = "transparent", color = "transparent"),
          legend.text = element_text(),
          legend.title = element_text()) 

################################################################################
# ACTIVE FARMS
################################################################################

# Filter the data
wind_active <- existing_prj %>%
    mutate(Type = if_else(Status == "Queue","Expected",
                          if_else(Status == "Active",Status,"Other")))

# Plot
Exist_WindMap<-  ggplot()+
  geom_raster(data = wind_profile, 
              aes(x = Longitude, y = Latitude, fill = Wind)) +
  geom_sf(data = AB_sf1, 
          aes(group = NAME_1), 
          fill = "transparent", colour = "black") +
  scale_fill_gradientn(colors =  colorspace::diverging_hcl(100,"Blue-red 2"),
                       limits=c(2.5,10.5),oob=squish, 
                       breaks=seq(2,10,by=2),
                       name = "Mean annual\nwind speed \nat 80m height \n(m/s)"
  ) +
  geom_point(data = wind_active,
             aes(x= Longitude, y = Latitude, size = Capacity, color = Type), 
             shape = 16) +#, color = "black") +
  geom_point(data = wind_active,
             aes(x= Longitude, y = Latitude, size = Capacity),colour="black",
             shape=1) + 
  scale_color_manual("Installation Date",
                     values = c("Expected"='#e6e6e6',"Active"= '#767171')) +
  guides(color = guide_legend(override.aes = list(size = 5), order = 1),
         size = guide_legend(order = 2),
         #fill = guide_legend(keyheight = unit(0.5,'cm'),
         #                    reverse = TRUE)
  ) +
  ggtitle("Current Wind Farms") +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(size=16, hjust = 0.5, vjust=-5),
        #legend.key.height = (unit(2,'cm')),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        legend.background = element_rect(fill = "transparent"),
        legend.key=element_rect(fill = "transparent"),
        rect = element_rect(fill="transparent")) 

################################################################################
# AURORA OPTIONS
################################################################################

  # Filter for included only
wind_new <- new_prj %>%
    filter(Included == "Yes") %>%
    mutate(Type = if_else(Status == "Potential",Status,
                          if_else(Status == "NR_built","Based on Existing",
                                  if_else(Status == "NR_queue","Based on Queue","Other"))),
           `Max Capacity` = Capacity_NR*`Overall Max`) 
  
  # Plot
Aurora_WindMap <-    ggplot()+
    geom_raster(data = wind_profile, 
                aes(x = Longitude, y = Latitude, fill = Wind)) +
    geom_sf(data = AB_sf1, 
            aes(group = NAME_1), 
            fill = "transparent", colour = "black") +
    scale_fill_gradientn(colors =  colorspace::diverging_hcl(100,"Blue-red 2"),
                         limits=c(2.5,10.5), 
                         breaks=seq(2,10,by=2),
                         name = "Mean annual\nwind speed \nat 80m \n(m/s)"
    ) +
    geom_point(data = wind_new,
               aes(x= Longitude, y = Latitude, size = `Max Capacity`, color = Type), 
               shape = 16) +
    geom_point(data = wind_new,
               aes(x= Longitude, y = Latitude, size = `Max Capacity`),colour="black",
               shape=1) + 
    scale_color_manual("New Plant Source",
                       values = c("Potential"="darkgoldenrod2",
                                  "Based on Existing"='#767171',
                                  "Based on Queue"='#e6e6e6')
                                  ) +
    
    scale_size(breaks=c(1600,2000,4000),range=c(3,6)) +
    
    guides(color = guide_legend(override.aes = list(size = 5), order = 1),
           size = guide_legend(order = 2)
    ) +
    ggtitle("Model New Wind Options") +
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          plot.title = element_text(hjust = 0.5, vjust=-5,size=16),
          #legend.key.height = (unit(2,'cm')),
          legend.text = element_text(size=14),
          legend.title = element_text(size=14),
          legend.background = element_rect(fill = "transparent"),
          legend.key=element_rect(fill = "transparent"),
          rect = element_rect(fill="transparent")) 

################################################################################
# AURORA BUILDS
################################################################################

# List of farms built




################################################################################
# SAVE EM!
################################################################################
GGSave_Loc_custom("Wind Maps","New Wind Options",Aurora_WindMap,8,12)
GGSave_Loc_custom("Wind Maps","Existing Wind",Exist_WindMap,8,12)

################################################################################
# SOLAR DATA (not done)
################################################################################

  # Solar_data = data.frame(matrix(ncol = 3, nrow = nrow(alberta_coordinates1)))%>%
  #   rename(Lat=X1,Long=X1,SR=X3)
  # 
  #  for(i in nrow(Solar_data)){
  #   Solar_data[i,1]=alberta_coordinates1[i,1]
  #   Solar_data[i,2]=alberta_coordinates1[i,2]
  #   Solar_data[i,3]=nasapower::get_power(community = "re", pars = "ALLSKY_SFC_SW_DWN",lonlat = c(alberta_coordinates1[1,1], alberta_coordinates1[1,2]), temporal_api  = "climatology")[16]
  # }






























