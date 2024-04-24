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
                    "lubridate","readxl","colorRamps","scales",'raster',
                    "ggmap","sp","geodata",'ggspatial','dplyr','nasapower','grid','gridExtra')
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

  # Add major cities to map
  city_data <- data.frame(
    city = c("Edmonton","Calgary","Red Deer","Lethbridge",
             "Grand Prairie","Fort McMurray","Pincher Creek","Medicine Hat"),
    lon = c(-113.295776,-113.827866,-113.8115,-112.8451,-118.7885,-111.3790,-113.9440,-110.7032),
    lat = c(53.541252,	51.046898,52.2690,49.6956,55.1707,56.7266,49.4849,50.0290)
  )
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
  alberta_coordinates2 = data.frame(st_coordinates(AB_sf2)[,1:2])

################################################################################
# WIND SPEED MAP
################################################################################

  wind_map <- ggplot() + 
    geom_raster(data = wind_profile, 
                aes(x = Longitude, y = Latitude, fill = Wind)) +
    geom_sf(data = AB_sf1, 
                 aes(group = NAME_1), 
                 fill = "transparent", colour = "black") +
    # https://colorspace.r-forge.r-project.org/reference/rainbow_hcl.html
    scale_fill_gradientn(#colours = colorspace::rainbow_hcl(100),
                         #colours = colorspace::diverging_hcl(100,"Cork"),
                         #colours = colorspace::sequential_hcl(100,"Mako"),
                         colours = matlab.like(100),
                         # colors =  colorspace::diverging_hcl(100,"Blue-yellow 2"),
                         limits=c(3,11),oob=squish, 
                         breaks=c(3,5,7,9,11),
                         labels=c("<3"," 5"," 7"," 9","<11"),
                         name = "Wind speed\nat 80m height \n(m/s)") +
    geom_point(data = city_data, aes(x = lon, y = lat), color = "black", size = 0.5) +
    geom_text(data = city_data, aes(x = lon, y = lat, label = city), vjust = -0.5,size=2) +
    
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
          plot.title = element_text(hjust=0.5),
          legend.title = element_text(hjust=0.5)) +
    labs(title="Mean Wind Speed")

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
  scale_fill_gradientn(colors =  colorspace::diverging_hcl(100,"Blue-yellow 2"),
                       limits=c(2.5,10.5),oob=squish, 
                       breaks=seq(2,10,by=2),
                       name = "Mean annual\nwind speed \nat 80m height \n(m/s)"
  ) +
  geom_point(data = city_data, aes(x = lon, y = lat), color = "black", size = 0.5) +
  geom_text(data = city_data, aes(x = lon, y = lat, label = city), vjust = -0.5,size=2) +
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
    mutate(Type = if_else(Status == "Potential","Potential Site",
                          if_else(Status == "NR_built","Based on Existing",
                                  if_else(Status == "NR_queue","Based on Queue","Other"))),
           `Max Capacity` = Capacity_NR*`Overall Max`) 
  
  wind_txt <- 14

  # Plot
Aurora_WindMap <- ggplot() +
    geom_raster(data = wind_profile, 
                aes(x = Longitude, y = Latitude, fill = Wind)) +
    geom_sf(data = AB_sf1, 
            #linewidth=1,
            size=1,
            aes(group = NAME_1), 
            fill = "transparent", colour = "black") +
    scale_fill_gradientn( #colors =  colorspace::diverging_hcl(100,"Blue-yellow 2"),
                          colours = colorspace::diverging_hcl(100,"Cork"),
                          #colours = matlab.like(100),
                          limits=c(3,11),oob=squish, 
                          breaks=c(3,5,7,9,11),
                          labels=c("<3"," 5"," 7"," 9","<11"),
                         # colors =  colorspace::diverging_hcl(100,"Blue-red 2"),
                         # limits=c(2.5,10.5), 
                         # breaks=seq(2,10,by=2),
                         name = "Mean annual speed at 80m (m/s)"
    ) +
    geom_point(data = city_data, aes(x = lon, y = lat), color = "black", size = 0.5) +
    geom_text(data = city_data, aes(x = lon, y = lat, label = city), vjust = -0.5,size=3) +
  
    geom_point(data = wind_new,
               aes(x= Longitude, y = Latitude, size = `Max Capacity`, color = Type), 
               shape = 16) +
    geom_point(data = wind_new,
               aes(x= Longitude, y = Latitude, size = `Max Capacity`),colour="black",
               shape=1) + 
    scale_color_manual("New Plant Source",
                       values = c("Potential Site"='#262626',
                                  "Based on Existing"='#6e6e6e',
                                  "Based on Queue"="#c6c6c6")
                                  ) +
    
    scale_size(breaks=c(1600,2000,4000),range=c(4,7)) +
    
    guides(color = guide_legend(override.aes = list(size = 5), order = 1),
           size = guide_legend(order = 2)
    ) +
    #ggtitle("Model New Wind Options") +
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          plot.title = element_text(hjust = 0.5, vjust=-5,size=16),
          #legend.key.height = (unit(2,'cm')),
          legend.text = element_text(size=wind_txt),
          legend.title = element_text(size=wind_txt),
          legend.background = element_rect(fill = "transparent"),
          legend.key=element_rect(fill = "transparent"),
          rect = element_rect(fill="transparent")) 

################################################################################
# AURORA OPTIONS WITH EXISTING
################################################################################

# Filter for included only
wind_new_exist <- wind_Aurora %>%
  rename(Capacity_plot=Capacity_NR)%>%
  mutate(Included = replace_na(Included,"Exists"),
         Capacity_plot = if_else(is.na(Capacity_plot),Capacity,Capacity_plot))%>%
  filter(Included != "No",
         Status != "Active_nonAESO") %>%
  mutate(Type = if_else(Status == "Potential","New Potential",
                        if_else(Status == "NR_built","New Existing",
                                if_else(Status == "NR_queue","New Queue",
                                        if_else(Status == "Active","Existing",
                                                if_else(Status == "Queue","Upcoming Exogenous","Other"))))))

wind_new_exist$Type <- factor(wind_new_exist$Type,levels=c("Existing","New Existing","New Queue","New Potential","Upcoming Exogenous"))

wind_new_exist <- wind_new_exist%>%
  arrange(Type)


wind_txt <- 14

# Plot
All_WindMap <- ggplot() +
  geom_raster(data = wind_profile,
              aes(x = Longitude, y = Latitude, fill = Wind),alpha=0.5) +

  scale_fill_gradientn(
    colours = alpha(matlab.like(100),alpha=0.5),
    limits=c(3,11),oob=squish,
    breaks=c(3,5,7,9,11),
    labels=c("<3"," 5"," 7"," 9","<11"),
    name = "Mean annual speed at 80m (m/s)") +
  
  geom_sf(data = AB_sf1,
          linewidth=1,
          size=1,
          aes(group = NAME_1),
          fill = "transparent", colour = "black") +
  geom_point(data = city_data, aes(x = lon, y = lat), color = "black", size = 0.5) +
  geom_text(data = city_data, aes(x = lon, y = lat, label = city), vjust = -0.5,size=2.5) +
  
  
  geom_point(data = wind_new_exist,
             aes(x = Longitude, y = Latitude, color = Type, shape = Type,size=Type), 
             #size = 3
             ) +
  
  scale_color_manual("Wind Plants",
                     values = c("New Potential"='#203764',
                                "New Existing"='#4472C4',
                                "New Queue"='#8EA9DB',
                                "Existing"='#252323',
                                "Upcoming Exogenous"="#767171")  ) +

  scale_shape_manual("Wind Plants",
                     values = c("New Potential"=16,
                                "New Existing"=16,
                                "New Queue"=16,
                                "Existing"=18,
                                "Upcoming Exogenous"=18)) +
  
  scale_size_manual("Wind Plants",
                     values = c("New Potential"=4,
                                "New Existing"=4,
                                "New Queue"=4,
                                "Existing"=3,
                                "Upcoming Exogenous"=3)) +
  

  theme(panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_blank(),
        #legend.key.height = (unit(2,'cm')),
        legend.text = element_text(size=wind_txt),
        legend.title = element_text(size=wind_txt),
        legend.background = element_rect(fill = "transparent"),
        legend.key=element_rect(fill = "transparent"),
        rect = element_rect(fill="transparent")) 


################################################################################
# AURORA BUILDS
################################################################################

# List of farms built

################################################################################
# SOLAR DATA (not done)
#   SOURCE: https://open.alberta.ca/opendata/gda-dde6ad60-dd08-4f21-bbda-7934c8cbdf1f
#   1971-2000 solar solar radiation, in megajoules per square metre (MJ/m2)
#
#   Original Coordinate System: raster::projection(solar_raster)
################################################################################

  # Read the downloaded TIF file
  solar_raster <- lapply(here("Data Files","Alberta Data","TIF","Annual Solar Radiation 1971-2000.tif"), raster)[[1]]
  
  # Apply new coordinate system
  solar_raster2 = projectRaster(solar_raster,crs=WGS84)
  
  # Convert to data frame
  solar_df <- as.data.frame(solar_raster2, xy = TRUE) %>%
    rename(Ann_Rad = "Annual.Solar.Radiation.1971.2000") %>%
    filter(!is.na(Ann_Rad))

  # Plot with ggplot2
  solar_map <-ggplot() + 
    geom_raster(data = solar_df, 
                aes(x = x, y = y, fill = Ann_Rad)) +
    geom_sf(data = AB_sf1, 
            aes(group = NAME_1), 
            fill = "transparent", colour = "black") +
    # https://colorspace.r-forge.r-project.org/reference/rainbow_hcl.html
    scale_fill_gradientn(#colours = matlab.like(100),
                         #colours = colorspace::diverging_hcl(100,"Vik"),
                         colours = colorspace::sequential_hcl(100,"Lajolla"),
                         limits=c(3500,5500),oob=squish, 
                         breaks=c(3500,4000,4500,5000,5500),
                         name = expression(atop("Solar Radiation",paste("(MJ/m"^2,")"))),
                         labels=c("<3500"," 4000"," 4500"," 5000","<5500")) +
    
    geom_point(data = city_data, aes(x = lon, y = lat), color = "black", size = 0.5) +
    geom_text(data = city_data, aes(x = lon, y = lat, label = city), vjust = -0.5,size=2) +
  
    
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
          plot.title = element_text(hjust=0.5),
          legend.title.align = 0,
          legend.title = element_text()) +
    labs(title="Annual Solar Radiation")
  
################################################################################
# SAVE EM!
################################################################################

  combine_map <- plot_grid(wind_map,solar_map, ncol=2, align="hv", axis = "t", rel_heights = c(1,1))
  
  plot(combine_map)
  
  GGSave_Loc_custom("Wind Maps","Wind and Solar Resource4",combine_map,12,14)
  
  GGSave_Loc_custom("Wind Maps","New Wind Options Update",Aurora_WindMap,8,12)
  
  GGSave_Loc_custom("Wind Maps","Existing Wind",Exist_WindMap,8,12)
  
  GGSave_Loc_custom("Wind Maps","All Wind",All_WindMap,8,12)
  
  
  
  
  
  
  
  
  
