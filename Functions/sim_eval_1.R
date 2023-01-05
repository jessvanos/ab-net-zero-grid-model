################################################################################
# TITLE: sim_eval_1
# DESCRIPTION: Functions To use for plotting and evaluating simulation data.

# ORIGINAL AUTHOR: Taylor Pawlenchuk (Retrieved June 3, 2022)
# EDITS & ADDITIONAL CONTENT: Jessica Van Os
# LAST EDIT: June 17, 2022
#
################################################################################
#
#
# TO USE IN PLOTING FUNCTIONS SECTION
#
#
################################################################################

################################################################################
## FUNCTION: HrTime
## Convert the date and select a subset for one day from the data pulled in
################################################################################
  
{ HrTime <- function(data, year, month, day) {
    subset(data,
          (date >= paste(year,"-", month, "-", day," 00:00:00", sep = "") & 
              date <= 
              paste(year,"-", month, "-", (day)," 24:00:00", sep = "")))  }
}

################################################################################
## FUNCTION: WkTime
## Convert the date and select a subset for one week from the data pulled in
################################################################################

{ WkTime <- function(data, year, month, day) {
  
  #Set start and end dates of week  
  wk_st <- as.POSIXct(paste(day,month,year, sep = "/"), format="%d/%m/%Y")
  wk_end <- as.POSIXct(paste(day+7,month,year, sep = "/"), format="%d/%m/%Y")
  
  #Create subset for specified week
  subset(data,
         (date >= wk_st & date <= wk_end)) 
  
  }
}

################################################################################
## FUNCTION: YrDay_Time
## Convert the date and select a subset for specific year and day
################################################################################

{ YrDay_Time <- function(data, year,day) {
  
  # Create column for year 
  data$YEAR <- format(data$date,format="%Y") # Reformat for year only
  
  year <- format(as.character(year), 
                    format = "%Y")
  
  #Create subset for specified days in year
  data <- data %>%
      filter(.,YEAR==year) %>%    # Filter year out
      filter(.,wday(date) == day)  #Filter for day specified only
  
}
}

################################################################################
## FUNCTION: YrTime
## Convert the date and select a subset for one week from the data pulled in
################################################################################

{ YrTime <- function(data, year) {
  
  #Set start and end dates of week  
  yr_st <- as.POSIXct(paste(01,01,year, sep = "/"), format="%d/%m/%Y")
  yr_end <- as.POSIXct(paste(31,12,year, sep = "/"), format="%d/%m/%Y")
  
  #Create subset for specified week
  subset(data,
         (date >= yr_st & date <= yr_end)) 
  
}
}

################################################################################
#
#
# PLOTING FUNCTIONS SECTION
#
#
################################################################################
################################################################################
## FUNCTION: week1
## Plots output for a single week given the case study
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupHr_sub - Filtered version of Resource Group Hour Table
##    ZoneHr_Avg - Average hourly info in zone
##    Export - Exports selected from Zone Hourly Table
################################################################################

  Week1 <- function(year, month, day, case) {
    
    # Title Formating
    wk_st <- as.Date(paste(year,month,day, sep = "-"),tz="MST")
    wk_end <- as.Date(paste(year,month,day+7, sep = "-"),tz="MST")
    
    # Filters for the desired case study from the resource groups
    data <- ResGroupHr_sub %>%
      sim_filt1(.) %>%
      subset(., select=-c(Report_Year,Capacity_Factor)) %>%
      rbind(.,Import) %>%
      filter(Run_ID == case) %>%
      filter(date >= wk_st) %>%
      filter(date <= wk_end)
    
    data$Output_MWH[data$Output_MWH<0.001] <-0
    
    # Set levels to each category in order specified
    data$ID <- factor(data$ID, levels=c("Import","Solar","Wind", "Other", "Hydro", 
                                        "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                        "Blended  Simple Cycle","Blended  Combined Cycle",
                                        "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                                        "Coal-to-Gas", 
                                        "Coal", "Cogeneration","Storage"))
    ## SELECT A SINGLE WEEK

    # Select only a single week from the zone Hourly, and Export data
    WK <- WkTime(data,year,month,day)%>%
      filter(Run_ID == case)
    ZPrice <- WkTime(ZoneHr_Avg,year,month,day) %>%
      filter(Run_ID == case)
    Expo <- WkTime(Export,year,month,day) %>%
      filter(Run_ID == case)
    
    # Get y-max, demand to meet + exports
    Max <- ZPrice$Demand + Expo$Output_MWH
    
    # Set the max and min for the plot Output axis (y), Set slightly above max (200 above)
    MX <- plyr::round_any(max(abs(Max))+500, 100, f = ceiling)
    MN <- plyr::round_any(min(Max), 100, f = floor)
    
    ## PLOT WITH AREA PLOT
    
    ggplot() +
      geom_area(data = WK, aes(x = date, y = Output_MWH, fill = ID, colour = ID), 
                alpha=0.7, size=0.5) +

      # Add hourly load line (black line on the top)
      geom_line(data = ZPrice, 
                aes(x = date, y = Demand), size=1.5, colour = "black") +
      scale_x_datetime(expand=c(0,0),date_labels = "%b-%e", breaks = "day") +
      
      # Set the theme for the plot
      theme_bw() +
      theme(panel.grid = element_blank()) +
      
      theme(text=element_text(family=Plot_Text)) +
      
      theme(plot.title = element_text(size= Tit_Sz)) +
      
      theme(axis.text.x = element_text(vjust = 1),
            axis.title.x = element_text(size= XTit_Sz),
            axis.title.y = element_text(size= YTit_Sz),
            panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent", color = NA),
            legend.title=element_blank(),
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.background = element_rect(fill='transparent',colour ='transparent'),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
            legend.key.size = unit(1,"lines"), #Shrink legend
            legend.position = "bottom",
            plot.title = element_text(),
            text = element_text(size= 15)
      ) +
      scale_y_continuous(expand=c(0,0), limits = c(0,MX), 
                         breaks = seq(0, MX, by = MX/4)) +
      guides(fill = guide_legend(nrow = 2)) +
      
      labs(x = "Date", y = "Output (MWh)", fill = "Resource", colour = "Resource",
           title=year,caption=SourceDB) +
      
      #Add colour
      scale_fill_manual(values = colours1) +
      
      # Make outline the same as fill colors
      scale_colour_manual(values = Outline1)
  }

################################################################################
## FUNCTION: Week14
## Same as week1 except adjusts limits for comparison
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupHr_sub - Filtered version of Resource Group Hour Table
##    ZoneHr_Avg - Average hourly info in zone
##    Export - Exports selected from Zone Hourly Table
################################################################################

Week14 <- function(year, month, day, case) {
  # Filters for the desired case study from the resource groups
  data <- ResGroupHr_sub%>%
    sim_filt1(.) %>%
    subset(., select=-c(Report_Year,Capacity_Factor)) %>%
    rbind(.,Import) %>%
    filter(Run_ID == case)
  
  # Set levels to each category in order specified
  data$ID <- factor(data$ID, levels=c("Import","Solar","Wind", "Other", "Hydro", 
                                      "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                      "Blended  Simple Cycle","Blended  Combined Cycle",
                                      "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                                      "Coal-to-Gas", 
                                      "Coal", "Cogeneration","Storage"))
  ## SELECT A SINGLE WEEK
  
  data$Output_MWH[data$Output_MWH<0.001] <-0
  
  # Select only a single week from the zone Hourly, and Export data
  WK <- WkTime(data,year,month,day)
  ZPrice <- WkTime(ZoneHr_Avg,year,month,day) %>%
    filter(Run_ID == case)
  Expo <- WkTime(Export,year,month,day) %>%
    filter(Run_ID == case)
  
  # Get y-max, demand to meet + exports
  WK$MX <- ZPrice$Demand + Expo$Output_MWH
  
  # Set the max and min for the plot
  MX1 <- WkTime(WK,Yr4Sp[[1]],month,day)
  MX2 <- WkTime(WK,Yr4Sp[[2]],month,day)
  MX3 <- WkTime(WK,Yr4Sp[[3]],month,day)
  MX4 <- WkTime(WK,Yr4Sp[[4]],month,day)
  MXtime <- rbind(MX1, MX2, MX3, MX4)
  
  # Set the max and min for the plot Output axis (y), Set slightly above max (500 above)
  MX <- plyr::round_any(max(abs(MXtime$MX))+500, 100, f = ceiling)
  MN <- plyr::round_any(min(MXtime$Output_MWH), 100, f = floor)
  
  
  # Title Formating
  wk_st <- as.POSIXct(paste(year,month,day, sep = "-"),tz="MST")
  wk_end <- as.POSIXct(paste(year,month,day+7, sep = "-"),tz="MST")
  
  ## PLOT WITH AREA PLOT
  
  ggplot() +
    geom_area(data = WK, aes(x = date, y = Output_MWH, fill = ID, colour = ID), 
              alpha=0.7, size=0.5) +
    
    # Add hourly load line (black line on the top)
    geom_line(data = ZPrice, 
              aes(x = date, y = Demand), size=1.5, colour = "black") +
    scale_x_datetime(expand=c(0,0),date_labels = "%b-%e" ,breaks = "day") +
    
    # Set the theme for the plot
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = "right",) +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(plot.title = element_text(size= Tit_Sz)) +
    
    theme(axis.text.x = element_text(vjust = 1),
          axis.title.x = element_text(size= XTit_Sz,face = 'bold'),
          axis.title.y = element_text(size= YTit_Sz,face = 'bold'),
          panel.background = element_rect(fill = "transparent"),
         # panel.grid.major.y = element_line(size=0.25,linetype=5,color = 'gray36'),
          panel.ontop = TRUE,
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.title=element_blank(),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          legend.key.size = unit(1,"lines"), #Shrink legend
          legend.position = "bottom",
          text = element_text(size= 15)
    ) +
    scale_y_continuous(expand=c(0,0), limits = c(0,MX), 
                       breaks = seq(MN, MX, by = MX/4)) +
    guides(fill = guide_legend(nrow = 2)) +
    
    labs(x = "Date", y = "Output (MWh)", fill = "Resource", colour = "Resource") +
    
    #Add colour
    scale_fill_manual(values = colours1) +
    
    # Make outline the same as fill colors
    scale_colour_manual(values = colours1)
}

################################################################################
## FUNCTION: week12
## Plots output for a single week given the case study
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupHr_sub - Filtered version of Resource Group Hour Table
##    ZoneHr_Avg - Average hourly info in zone
##    Export - Exports selected from Zone Hourly Table
################################################################################

Week12 <- function(year, month, day, case) {
 
  # Title Formating & filter between dates
  wk_st <- as.Date(paste(year,month,day, sep = "-"),tz="MST")
  wk_end <- as.Date(paste(year,month,day+7, sep = "-"),tz="MST")
  
   # Filters for the desired case study from the resource groups
  data <- ResGroupHr_sub%>%
    sim_filt1(.) %>%
    subset(., select=-c(Report_Year,Capacity_Factor)) %>%
    rbind(.,Import) %>%
    filter(Run_ID == case) %>%
    filter(date >= wk_st) %>%
    filter(date <= wk_end)
  
  # Set levels to each category in order specified
  data$ID <- factor(data$ID, levels=c("Import","Solar","Wind", "Other", "Hydro", 
                                      "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                      "Blended  Simple Cycle","Blended  Combined Cycle",
                                      "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                                      "Coal-to-Gas", 
                                      "Coal", "Cogeneration","Storage"))
  
  data$Output_MWH[data$Output_MWH<0.001] <-0
  
  ## SELECT A SINGLE WEEK
  
  # Select only a single week from the zone Hourly, and Export data
  WK <- WkTime(data,year,month,day)
  ZPrice <- WkTime(ZoneHr_Avg,year,month,day) %>%
    filter(Run_ID == case)
  Expo <- WkTime(Export,year,month,day) %>%
    filter(Run_ID == case)
  
  # # Set the max and min for the plot
  ZPrice2 <- ZoneHr_Avg %>%
    filter(Run_ID == case) 
        ZPrice2$YEAR  <- as.POSIXct(as.character(ZPrice2$date), format = "%Y")
        ZPrice2$YEAR <-(format(ZPrice2$YEAR,format="%Y")) # Reformat for year only
  ZPrice2 <- ZPrice2 %>%
        filter(YEAR == year)
    
  Expo2 <- Export%>%
    filter(Run_ID == case)
        Expo2$YEAR  <- as.POSIXct(as.character(Expo2$date), format = "%Y")
        Expo2$YEAR <-(format(Expo2$YEAR,format="%Y")) # Reformat for year only
  Expo2 <- Expo2 %>%
        filter(YEAR == year)
        
  # Get y-max, demand to meet + exports
  Max <- (ZPrice2$Demand + Expo2$Output_MWH) 
  
  # # Set the max and min for the plot Output axis (y), Set slightly above max (200 above)
  MX <- plyr::round_any(max(abs(Max))+500, 100, f = ceiling)

  #MX <- 15000

  Mtitle=month.abb[month]
  
  ## PLOT WITH AREA PLOT
  
  ggplot() +
    geom_area(data = WK, aes(x = date, y = Output_MWH, fill = ID, colour=ID),alpha=0.7, size=.25,color='black') +
    
    # Add hourly load line (black line on the top)
    geom_line(data = ZPrice, 
              aes(x = date, y = Demand), size=1.25, colour = "black") +
    scale_x_datetime(expand=c(0,0),date_labels = "%e", breaks = "day") +
    
    # Set the theme for the plot
    theme_bw() +
    theme(panel.grid = element_blank()) +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(plot.title = element_text(size= 10)) +
    
    theme(axis.text.x = element_text(vjust = 1),
          axis.title.x = element_text(size= 10),
          axis.title.y = element_text(size= 10),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.title=element_blank(),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent',colour ='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          legend.key.size = unit(1,"lines"), #Shrink legend
          legend.position = "bottom",
          legend.text = element_text(size= 12),
          text = element_text(size= 8)
    ) +
    scale_y_continuous(expand=c(0,0), limits = c(0,MX), 
                       breaks = seq(0, MX, by = MX/8)) +
    
    labs(x = "Date", y = "Output (MWh)", fill = "Resource", colour = "Resource",title=Mtitle) +
    
    guides(fill = guide_legend(nrow = 2)) +
    
    #Add colour
    scale_fill_manual(values = colours1)
}
################################################################################
## FUNCTION: day1
## Plots output for a single day given the case study
##
## INPUTS: 
##    year, month, day - Date to plot
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupHr_sub - Filtered version of Resource Group Hour Table
##    ZoneHr_Avg - Average hourly info in zone
##    Export - Exports selected from Zone Hourly Table
################################################################################
  
  day1 <- function(year, month, day, case) {
    # Filters for the desired case study from the resource groups
    data <- ResGroupHr_sub%>%
      sim_filt1(.) %>%
      subset(., select=-c(Report_Year,Capacity_Factor)) %>%
      rbind(.,Import) %>%
      filter(Run_ID == case)
    
    # Set levels to each category in order specified
    data$ID <- factor(data$ID, levels=c("Import","Solar","Wind", "Other", "Hydro", 
                                        "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                        "Blended  Simple Cycle","Blended  Combined Cycle",
                                        "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                                        "Coal-to-Gas", 
                                        "Coal", "Cogeneration","Storage"))
    # Get full date
    day_report <- as.POSIXct(paste(day,month,year, sep = "/"), format="%d/%m/%Y")
    
    ## SELECT A SINGLE DAY
    # Select only a single day from the zone Hourly, and Export data
    DY <- HrTime(data,year,month,day)
    ZPrice <- HrTime(ZoneHr_Avg,year,month,day) %>%
      filter(Run_ID == case)
    Expo <- HrTime(Export,year,month,day) %>%
      filter(Run_ID == case)
    
    # Get y-max, demand to meet + exports
    DY$MX <- ZPrice$Demand + Expo$Output_MWH
    
    # Set the max and min for the plot Output axis (y), Set slightly above max ( round up to nearest 500)
    MX <- plyr::round_any(max(abs(DY$MX+11)), 500, f = ceiling)
    
    
    # # Fill anoying gaps 
    # 
    # DATE <- unique(DY$date)
    # ID_1 <- unique(DY$ID)
    # 
    # combinations <- expand.grid(date = DATE, ID = ID_1)
    # 
    # DY <- full_join(DY, combinations, by = c("date" = "date", "ID" = "ID")) %>%
    #   mutate(Output_MWH = ifelse(is.na(Output_MWH), 0, Output_MWH)) %>%
    #   arrange(date, ID)
    
    ## PLOT WITH AREA PLOT
    
    ggplot() +
      geom_area(data = DY, aes(x = date, y = Output_MWH, fill = ID, colour = ID), 
                alpha=0.7, size=0.5) +
      
      # Add hourly load line (black line on the top)
      geom_line(data = ZPrice, 
                aes(x = date, y = Demand,colour = "Demand"), size=1.5, colour = "black") +
      scale_x_datetime(expand=c(0,0)) +
      
      # Set the theme for the plot
      theme_bw() +
      theme(legend.position = "bottom") +
      
      theme(text=element_text(family=Plot_Text)) +
      
      theme(plot.title = element_text(size= Tit_Sz)) +
      
      theme(axis.text.x = element_text(vjust = 1),
            panel.background = element_rect(fill = "transparent", colour = "transparent"),
            panel.grid = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            plot.background = element_blank(),
            legend.title=element_blank(),
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.key.size = unit(1,"lines"), #Shrink legend
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
            axis.title.x = element_text(size=XTit_Sz,face='bold'),
            axis.title.y = element_text(size=YTit_Sz,face='bold'),
            text = element_text(size= Overall_Sz)
      ) +
      guides(fill = guide_legend(nrow = 2)) +
      
      scale_y_continuous(expand=c(0,0), limits = c(0,MX), 
                         breaks = seq(0, MX, by = MX/4)) +
      
      labs(title=paste("Resource Output, ",day_report),x = "Date", y = "Output (MWh)", fill = "Resource",colour = "Resource",caption=SourceDB ) +
      
      #Add colour
      scale_fill_manual(values = colours1) +
      
      # Make outline the same as fill colors
      scale_colour_manual(values = colours1)
  }
  
################################################################################  
## FUNCTION: Stor1
## Weekly storage output
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupHr_sub - Filtered version of Resource Group Hour Table
################################################################################
  
  Stor1 <- function(year, month, day, case) {

    # Filters for the desired case study
    data <- ResGroupHr_sub%>%
      filter(ID=="LTO_Storage") %>%
      filter(Run_ID == case)
    
    # Select only a single week
    WK <- WkTime(data,year,month,day)
    
    # Set the max and min for the plot
    MX <- plyr::round_any(max(abs(WK$Output_MWH)), 10, f = ceiling)
    
    # Plot the data    
    ggplot() +
      geom_area(data = WK, aes(x = date, y = Output_MWH,fill="Storage"), 
                alpha=0.7, size=.5, colour="black") +
      ggtitle(year)+
      
      # Set the theme for the plot
      theme_bw() +
      
      theme(text=element_text(family=Plot_Text)) +
      
      theme(panel.grid = element_blank(),
            axis.text.x=element_text(vjust = 0),
            plot.title = element_text(size=Tit_Sz),
            legend.title = element_blank(),
            legend.position = "none",
            axis.title.x = element_text(size=XTit_Sz,face='bold',vjust = -1),
            axis.title.y = element_text(size=YTit_Sz,face='bold'),
            text = element_text(size= Overall_Sz)
      ) +
      scale_x_datetime(expand=c(0,0),date_labels = "%b-%e",breaks = "day") +
      scale_y_continuous(breaks = seq(-MX, MX, by = MX), 
                         limits = c(-MX-1,MX+1),
                         labels = label_number(accuracy = 1)) +
      labs(x = "Date", y = "Storage (MWh)") +
      scale_fill_manual(values = cOL_STORAGE)
  }
  
################################################################################  
## FUNCTION: week_price 
## Electricity price for one week
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Zone Hour average condition only table
################################################################################
  
  week_price <- function(year, month, day,case) {
    # Filters for the desired case study
    data <- ZoneHr_Avg%>%
      filter(Run_ID == case)
    
    # Select only a single week using function WkTime
    ZPrice <- WkTime(data,year,month,day)
    
    # Set the max and min for the plot
    MX <- plyr::round_any(max(abs(ZPrice$Price)+10), 10, f = ceiling)
    MN <- plyr::round_any(min(abs(ZPrice$Price)), 10, f = floor) #Could put in scale y limits
    
    #Max min for date (x-axis)
    day_MN <- as.POSIXct(paste(day,month,year, sep = "/"), format="%d/%m/%Y")
    day_MX <- as.POSIXct(paste(day+7,month,year, sep = "/"), format="%d/%m/%Y")
    
    # Plot the data    
    ggplot() +
      geom_line(data = ZPrice, 
                aes(x = date, y = Price), 
                size = 1.5, colour = "darkred") +
      theme_bw() +
      theme(text=element_text(family=Plot_Text)) +
      theme(panel.background = element_rect(fill = "transparent"),
            axis.text.x=element_text(vjust=-1),
            axis.title.x = element_text(vjust=-1,size= XTit_Sz,face="bold"),
            axis.text.y=element_text(hjust=-0.5),
            axis.title.y = element_text(vjust=2,size= YTit_Sz,face="bold"),
            panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'grey'),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid = element_blank(),
            plot.background = element_rect(fill = "transparent", color = NA),
            plot.title = element_text(size = Tit_Sz),
            text = element_text(size = 15) 
            
      ) +
      labs(y = "Pool Price ($/MWh)", x="Date",fill = "Resource",caption=SourceDB) +
      scale_x_datetime(expand=c(0,0),limits=c(day_MN,day_MX),breaks = "day",date_labels = "%b-%e") +
      scale_y_continuous(expand=c(0,0), 
                         limits= c(0,MX),
                         #                       labels = label_number(accuracy = 1),
                         breaks = seq(0, MX, by = MX/4)
      )
  }
  

  ################################################################################  
  ## FUNCTION: Evalyr 
  ## Plotting year profiles of resource output
  ##
  ## INPUTS: 
  ##    input - ResgroupMnor ResGroupYr
  ##    case - Run_ID which you want to plot
  ## TABLES REQUIRED: 
  ##    Import - Import table derived of zone average table
  ################################################################################
  
  Evalyr <- function(input,case) {
    
    # Get imports
    Imp <- Import_Yr %>%
      filter(Name == "WECC_Alberta") %>%
      filter(Run_ID == case) %>%
      select(ID, Time_Period, Output_MWH) %>%
      mutate(ID = "Import") 

    # Filters for the desired case study
    data <- input %>%
      filter(Run_ID == case & Condition == "Average") %>%
      select(ID, Time_Period, Output_MWH) %>%
      sim_filt(.) %>%
      rbind(.,Imp) 
    
    # Add imports to factor list
    data$ID<-fct_relevel(data$ID, "Import")
    
    # Get year and format as a number (easier to plot!)
    data$YEAR <- as.Date(data$Time_Period)
    data$YEAR <- as.numeric(format(data$YEAR,"%Y"))

    # Get max and min year for plot
    YearMX<-max(data$YEAR)
    YearMN<-min(data$YEAR)
    
    # Set the max generation for the plot
    GenMX <- aggregate(data["Output_MWH"], by=data["Time_Period"], sum)
    MX <- plyr::round_any(max(abs(GenMX$Output_MWH)/1000000), 20, f = ceiling)
    
    # Plot
    data %>%
      ggplot() +
      aes(YEAR, (Output_MWH/1000000), fill = ID) +
      geom_area(alpha=0.7, linewidth=.5, colour="black") +
      
      theme_bw() +
      
      theme(text=element_text(family=Plot_Text)) +
      
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(vjust = 1),
            axis.title.x = element_text(size = XTit_Sz),
            axis.title.y = element_text(size = YTit_Sz),
            plot.title = element_text(size = Tit_Sz),
            plot.subtitle = element_text(hjust = 0.5), 
            panel.background = element_rect(fill = NA),
            legend.key.size = unit(1,"lines"), #Shrink legend
            legend.position = "bottom",
            legend.justification = c(0.5,0.5),
            legend.text = element_text(size =15),
            legend.title=element_blank(),
            text = element_text(size = 15)) +
      
      scale_x_continuous(expand=c(0,0),limits = c(YearMN,YearMX),breaks=seq(YearMN, YearMX, 1)) +
      scale_y_continuous(expand=c(0,0),limits = c(0,MX),breaks=pretty_breaks(6)) +
      
      labs(x = "Date", y = "Annual Generation (TWh)", fill = "Resource",colour="Resource",caption = SourceDB) +
      
      guides(fill = guide_legend(nrow = 2)) +
      
      scale_fill_manual(values = colours4,drop = FALSE) 
    
  }
################################################################################  
## FUNCTION: Evalcap 
## Plotting month/year profiles of resource capacity
##
## INPUTS: 
##    input - ResgroupMn or ResGroupYr
##    case - Run_ID which you want to plot
################################################################################
  
  Evalcap <- function(input,case) {
    
    # Filters for the desired case study
    data <- input %>%
      filter(Run_ID == case & Condition == "Average") %>%
      select(ID, Time_Period, Capacity) %>%
      sim_filt(.)
    
    # Get year and format as a number (easier to plot!)
    data$YEAR <- as.Date(data$Time_Period)
    data$YEAR <- as.numeric(format(data$YEAR,"%Y"))
    
    # Get max and min year for plot
    YearMX<-max(data$YEAR)
    YearMN<-min(data$YEAR)
    
    # Set the max and min for the plot Output axis (y)
    # Set the max for the plot
    ppMX <-  aggregate(data["Capacity"], by=data["Time_Period"], sum)
    MX <- plyr::round_any(max(abs(ppMX$Capacity)), 5000, f = ceiling)
    
    
    data %>%
      ggplot() +
      geom_area(aes(YEAR, (Capacity), fill = ID, colour=ID),alpha=0.7, size=.5,color='black') +
      theme_bw() +
      
      theme(text=element_text(family=Plot_Text)) +
      
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(),
            axis.title.x = element_text(size = XTit_Sz,face="bold"),
            axis.title.y = element_text(size = YTit_Sz,face="bold"),
            plot.title = element_text(size = Tit_Sz),
            panel.background = element_rect(fill = "transparent"),
            #panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
            plot.subtitle = element_text(hjust = 0.5), 
            legend.position = "bottom",
            legend.key.size = unit(1,"lines"),
            legend.title = element_blank(),
            legend.text = element_text(size =15),
            legend.justification = c(0.5,0.5),
            text = element_text(size = 15)) +
      
      scale_x_continuous(expand=c(0,0),limits = c(YearMN,YearMX),breaks=seq(YearMN, YearMX, 1)) +
      
      scale_y_continuous(expand=c(0,0), limits=c(0,MX),breaks = pretty_breaks(6)) +
      
      labs(x = "Date", y = "Capacity (MW)", fill = "Resource",colour="Resource") +
    
      guides(fill = guide_legend(nrow = 2)) +
      
      scale_fill_manual(values = colours2,drop = FALSE)
    
  }
 
################################################################################  
## FUNCTION: EvalPerc 
## Year/month profiles as a percentage of the total
##
## INPUTS: 
##    input - 
##    case - Run_ID which you want to plot
################################################################################ 
  
  EvalPerc <- function(input,case) {
    # Filters for the desired case study
    data <- input %>%
      filter(Run_ID == case & Condition == "Average")# %>%

    # Filter the data by resource
    case_Time <- sim_filt(data)
    
    # Remove negative generation (Storage)
    case_Time$Output_MWH[case_Time$Output_MWH < 0] <- NA
    
    # Get year and format as a number (easier to plot!)
    case_Time$YEAR <- as.Date(case_Time$Time_Period)
    case_Time$YEAR <- as.numeric(format(case_Time$YEAR,"%Y"))
    
    # Get max and min year for plot
    YearMX<-max(case_Time$YEAR)
    YearMN<-min(case_Time$YEAR)
    
    case_Time %>%
      ggplot() +
      geom_area(aes(YEAR, Output_MWH, fill = ID,colour= ID),
                position = "fill", alpha = 0.7, size=.5,color="black") +
      
      geom_vline(xintercept = as.numeric(2035),
                 linetype = "dashed", color = "black", size = 1) +
      theme_bw() +
      
      theme(text=element_text(family=Plot_Text)) +
    
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5), 
            legend.title = element_blank(),
            panel.background = element_rect(fill = "transparent"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            plot.background = element_rect(fill = "transparent", color = NA),
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.background = element_rect(fill='transparent',colour='transparent'),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
            legend.key.size = unit(1,"lines"),
            legend.position = "bottom",
            legend.justification = c(0.5,0)) +
      
      scale_x_continuous(expand=c(0,0),limits = c(YearMN,YearMX),breaks=seq(YearMN, YearMX, 1)) +
      scale_y_continuous(expand=c(0,0),
                         labels = scales::percent, 
                         breaks = sort(c(seq(0,1,length.out=5)))) +
      
      guides(fill = guide_legend(nrow = 2)) +
      labs(x = "Date", y = "Percentage of Generation", fill = "Resource",colour="Resource") +
      scale_fill_manual(values = colours2,drop = FALSE) 
      
  }
  
################################################################################  
## FUNCTION: Units
## Unit specific bar chart showing builds
##
## INPUTS: 
##    case - Run_ID which you want to plot
##    Fuel - Fuel type matching Fuel_ID 
## TABLES REQUIRED: 
##    Build - Build table describing all new resources
################################################################################

  Units <- function(case, Fuel) {
    
    data <- Build %>%
      filter(Run_ID == case & LT_Iteration == max(LT_Iteration) & 
               Time_Period == "Study" & Fuel_Type == Fuel) 
    
    data %>%
      ggplot() +
      aes(Name, Units_Built,fill = Fuel_Type) + 
      geom_col() +
      labs(x = "Plant Name", y = "Units Built") +
      scale_y_continuous(expand = c(0,0),
                         limits = c(0,(max(data$Units_Built)+1))) +
      
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),

            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.title.x = element_text(size = XTit_Sz,face="bold"),
            axis.title.y = element_text(size = YTit_Sz,face="bold"),
            plot.title = element_text(size = Tit_Sz),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.background = element_rect(fill = "transparent"),
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.key.size = unit(1,"lines"), 
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
            text = element_text(size = 15),
            panel.border = element_rect(colour = "black", fill = "transparent"), 
            panel.grid.major.y = element_line(size=0.25,linetype=5,color = "gray36")) +
          
    
            scale_fill_manual(values="gray") +
      
            theme(text=element_text(family=Plot_Text))
  }
  
################################################################################  
## FUNCTION: Slack
## Unit specific bar chart showing units not built
##
## INPUTS: 
##    case - Run_ID which you want to plot
##    Fuel - Fuel type matching Fuel_ID 
## TABLES REQUIRED: 
##    Build - Build table describing all new resources
################################################################################
  
  Slack <- function(case, Fuel) {
    data <- Build %>%
      filter(Run_ID == case & LT_Iteration == max(LT_Iteration) & 
               Time_Period == "Study" & Fuel_Type == Fuel) 
    
    data %>%
      ggplot() +
      aes(Name, Max_Limit_Slack,fill = Fuel_Type) + 
      geom_col() +
      labs(x = "Plant Name", y = "Units Available") +
      scale_y_continuous(expand = c(0,0),
                         limits = c(0,(max(data$Max_Limit_Slack)+1))) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            
            panel.background = element_rect(fill = "transparent"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.title.x = element_text(size = XTit_Sz,face="bold"),
            axis.title.y = element_text(size = YTit_Sz,face="bold"),
            plot.title = element_text(size = Tit_Sz),
            plot.background = element_rect(fill = "transparent", color = NA),
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.key.size = unit(1,"lines"), 
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
            text = element_text(size = 15),
            panel.border = element_rect(colour = "black", fill = "transparent"), 
            panel.grid.major.y = element_line(size=0.25,linetype=5,color = "gray36")) +  
      
      scale_fill_manual(values="gray") +
      
      theme(text=element_text(family=Plot_Text))
  }
  
  
###############################################################################  
## FUNCTION: Sim_dur 
## Simulation duration curve ploted each year
## The price duration curve represents the percentage of hours in which pool price 
## equaled or exceeded a specified level.
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_All - Zone hourly table for all conditions
################################################################################  
  
  Sim_dur <- function(case) {
    
    tot <- ZoneHr_All%>%
      group_by(Condition, Report_Year)%>%
      mutate(perc = 1-ecdf(Price)(Price))
    
    tot$Report_Year <- as.factor(tot$Report_Year)
    
    tot <- tot %>%
      filter(Report_Year %in% Years2Disp)
    
    ggplot() +
      geom_line(data = tot, 
                aes(x = perc, y = Price, colour = Report_Year), size = 1.25) +
      facet_grid(cols = vars(Condition)) +
      
      theme_bw() +
      
      theme(text=element_text(family=Plot_Text)) +
      
      theme(panel.grid = element_blank(),
            panel.spacing = unit(2, "lines"),
            axis.title.x = element_text(size = XTit_Sz,face="bold"),
            axis.title.y = element_text(size = YTit_Sz,face="bold"),
            text = element_text(size = 15),
            legend.title = element_blank(),
            panel.grid.major.y = element_line(size=0.25,linetype=5,color = "gray70")) +
            
      labs(y = "Pool Price ($/MWh)", x = "Percentage of Time",caption = SourceDB) +
      
      #scale_color_brewer(palette= "Dark2") +
  
      scale_x_continuous(expand=c(0,0), 
                         limits = c(0,1),
                         labels = percent) +
      
      scale_y_continuous(expand=c(0,0),limits = c(0,1000),breaks = pretty_breaks(5)) 
  }


################################################################################
#
#
# COMBINED PLOTS SECTION
#
#
################################################################################ 
  
################################################################################
## FUNCTION: year_weeks
## Plots output for a single week given the case study
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupHr_sub - Filtered version of Resource Group Hour Table
##    ZoneHr_Avg - Average hourly info in zone
##    Export - Exports selected from Zone Hourly Table
################################################################################
year_weeks <- function(year,case) {
  
  
  # Create a graph for each month of the year
  p1 <- Week12(year,01,08,case) +
    theme(axis.title.y=element_blank(),
          axis.title.x=element_blank())
          

  p2 <- Week12(year,02,08,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p3 <- Week12(year,03,08,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p4 <- Week12(year,04,08,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p5 <- Week12(year,05,08,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p6 <- Week12(year,06,08,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p7 <- Week12(year,07,08,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p8 <- Week12(year,08,08,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p9 <- Week12(year,09,08,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p10 <- Week12(year,10,08,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p11 <- Week12(year,11,08,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p12 <- Week12(year,12,08,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  # Get a common legend
  legend <- get_legend(p1)
  p1 <- p1 + theme(legend.position ="none")
  
  # Plot Labels
  yleft <- textGrob("Output (MWh)", rot = 90, gp = gpar(fontsize = 15))
  bottom <- textGrob("Date", gp = gpar(fontsize = 15))
  
  # Cheat way to put an x title in
  xtitle <- ggplot() +
    annotate("text", x = 10,  y = 10,
             size = 6,
             label = "Date") + 
               theme_void()
  
  # Label the source and year
  xsubtitle <- ggplot() +
    annotate("text", x = 10,  y = 10,
             size = 4,
             label = paste("year:",year,", Database:",SourceDB)) + 
    theme_void()
  
  #Create a big window
  windows(18,12)
  
  # Arrange all the plots
  grid.arrange(plot_grid(p1, p2, p3, p4, ncol=4, align="v", axis = "l", rel_widths = c(1,1,1,1)),
                    plot_grid(p5,p6, p7, p8, ncol=4, align="v", axis = "l", rel_widths = c(1,1,1,1)),
                            plot_grid(p9, p10, p11, p12, ncol=4, align="v", axis = "l", rel_widths = c(1,1,1,1)),
               plot_grid(xtitle),
               plot_grid(legend),
               plot_grid(xsubtitle),
               ncol=1,nrow=6, 
               heights=c(1, 1,1,0.1,0.2,0.1),
               left=yleft)
  

  }
    
################################################################################
## FUNCTION: Week4
## Function to plot four years for a specific case study
################################################################################
  
  Week4 <- function(month,day,case) {
    ggdraw(add_sub(ggarrange(Week14(Yr4Sp[[1]],month,day,case) +
                             theme(legend.position ="none"),
                             Week14(Yr4Sp[[2]],month,day,case) +
                               theme(legend.position ="none"),
                             Week14(Yr4Sp[[3]],month,day,case) +
                               theme(legend.position ="none"),
                             Week14(Yr4Sp[[4]],month,day,case) +
                               theme(legend.position ="none"),
                             labels = c(Yr4Sp[[1]],Yr4Sp[[2]],Yr4Sp[[3]],Yr4Sp[[4]]),
                             common.legend = TRUE, 
                             legend = "bottom",ncol = 2, nrow = 2),paste("Simulation Name: SourceDB")))
  }
  
################################################################################
## FUNCTIONS: PrOt, PrOut, PrOut4 ** Not Done
## Plot Price and Output together
################################################################################
  
  PrOt <- function(year,month,day,case) {
    plot_grid(week_price(year,month,day,case) + 
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.title=element_blank()),
                Week1(year,month,day,case)+
                theme(legend.position ="bottom"), 
                ncol = 1, align="v", axis = "l",rel_heights = c(1,2.5)) +
      
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) 
  }
  
#Includes storage
  PrOut <- function(year,month,day,case) {
    plot_grid(Stor1(year,month,day,case),
              week_price(year,month,day,case) + theme(axis.title.x=element_blank(),
                                                      axis.text.x=element_blank()),
              Week1(year,month,day,case)+theme(legend.position ="none"), 
              ncol = 1, align="v", axis = "l",rel_heights = c(1,1,2.5))
  }

################################################################################
## FUNCTION: EvalOut
## Plotting the month/year profile with the build
################################################################################
  
  EvalOut <- function(input,case) {
    p1 <- Eval(input,case) +
                      theme(legend.position="bottom",
                            legend.spacing.x = unit(1,"lines")) +
                      guides(fill = guide_legend(nrow = 1, byrow = TRUE)) 
    p2 <- Builtcol(case) +
            theme(legend.position ="none",
               axis.title.x = element_blank(),
               axis.text.x = element_blank())
    p3 <- BuiltMW(case) +
            theme(legend.position ="none",
                  axis.title.x = element_blank()) 
                    
    p4 <- plot_grid(p3, p2, p1, ncol = 1, nrow=3, align="v", axis = "l",rel_heights = c(1,1,2.5))
    
    ggdraw(add_sub(p4,paste("Simulation: ",SourceDB, sep = "")))
  }
################################################################################
## FUNCTIONS: BuildUnits, BuildUnits2
## Plot four years for a specific case study of the combined plots
################################################################################
  
  BuildUnits <- function(case, Fuel) {
    p1 <- Units(case,Fuel)+
            theme(axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                legend.position ="none")
    
    p2 <- Slack(case,Fuel)  +
            theme(legend.position ="none")
    
    p3 <- plot_grid(p1, p2, ncol = 1, align="v", axis = "l", rel_heights = c(1,1))
    
    ggdraw(add_sub(p3,paste("Simulation: ",SourceDB,"; ", "Fuel Type:",Fuel, sep = "")))
  }
  
  BuildUnits2 <- function(case, Fuel) {
    p1 <- plot_grid(Units2(case,Fuel)+theme(axis.title.x = element_blank(),
                                            axis.text.x = element_blank(),
                                            text = element_text(size= 15)),
                    Slack2(case,Fuel)+theme(text = element_text(size= 15)),
                    ncol = 1, align="v", axis = "l",rel_heights = c(1,1.5))
    
    ggdraw(add_sub(p1,paste("Simulation: ",SourceDB, sep = "")))
  }
  
