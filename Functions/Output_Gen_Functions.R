################################################################################
# TITLE: Output_Gen_Functions
# DESCRIPTION: Functions To use for plotting and evaluating simulation data on resource outputs and generation.
# Also plots other misc things not covered elsewhere
#
# ORIGINAL AUTHOR: Taylor Pawlenchuk (Retrieved June 3, 2022)
# EDITS & ADDITIONAL CONTENT: Jessica Van Os
# LAST EDIT: January 6, 2023
#
################################################################################

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
      geom_area(data = WK, aes(x = date, y = Output_MWH, fill = ID), colour = "black", 
                alpha=Plot_Trans, size=0.5) +

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
      scale_fill_manual(values = colours1) 
  }

################################################################################
## FUNCTION: day1
## Plots output for a single day given the case study.
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
      geom_area(data = DY, aes(x = date, y = Output_MWH, fill = ID), colour = "black", 
                alpha=Plot_Trans, size=0.5) +
      
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
                alpha=Plot_Trans, size=.5, colour="black") +
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
            text = element_text(size= Overall_Sz),
            panel.grid.major.y = element_line(size=0.25,
            linetype=2,color = 'gray90'),                         # Adds horizontal lines
      ) +
      scale_x_datetime(expand=c(0,0),date_labels = "%b-%e",breaks = "day") +
      scale_y_continuous(breaks = seq(-MX, MX, by = MX/4), 
                         limits = c(-MX-1,MX+1),
                         labels = label_number(accuracy = 1)) +
      labs(x = "Date", y = "Storage (MWh)") +
      
      geom_hline(yintercept=0, color = "black", size=0.5)+
      
      scale_fill_manual(values = cOL_STORAGE)
  }
  
################################################################################  
## FUNCTION: Stor2
## Weekly storage output with pool price.
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupHr_sub - Filtered version of Resource Group Hour Table
################################################################################
  
  Stor2 <- function(year, month, day, case) {
    
    # Get storage data
    StorData <- ResGroupHr_sub%>%
      filter(ID=="LTO_Storage",
             Run_ID == case,
             Report_Year == year)
    
    # Select only a single week
    WK_Stor <- WkTime(StorData,year,month,day)
    
    # Get price data
    PriceData <- ZoneHr_Avg%>%
      filter(Run_ID == case)
    
    # Select only a single week using function WkTime
    WK_Price <- WkTime(PriceData,year,month,day)
    
    # Set the max and min for the plot using annual max storage. Add factor to move price plot up
    MX <- plyr::round_any(max(abs(StorData$Output_MWH)), 10, f = ceiling)*1.2
    ylimStor<-c(-MX,MX)
    ylimPrice<-c(-1050,1050)
    
    b <- diff(ylimStor)/diff(ylimPrice)
    a <- ylimStor[1] - b*ylimPrice[1] 
    
    # PLOT DATA  
    ggplot() +
      geom_area(data = WK_Stor, aes(x = date, y = Output_MWH,fill="Storage"), 
                alpha=Plot_Trans, size=.5, colour="black") +
      geom_hline(yintercept=0, color = "black", size=0.5)+
      geom_line(data = WK_Price, aes(x = date, y = a + Price*b), 
                size = 1.25, colour = "darkblue",linetype=1) +
      
      # Set up plot look and feel
      theme_bw() +
      theme(text=element_text(family=Plot_Text)) +
      
      # Set the theme for the plot
      theme(panel.grid = element_blank(),
            axis.text.x=element_text(vjust = 0),
            plot.title = element_text(size=Overall_Sz),
            legend.title = element_blank(),
            legend.position = "none",
            axis.title.x = element_text(size=XTit_Sz,face='bold',vjust = -1),
            axis.title.y = element_text(size=YTit_Sz,face='bold'),
            text = element_text(size= Overall_Sz),
            panel.grid.major.y = element_line(size=0.25,
                                              linetype=2,color = 'gray90'),                         # Adds horizontal lines
      ) +
      scale_x_datetime(expand=c(0,0),date_labels = "%b-%e",breaks = "day") +
      
      scale_y_continuous(name="Storage Output (MWh)",
                         breaks = seq(-MX, MX, by = MX/4), 
                         limits = c(-MX,MX),
                         sec.axis = sec_axis(~(. - a)/b, name="Pool Price ($/MWh)",
                         breaks = seq(0,1000,by=250))) +
      
      theme(axis.ticks.y.left = element_line(color = cOL_STORAGE),
            axis.text.y.left = element_text(color = cOL_STORAGE), 
            axis.title.y.left = element_text(color = cOL_STORAGE)) +
      
      labs(x = paste("Date (",year,")"),title=month.abb[month]) +
      
      scale_fill_manual(values = cOL_STORAGE)
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
  
  Evalyr <- function(case) {
    
    # Get imports
    Imp <- Import_Yr %>%
      filter(Name == "WECC_Alberta") %>%
      filter(Run_ID == case) %>%
      select(ID, Time_Period, Output_MWH) %>%
      mutate(ID = "Import") 

    # Filters for the desired case study
    data <- ResGroupYr %>%
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
    YearMX<-max(data$YEAR)-5 #Take off the last 5 years
    YearMN<-min(data$YEAR)
    
    # Filter to remove the final 5 years (as per AURORA, want to run 5 years past year of interest)
    data <- data%>%
      filter(YEAR<=YearMX)
    
    # Set the max generation for the plot
    GenMX <- aggregate(data["Output_MWH"], by=data["Time_Period"], sum)
    MX <- plyr::round_any(max(abs(GenMX$Output_MWH)/1000000), 20, f = ceiling)
    
    # Plot
    data %>%
      ggplot() +
      aes(YEAR, (Output_MWH/1000000), fill = ID) +
      geom_area(alpha=Plot_Trans, linewidth=.5, colour="black") +
      
      theme_bw() +
      
      # Changes the font type
      theme(text=element_text(family=Plot_Text)) +             
      
      theme(
        # General Plot Settings
            panel.grid = element_blank(),
            # (t,r,b,l) margins, adjust to show full x-axis, default: (5.5,5.5,5.5,5.5)
            plot.margin = unit(c(6, 12, 5.5, 5.5), "points"),      # Plot margins
            panel.background = element_rect(fill = "transparent"), # Transparent background
            text = element_text(size = GenText_Sz),                # Text size
            plot.title = element_text(size = Tit_Sz),              # Plot title size (if present)
            plot.subtitle = element_text(hjust = 0.5),             # Plot subtitle size (if present)
            #panel.grid.major.y = element_line(size=0.25,
            #linetype=1,color = 'gray90'),                         # Adds horizontal lines
        # X-axis
            axis.text.x = element_text(vjust = 1),                 # Horizontal text
            axis.title.x = element_text(size = XTit_Sz),           # x-axis title text size
        # Y-axis
            axis.title.y = element_text(size = YTit_Sz),           # y-axis title text size
        # Legend
            legend.key.size = unit(1,"lines"),                     # Shrink legend boxes
            legend.position = "bottom",                            # Move legend to the bottom
            legend.justification = c(0.5,0.5),                     # Center the legend
            legend.text = element_text(size =Leg_Sz),              # Size of legend text
            legend.title=element_blank()) +                        # Remove legend title
      
      # Set axis scales
      scale_x_continuous(expand=c(0,0),limits = c(YearMN,YearMX),breaks=seq(YearMN, YearMX, 1)) +
      scale_y_continuous(expand=c(0,0),limits = c(0,MX),breaks=pretty_breaks(6)) +
      
      # Plot labels
      labs(x = "Year", y = "Annual Generation (TWh)", fill = "Resource",colour="Resource",caption = SourceDB) +
      
      # Legend guides - number of rows
      guides(fill = guide_legend(nrow = 2)) +
      
      # Legend color scheme
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
  
  Evalcap <- function(case) {
    
    # Filters for the desired case study
    data <- ResGroupYr %>%
      filter(Run_ID == case & Condition == "Average") %>%
      select(ID, Time_Period, Capacity) %>%
      sim_filt(.)
    
    # Get year and format as a number (easier to plot!)
    data$YEAR <- as.Date(data$Time_Period)
    data$YEAR <- as.numeric(format(data$YEAR,"%Y"))
    
    # Get max and min year for plot
    YearMX<-max(data$YEAR)-5 #Take off the last 5 years
    YearMN<-min(data$YEAR)
    
    # Filter to remove the final 5 years (as per AURORA, want to run 5 years past year of interest)
    data <- data%>%
      filter(YEAR<=YearMX)
    
    # Set the max and min for the plot Output axis (y)
    # Set the max for the plot
    ppMX <-  aggregate(data["Capacity"], by=data["Time_Period"], sum)
    MX <- plyr::round_any(max(abs(ppMX$Capacity)), 5000, f = ceiling)
    
    
    data %>%
      ggplot() +
      geom_area(aes(YEAR, (Capacity), fill = ID, colour=ID),alpha=Plot_Trans, size=.5,color='black') +
      theme_bw() +
      
      theme(text=element_text(family=Plot_Text)) +
      
      theme(
        # General Plot Settings
            panel.grid = element_blank(),
            # (t,r,b,l) margins, adjust to show full x-axis, default: (5.5,5.5,5.5,5.5)
            plot.margin = unit(c(6, 12, 5.5, 5.5), "points"),      # Plot margins
            panel.background = element_rect(fill = "transparent"), # Transparent background
            text = element_text(size = GenText_Sz),                # Text size
            plot.title = element_text(size = Tit_Sz),              # Plot title size (if present)
            plot.subtitle = element_text(hjust = 0.5),             # Plot subtitle size (if present)
            #panel.grid.major.y = element_line(size=0.25,
            #linetype=1,color = 'gray90'),                         # Adds horizontal lines
        # X-axis
            axis.text.x = element_text(vjust = 1),                 # Horizontal text
            axis.title.x = element_text(size = XTit_Sz),           # x-axis title text size
        # Y-axis
            axis.title.y = element_text(size = YTit_Sz),           # y-axis title text size
        # Legend
            legend.key.size = unit(1,"lines"),                     # Shrink legend boxes
            legend.position = "bottom",                            # Move legend to the bottom
            legend.justification = c(0.5,0.5),                     # Center the legend
            legend.text = element_text(size =Leg_Sz),              # Size of legend text
            legend.title=element_blank()) +                        # Remove legend title
      
      scale_x_continuous(expand=c(0,0),limits = c(YearMN,YearMX),breaks=seq(YearMN, YearMX, 1)) +
      
      scale_y_continuous(expand=c(0,0), limits=c(0,MX),breaks = pretty_breaks(6)) +
      
      labs(x = "Year", y = "Capacity (MW)", fill = "Resource",colour="Resource") +
    
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
  
  EvalPerc <- function(case) {
    # Filters for the desired case study
    data <- ResGroupYr %>%
      filter(Run_ID == case & Condition == "Average")# %>%

    # Filter the data by resource
    case_Time <- sim_filt(data)
    
    # Remove negative generation (Storage)
    case_Time$Output_MWH[case_Time$Output_MWH < 0] <- NA
    
    # Get year and format as a number (easier to plot!)
    case_Time$YEAR <- as.Date(case_Time$Time_Period)
    case_Time$YEAR <- as.numeric(format(case_Time$YEAR,"%Y"))
    
    # Get max and min year for plot
    YearMX<-max(case_Time$YEAR)-5
    YearMN<-min(case_Time$YEAR)
    
    # Filter to remove the final 5 years (as per AURORA, want to run 5 years past year of interest)
    data <- case_Time%>%
      filter(YEAR<=YearMX)
    
    case_Time %>%
      ggplot() +
      geom_area(aes(YEAR, Output_MWH, fill = ID,colour= ID),
                position = "fill", alpha = Plot_Trans, size=.5,color="black") +
      
      # geom_vline(xintercept = as.numeric(2035),
      #            linetype = "dashed", color = "black", size = 1) +
      theme_bw() +
      
      theme(text=element_text(family=Plot_Text)) +
    
      theme(
        # General Plot Settings
            panel.grid = element_blank(),
            # (t,r,b,l) margins, adjust to show full x-axis, default: (5.5,5.5,5.5,5.5)
            plot.margin = unit(c(6, 12, 5.5, 5.5), "points"),      # Plot margins
            panel.background = element_rect(fill = "transparent"), # Transparent background
            text = element_text(size = GenText_Sz),                # Text size
            plot.title = element_text(size = Tit_Sz),              # Plot title size (if present)
            plot.subtitle = element_text(hjust = 0.5),             # Plot subtitle size (if present)
            panel.grid.major.y = element_line(size=0.25,
            linetype=2,color = 'gray70'),                         # Adds horizontal lines
        # X-axis
            axis.text.x = element_text(vjust = 1),                 # Horizontal text
            axis.title.x = element_text(size = XTit_Sz),           # x-axis title text size
            # Y-axis
            axis.title.y = element_text(size = YTit_Sz),           # y-axis title text size
        # Legend
            legend.key.size = unit(1,"lines"),                     # Shrink legend boxes
            legend.position = "bottom",                            # Move legend to the bottom
            legend.justification = c(0.5,0.5),                     # Center the legend
            legend.text = element_text(size =Leg_Sz),              # Size of legend text
            legend.title=element_blank()) +                        # Remove legend title
      
      scale_x_continuous(expand=c(0,0),limits = c(YearMN,YearMX),breaks=seq(YearMN, YearMX, 1)) +
      scale_y_continuous(expand=c(0,0),
                         labels = scales::percent, 
                         breaks = sort(c(seq(0,1,length.out=5)))) +
      
      guides(fill = guide_legend(nrow = 2)) +
      labs(x = "Year", y = "Percentage of Generation", fill = "Resource",colour="Resource") +
      scale_fill_manual(values = colours2,drop = FALSE) 
      
  }

################################################################################  
## FUNCTION: Output_Comp 
## Plotting the capacity of resources individually for selected years as
## Side by side bar charts
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupYr - Resoruce group year output
##    Import - All imports
################################################################################
  
  Output_Comp <- function(case) {
    
    # Add imports for each year together 
    Imp <- Import_Yr %>%
      filter(Name == "WECC_Alberta") %>%
      filter(Run_ID == case) %>%
      mutate(Time_Period = format(.$Time_Period, format="%Y")) %>%
      select(ID, Time_Period, Output_MWH) %>%
      mutate(ID = "Import") 
    
    # Format the time period as a date
    Imp$Time_Period  <- as.Date(as.character(Imp$Time_Period), 
                                format = "%Y")
    
    # Filters for the desired case study and attached imports
    data <- ResGroupYr %>%
      filter(Run_ID == case & Condition == "Average") %>%
      select(ID, Time_Period, Output_MWH) %>%
      sim_filt(.) %>%
      rbind(.,Imp) 
    
    # re-order the bars for aesthetics
    data$ID <- factor(data$ID,levels=c("Coal-to-Gas", "Natural Gas","Natural Gas + CCS","Natual Gas and Hydrogen Blend","Hydrogen" ,"Import",
                                       "Hydro","Other","Wind", "Solar", "Storage","Coal","Cogeneration"),ordered=FALSE)
    
    # Get chosen years
    data$Time_Period <- format(data$Time_Period,format="%Y")
    
    # Filter by year and re-arrange in assending order
    data <- data %>%
      filter(Time_Period %in% Years2Disp) %>%
      group_by(Time_Period) %>%
      arrange(Output_MWH, .by_group = TRUE) %>%
      ungroup() %>%
      mutate(PlotOrder= row_number())
    
    # Set the max for the plot
    MX <- plyr::round_any(max(abs(data$Output_MWH)/1000000), 10, f = ceiling)
    
    # Plot
    data %>%
      ggplot() +
      aes(Time_Period, (Output_MWH/1000000), fill = reorder(ID, PlotOrder)) +
      geom_bar(position="dodge",stat="identity",alpha=Plot_Trans,'color'="black") +
      
      theme_bw() +
      
      theme(text=element_text(family=Plot_Text)) +
      
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(vjust = 1),
            axis.title.x = element_text(size = XTit_Sz),
            axis.title.y = element_text(size = YTit_Sz),
            plot.title = element_text(size = Tit_Sz),
            plot.subtitle = element_text(hjust = 0.5), 
            panel.background = element_rect(fill = NA),
            panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray90'),
            legend.key.size = unit(1,"lines"), #Shrink legend
            legend.position = "bottom",
            legend.justification = c(0.5,0.5),
            legend.title=element_blank(),
            text = element_text(size = 15)) +
      
      scale_y_continuous(expand=c(0,0),limits = c(0,MX),breaks=pretty_breaks(6)) +
      scale_x_discrete(drop=TRUE) +
      
      #  geom_text(aes(label = sprintf("%0.1f",Output_MWH/1000000)),
      #            position = position_dodge(width = 1),vjust=-0.5) +
      
      labs(x = "Year", y = "Annual Generation (TWh)", fill = "Resource") +
      
      guides(fill = guide_legend(nrow = 2)) +
      
      scale_fill_manual(values = colours4,drop = FALSE) 
    
  }
  
################################################################################  
## FUNCTION: AnnualDemand 
## Plot average demand in Zone
##
## INPUTS: 
##    case - Run_ID which you want to plot
##    input - ZoneYr, ZoneMn
## TABLES REQUIRED: 
##    Build - Build table describing all new resources
################################################################################
  AnnualDemand <- function(input,case) {
    
    # Select data
    ZPrice <- input %>%
      filter(Name == "WECC_Alberta") %>%
      filter(Condition == "Average") %>%
      filter(Run_ID==case) %>%
      subset(., select = c(Time_Period, Price, Baseline_Demand, Demand, Demand_Total,
                           Net_Load, Net_Load_Total, 
                           Smp_Max_Date_Time, Smp_Max_Demand, Smp_Max_Capacity, 
                           Run_ID, Imports, Exports))
    
    # Set the max and min for the plot Output axis (y), Set slightly above max (200 above)
    MX <- plyr::round_any(max(abs((ZPrice$Demand_Total/1000000))), 10, f = ceiling)
    
    # Title Formating
    
    
    ## PLOT WITH AREA PLOT
    
    ggplot() +
      
      # Add hourly load line (black line on the top)
      geom_line(data = ZPrice, 
                aes(x = Time_Period, y = (Demand_Total/1000000)), size=1.5, colour = "black") +
      
      scale_x_date(expand=c(0,0),date_labels = "%Y", breaks = "year") +
      
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
            text = element_text(size= 15)) +
      
      scale_y_continuous(expand=c(0,0), limits = c(0,MX), 
                         breaks = seq(0, MX, by = MX/4)) +
      
      labs(x = "Year", y = "Average Total Demand (TWh)") 
    
    
  }
  
################################################################################
## FUNCTION: CFcompare
## Compares capacity factor for two chosen years. Modified from Taylor Pawlenchuk.
##
## INPUTS: 
##    year1 - First year
##    Year2 - Seond year
##    case - case to see 
## TABLES REQUIRED: 
##    ResGroupHr_sub - Hourly resource group tables
################################################################################
  CFcompare <- function(year1, year2, case) {
    # Plots the capacity factor by technology for AESO and Sim
    # Like AESO Market Report 2021 Figure 15
    
    Sim <- ResGroupHr_sub %>%
      sim_filt1(.) %>%
      group_by(Report_Year, ID) %>%
      summarise(Cap = mean(Capacity_Factor))
    
    colnames(Sim) <- c("Year", "Plant_Type", "Cap")
    
    Sim$Plant_Type <- factor(Sim$Plant_Type, levels=c("Import","Solar","Wind", "Other", "Hydro", 
                                                      "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                                      "Blended  Simple Cycle","Blended  Combined Cycle",
                                                      "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                                                      "Coal-to-Gas", 
                                                      "Coal", "Cogeneration","Storage"))
    # Filter for year
    Sim <- Sim %>%
      filter(Year %in% c(year1,year2))
    
    sz <- 15
    
    ggplot() +
      geom_col(data = Sim, position = "dodge", alpha = Plot_Trans, width = 0.7,
               aes(x = Plant_Type, y = Cap,
                   fill=as.factor(Year)
               )) +
      #facet_grid(~Year) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            axis.title.x = element_blank(),
            axis.text = element_text(size = sz),
            axis.title = element_text(size = sz),
            plot.title = element_text(size = sz+2),
            legend.text = element_text(size = sz),
            panel.grid = element_blank(),
            legend.title = element_blank(),
            
            # For transparent background
            panel.background = element_rect(fill = "transparent"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.spacing = unit(1.5, "lines"),
            plot.background = element_rect(fill = "transparent", color = NA),
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
      ) +
      labs(y = "Average Annual Capacity Factor", 
           #title = "AESO Data vs Simulation",
           #subtitle = DB
      ) +
      scale_fill_manual(values = c("grey50","black")) +
      #scale_fill_manual(values = colours1) +
      #    scale_x_continuous(expand=c(0,0), 
      #                       limits = c(0,1.1),
      #                       labels = percent) +
      scale_y_continuous(expand=c(0,0),
                         limits = c(0,1),
                         breaks = seq(0,1, by = 0.2)
      )
  } 
  
################################################################################
## FUNCTION: CF_Annual
## Compares capacity factor for two chosen years. 
## Similar to plot seen on page 10 of AESOs net zero report dashboard. 
##
## INPUTS: 
##
##    case - case to see 
## TABLES REQUIRED: 
##    ResGroupHr_sub - Hourly resource group tables
################################################################################
  CF_Annual <- function(case) {
    
    CFData <- ResGroupHr_sub %>%
      sim_filt5(.) %>%
      group_by(Report_Year, ID) %>%
      summarise(Cap = mean(Capacity_Factor))%>%
      mutate(CF_perc=Cap*100)
    
    colnames(CFData) <- c("Year", "Plant_Type", "Cap_Factor","CF_perc")
    
    CFData$Plant_Type <- factor(CFData$Plant_Type, levels=c("Coal-to-Gas", "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                                       "Blended  Simple Cycle","Blended  Combined Cycle",
                                                       "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                                                       "Hydro", "Other","Wind", 
                                                       "Solar","Storage - Battery", "Storage - Pumped Hydro", "Storage - Compressed Air",
                                                       "Coal", "Cogeneration"))
    
    # Get max and min year for plot
    YearMX<-max(CFData$Year)-5 #Take off the last 5 years
    YearMN<-min(CFData$Year)
    
    # Filter to remove the final 5 years (as per AURORA, want to run 5 years past year of interest)
    CFData <- CFData%>%
      filter(Year<=YearMX)
    
    # Plot
    ggplot() +
      geom_line(data = CFData,
                aes(x = Year, y = CF_perc, colour = Plant_Type), 
                size = 1.5) +
      
      theme_bw() +
      
      # Changes the font type
      theme(text=element_text(family=Plot_Text)) +             
      
      theme(
        # General Plot Settings
            panel.grid = element_blank(),
            # (t,r,b,l) margins, adjust to show full x-axis, default: (5.5,5.5,5.5,5.5)
            plot.margin = unit(c(6, 12, 5.5, 5.5), "points"),      # Plot margins
            panel.background = element_rect(fill = "transparent"), # Transparent background
            text = element_text(size = GenText_Sz),                # Text size
            plot.title = element_text(size = Tit_Sz),              # Plot title size (if present)
            plot.subtitle = element_text(hjust = 0.5),             # Plot subtitle size (if present)
            panel.grid.major.y = element_line(size=0.25,
            linetype=1,color = 'gray90'),                         # Adds horizontal lines
        # X-axis
            axis.text.x = element_text(vjust = 1),                 # Horizontal text
            axis.title.x = element_text(size = XTit_Sz),           # x-axis title text size
        # Y-axis
            axis.title.y = element_text(size = YTit_Sz),           # y-axis title text size
        # Legend
            legend.key.size = unit(1,"lines"),                     # Shrink legend boxes
            legend.position = "right",                             # Move legend to the bottom
            legend.justification = c(0.5,0.5),                     # Center the legend
            legend.text = element_text(size =Leg_Sz),              # Size of legend text
            legend.title=element_blank()) +                        # Remove legend title
      
      # Set axis scales
      scale_x_continuous(expand=c(0,0),limits = c(YearMN,YearMX),breaks=seq(YearMN, YearMX, 1)) +
      scale_y_continuous(expand=c(0.01,0),limits = c(0,100),breaks=pretty_breaks(5)) +
      
      # Plot labels
      labs(x = "Year", y = "Annual Average Capacity Factor (%)", 
           colour="Plant_Type",caption = SourceDB) +

      # Legend color scheme
      scale_colour_manual(values = colours8,drop = FALSE) 
  } 
  
################################################################################  
## FUNCTION: Wind_Dur
## Plot wind duration curve in chosen years as % Hours vs Fleet Output (MW)
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupHr -Yearly resource group data
################################################################################
  Wind_Dur <- function(case) {
    
    # Bring in all data
    WindData <- ResGroupHr%>%
      filter(ID=="LTO_Wind",
             Condition=="Average",
             Run_ID == case,
             Report_Year<2036) %>%
      rename(Year=Report_Year)%>%
      group_by(Year) %>%
      # Get an empirical distribution for grouped data, 
      mutate(perc = 1-ecdf(Output)(Output)) %>%
      subset(select=c(Output,perc,date,Year))
    
    # Get the report year as a factor to plot
    WindData$Year <- as.factor(WindData$Year)
    
    # Set the max for the plot
    MX <- plyr::round_any(max(abs(WindData$Output)+17), 100, f = ceiling)
    
    # Plot
    ggplot() +
      geom_line(data = WindData, 
                aes(x = perc, y = Output, colour = Year), size = 1.25) +
      scale_color_viridis_d() +
      theme_bw() +
      
      theme(text=element_text(family=Plot_Text)) +
      
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(vjust = 1),
            axis.title.x = element_text(size = XTit_Sz),
            axis.title.y = element_text(size = YTit_Sz),
            plot.title = element_text(size = Tit_Sz),
            plot.subtitle = element_text(hjust = 0.5), 
            panel.background = element_rect(fill = NA),
            # panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
            #legend.key.size = unit(1,"lines"), #Shrink legend
            legend.position = "right",
            legend.justification = c(0.5,0.5),
            legend.title=element_blank(),
            text = element_text(size = 15)) +
      
      labs(x = "Percent of Hours per Year", y = "Fleet Output (MW)",colour="ID",linetype="ID",caption = paste(SourceDB,',')) +
      
      scale_y_continuous(expand = c(0, 0),limits = c(0,MX),breaks=seq(0, MX, by=1000)) +
      
      scale_x_continuous(expand=c(0,0.01),breaks=seq(0, 1, by=0.2),labels = percent) 
      
  }  
  
################################################################################  
## FUNCTION: Wind_DurNorm
## Plot wind duration curve in chosen years as % Hours vs output as Percent of max
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupHr -Yearly resource group data
################################################################################
  Wind_DurNorm <- function(case) {
  
  # Bring in all data
  WindData <- ResGroupHr%>%
    filter(ID=="LTO_Wind",
           Condition=="Average",
           Run_ID == case) %>%
    rename(Year=Report_Year)%>%
    group_by(Year) %>%
    mutate(Norm_Output=Output/(Capacity))%>%
    # Get an empirical distribution for grouped data, 
    mutate(perc = 1-ecdf(Norm_Output)(Norm_Output))%>%
    subset(select=c(Output,Norm_Output,Capacity_Factor,perc,date,Year))
  
  # filter out years of interest
  WindData <- WindData %>%
    filter(Year < 2036)
  
  # Get the report year as a factor to plot
  WindData$Year <- as.factor(WindData$Year)
  
  # Plot
  ggplot() +
    geom_line(data = WindData, 
              aes(x = perc, y = Norm_Output, colour = Year), size = 1.25) +
    scale_color_viridis_d() +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(vjust = 1),
          axis.title.x = element_text(size = XTit_Sz),
          axis.title.y = element_text(size = YTit_Sz),
          plot.title = element_text(size = Tit_Sz),
          plot.subtitle = element_text(hjust = 0.5), 
          panel.background = element_rect(fill = NA),
          # panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          #legend.key.size = unit(1,"lines"), #Shrink legend
          legend.position = "right",
          legend.justification = c(0.5,0.5),
          legend.title=element_blank(),
          text = element_text(size = 15)) +
         # plot.margin=unit(c(5,1,5,1), 'cm')) +
    
    labs(x = "Percent of Hours per Year", y = "Fleet Capacity Factor (Output/Capacity)", 
         colour="ID",linetype="ID",caption = paste(SourceDB,',')) +
    
    scale_x_continuous(expand = c(0, 0),limits = c(0,1),breaks=seq(0, 1, by=0.2),labels = percent) +
    
    scale_y_continuous(expand=c(0,0.05),breaks=seq(0, 1, by=0.2),labels = percent) 
  
  }
################################################################################
#
# COMBINED PLOTS SECTION
# Combined plots and supporting functions
#
################################################################################ 
  
################################################################################
## FUNCTION: week12
## Plots output for a single week given the case study. 
## Supporting function To be used in year of weeks function. 
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
    
    # Get any demand curtailment
    DSM <- ZoneHr_Avg%>%
      mutate(ID="Demand Curtailment")%>%
      subset(., select=c(ID,date,Demand_Side_Output,Run_ID))%>%
        rename(Output_MWH=Demand_Side_Output)

    # Filters for the desired case study from the resource groups
    data <- ResGroupHr_sub%>%
      sim_filt1(.) %>%
      subset(., select=-c(Report_Year,Capacity_Factor)) %>%
     # rbind(DSM) %>%
      rbind(.,Import) %>%
      filter(Run_ID == case) %>%
      filter(date >= wk_st) %>%
      filter(date <= wk_end)
    
    # Set levels to each category in order specified
    data$ID <- factor(data$ID, levels=c(
      #"Demand Curtailment",
      "Import","Solar","Wind", "Other", "Hydro", 
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
    MX <- round_any(max(ZPrice2$Baseline_Demand) + max(Expo2$Output_MWH)+1100,1000,f=ceiling) 
    
    # # Set the max and min for the plot Output axis (y), Set slightly above max (200 above)
    #MX <- plyr::round_any(max(abs(Max))+1111, 2000, f = ceiling)
    
    Mtitle=month.abb[month]
    
    ## PLOT WITH AREA PLOT
    
    ggplot() +
      geom_area(data = WK, aes(x = date, y = Output_MWH, fill = ID, colour=ID),alpha=Plot_Trans, size=.25,color='black') +
      
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
## FUNCTION: PrOt
## Plots pool price over one week of output data
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
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
  
################################################################################
## FUNCTION: PrOut
## Plots pool price over one week of output data with storage utilization.
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
################################################################################
  PrOut <- function(year,month,day,case) {
    plot_grid(Stor1(year,month,day,case),
              week_price(year,month,day,case) + theme(axis.title.x=element_blank(),
                                                      axis.text.x=element_blank()),
              Week1(year,month,day,case)+theme(legend.position ="none"), 
              ncol = 1, align="v", axis = "l",rel_heights = c(1,1,2.5))
  }

################################################################################
## FUNCTION: year_stor
## Plots output of storage and pool price for a single week given the case study.
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResHr - Filtered version of Resource Group Hour Table
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
  year_stor <- function(year,case) {
    
    
    # Create a graph for each month of the year
    p1 <- Stor2(year,01,08,case) +
      theme(axis.title.y.left=element_blank(),
            axis.title.y.right=element_blank(),
            axis.title.x=element_blank())
    
    
    p2 <- Stor2(year,02,08,case) +
      theme(axis.title.y.left=element_blank(),
            axis.title.y.right=element_blank(),
            axis.title.x=element_blank())
    
    p3 <- Stor2(year,03,08,case) +
      theme(axis.title.y.left=element_blank(),
            axis.title.y.right=element_blank(),
            axis.title.x=element_blank())
    
    p4 <- Stor2(year,04,08,case) +
      theme(axis.title.y.left=element_blank(),
            axis.title.y.right=element_blank(),
            axis.title.x=element_blank())
    
    p5 <- Stor2(year,05,08,case) +
      theme(axis.title.y.left=element_blank(),
            axis.title.y.right=element_blank(),
            axis.title.x=element_blank())
    
    p6 <- Stor2(year,06,08,case) +
      theme(axis.title.y.left=element_blank(),
            axis.title.y.right=element_blank(),
            axis.title.x=element_blank())
    
    p7 <- Stor2(year,07,08,case) +
      theme(axis.title.y.left=element_blank(),
            axis.title.y.right=element_blank(),
            axis.title.x=element_blank())
    
    p8 <- Stor2(year,08,08,case) +
      theme(axis.title.y.left=element_blank(),
            axis.title.y.right=element_blank(),
            axis.title.x=element_blank())
    
    p9 <- Stor2(year,09,08,case) +
      theme(axis.title.y.left=element_blank(),
            axis.title.y.right=element_blank(),
            axis.title.x=element_blank())
    
    p10 <- Stor2(year,10,08,case) +
      theme(axis.title.y.left=element_blank(),
            axis.title.y.right=element_blank(),
            axis.title.x=element_blank())
    
    p11 <- Stor2(year,11,08,case) +
      theme(axis.title.y.left=element_blank(),
            axis.title.y.right=element_blank(),
            axis.title.x=element_blank())
    
    p12 <- Stor2(year,12,08,case) +
      theme(axis.title.y.left=element_blank(),
            axis.title.y.right=element_blank(),
            axis.title.x=element_blank())
    
    # Plot Labels
    yleft <- textGrob("Storage Output (MWh)", rot = 90, gp = gpar(fontsize = 15,col=cOL_STORAGE))
    yright <- textGrob("Wholesale Pool Price ($/MWh)", rot = -90, gp = gpar(fontsize = 15))
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
               label = paste("Year:",year,", Database:",SourceDB)) + 
      theme_void()
    
    #Create a big window
    windows(18,12)
    
    # Arrange all the plots
    grid.arrange(plot_grid(p1, p2, p3, p4, ncol=4, align="v", axis = "l", rel_widths = c(1,1,1,1)),
                 plot_grid(p5,p6, p7, p8, ncol=4, align="v", axis = "l", rel_widths = c(1,1,1,1)),
                 plot_grid(p9, p10, p11, p12, ncol=4, align="v", axis = "l", rel_widths = c(1,1,1,1)),
                 plot_grid(xtitle),
                 plot_grid(xsubtitle),
                 ncol=1,nrow=5, 
                 heights=c(1, 1,1,0.1,0.1),
                 left=yleft,
                 right=yright)
    
    
  }