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
    
    # Get exports
    TRADE <- Export %>%
      filter(Run_ID == case)%>%
      mutate(Output_MWH=Output_MWH*-1+Import$Output_MWH,
             ID="Trade")
    
    # Filters for the desired case study from the resource groups
    data <- ResGroupHr_sub %>%
      sim_filt1(.) %>%
      subset(., select=-c(Report_Year,Capacity_Factor)) %>%
      rbind(.,TRADE) %>%
      filter(Run_ID == case) %>%
      filter(date >= wk_st) %>%
      filter(date <= wk_end)
    
    data$Output_MWH[data$Output_MWH<0.001] <-0
    
    # Set levels to each category in order specified
    data$ID <- factor(data$ID, levels=c("Solar","Wind","Hydro","Other", 
                                        "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                        "Blended  Simple Cycle","Blended  Combined Cycle",
                                        "Natural Gas Combined Cycle + CCS","Natural Gas Simple Cycle", "Natural Gas Combined Cycle","Coal-to-Gas", 
                                        "Coal", "Cogeneration","Storage","Trade"))
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
      
      theme(axis.text.x = element_text(vjust = 1,color="black"),
            axis.text.y = element_text(color="black"),
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
                         breaks = seq(0, MX, by = MX/4),labels=comma) +
      guides(fill = guide_legend(nrow = 2)) +
      
      labs(x = "Date", y = "Output (MWh)", fill = "Resource", colour = "Resource",
           title=year,caption=SourceDB) +
      
      #Add colour
      scale_fill_manual(values = colours1) 
  }

################################################################################
## FUNCTION: Day1
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
  
  Day1 <- function(year, month, day, case) {
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
      
      theme(axis.text.x = element_text(vjust = 1,color="black"),
            axis.text.y = element_text(color="black"),
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
                         breaks = seq(0, MX, by = MX/4),labels=comma) +
      
      labs(title=paste("Resource Output, ",day_report),x = "Date", y = "Output (MWh)", fill = "Resource",colour = "Resource",caption=SourceDB ) +
      
      #Add colour
      scale_fill_manual(values = colours1) +
      
      # Make outline the same as fill colors
      scale_colour_manual(values = colours1)
  }
  
  
################################################################################
## FUNCTION: Day2
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
  Day2 <- function(year, month, day, MX, case) {
    
    # Filters for the desired case study from the resource groups. Format WkDay as day only (no time)
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
    
    levels(data$ID)<-c("Import","Solar","Wind", "Other", "Hydro", 
                       "H2SC","H2CC",
                       "Blended  Simple Cycle","Blended  Combined Cycle",
                       "SCCT", "NGCC+CCS","NGCC", 
                       "Coal-to-Gas", 
                       "Coal", "Cogeneration","Storage")
    
    # Get date to report
    day_filt <-as.POSIXct(paste(year,month,day," 00:00:00", sep = "-"),tz = "MST")
    day_report <- paste(month.abb[month]," ",day,", ",year," (",wday(day_filt,label=TRUE),")",sep="")
    
    ## SELECT A SINGLE DAY
    # Select only a single day from the zone Hourly, and Export data
    DY <- HrTime(data,year,month,day)
    
    ZPrice <- HrTime(ZoneHr_Avg,year,month,day) %>%
      filter(Run_ID == case)
    
    Expo <- HrTime(Export,year,month,day) %>%
      filter(Run_ID == case)
    
    
    ## PLOT WITH AREA PLOT
    
    ggplot() +
      geom_area(data = DY, aes(x = date, y = Output_MWH, fill = ID), colour = "black", 
                alpha=Plot_Trans, size=0.5) +
      
      # Add hourly load line (black line on the top)
      geom_line(data = ZPrice, 
                aes(x = date, y = Demand,colour = "Demand"), size=1.5, colour = "black") +
      
      # Set the theme for the plot
      theme_bw() +
      theme(legend.position = "bottom") +
      
      theme(text=element_text(family=Plot_Text)) +
      
      theme(axis.text = element_text(color="black"),
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
            legend.position = 'right',
            legend.text = element_text(size=14),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
            axis.title.x = element_text(size=20),
            axis.title.y = element_text(size=26),
            text = element_text(size= 20)
      ) +
      
      guides(fill = guide_legend(ncol = 1)) +
      
      scale_y_continuous(expand=c(0,0), limits = c(0,MX), 
                         breaks = seq(0, MX, by = MX/4),labels=comma) +
      
      scale_x_datetime(expand=c(0,0),date_labels = "%H:%M",date_breaks = "4 hours") +
      
      labs(title=SourceDB,x = day_report, y = "Output (MWh)", fill = "Resource",colour = "Resource" ) +
      
      #Add colour, keep all in legend for now
      scale_fill_manual(values = colours1c,drop=FALSE) 
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
            axis.text=element_text(color="black"),
            plot.title = element_text(size=Tit_Sz),
            legend.title = element_blank(),
            legend.position = "none",
            axis.title.x = element_text(size=XTit_Sz,face='bold',vjust = -1),
            axis.title.y = element_text(size=YTit_Sz,face='bold'),
            text = element_text(size= Overall_Sz),
           # panel.grid.major.y = element_line(size=0.25,linetype=2,color = 'gray90'),                         # Adds horizontal lines
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
    ylimPrice<-c(-1020,1020)
    
    b <- diff(ylimStor)/diff(ylimPrice)
    a <- ylimStor[1] - b*ylimPrice[1] 
    
    # PLOT DATA  
    ggplot() +
      geom_area(data = WK_Stor, aes(x = date, y = Output_MWH,fill="Storage"), 
                alpha=Plot_Trans, size=.5, colour="black") +
      geom_hline(yintercept=0, color = "black", size=0.5)+
      geom_line(data = WK_Price, aes(x = date, y = a + Price*b), 
                size = 1.25, colour = "black",linetype=1) +
      
      # Set up plot look and feel
      theme_bw() +
      theme(text=element_text(family=Plot_Text)) +
      
      # Set the theme for the plot
      theme(panel.grid = element_blank(),
            axis.text.x=element_text(vjust = 0),
            plot.title = element_text(size=Overall_Sz),
            legend.title = element_blank(),
            legend.position = "none",
            axis.text=element_text(color="black"),
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
                         breaks = seq(0,1000,by=250),
                         labels=comma)) +
      
      theme(axis.ticks.y.left = element_line(color = "black"),
            axis.text.y.left = element_text(color = "black"), 
            axis.title.y.left = element_text(color = "black")) +
      
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
    
    # Color definition
    C_to_Fill<-colours4

    # Get imports
    Imp <- Import_Yr %>%
      filter(Name == "WECC_Alberta") %>%
      filter(Run_ID == case) %>%
      select(ID, Time_Period, Output_MWH) %>%
      mutate(ID = "Import") 

    # Filters for the desired case study
    data <- ResGroupYr %>%
      filter(Run_ID == case & Condition == "Average") %>%
      select(ID, Time_Period, Output_MWH,Capacity) %>%
      sim_filtg(.) %>%
      select(ID, Time_Period, Output_MWH) %>%
      rbind(.,Imp)
    
    # Add imports to factor list
    data$ID<-fct_relevel(data$ID, "Import")
    
    # Get year and format as a number (easier to plot!)
    data$YEAR <- as.Date(data$Time_Period)
    data$YEAR <- as.numeric(format(data$YEAR,"%Y"))

    # Get max and min year for plot
    YearMX<-MaxYrStudy #Take off the last 5 years
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
            plot.title = element_text(size =GenText_Sz ),              # Plot title size (if present)
            plot.subtitle = element_text(hjust = 0.5),             # Plot subtitle size (if present)
            #panel.grid.major.y = element_line(size=0.25,
            #linetype=1,color = 'gray90'),  
            # Adds horizontal lines
            plot.caption=element_text(size =30), 
        # X-axis
            axis.text.x = element_text(vjust = 1,colour = "black"),                 # Horizontal text
            #axis.title.x = element_text(size = GenText_Sz),           # x-axis title text size
            axis.title.x = element_blank(),
        # Y-axis
            axis.title.y = element_text(size = GenText_Sz+6),           # y-axis title text size
            axis.text.y = element_text(colour = "black"),                 # Horizontal text
        
        # Legend
            legend.key.size = unit(1,"lines"),                     # Shrink legend boxes
            legend.position = "right",                            # Move legend to the bottom
            legend.justification = c(0.5,0.5),                     # Center the legend
            legend.text = element_text(size = GenText_Sz-6),              # Size of legend text
            legend.title=element_blank()) +                        # Remove legend title
      
      # Set axis scales
      scale_x_continuous(expand=c(0,0),limits = c(YearMN,YearMX),breaks=seq(YearMN, YearMX, 1)) +
      scale_y_continuous(expand=c(0,0),limits = c(0,MX),breaks=pretty_breaks(6)) +
      
      #guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
      # Plot labels
      labs(x = "Year", y = "Annual Generation (TWh)", fill = "Resource",colour="Resource",caption = SourceDB) +

      
      # Legend color scheme
      scale_fill_manual(values = C_to_Fill,drop = TRUE) 
    
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
    
    #Set color
    C_to_Fill<-colours4
    
    # Filters for the desired case study
    data <- ResGroupYr %>%
      filter(Run_ID == case & Condition == "Average") %>%
      select(ID, Time_Period,Output_MWH,Capacity) %>%
      sim_filtg(.)
    
    # Get year and format as a number (easier to plot!)
    data$YEAR <- as.Date(data$Time_Period)
    data$YEAR <- as.numeric(format(data$YEAR,"%Y"))
    
    # Get max and min year for plot
    YearMX<-MaxYrStudy #Take off the last 5 years
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
            plot.title = element_text(size = GenText_Sz),              # Plot title size (if present)
            plot.subtitle = element_text(hjust = 0.5),             # Plot subtitle size (if present)
            #panel.grid.major.y = element_line(size=0.25,
            #linetype=1,color = 'gray90'),                         # Adds horizontal lines
        # X-axis
            axis.text.x = element_text(vjust = 1,color="black"),                 # Horizontal text
            #axis.title.x = element_text(size = GenText_Sz),           # x-axis title text size
            axis.title.x = element_blank(),        # Y-axis
            axis.title.y = element_text(size = GenText_Sz+6),           # y-axis title text size
            axis.text.y = element_text(color="black"),
        # Legend
            legend.key.size = unit(1,"lines"),                     # Shrink legend boxes
            legend.position = "right",                            # Move legend to the bottom
            legend.justification = c(0.5,0.5),                     # Center the legend
            legend.text = element_text(size =GenText_Sz-6),              # Size of legend text
            legend.title=element_blank()) +                        # Remove legend title
      
      scale_x_continuous(expand=c(0,0),limits = c(YearMN,YearMX),breaks=seq(YearMN, YearMX, 1)) +
      
      scale_y_continuous(expand=c(0,0), limits=c(0,MX),breaks = pretty_breaks(6),labels=comma) +
      
      labs(x = "Year", y = "Capacity (MW)", fill = "Resource",colour="Resource") +
    
      #guides(fill = guide_legend(nrow = 1)) +
      
      scale_fill_manual(values = C_to_Fill,drop = TRUE)
    
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
    
    #Set color
    C_to_Fill<-colours4
    
    # Filters for the desired case study
    data <- ResGroupYr %>%
      filter(Run_ID == case & Condition == "Average") %>%
      select(ID, Time_Period,Output_MWH,Capacity) 
    
    # Filter the data by resource
    case_Time <- sim_filtg(data)
    
    # Remove negative generation (Storage)
    case_Time$Output_MWH[case_Time$Output_MWH < 0] <- NA
    
    # Get year and format as a number (easier to plot!)
    case_Time$YEAR <- as.Date(case_Time$Time_Period)
    case_Time$YEAR <- as.numeric(format(case_Time$YEAR,"%Y"))
    
    # Get max and min year for plot
    YearMX<-MaxYrStudy
    YearMN<-min(case_Time$YEAR)
    
    # Filter to remove the final 5 years (as per AURORA, want to run 5 years past year of interest)
    data <- case_Time%>%
      filter(YEAR<=YearMX)
    
    # Get max
    FinalYr<-case_Time %>%
      filter(YEAR==YearMX)%>%
      summarise(TotalGen=sum(Output_MWH))
    FinalYrMax<-as.numeric(max(FinalYr))
    
    case_Time %>%
      ggplot() +
      aes(YEAR, Output_MWH)+
      geom_area(aes(fill = ID,colour= ID),
                position = "fill", alpha = Plot_Trans, size=.5,color="black") +
      
      # geom_text(aes(label = scales::percent(Output_MWH/FinalYrMax)),
      #           data=filter(case_Time,YEAR==YearMX),hjust=1,check_overlap =TRUE)+
      # 
      theme_bw() +
      
      theme(text=element_text(family=Plot_Text)) +
    
      theme(
        # General Plot Settings
            panel.grid = element_blank(),
            # (t,r,b,l) margins, adjust to show full x-axis, default: (5.5,5.5,5.5,5.5)
            plot.margin = unit(c(6, 12, 5.5, 5.5), "points"),      # Plot margins
            panel.background = element_rect(fill = "transparent"), # Transparent background
            text = element_text(size = GenText_Sz),                # Text size
            plot.title = element_text(size = GenText_Sz),              # Plot title size (if present)
            plot.subtitle = element_text(hjust = 0.5),             # Plot subtitle size (if present)
            panel.grid.major.y = element_line(size=0.25,
            linetype=2,color = 'gray70'),                         # Adds horizontal lines
        # X-axis
            axis.text.x = element_text(vjust = 1,color="black"),                 # Horizontal text
            #axis.title.x = element_text(size = GenText_Sz),           # x-axis title text size
            axis.title.x = element_blank(),            # Y-axis
            axis.title.y = element_text(size = GenText_Sz+6),           # y-axis title text size
            axis.text.y=element_text(color="black"),
        # Legend
            legend.key.size = unit(1,"lines"),                     # Shrink legend boxes
            legend.position = "right",                            # Move legend to the bottom
            legend.justification = c(0.5,0.5),                     # Center the legend
            legend.text = element_text(size =GenText_Sz-6),              # Size of legend text
            legend.title=element_blank()) +                        # Remove legend title
      
      scale_x_continuous(expand=c(0,0),limits = c(YearMN,YearMX),breaks=seq(YearMN, YearMX, 1)) +
      scale_y_continuous(expand=c(0,0),
                         labels = scales::percent, 
                         breaks = sort(c(seq(0,1,length.out=5)))) +
      
      #guides(fill = guide_legend(nrow = 1)) +
      labs(x = "Year", y = "Percentage of Total Generation", fill = "Resource",colour="Resource") +
      scale_fill_manual(values = C_to_Fill,drop = TRUE) 
      
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
      filter(Time_Period %in% Years2Disp,
             !ID == "Storage") %>%
      group_by(Time_Period) %>%
      arrange(Output_MWH, .by_group = TRUE) %>%
      ungroup() %>%
      mutate(PlotOrder= row_number())
    
    # Set the max for the plot
    MX <- plyr::round_any(max(abs(data$Output_MWH)/1000000), 5, f = ceiling)
    
    # Plot
    data %>%
      ggplot() +
      aes(Time_Period, (Output_MWH/1000000), fill = reorder(ID, PlotOrder)) +
      geom_bar(position="dodge",stat="identity",alpha=Plot_Trans,'color'="black") +
      
      theme_bw() +
      
      theme(text=element_text(family=Plot_Text)) +
      
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(vjust = 1,color="black"),
            #axis.title.x = element_text(size = GenText_Sz),
            axis.title.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_text(size = GenText_Sz+6),
            axis.text.y=element_text(color="black"),
            plot.title = element_text(size = GenText_Sz),
            plot.subtitle = element_text(hjust = 0.5), 
            panel.background = element_rect(fill = NA),
            #panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray90'),
            legend.key.size = unit(1,"lines"), #Shrink legend
            legend.position = "bottom",
            legend.justification = c(0.5,0.5),
            legend.title=element_blank(),
            text = element_text(size = GenText_Sz)) +
      
      scale_y_continuous(expand=c(0,0),limits = c(0,MX),breaks=pretty_breaks(6)) +
      scale_x_discrete(drop=TRUE) +
      
      #  geom_text(aes(label = sprintf("%0.1f",Output_MWH/1000000)),
      #            position = position_dodge(width = 1),vjust=-0.5) +
      
      labs(x = "Year", y = "Annual Generation (TWh)", fill = "Resource") +
      
      guides(fill = guide_legend(nrow = 2)) +
      
      scale_fill_manual(values = colours4,drop = TRUE) 
    
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
      
      theme(plot.title = element_text(size= GenText_Sz)) +
      
      theme(axis.text.x = element_text(vjust = 1),
            axis.title.x = element_text(size= GenText_Sz+6),
            axis.title.y = element_text(size= GenText_Sz+6),
            panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent", color = NA),
            legend.title=element_blank(),
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.background = element_rect(fill='transparent',colour ='transparent'),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
            legend.key.size = unit(1,"lines"), #Shrink legend
            legend.position = "bottom",
            text = element_text(size= GenText_Sz)) +
      
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
      summarise(Cap = mean(Capacity_Factor))%>%
      filter(ID!="Storage")
    
    colnames(Sim) <- c("Year", "Plant_Type", "Cap")
    
    Sim$Plant_Type <- factor(Sim$Plant_Type, levels=c("Import","Solar","Wind","Hydro","Other", 
                                                                 "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                                                 "Blended  Simple Cycle","Blended  Combined Cycle",
                                                                 "Natural Gas Combined Cycle + CCS","Natural Gas Simple Cycle", "Natural Gas Combined Cycle","Coal-to-Gas", 
                                                                 "Coal", "Cogeneration"))
    # Filter for year
    Sim <- Sim %>%
      filter(Year %in% c(year1,year2))
    ggplot() +
      geom_col(data = Sim, position = "dodge", alpha = Plot_Trans, width = 0.8,color="black",
               aes(x = Plant_Type, y = Cap,
                   fill=as.factor(Year)
               )) +
      
      #facet_grid(~Year) +
      theme_bw() +
      
      theme(text=element_text(family=Plot_Text)) +
      
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            axis.title.x = element_blank(),
            axis.text = element_text(size = GenText_Sz,color="black"),
            axis.title = element_text(size = GenText_Sz+6 ,color="black"),
            plot.title = element_text(size=GenText_Sz),
            legend.text = element_text(size=GenText_Sz-6),
            panel.grid = element_blank(),
            legend.title = element_blank(),
            
            # For transparent background
            panel.background = element_rect(fill = "transparent"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.spacing = unit(1.5, "lines"),
            #plot.background = element_rect(fill = "transparent", color = NA),
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.background = element_rect(fill='transparent'),
            legend.position = c(.95, .925),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
      ) +
      labs(y = "Average Annual Capacity Factor", 
           #title = "AESO Data vs Simulation",
           #subtitle = DB
      ) +
      scale_fill_manual(values = c("grey70","black")) +
      #scale_fill_manual(values = colours1) +
      #    scale_x_continuous(expand=c(0,0), 
      #                       limits = c(0,1.1),
      #                       labels = percent) +
      scale_y_continuous(expand=c(0,0),
                         limits = c(0,1),
                         labels=percent,
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
      mutate(CF_perc=Cap)
    
    colnames(CFData) <- c("Year", "Plant_Type", "Cap_Factor","CF_perc")
    
    CFData$Plant_Type <- factor(CFData$Plant_Type, levels=c("Coal-to-Gas", "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                                       "Blended  Simple Cycle","Blended  Combined Cycle",
                                                       "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                                                       "Hydro", "Other","Wind", 
                                                       "Solar","Storage - Battery", "Storage - Pumped Hydro", "Storage - Compressed Air",
                                                       "Coal", "Cogeneration"))
    
    # Get max and min year for plot
    YearMX<-MaxYrStudy #Take off the last 5 years
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
            plot.title = element_text(size = GenText_Sz),              # Plot title size (if present)
            plot.subtitle = element_text(hjust = 0.5),             # Plot subtitle size (if present)
            #panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray90'), # Adds horizontal lines
            plot.caption = element_text(size = 30),
        # X-axis
            axis.text.x = element_text(vjust = 1,color="black"),                 # Horizontal text
            axis.title.x = element_blank(),                         # y-axis title text size
            #axis.title.x = element_text(size = XTit_Sz),           # x-axis title text size
        # Y-axis
            axis.title.y = element_text(size = GenText_Sz+6),           # y-axis title text size
            axis.text.y=element_text(color="black"),
        # Legend
            legend.key.size = unit(1,"lines"),                     # Shrink legend boxes
            legend.position = "right",                             # Move legend to the bottom
            legend.justification = c(0.5,0.5),                     # Center the legend
            legend.text = element_text(size =GenText_Sz-6),              # Size of legend text
            legend.title=element_blank()) +                        # Remove legend title
      
      # Set axis scales
      scale_x_continuous(expand=c(0,0),limits = c(YearMN,YearMX),breaks=seq(YearMN, YearMX, 1)) +
      scale_y_continuous(expand=c(0.01,0),limits = c(0,1),breaks=pretty_breaks(5),labels=percent) +
      
      # Plot labels
      labs(x = "Year", y = "Annual Average Capacity Factor", 
           colour="Plant_Type",caption = SourceDB) +

      # Legend color scheme
      scale_colour_manual(values = colours8,drop = FALSE) 
  } 
  
################################################################################
## FUNCTION: CF_CER_Res
## Compares capacity factor for two chosen years. 
## Similar to plot seen on page 10 of AESOs net zero report dashboard. 
##
## INPUTS: 
##
##    case - case to see 
## TABLES REQUIRED: 
##    ResGroupHr_sub - Hourly resource group tables
################################################################################
  CF_CER_Res <- function(case) {
    
    # Fitler resources impacted by CER. Add year of CER based on active constraints and remove others
    
    data <- ResYr%>%
      sim_filt3(.) %>% #Filter to rename fuels
      subset(., select=c(YEAR,Time_Period,End_Date,Beg_Date,Name,Condition,Capacity,Run_ID,Primary_Fuel,Capacity_Factor,Total_Hours_Run,Active_Constraints)) %>%
      filter(Run_ID == case,
             Condition == "Average",
             Primary_Fuel %in% c("Coal-to-Gas","Natural Gas Simple Cycle","Natural Gas Combined Cycle"),
             YEAR <= MaxYrStudy) %>%
      mutate(Time_Period=as.numeric(Time_Period),
             YEAR=as.numeric(YEAR),
             CER_year=if_else(grepl('2035',Active_Constraints)==TRUE,"CER 2035",
                              if_else(grepl('2036',Active_Constraints)==TRUE,"CER 2036",
                                      if_else(grepl('2040',Active_Constraints)==TRUE,"CER 2040",
                                              if_else(grepl('2044',Active_Constraints)==TRUE,"CER 2044",
                                                      if_else(grepl('2045',Active_Constraints)==TRUE,"CER 2045","CER NA")))))) %>%
      filter(!CER_year == "CER NA")
    
    
    # Set levels to each category in order specified
    data$Primary_Fuel <- factor(data$Primary_Fuel, levels=c("Cogeneration","Coal-to-Gas", "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                                            #"Blended  Simple Cycle","Blended  Combined Cycle",
                                                            "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS",
                                                            "Natural Gas Combined Cycle CCS Retrofit","Natural Gas Combined Cycle", 
                                                            "Hydro", "Other",
                                                            "Wind", "Solar", 
                                                            "Storage - Battery", "Storage - Compressed Air", "Storage - Pumped Hydro",
                                                            "Nuclear"
    ) )

    
    ggplot(data)+
      geom_line(aes(x = YEAR, y = Capacity_Factor, colour = CER_year,group=Name), 
                size = 1.5) +

      theme_bw() +
      
      # Changes the font type
      theme(text=element_text(family=Plot_Text)) +             
      
      geom_hline(yintercept=0.05, color = "darkred",size=0.25,linetype=2) +
      geom_text(data = data.frame(x=1,y=1),aes(2023.5,0.05,label = "Capacity Factor Constraint = 5%"), vjust = -1,hjust=0, colour="darkred",size=5)  +
      
      theme(
        # General Plot Settings
        panel.grid = element_blank(),
        # (t,r,b,l) margins, adjust to show full x-axis, default: (5.5,5.5,5.5,5.5)
        plot.margin = unit(c(6, 12, 5.5, 5.5), "points"),      # Plot margins
        panel.background = element_rect(fill = "transparent"), # Transparent background
        text = element_text(size = GenText_Sz),                # Text size
        plot.title = element_text(size = GenText_Sz),              # Plot title size (if present)
        plot.subtitle = element_text(hjust = 0.5),             # Plot subtitle size (if present)
        #panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray90'), # Adds horizontal lines
        plot.caption = element_text(size = GenText_Sz-10),
        # X-axis
        axis.text.x = element_text(vjust = 1,color="black"),                 # Horizontal text
        axis.title.x = element_blank(),                         # y-axis title text size
        #axis.title.x = element_text(size = XTit_Sz),           # x-axis title text size
        # Y-axis
        axis.title.y = element_text(size = GenText_Sz+6),           # y-axis title text size
        axis.text.y=element_text(color="black"),
        # Legend
        legend.key.size = unit(1,"lines"),                     # Shrink legend boxes
        legend.position = "bottom",                             # Move legend to the bottom
        legend.justification = c(0.5,0.5),                     # Center the legend
        legend.text = element_text(size =GenText_Sz-6),              # Size of legend text
        legend.title=element_blank()) +                        # Remove legend title
      
      guides(color = guide_legend(nrow = 1)) +

      # Set axis scales
      scale_x_continuous(expand=c(0,0),limits=c(2023,MaxYrStudy)) +
      scale_y_continuous(expand=c(0,0),limits = c(0,1),breaks=pretty_breaks(5),labels=percent) +
      
      # Plot labels
      labs(x = "Year", y = "Average Capacity Factor", 
           colour="Plant_Type",caption = SourceDB) +
      
      # Legend color scheme
      scale_colour_manual(values = c("CER 2035"='black',"CER 2036"='grey30',"CER 2040"='gray60',"CER 2044"='grey75','CER 2045'='grey90'),drop = FALSE)  
  } 
  
################################################################################
## FUNCTION: Hours_CER_Res
## Show hours of opperation for each resource in each year.  
##
## INPUTS: 
##
##    case - case to see 
## TABLES REQUIRED: 
##    ResGroupHr_sub - Hourly resource group tables
################################################################################
  Hours_CER_Res <- function(case) {
    
    # Plots the capacity factor by technology for AESO and Sim
    # Like AESO Market Report 2021 Figure 15
    
    data <- ResYr%>%
      sim_filt3(.) %>% #Filter to rename fuels
      subset(., select=c(YEAR,Time_Period,End_Date,Beg_Date,Name,Condition,Capacity,Run_ID,Primary_Fuel,Capacity_Factor,Total_Hours_Run,Active_Constraints)) %>%
      filter(Run_ID == case,
             Condition == "Average",
             Primary_Fuel %in% c("Coal-to-Gas","Natural Gas Simple Cycle","Natural Gas Combined Cycle"),
             Capacity >= 25,
             YEAR <= MaxYrStudy)  %>%
      mutate(Time_Period=as.numeric(Time_Period),
             YEAR=as.numeric(YEAR),
             CER_year=if_else(grepl('2035',Active_Constraints)==TRUE,"CER 2035",
                              if_else(grepl('2036',Active_Constraints)==TRUE,"CER 2036",
                                      if_else(grepl('2040',Active_Constraints)==TRUE,"CER 2040",
                                              if_else(grepl('2044',Active_Constraints)==TRUE,"CER 2044",
                                                      if_else(grepl('2045',Active_Constraints)==TRUE,"CER 2045","CER NA")))))) %>%
      filter(!CER_year == "CER NA")
    
    
    
    # Set levels to each category in order specified
    data$Primary_Fuel <- factor(data$Primary_Fuel, levels=c("Cogeneration","Coal-to-Gas", "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                                            #"Blended  Simple Cycle","Blended  Combined Cycle",
                                                            "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS",
                                                            "Natural Gas Combined Cycle CCS Retrofit","Natural Gas Combined Cycle", 
                                                            "Hydro", "Other",
                                                            "Wind", "Solar", 
                                                            "Storage - Battery", "Storage - Compressed Air", "Storage - Pumped Hydro",
                                                            "Nuclear"
    ) )
    
    hrs_max<-max(data$Total_Hours_Run)+100
    
    ggplot(data)+
      geom_line(aes(x = YEAR, y = Total_Hours_Run, colour = CER_year,group=Name), 
                size = 1.5) +
      
      theme_bw() +
      
      # Changes the font type
      theme(text=element_text(family=Plot_Text)) +             
      
      geom_hline(yintercept=450, color = "darkred",size=0.25,linetype=2) +
      geom_text(data = data.frame(x=1,y=1),aes(2023.5,450,label = "Maximum Hours Constraint = 450"), vjust = -1,hjust=0, colour="darkred",size=5)  +
      
      theme(
        # General Plot Settings
        panel.grid = element_blank(),
        # (t,r,b,l) margins, adjust to show full x-axis, default: (5.5,5.5,5.5,5.5)
        plot.margin = unit(c(6, 12, 5.5, 5.5), "points"),      # Plot margins
        panel.background = element_rect(fill = "transparent"), # Transparent background
        text = element_text(size = GenText_Sz),                # Text size
        plot.title = element_text(size = GenText_Sz),              # Plot title size (if present)
        plot.subtitle = element_text(hjust = 0.5),             # Plot subtitle size (if present)
        #panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray90'), # Adds horizontal lines
        plot.caption = element_text(size = GenText_Sz-10),
        # X-axis
        axis.text.x = element_text(vjust = 1,color="black"),                 # Horizontal text
        axis.title.x = element_blank(),                         # y-axis title text size
        #axis.title.x = element_text(size = XTit_Sz),           # x-axis title text size
        # Y-axis
        axis.title.y = element_text(size = GenText_Sz+6),           # y-axis title text size
        axis.text.y=element_text(color="black"),
        # Legend
        legend.key.size = unit(1,"lines"),                     # Shrink legend boxes
        legend.position = "right",                             # Move legend to the bottom
        legend.justification = c(0.5,0.5),                     # Center the legend
        legend.text = element_text(size =GenText_Sz-6),              # Size of legend text
        legend.title=element_blank()) +                        # Remove legend title
      
      # Set axis scales
      scale_x_continuous(expand=c(0,0),limits=c(2023,MaxYrStudy)) +
      scale_y_continuous(expand=c(0,0),limits=c(0,hrs_max),breaks=pretty_breaks(5)) +
      
      # Plot labels
      labs(x = "Year", y = "Total Hours Run", 
           colour="Plant_Type",caption = SourceDB) +
      
      # Legend color scheme
      scale_colour_manual(values = c("CER 2035"='black',"CER 2036"='grey30',"CER 2040"='gray60',"CER 2044"='grey75','CER 2045'='grey90'),drop = FALSE)  
  }
  
################################################################################
## FUNCTION: CF_CER_groups
## Compares capacity factor for two chosen years. 
## Similar to plot seen on page 10 of AESOs net zero report dashboard. 
##
## INPUTS: 
##
##    case - case to see 
## TABLES REQUIRED: 
##    ResGroupHr_sub - Hourly resource group tables
################################################################################
  CF_CER_groups <- function(case) {
    
    # Plots the capacity factor by technology for AESO and Sim
    # Like AESO Market Report 2021 Figure 15
    
    data <- ResGroupYr%>%
      filter(Run_ID == case,
             Condition == "Average",
             ID %in% c("CER_2035","CER_2036","CER_2044","CER_2045"),
             Report_Year <= MaxYrStudy) %>%
      mutate(Time_Period=as.numeric(Time_Period)) %>%
      subset(., select=c(Report_Year,ID,Name,Capacity,Output_MWH,Capacity_Factor,Total_Hours_Run))
    
    
    ggplot(data)+
      geom_line(aes(x = Report_Year, y = Capacity_Factor, colour = ID), 
                size = 1.5) +
      
      theme_bw() +
      
      # Changes the font type
      theme(text=element_text(family=Plot_Text)) +             
      
      geom_hline(yintercept=0.05, color = "darkred",size=0.25,linetype=2) +
      geom_text(data = data.frame(x=1,y=1),aes(2023.5,0.05,label = "Capacity Factor Constraint = 5%"), vjust = -1,hjust=0, colour="darkred",size=5)  +
      
      theme(
        # General Plot Settings
        panel.grid = element_blank(),
        # (t,r,b,l) margins, adjust to show full x-axis, default: (5.5,5.5,5.5,5.5)
        plot.margin = unit(c(6, 12, 5.5, 5.5), "points"),      # Plot margins
        panel.background = element_rect(fill = "transparent"), # Transparent background
        text = element_text(size = GenText_Sz),                # Text size
        plot.title = element_text(size = GenText_Sz),              # Plot title size (if present)
        plot.subtitle = element_text(hjust = 0.5),             # Plot subtitle size (if present)
        #panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray90'), # Adds horizontal lines
        plot.caption = element_text(size = GenText_Sz-10),
        # X-axis
        axis.text.x = element_text(vjust = 1,color="black"),                 # Horizontal text
        axis.title.x = element_blank(),                         # y-axis title text size
        #axis.title.x = element_text(size = XTit_Sz),           # x-axis title text size
        # Y-axis
        axis.title.y = element_text(size = GenText_Sz+6),           # y-axis title text size
        axis.text.y=element_text(color="black"),
        # Legend
        legend.key.size = unit(1,"lines"),                     # Shrink legend boxes
        legend.position = "right",                             # Move legend to the bottom
        legend.justification = c(0.5,0.5),                     # Center the legend
        legend.text = element_text(size =GenText_Sz-6),              # Size of legend text
        legend.title=element_blank()) +                        # Remove legend title
      
      # Set axis scales
      scale_x_continuous(expand=c(0,0),limits=c(2023,MaxYrStudy),breaks=seq(2023, MaxYrStudy, 2)) +
      scale_y_continuous(expand=c(0.01,0),limits = c(0,1),breaks=pretty_breaks(5),labels=percent) +
      
      # Plot labels
      labs(x = "Year", y = "Average Capacity Factor", 
           colour="Plant_Type",caption = SourceDB) +
      
      # Legend color scheme
      scale_colour_manual(values = c("CER_2035"='grey20',"CER_2036"='grey40',"CER_2044"='grey70',CER_2045='grey90'),drop = FALSE) 
    
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
            axis.text = element_text(color="black"),
            axis.title.x = element_text(size = GenText_Sz+6),
            axis.title.y = element_text(size = GenText_Sz+6),
            plot.title = element_text(size = GenText_Sz),
            plot.subtitle = element_text(hjust = 0.5), 
            panel.background = element_rect(fill = NA),
            # panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
            #legend.key.size = unit(1,"lines"), #Shrink legend
            legend.position = "right",
            legend.justification = c(0.5,0.5),
            legend.title=element_blank(),
            text = element_text(size = GenText_Sz)) +
      
      labs(x = "Hours per Year", y = "Fleet Output (MW)",colour="ID",linetype="ID",caption = paste(SourceDB,',')) +
      
      scale_y_continuous(expand = c(0, 0),limits = c(0,MX),breaks=seq(0, MX, by=1000),labels=comma) +
      
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
          axis.text = element_text(color="black"),
          axis.title.x = element_text(size = GenText_Sz+6),
          axis.title.y = element_text(size = GenText_Sz+6),
          plot.title = element_text(size = GenText_Sz),
          plot.subtitle = element_text(hjust = 0.5), 
          panel.background = element_rect(fill = NA),
          # panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          #legend.key.size = unit(1,"lines"), #Shrink legend
          legend.position = "right",
          legend.justification = c(0.5,0.5),
          legend.title=element_blank(),
          text = element_text(size = GenText_Sz)) +
         # plot.margin=unit(c(5,1,5,1), 'cm')) +
    
    labs(x = "Hours per Year", y = "Fleet Capacity Factor (Output/Capacity)", 
         colour="ID",linetype="ID",caption = paste(SourceDB)) +
    
    scale_x_continuous(expand = c(0, 0),limits = c(0,1),breaks=seq(0, 1, by=0.2),labels = percent) +
    
    scale_y_continuous(expand=c(0,0),limits = c(0,1),breaks=seq(0, 1, by=0.2),labels = percent) 
  
  }
  
################################################################################  
## FUNCTION: MaxCurtail 
## Max curtailed load annually
##
## INPUTS: 
##    input - ResgroupMnor ResGroupYr
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    Import - Import table derived of zone average table
################################################################################
  
  MaxCurtail <- function(case) {
    
    # Max magnitude
    Amount_Max <- ZoneHr %>%
      filter(Run_ID == case & Condition == "Average",Name=="WECC_Alberta") %>%
      select(Name, date, Demand,Price,
             Demand_Side_Capability,Demand_Side_Output,Demand_Side_Output_Total,Demand_Side_Output_Hours,
             Load_Control_Output,Load_Control_Output_Total)%>%
      mutate(Time_Period=as.numeric(year(date)))%>%
      group_by(Time_Period)%>%
      summarise(Max_dmd = max(Demand_Side_Output),
                Max_load = max(Load_Control_Output))%>%
      ungroup()
   
      # Max hours
      Amount_Total <- ZoneYr %>%
        filter(Run_ID == case & Condition == "Average",Name=="WECC_Alberta") %>%
        select(Name, Time_Period, Demand,Price,
               Demand_Side_Capability,Demand_Side_Output,Demand_Side_Output_Total,Demand_Side_Output_Hours,
               Load_Control_Output,Load_Control_Output_Total)%>%
        mutate(Time_Period=as.numeric(year(Time_Period)))
      
    # Get max and min year for plot
    YearMX<-max(Amount_Max$Time_Period) #Take off the last 5 years
    YearMN<-2025
    
    # Plot
    Amount_Max %>%
      ggplot() +
      aes(Time_Period, Max_dmd) +
      geom_area(alpha=Plot_Trans,linewidth=.5, colour="black") +
      
      geom_label(aes(label = round(MaxCap,0)),
                 position = position_stack(1),alpha=0.5,label.size = NA,show.legend = NA) +
      
      theme_bw() +
      
      theme(
        # General Plot Settings
        panel.grid = element_blank(),
        # (t,r,b,l) margins, adjust to show full x-axis, default: (5.5,5.5,5.5,5.5)
        plot.margin = unit(c(6, 12, 5.5, 5.5), "points"),      # Plot margins
        panel.background = element_rect(fill = "transparent"), # Transparent background
        text = element_text(size = GenText_Sz-4),                # Text size
        plot.title = element_text(size = Tit_Sz),              # Plot title size (if present)
        plot.subtitle = element_text(hjust = 0.5),             # Plot subtitle size (if present)
        #panel.grid.major.y = element_line(size=0.25,
        #linetype=1,color = 'gray90'),                         # Adds horizontal lines
        # X-axis
        axis.text.x = element_text(vjust = 0.5,angle=45,size =GenText_Sz),                 # Horizontal text
        axis.title.x = element_text(size = XTit_Sz),           # x-axis title text size
        # Y-axis
        axis.title.y = element_text(size = YTit_Sz),           # y-axis title text size
        axis.text.y = element_text(size =GenText_Sz),                 # Horizontal text
        
        # Legend
        legend.position = c(.01, .99),                            # Move legend to the bottom
        legend.justification = c("left", "top"),                     # Center the legend
        legend.text = element_text(size =GenText_Sz-5),              # Size of legend text
        legend.title=element_blank(),                                  # Remove legend title
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(1,"lines")
        ) +                        
      
      # Set axis scales
      scale_x_continuous(expand=c(0.02,0.02),limits = c(YearMN,YearMX),breaks=seq(YearMN, YearMX, 1)) +
      scale_y_continuous(expand=c(0,0),limits=c(0,4000),breaks=pretty_breaks(6)) +
      
      # Plot labels
      labs(x = "Year", y = "Maximum Capacity Curtailed in Any Hour (MW)", fill = "Level",colour="Level",caption = SourceDB)
    
    
  }
  
################################################################################
## FUNCTION: week12_Curt
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
  
  week12_Curt <- function(year, month, day, case) {
    
    # Title Formating & filter between dates
    wk_st <- as.Date(paste(year,month,day, sep = "-"),tz="MST")
    wk_end <- as.Date(paste(year,month,day+7, sep = "-"),tz="MST")
    
    # Get any demand curtailment
    DSM <- ZoneHr_Avg%>%
      mutate(ID="Demand Curtailment")%>%
      subset(., select=c(ID,date,Demand_Side_Output,Run_ID))%>%
      rename(Output_MWH=Demand_Side_Output)
    
    # Get exports
    TRADE <- Export %>%
      filter(Run_ID == case)%>%
      mutate(Output_MWH=Output_MWH*-1+Import$Output_MWH,
             ID="Trade")
    
    # Filters for the desired case study from the resource groups
    data <- ResGroupHr_sub%>%
      sim_filt1(.) %>%
      subset(., select=-c(Report_Year,Capacity_Factor)) %>%
      rbind(DSM) %>%
      rbind(.,TRADE) %>%
      filter(Run_ID == case) %>%
      filter(date >= wk_st) %>%
      filter(date <= wk_end)
    
    # Set levels to each category in order specified
    data$ID <- factor(data$ID, levels=c(
      "Demand Curtailment",
      "Solar","Wind","Hydro","Other", 
      "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
      "Blended  Simple Cycle","Blended  Combined Cycle",
      "Natural Gas Combined Cycle + CCS","Natural Gas Simple Cycle", "Natural Gas Combined Cycle","Coal-to-Gas", 
      "Coal", "Cogeneration","Trade","Storage"))
    
    # data$Output_MWH[data$Output_MWH<0.001] <-0
    
    ## SELECT A SINGLE WEEK
    
    # Select only a single week from the zone Hourly, and Export data
    WK <- WkTime(data,year,month,day)
    ZPrice <- WkTime(ZoneHr_Avg,year,month,day) %>%
      filter(Run_ID == case)
    
    # # Set the max and min for the plot
    ZPrice2 <- ZoneHr_Avg %>%
      filter(Run_ID == case) 
    ZPrice2$YEAR  <- as.POSIXct(as.character(ZPrice2$date), format = "%Y")
    ZPrice2$YEAR <-(format(ZPrice2$YEAR,format="%Y")) # Reformat for year only
    ZPrice2 <- ZPrice2 %>%
      filter(YEAR == year)
    
    # Get y-max, demand to meet + exports
    MX <- round_any(max(ZPrice2$Baseline_Demand) + max(ZPrice2$Exports)+1100,1000,f=ceiling) 
    
    # Get y-min, based on exports
    MN <- round_any(max(ZPrice2$Exports)+1100,1000,f=floor)*-1 
    
    Mtitle=month.abb[month]
    
    ## PLOT WITH AREA PLOT
    
    ggplot() +
      geom_area_pattern(data = WK, aes(x = date, y = Output_MWH, fill = ID, colour=ID,pattern=ID),
                        alpha=Plot_Trans, size=.25,color='black',
                        pattern_density = 0.45,
                        pattern_fill    = "white",
                        pattern_colour  = "white",
                        pattern_spacing=0.01) +
      
      # Add hourly load line (black line on the top)
      geom_line(data = ZPrice,
                aes(x = date, y = Demand,color="Demand"), size=1.25) +
      
      geom_hline(yintercept=0, color = "black", size=0.5)+
      
      scale_x_datetime(expand=c(0,0),date_labels = "%b-%e", breaks = "day") +
      
      # Set the theme for the plot
      theme_bw() +
      theme(panel.grid = element_blank()) +
      
      theme(text=element_text(family=Plot_Text)) +
      
      theme(plot.title = element_text(size= Tit_Sz),
            axis.text.y = element_text(color="black"),
            axis.text.x = element_text(vjust = 1,color="black"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size= YTit_Sz),
            panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent", color = NA),
            legend.title=element_blank(),
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.background = element_rect(fill='transparent',colour ='transparent'),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
            legend.key.size = unit(1,"lines"), #Shrink legend
            legend.position = "right",
            legend.text = element_text(size= Leg_Sz),
            text = element_text(size= GenText_Sz)
      ) +
      
      guides(color = guide_legend(ncol = 1)) +
      guides(fill = guide_legend(ncol = 1)) +
      
      scale_y_continuous(expand=c(0,0), limits = c(MN,MX),breaks=seq(MN,MX,by=2000),
                         labels=comma) +
      
      labs(x = "Date", y = "Output (MWh)", fill = "Resource", colour = "Resource",pattern="Resource",title=year) +
      
      #Add colour
      scale_fill_manual(values = colours1b) +
      
      scale_color_manual(values=c("Demand"="black"))+
      
      scale_pattern_manual(values=c("Trade"= "none", "Coal"="none", "Cogeneration"="none", 
                                    "Coal-to-Gas"="none","Hydrogen Simple Cycle"="none","Hydrogen Combined Cycle"="none",
                                    "Natural Gas Combined Cycle + CCS"="none",
                                    "Natural Gas Simple Cycle"="none", "Natural Gas Combined Cycle"="none", 
                                    "Hydro"="none", "Other"="none", "Wind"="none", 
                                    "Solar"="none", "Storage"="none","Demand Curtailment"="stripe"))
  }
  
################################################################################
## FUNCTION: EachResWeek
## Plots output for a single week given the case study. 
## Separates each resource into individual plots.
## Scale options: "free_y" or "fixed"
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupHr_sub - Filtered version of Resource Group Hour Table
##    ZoneHr_Avg - Average curtailment
##    Export - Exports selected from Zone Hourly Table
################################################################################
  
  EachResWeek <- function(year, month, day, case,ScaleY) {
    
    # Title Formating & filter between dates
    wk_st <- as.Date(paste(year,month,day, sep = "-"),tz="MST")
    wk_end <- as.Date(paste(year,month,day+7, sep = "-"),tz="MST")
    
    # Get any demand curtailment
    DSM <- ZoneHr_Avg%>%
      mutate(ID="Demand Curtailment")%>%
      subset(., select=c(ID,date,Demand_Side_Output,Run_ID))%>%
      rename(Output_MWH=Demand_Side_Output)%>%
      filter(Output_MWH>0)
    
    # Get exports
    TRADE <- Export %>%
      filter(Run_ID == case)%>%
      mutate(Output_MWH=Output_MWH*-1+Import$Output_MWH,
             ID="Trade")
    
    # Filters for the desired case study from the resource groups
    data <- ResGroupHr_sub%>%
      sim_filt1(.) %>%
      subset(., select=-c(Report_Year,Capacity_Factor)) %>%
      rbind(DSM) %>%
      rbind(.,TRADE) %>%
      filter(Run_ID == case) %>%
      filter(date >= wk_st) %>%
      filter(date <= wk_end)
    
    # Set levels to each category in order specified
    data$ID <- factor(data$ID, levels=c(
      "Demand Curtailment",
      "Solar","Wind","Hydro","Other", 
      "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
      "Blended  Simple Cycle","Blended  Combined Cycle",
      "Natural Gas Combined Cycle + CCS","Natural Gas Simple Cycle", "Natural Gas Combined Cycle","Coal-to-Gas", 
      "Coal", "Cogeneration","Trade","Storage"))
    
    ## SELECT A SINGLE WEEK
    
    # Select only a single week from the zone Hourly, and Export data.
    WK <- WkTime(data,year,month,day)
    
    # Remove unused levels
    WK <- droplevels(WK)
    
    # Get y-max and min for combined approach
    MX <- round_any(max(WK$Output_MWH)+501,100,f=ceiling) 
    MN <- round_any(min(WK$Output_MWH)+501,100,f=floor)*-1 
    
    
    ## PLOT WITH AREA PLOTS
    ggplot() +
      geom_area(data = na.omit(WK), aes(x = date, y = Output_MWH, 
                                        #fill = fct_reorder(ID, Output_MWH, .desc = TRUE)),
                                        fill=ID),
                alpha=Plot_Trans, size=.25,color='black',position = "identity") +
      
      # line at 0
      geom_hline(yintercept=0, color = "black",size=0.5,linetype=1)+
      
      scale_x_datetime(expand=c(0,0),date_labels = "%b-%e", breaks = "day") +
      
      # Set the theme for the plot
      theme_bw() +
      
      # Individual plots for each resource type
      facet_wrap(~ID, scales = ScaleY) +
      theme(strip.background = element_blank(),
            strip.text = element_text(size= GenText_Sz-6)) +
      
      theme(panel.grid = element_blank()) +
      
      theme(text=element_text(family=Plot_Text)) +
      
      theme(plot.title = element_text(size= Tit_Sz),
            axis.text.y = element_text(color="black"),
            axis.text.x = element_text(vjust = 0.5,color="black",angle=45),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size= YTit_Sz),
            panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent", color = NA),
            legend.title=element_blank(),
            legend.position = "none",
            text = element_text(size= GenText_Sz)) +
      
      labs(x = "Date", y = "Output (MWh)", fill = "Resource", colour = "Resource",pattern="Resource",title=paste("Year: ",year)) +
      
      #Add colour
      scale_fill_manual(values = colours1) +
      scale_linetype_manual(values=c("Demand"=1,"Demand + Exports"=3))   +
      
      # Adjust scale as needed
      if (ScaleY=="fixed") {
        scale_y_continuous(expand=c(0,0), limits = c(MN,MX),breaks=seq(MN,MX,by=500),
                           labels=comma)}else{
                             scale_y_continuous(labels=comma)}
  }
  
################################################################################
## FUNCTION: Num_Startups
## Plots total startups in each year by tech.
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResYr - Filtered version of Resource year Table
################################################################################  
Num_Startups <- function(case)   {
   
  # Select startup data from table
   StartData<-ResYr%>%
      sim_filt3(.) %>% #Filter to rename fuels
      subset(., select=c(Name,Condition,Capacity,End_Date,Beg_Date,Run_ID,Primary_Fuel,Time_Period,Capacity_Factor,Startups)) %>%
      filter(Run_ID == case) %>%
      filter(Condition == "Average") %>%
      mutate(Time_Period=as.numeric(Time_Period))
    
    
    # Set levels to each category in order specified
    StartData$Primary_Fuel <- factor(StartData$Primary_Fuel, levels=c("Cogeneration","Coal-to-Gas", "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                                                      #"Blended  Simple Cycle","Blended  Combined Cycle",
                                                                      "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS",
                                                                      "Natural Gas Combined Cycle CCS Retrofit","Natural Gas Combined Cycle", 
                                                                      "Hydro", "Other",
                                                                      "Wind", "Solar", 
                                                                      "Storage - Battery", "Storage - Compressed Air", "Storage - Pumped Hydro",
                                                                      "Nuclear") )
    
    # Filter out other fuels
    fuels_exclude=c("Wind","Solar","Storage - Battery", "Storage - Compressed Air", "Storage - Pumped Hydro","Hydro","Other")
    
    # Summarize data by resource group
    ByType<- StartData %>%
      filter(!Primary_Fuel %in% fuels_exclude,
             Time_Period<= max(StartData$Time_Period-5)) %>%
      group_by(Time_Period,Primary_Fuel) %>%
      summarise(Capacity=sum(Capacity),
                Capacity_Factor=mean(Capacity_Factor),
                Startups=sum(Startups)) %>%
      mutate(Plant_Type=ifelse(grepl('Simple',Primary_Fuel)==TRUE,'SC',ifelse(grepl('Combined',Primary_Fuel)==TRUE,'CC','Gas_Steam')))
  
  # ADDITIONAL CALCS - NOT FOR PLOT
    # Start up cost estimations (per MW)
    StartupC<-data.frame(Plant_Type=c("CC",'SC','Gas_Steam','Coal','Nuclear'),
                         Cost_MW_low=c(75.60,15.12,73.08,112.14,73.5),
                         Cost_MW_high=c(75.60,47.88,73.08,112.14,73.5))
    
    # Calculate estimated cost
    GroupStartData<-merge(ByType,StartupC,by=c("Plant_Type"), all.x = TRUE) %>%
      mutate(EstCost_low_M=Cost_MW_low*Capacity*Startups/1000000,
             EstCost_high_M=Cost_MW_high*Capacity*Startups/1000000)
    
    # Remove na data
    GroupStartData$Startups[is.na(GroupStartData$Startups)]=0
    
    # Get total cost range for each plant type in each year
    TotalCost <- GroupStartData %>%
      group_by(Time_Period,Plant_Type) %>%
      summarise( Capacity=sum(Capacity),
                 Startups=sum(Startups),
                 EstCost_low_M=sum(EstCost_low_M),
                 EstCost_high_M=sum(EstCost_high_M)) %>%
      arrange(Plant_Type,Time_Period) %>%
      ungroup()
    
    # Get overall totals
    GroupTotals <-TotalCost %>%
      group_by(Plant_Type)%>%
      summarise(total_low=sum(EstCost_low_M/1000),
                total_high=sum(EstCost_high_M/1000)) %>%
      bind_rows(summarise(., across(where(is.numeric), sum),
                          across(where(is.character), ~'Total_(Billions)')))
    print('Estimated startup costs based on number of startups and costs from Power Plant Cycling Costs, NREL, 2012')
    print(GroupTotals)
    
  # Plot
  ByType %>%
    ggplot() +
    aes(Time_Period, Startups, fill = reorder(Primary_Fuel, Startups)) +
    geom_bar(position="dodge",stat="identity",alpha=Plot_Trans,'color'="black") +
    
    facet_grid(cols = vars(Plant_Type), scales = "free_y") +
    
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(vjust = 1,color="black"),
          #axis.title.x = element_text(size = XTit_Sz),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = YTit_Sz),
          axis.text.y=element_text(color="black"),
          plot.title = element_text(size = Tit_Sz),
          plot.subtitle = element_text(hjust = 0.5), 
          panel.background = element_rect(fill = NA),
          panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray90'),
          legend.key.size = unit(1,"lines"), #Shrink legend
          legend.position = "bottom",
          legend.justification = c(0.5,0.5),
          legend.title=element_blank(),
          text = element_text(size = 15)) +
    
    scale_y_continuous(expand=c(0,0),limits = c(0,max(ByType$Startups+10)),breaks=pretty_breaks(15)) +
    scale_x_continuous(expand=c(0,0),breaks=seq(min(ByType$Time_Period),max(ByType$Time_Period),by=5)) +
    
    #  geom_text(aes(label = sprintf("%0.1f",Output_MWH/1000000)),
    #            position = position_dodge(width = 1),vjust=-0.5) +
    
    labs(x = "Year", y = "Sum of Individual Plant Startups", fill = "Resource") +
    
    guides(fill = guide_legend(nrow = 2)) +
    
    scale_fill_manual(values=colours5,drop=TRUE) +
    
    scale_pattern_manual(values=Patterns5,drop=TRUE)
  
}
  
################################################################################
## FUNCTIONS: Resource_Ridge
## Plot ridgeline for capacity factor of selected resource
##
## INPUTS:
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
################################################################################
Resource_Ridge <- function(RType,MinYr,MaxYr,yrgap,case) {
  
  # Years to get 
  years_in <-as.list(seq(MinYr, MaxYr ,by= yrgap))
  
  # Bring in sim data
      Res_Data_all <- ResGroupHr%>%
        filter(ID==RType,
               Condition=="Average",
               Run_ID == case,
               Report_Year %in% years_in) %>%
        rename(YearS=Report_Year,
               CF=Capacity_Factor)%>%
        mutate(Year_char=as.character(YearS))%>%
        subset(.,select=c(Output,CF,date,YearS,Year_char))%>%
      Res_Data_all[is.na(Res_Data_all)] <- 0
      
      sub_type <-"un-weighted"

  # PMax <- round_any(max(Res_Data$CF),0.1,f=ceiling)
  PMax=1

  # Plot
  ggplot() +
    geom_density_ridges_gradient(data = Res_Data_all, 
                                 aes(x = CF, y = Year_char, fill = 1-stat(ecdf)),
                                 calc_ecdf = TRUE,vline_color="black",vline_linetype = 2,
                                 quantile_lines=TRUE, quantile_fun=function(CF,...)mean(CF),
                                 alpha = 0.8,scale=1.2) +
    scale_fill_viridis_c(option = "viridis",name = "ECDF") +
    scale_color_manual(values = c( mean = "black")) +
  
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(vjust = 1),
          axis.title.x = element_text(size = GenText_Sz+6),
          axis.title.y = element_text(size = GenText_Sz+6),
          plot.title = element_text(size = GenText_Sz+6),
          plot.subtitle = element_text(hjust = 0.5), 
          panel.background = element_rect(fill = NA),
          legend.position = "right",
          legend.justification = c(0.5,0.5),
          text = element_text(size = GenText_Sz)) +
    
    labs(x = "Hourly Capacity Factor (Actual Output/Max Output)", y = "Frequency",caption = paste('Sim Name: ',SourceDB,", Type: ",RType,", ",sub_type)) +
    
    scale_x_continuous(expand=c(0,0),limits = c(0,PMax),breaks=seq(0, PMax, by=0.2),labels = percent) 
}

################################################################################
## FUNCTIONS: Resource_Ridge_w
## Plot ridgeline for capacity factor of selected resource
##
## INPUTS:
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
################################################################################
Resource_Ridge_w <- function(RType,MinYr,MaxYr,yrgap,case) {
  
  # Years to get 
  years_in <-as.list(seq(MinYr, MaxYr ,by= yrgap))
  
  # Bring in sim data
  Res_Data_all <- ResGroupHr%>%
    filter(ID==RType,
           Condition=="Average",
           Run_ID == case,
           Report_Year %in% years_in) %>%
    rename(YearS=Report_Year,
           CF=Capacity_Factor)%>%
    mutate(Year_char=as.character(YearS),)%>%
    subset(.,select=c(Output,CF,date,YearS,Year_char))%>%
    mutate(mn_days=as.numeric(days_in_month(date)),
           rep_days=(1/7)*mn_days)
  
  Res_Data_all[is.na(Res_Data_all)] <- 0
  
  sub_type <-"un-weighted"
  
  # PMax <- round_any(max(Res_Data$CF),0.1,f=ceiling)
  PMax=1
  
  # Plot
  ggplot() +
    geom_density_ridges_gradient(data = Res_Data_all, 
                                 aes(x = CF, y = Year_char,
                                     height=..density..,
                                     weight=rep_days,
                                     fill=stat(x)),scale=1.2,
                                 stat="density")+
    scale_fill_viridis_c(option = "viridis",name = "Capacity Factor") +
    scale_color_manual(values = c( mean = "black")) +

    geom_density_ridges(data = Res_Data_all, 
                                 aes(x = CF, y = Year_char),fill=NA,color=NA,
                                 calc_ecdf = TRUE,vline_color="black",vline_linetype = 2,
                                 quantile_lines=TRUE, quantile_fun=function(CF,...)mean(CF),
                                 alpha = 0.8,scale=1.2) +
    
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(vjust = 1),
          axis.title.x = element_text(size = GenText_Sz+6),
          axis.title.y = element_text(size = GenText_Sz+6),
          plot.title = element_text(size = GenText_Sz+6),
          plot.subtitle = element_text(hjust = 0.5), 
          panel.background = element_rect(fill = NA),
          legend.position = "right",
          legend.justification = c(0.5,0.5),
          text = element_text(size = GenText_Sz)) +
    
    labs(x = "Hourly Capacity Factor (Actual Output/Max Output)", y = "Frequency",caption = paste('Sim Name: ',SourceDB,", Type: ",RType,", ",sub_type)) +
    
    scale_x_continuous(expand=c(0,0),limits = c(0,PMax),breaks=seq(0, PMax, by=0.2),labels = percent) 
}

################################################################################
## FUNCTIONS: Renew_Curtail_MWa
## Estimated curtailment for renewables in MWa
##
## INPUTS:
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
################################################################################
Renew_Curtail_MWa <- function(case) {
  
  # Bring in sim data
      Renew_Data <- ResGroupYr%>%
        sim_filt(.)%>%
        filter(ID %in% c("Wind","Solar"),
               Condition=="Average",
               Run_ID == case,
               Report_Year %in% Years2Disp) %>%
        group_by(ID,Report_Year) %>%
        summarise(Hours_tot=Output_MWH/Output,
                  Capability,
                  Capacity,
                  Output_MWa=Output,
                  Factor = Capacity_Factor,
                  Output_MWH,
                  Type = "Actual")
      
      # Calc values
      Curtail_Data <- ResGroupYr%>%
        sim_filt(.)%>%
        filter(ID %in% c("Wind","Solar"),
               Condition=="Average",
               Run_ID == case,
               Report_Year %in% Years2Disp) %>%
        group_by(ID,Report_Year) %>%
        summarise(Hours_tot=Output_MWH/Output,
                  Capability,
                  Capacity,
                  Output_MWa=Capability-Output,
                  Factor=Output_MWa/Capacity,
                  Output_MWH=Output_MWa*Hours_tot,
                  Type = "Curtailed")
      # Combine
      df_combined <- rbind(Curtail_Data, Renew_Data) %>%
        arrange(Type)
      
      plt_max <- round_any(max(Curtail_Data$Output_MWa+Renew_Data$Output_MWa),1000, f = ceiling)
      
      # Plot
      df_combined %>%
        ggplot() +
        aes(x =ID, Output_MWa, fill = Type, group=ID) +
        geom_bar(position="stack",stat="identity",alpha=Plot_Trans,'color'="black") +
        facet_grid(. ~ Report_Year) +
        
        theme_bw() +
        
        theme(text=element_text(family=Plot_Text)) +
        
        theme(panel.grid = element_blank(),
              axis.text.x = element_text(vjust = 1,color="black"),
              axis.title.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.y = element_text(size = GenText_Sz+6),
              axis.text.y=element_text(color="black"),
              plot.title = element_text(size = GenText_Sz),
              plot.subtitle = element_text(hjust = 0.5), 
              panel.background = element_rect(fill = NA),
              legend.position = "bottom",
              legend.justification = c(0.5,0.5),
              legend.title = element_blank(),
              legend.text = element_text(size = GenText_Sz-6),
              text = element_text(size = GenText_Sz),
              # Facet grids
              strip.background = element_rect(colour=NA, fill=NA),
              panel.border = element_rect(fill = NA, color = "black")) +
        
        scale_y_continuous(expand=c(0,0),limits=c(0,plt_max),breaks=pretty_breaks(6),labels = comma) +
        scale_x_discrete(drop=TRUE) +
        
        
        labs(x = "Year", y = "Annual Output (MWa)", fill = "Generation Type") +
        
        scale_fill_manual(values = c("Actual" = "#238b45", "Curtailed"= '#e6e6e6')) 

}

################################################################################
## FUNCTIONS: Renew_Curtail_perc
## Estimated curtailment for renewables in MWa
##
## INPUTS:
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
################################################################################
Renew_Curtail_perc <- function(case) {
  
  # Bring in sim data
  Renew_Data <- ResGroupYr%>%
    sim_filt(.)%>%
    filter(ID %in% c("Wind","Solar"),
           Condition=="Average",
           Run_ID == case,
           Report_Year %in% Years2Disp) %>%
    group_by(ID,Report_Year) %>%
    summarise(Hours_tot=Output_MWH/Output,
              Capability,
              Capacity,
              Output_MWa=Output,
              Factor = Capacity_Factor,
              Output_MWH,
              Type = "Capacity Factor")
  
  # Calc values
  Curtail_Data <- ResGroupYr%>%
    sim_filt(.)%>%
    filter(ID %in% c("Wind","Solar"),
           Condition=="Average",
           Run_ID == case,
           Report_Year %in% Years2Disp) %>%
    group_by(ID,Report_Year) %>%
    summarise(Hours_tot=Output_MWH/Output,
              Capability,
              Capacity,
              Output_MWa=Capability-Output,
              Factor=Output_MWa/Capacity,
              Output_MWH=Output_MWa*Hours_tot,
              Type = "Curtailment")
  # Combine
  df_combined <- rbind(Curtail_Data, Renew_Data) %>%
    arrange(Type)
  
  # Plot
  df_combined %>%
    ggplot() +
    aes(x =ID, Factor, fill = Type, group=ID) +
    geom_bar(position="stack",stat="identity",alpha=Plot_Trans,'color'="black") +
    facet_grid(. ~ Report_Year) +
    
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(vjust = 1,color="black"),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_text(size = GenText_Sz+6),
          axis.text.y=element_text(color="black"),
          plot.title = element_text(size = GenText_Sz),
          plot.subtitle = element_text(hjust = 0.5), 
          panel.background = element_rect(fill = NA),
          legend.position = "bottom",
          legend.justification = c(0.5,0.5),
          legend.title = element_blank(),
          legend.text = element_text(size = GenText_Sz-6),
          text = element_text(size = GenText_Sz),
          
          # Facet grids
          strip.background = element_rect(colour=NA, fill=NA),
          panel.border = element_rect(fill = NA, color = "black")) +
    
    scale_y_continuous(expand=c(0,0),limits=c(0,0.6),breaks=pretty_breaks(6),labels = comma) +
    scale_x_discrete(drop=TRUE) +
    
    
    labs(x = "Year", y = "Annual Capacity Factor", fill = "Generation Type") +
    
    scale_fill_manual(values = c("Capacity Factor" = "#238b45", "Curtailment"= '#e6e6e6')) 
  
}

################################################################################
## FUNCTION: Week12_wCURTAIL_simple
## Plots output for a single week given the case study. Inlclude estimated curtailment, simplify other gen.
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
Week12_wCURTAIL_simple <- function(year,month,day,case) {
  

# Title formating & filter between dates
wk_st <- as.Date(paste(year,month,day, sep = "-"),tz="MST")
wk_end <- as.Date(paste(year,month,day+7, sep = "-"),tz="MST")

# Get exports
TRADE <- Export %>%
  filter(Run_ID == case)%>%
  mutate(Output_MWH=Output_MWH*-1+Import$Output_MWH,
         ID="Net Exports")

# Estimate Curtilment
CURTAIL_RENEW <- ResGroupHr%>%
  sim_filt(.)%>%
  filter(ID %in% c("Wind","Solar"),
         Condition=="Average",
         Run_ID == case) %>%
  group_by(ID,date,Run_ID) %>%
  summarise(Capability,
            Capacity,
            Output_MWa=Capability-Output,
            Factor=Output_MWa/Capacity,
            Output_MWH=Output_MWa,
            Type = "Curtailed") %>%
  mutate(ID = paste(Type,ID),
         Output_MWH = if_else(is.na(Output_MWH),0,Output_MWH))%>%
  select(.,c(date,Output_MWH,Run_ID,ID))

# Filters for the desired case study from the resource groups
data_all <- ResGroupHr_sub%>%
  sim_filt1(.) %>%
  subset(., select=-c(Report_Year,Capacity_Factor)) %>%
  rbind(.,TRADE) %>%
  rbind(.,CURTAIL_RENEW) %>%
  filter(Run_ID == case,
         year(date)==year) %>%
  mutate(Type = as.character(ID),
         Type = if_else(!Type %in% c("Wind","Solar","Curtailed Solar","Curtailed Wind","Net Exports"),"Other Generation",Type),
         Type = as.factor(Type))%>%
  group_by(Type,date,Run_ID)%>%
  summarise(Output_MWH=sum(Output_MWH))

data<- data_all %>%
  filter(date >= wk_st) %>%
  filter(date <= wk_end)

# Set levels to each category in order specified
data$Type <- factor(data$Type, levels=c(
  "Curtailed Solar","Curtailed Wind",
  "Solar","Wind","Other Generation","Net Exports"))

# Removes negatives - storage charge
# data$Output_MWH[data$Output_MWH<0.001] <-0

## SELECT A SINGLE WEEK

# Select only a single week from the zone Hourly, and Export data
WK <- WkTime(data,year,month,day)
ZPrice <- WkTime(ZoneHr_Avg,year,month,day) %>%
  filter(Run_ID == case)%>%
  group_by(date)%>%
  summarise(Price,
            Net_Demand=Demand,
            Imports,
            Exports,
            Demand_Expo=Demand+Exports,
            Net_Load,
            Marginal_Resource,
            Demand_Side_Output)

# # Set the max and min for the plot
ZPrice2 <- ZoneHr_Avg %>%
  filter(Run_ID == case) 
ZPrice2$YEAR  <- as.POSIXct(as.character(ZPrice2$date), format = "%Y")
ZPrice2$YEAR <-(format(ZPrice2$YEAR,format="%Y")) # Reformat for year only
ZPrice2 <- ZPrice2 %>%
  filter(YEAR == year)

# Get y-max, demand to meet + exports
max_out <- data_all %>%
  group_by(date)%>%
  summarise(max_OUTPUT = sum(Output_MWH))

MX <- round_any(max(max_out$max_OUTPUT)+1100,1000,f=ceiling) 

# Get y-min, based on exports
MN <- round_any(max(ZPrice2$Exports)+1100,1000,f=floor)*-1 

Mtitle=month.abb[month]

# Adjust textsize
GenText_Sz=14

## PLOT WITH AREA PLOT

ggplot() +
  geom_area_pattern(data = na.omit(WK), aes(x = date, y = Output_MWH, fill = Type,pattern=Type),alpha=Plot_Trans, size=.25,color='black',
                    pattern_density = 0.3,
                    pattern_fill = "black",
                    pattern_colour  = NA,
                    pattern_spacing=0.015) +
  
  # Add hourly load line (black line on the top)
  geom_line(data = ZPrice, 
            aes(x = date, y = Net_Demand,linetype="Demand"), size=1.25,color='black') +
  
  # geom_line(data = ZPrice,
  #           aes(x = date, y = Demand_Expo,linetype="Demand + Exports"), size=1.25,color='black') +
  
  geom_hline(yintercept = 0)+
  scale_x_datetime(expand=c(0,0),date_labels = "%e", breaks = "day") +
  
  # Set the theme for the plot
  theme_bw() +
  theme(panel.grid = element_blank()) +
  
  #theme(text=element_text(family=Plot_Text)) +
  
  theme(plot.title = element_text(size= GenText_Sz)) +
  
  theme(axis.text.x = element_text(vjust = 1,color="black"),
        axis.title.x = element_text(size= GenText_Sz),
        axis.text.y = element_text(color="black"),
        axis.title.y = element_text(size= GenText_Sz),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.title=element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
        legend.background = element_rect(fill='transparent',colour ='transparent'),
        legend.box.background = element_rect(fill='transparent', colour = "transparent"),
        #legend.key.size = unit(1,"lines"), #Shrink legend
        legend.position = "bottom",
        legend.text = element_text(size= GenText_Sz-2),
        text = element_text(size= GenText_Sz)
  ) +
  scale_y_continuous(expand=c(0,0), limits = c(MN,MX),breaks=seq(MN,MX,by=2000),
                     labels=comma) +
  
  labs(x = "Date", y = "Output (MWh)", fill = "Resource", pattern = "Resource",colour = "Resource",title=Mtitle) +
  
  guides(fill = guide_legend(nrow = 1)) +
  guides(color = guide_legend(nrow = 1)) +
  guides(linetype = guide_legend(nrow = 1)) +
  
  #Add colour
  scale_fill_manual(values = c("Curtailed Solar"='darkgoldenrod1',"Curtailed Wind"="#238b45",
                               "Solar"='darkgoldenrod3',"Wind"="#1d632d","Other Generation"="#A6A6A6","Net Exports"='#e6e6e6')) +
  scale_pattern_manual(values = c("Curtailed Solar"="stripe","Curtailed Wind"="stripe",
                                  "Solar"="none","Wind"="none","Other Generation"="none","Net Exports"="none")) +
  scale_linetype_manual(values=c("Demand"=1,"Demand + Exports"=3))  

}


################################################################################
#
# COMBINED PLOTS SECTION
# Combined plots and supporting functions
#
################################################################################ 
  
################################################################################
## FUNCTION: Week12_rCURTAIL
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
  
Week12_rCURTAIL <- function(year, month, day, case) {
    
  # Col choice
  col_plot = colours1_rcurt
  
    # Title Formating & filter between dates
    wk_st <- as.Date(paste(year,month,day, sep = "-"),tz="MST")
    wk_end <- as.Date(paste(year,month,day+7, sep = "-"),tz="MST")

    # Get exports
    TRADE <- Export %>%
      filter(Run_ID == case)%>%
      mutate(Output_MWH=Output_MWH*-1+Import$Output_MWH,
             ID="Trade")
    
    # Estimate Curtilment
    CURTAIL_RENEW <- ResGroupHr%>%
      sim_filt(.)%>%
      filter(ID %in% c("Wind","Solar"),
             Condition=="Average",
             Run_ID == case) %>%
      group_by(ID,date,Run_ID) %>%
      summarise(Capability,
                Capacity,
                Output_MWa=Capability-Output,
                Factor=Output_MWa/Capacity,
                Output_MWH=Output_MWa,
                Type = "Curtailed") %>%
      mutate(ID = paste(Type,ID),
             Output_MWH = if_else(is.na(Output_MWH),0,Output_MWH))%>%
      select(.,c(date,Output_MWH,Run_ID,ID))
    
    # Filters for the desired case study from the resource groups
    data_all <- ResGroupHr_sub%>%
      sim_filt1(.) %>%
      subset(., select=-c(Report_Year,Capacity_Factor)) %>%
      rbind(CURTAIL_RENEW) %>%
      rbind(.,TRADE) %>%
      filter(Run_ID == case,
             year(date)==year) 
    
    data<- data_all %>%
      filter(date >= wk_st) %>%
      filter(date <= wk_end)
    
    # Set levels to each category in order specified
    data$ID <- factor(data$ID, levels=c("Curtailed Solar","Curtailed Wind",
      "Solar","Wind","Hydro","Other", 
                 "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                 "Blended  Simple Cycle","Blended  Combined Cycle",
                 "Natural Gas Combined Cycle + CCS","Natural Gas Simple Cycle", "Natural Gas Combined Cycle","Coal-to-Gas", 
                 "Coal", "Cogeneration","Trade","Storage"))
    
    # Removes negatives - storage charge
    # data$Output_MWH[data$Output_MWH<0.001] <-0
    
    ## SELECT A SINGLE WEEK
    
    # Select only a single week from the zone Hourly, and Export data
    WK <- WkTime(data,year,month,day) %>%
      mutate(Output_GWH=Output_MWH/1000)
    ZPrice <- WkTime(ZoneHr_Avg,year,month,day) %>%
      filter(Run_ID == case)%>%
      group_by(date)%>%
      summarise(Price,
                Net_Demand=Demand,
                Imports,
                Exports,
                Demand_Expo=Demand+Exports,
                Net_Load,
                Marginal_Resource,
                Demand_Side_Output) %>%
      mutate(Net_Demand_GWh=Net_Demand/1000)
    
    # # Set the max and min for the plot
    ZPrice2 <- ZoneHr_Avg %>%
      filter(Run_ID == case) 
    ZPrice2$YEAR  <- as.POSIXct(as.character(ZPrice2$date), format = "%Y")
    ZPrice2$YEAR <-(format(ZPrice2$YEAR,format="%Y")) # Reformat for year only
    ZPrice2 <- ZPrice2 %>%
      filter(YEAR == year)
    
    # Get y-max, demand to meet + exports
    # Get y-max, demand to meet + exports
    max_out <- data_all %>%
      group_by(date)%>%
      summarise(max_OUTPUT = sum(Output_MWH[Output_MWH>0])/1000,
                min_OUTPUT =  sum(Output_MWH[Output_MWH<0])/1000)
    MX <- round_any(max(max_out$max_OUTPUT)+1,1,f=ceiling) 

    # Get y-min, based on exports
    MN <- round_any(min(max_out$min_OUTPUT)-1,1,f=floor) 
    
    Mtitle=month.abb[month]
    
    # Adjust textsize
    GenText_Sz=14
    
    ## PLOT WITH AREA PLOT
    
    ggplot() +
      geom_area_pattern(data = na.omit(WK), aes(x = date, y = Output_GWH, fill = ID,pattern=ID),
                        size=.25,color='black',
                        pattern_density = 0.3,
                        pattern_fill = "black",
                        pattern_colour  = NA,
                        pattern_spacing=0.015) +
      
      # Add hourly load line (black line on the top)
      geom_line(data = ZPrice, 
                aes(x = date, y = Net_Demand_GWh,linetype="Demand"), size=1.25,color='black') +
      geom_hline(yintercept =0,size=0.25)+
      
      # geom_line(data = ZPrice,
      #           aes(x = date, y = Demand_Expo,linetype="Demand + Exports"), size=1.25,color='black') +
      
      scale_x_datetime(expand=c(0,0),date_labels = "%e", breaks = "day") +
      
      # Set the theme for the plot
      theme_bw() +
      theme(panel.grid = element_blank()) +
      
      theme(text=element_text(family=Plot_Text)) +
      
      theme(plot.title = element_text(size= GenText_Sz)) +
      
      theme(axis.text.x = element_text(vjust = 1,color="black"),
            axis.title.x = element_text(size= GenText_Sz),
            axis.text.y = element_text(color="black"),
            axis.title.y = element_text(size= GenText_Sz),
            panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent", color = NA),
            legend.title=element_blank(),
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.background = element_rect(fill='transparent',colour ='transparent'),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
            #legend.key.size = unit(1,"lines"), #Shrink legend
            legend.position = "right",
            legend.spacing.y = unit(0.1, 'cm'),
            legend.key.size = unit(0.5, 'cm'),
            legend.text = element_text(size= GenText_Sz-2),
            text = element_text(size= GenText_Sz)
      )+
      
      labs(x = "Date", y = "Output (GWh)", fill = "Resource", pattern ="Resource",colour = "Resource",title=Mtitle) +
      
      guides(fill = guide_legend(ncol = 1)) +
      guides(color = guide_legend(ncol = 1)) +
      guides(linetype = guide_legend(ncol = 1)) +
      
      #Add colour
      scale_fill_manual(values = col_plot) +
      scale_linetype_manual(values=c("Demand"=1,"Demand + Exports"=3)) + 
      scale_pattern_manual(values=pattern1_rcurt)  +
      
      scale_y_continuous(expand=c(0,0), 
                         limits = c(MN,MX),
                         breaks=seq(MN,MX,by=2)) 
  }
  
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
    
    # Col choice
    col_scale = colours1
    
    # Title Formating & filter between dates
    wk_st <- as.Date(paste(year,month,day, sep = "-"),tz="MST")
    wk_end <- as.Date(paste(year,month,day+7, sep = "-"),tz="MST")
    
    # Get any demand curtailment
    DSM <- ZoneHr_Avg%>%
      mutate(ID="Demand Curtailment")%>%
      subset(., select=c(ID,date,Demand_Side_Output,Run_ID))%>%
      rename(Output_MWH=Demand_Side_Output)
    
    # Get exports
    TRADE <- Export %>%
      filter(Run_ID == case)%>%
      mutate(Output_MWH=Output_MWH*-1+Import$Output_MWH,
             ID="Trade")
    
    # Filters for the desired case study from the resource groups
    data <- ResGroupHr_sub%>%
      sim_filt1(.) %>%
      subset(., select=-c(Report_Year,Capacity_Factor)) %>%
      rbind(DSM) %>%
      rbind(.,TRADE) %>%
      filter(Run_ID == case) %>%
      filter(date >= wk_st) %>%
      filter(date <= wk_end)
    
    # Set levels to each category in order specified
    data$ID <- factor(data$ID, levels=c(
      #"Demand Curtailment",
      "Solar","Wind","Hydro","Other", 
      "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
      "Blended  Simple Cycle","Blended  Combined Cycle",
      "Natural Gas Combined Cycle + CCS","Natural Gas Simple Cycle", "Natural Gas Combined Cycle","Coal-to-Gas", 
      "Coal", "Cogeneration","Trade","Storage"))
    
    # Removes negatives - storage charge
    # data$Output_MWH[data$Output_MWH<0.001] <-0
    
    ## SELECT A SINGLE WEEK
    
    # Select only a single week from the zone Hourly, and Export data
    WK <- WkTime(data,year,month,day) %>%
      mutate(Output_GWH=Output_MWH/1000)
    ZPrice <- WkTime(ZoneHr_Avg,year,month,day) %>%
      filter(Run_ID == case)%>%
      group_by(date)%>%
      summarise(Price,
                Net_Demand=Demand,
                Imports,
                Exports,
                Demand_Expo=Demand+Exports,
                Net_Load,
                Marginal_Resource,
                Demand_Side_Output) %>%
      mutate(Net_Demand_GWh=Net_Demand/1000)
    
    # # Set the max and min for the plot
    ZPrice2 <- ZoneHr_Avg %>%
      filter(Run_ID == case) 
    ZPrice2$YEAR  <- as.POSIXct(as.character(ZPrice2$date), format = "%Y")
    ZPrice2$YEAR <-(format(ZPrice2$YEAR,format="%Y")) # Reformat for year only
    ZPrice2 <- ZPrice2 %>%
      filter(YEAR == year)
    
    # Get y-max, demand to meet + exports
    max_out <- ResGroupHr_sub %>%
      sim_filt1(.) %>%
      filter(Report_Year==year)%>%
      group_by(date)%>%
      summarise(max_OUTPUT = sum(Output_MWH[Output_MWH>0])/1000,
                min_OUTPUT =  sum(Output_MWH[Output_MWH<0])/1000)
    MX <- round_any(max(max_out$max_OUTPUT)+1,1,f=ceiling) 
    
    # Get y-min, based on exports
    MN <- round_any(min(max_out$min_OUTPUT)-1,1,f=floor) 
    
    Mtitle=month.abb[month]
    
    # Adjust textsize
    GenText_Sz=14
    
    ## PLOT WITH AREA PLOT
    
    ggplot() +
      geom_area(data = na.omit(WK), aes(x = date, y = Output_GWH, fill = ID),alpha=Plot_Trans, size=.25,color='black') +
      
      # Add hourly load line (black line on the top)
      geom_line(data = ZPrice, 
                aes(x = date, y = Net_Demand_GWh,linetype="Demand"), size=1.25,color='black') +
      
      # geom_line(data = ZPrice,
      #           aes(x = date, y = Demand_Expo,linetype="Demand + Exports"), size=1.25,color='black') +
      
      scale_x_datetime(expand=c(0,0),date_labels = "%e", breaks = "day") +
      geom_hline(yintercept =0,size=0.25)+
      
      # Set the theme for the plot
      theme_bw() +
      theme(panel.grid = element_blank()) +
      
      #theme(text=element_text(family=Plot_Text)) +
      
      theme(plot.title = element_text(size= GenText_Sz)) +
      
      theme(axis.text.x = element_text(vjust = 1,color="black"),
            axis.title.x = element_text(size= GenText_Sz),
            axis.text.y = element_text(color="black"),
            axis.title.y = element_text(size= GenText_Sz),
            panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent", color = NA),
            legend.title=element_blank(),
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.background = element_rect(fill='transparent',colour ='transparent'),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
            #legend.key.size = unit(1,"lines"), #Shrink legend
            legend.position = "bottom",
            legend.text = element_text(size= GenText_Sz-2),
            text = element_text(size= GenText_Sz)
      ) +
      scale_y_continuous(expand=c(0,0), limits = c(MN,MX),breaks=seq(MN,MX,by=2)) +
      
      labs(x = "Date", y = "Output (GWh)", fill = "Resource", colour = "Resource",title=Mtitle) +
      
      guides(fill = guide_legend(nrow = 2)) +
      guides(color = guide_legend(nrow = 2)) +
      guides(linetype = guide_legend(nrow = 2)) +
      
      #Add colour
      scale_fill_manual(values = col_scale) +
      scale_linetype_manual(values=c("Demand"=1,"Demand + Exports"=3))            
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
  yleft <- textGrob("Output (GWh)", rot = 90, gp = gpar(fontsize = 20,fontface ='bold'))

  # Cheat way to put an x title in
  xtitle <- ggplot() +
    annotate("text", x = 10,  y = 10,
             size = 6, fontface ='bold',
             label = "Day of Month") + 
               theme_void()
  
  # Label the source and year
  xsubtitle <- ggplot() +
    annotate("text", x = 10,  y = 10,
             size = 4,
             label = paste("year:",year,", Database:",SourceDB)) + 
    theme_void()
  
  #Create a big window
  #windows(18,12)
  
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
## FUNCTION: year_weeks_rCURTAIL
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
year_weeks_rCURTAIL <- function(year,case,type) {
  
  if (type == "simple"){
    # Create a graph for each month of the year
        p1 <- Week12_wCURTAIL_simple(year,01,08,case) +
          theme(axis.title.y=element_blank(),
                axis.title.x=element_blank())
        
        
        p2 <- Week12_wCURTAIL_simple(year,02,08,case) +
          theme(legend.position ="none",
                axis.title.y=element_blank(),
                axis.title.x=element_blank())
        
        p3 <- Week12_wCURTAIL_simple(year,03,08,case) +
          theme(legend.position ="none",
                axis.title.y=element_blank(),
                axis.title.x=element_blank())
        
        p4 <- Week12_wCURTAIL_simple(year,04,08,case) +
          theme(legend.position ="none",
                axis.title.y=element_blank(),
                axis.title.x=element_blank())
        
        p5 <- Week12_wCURTAIL_simple(year,05,08,case) +
          theme(legend.position ="none",
                axis.title.y=element_blank(),
                axis.title.x=element_blank())
        
        p6 <- Week12_wCURTAIL_simple(year,06,08,case) +
          theme(legend.position ="none",
                axis.title.y=element_blank(),
                axis.title.x=element_blank())
        
        p7 <- Week12_wCURTAIL_simple(year,07,08,case) +
          theme(legend.position ="none",
                axis.title.y=element_blank(),
                axis.title.x=element_blank())
        
        p8 <- Week12_wCURTAIL_simple(year,08,08,case) +
          theme(legend.position ="none",
                axis.title.y=element_blank(),
                axis.title.x=element_blank())
        
        p9 <- Week12_wCURTAIL_simple(year,09,08,case) +
          theme(legend.position ="none",
                axis.title.y=element_blank(),
                axis.title.x=element_blank())
        
        p10 <- Week12_wCURTAIL_simple(year,10,08,case) +
          theme(legend.position ="none",
                axis.title.y=element_blank(),
                axis.title.x=element_blank())
        
        p11 <- Week12_wCURTAIL_simple(year,11,08,case) +
          theme(legend.position ="none",
                axis.title.y=element_blank(),
                axis.title.x=element_blank())
        
        p12 <- Week12_wCURTAIL_simple(year,12,08,case) +
          theme(legend.position ="none",
                axis.title.y=element_blank(),
                axis.title.x=element_blank())
  }else{
      # Create a graph for each month of the year
      p1 <- Week12_rCURTAIL(year,01,08,case) +
        theme(axis.title.y=element_blank(),
              axis.title.x=element_blank())
      
      
      p2 <- Week12_rCURTAIL(year,02,08,case) +
        theme(legend.position ="none",
              axis.title.y=element_blank(),
              axis.title.x=element_blank())
      
      p3 <- Week12_rCURTAIL(year,03,08,case) +
        theme(legend.position ="none",
              axis.title.y=element_blank(),
              axis.title.x=element_blank())
      
      p4 <- Week12_rCURTAIL(year,04,08,case) +
        theme(legend.position ="none",
              axis.title.y=element_blank(),
              axis.title.x=element_blank())
      
      p5 <- Week12_rCURTAIL(year,05,08,case) +
        theme(legend.position ="none",
              axis.title.y=element_blank(),
              axis.title.x=element_blank())
      
      p6 <- Week12_rCURTAIL(year,06,08,case) +
        theme(legend.position ="none",
              axis.title.y=element_blank(),
              axis.title.x=element_blank())
      
      p7 <- Week12_rCURTAIL(year,07,08,case) +
        theme(legend.position ="none",
              axis.title.y=element_blank(),
              axis.title.x=element_blank())
      
      p8 <- Week12_rCURTAIL(year,08,08,case) +
        theme(legend.position ="none",
              axis.title.y=element_blank(),
              axis.title.x=element_blank())
      
      p9 <- Week12_rCURTAIL(year,09,08,case) +
        theme(legend.position ="none",
              axis.title.y=element_blank(),
              axis.title.x=element_blank())
      
      p10 <- Week12_rCURTAIL(year,10,08,case) +
        theme(legend.position ="none",
              axis.title.y=element_blank(),
              axis.title.x=element_blank())
      
      p11 <- Week12_rCURTAIL(year,11,08,case) +
        theme(legend.position ="none",
              axis.title.y=element_blank(),
              axis.title.x=element_blank())
      
      p12 <- Week12_rCURTAIL(year,12,08,case) +
        theme(legend.position ="none",
              axis.title.y=element_blank(),
              axis.title.x=element_blank())
  }
  # Get a common legend
  legend <- get_legend(p1)
  p1 <- p1 + theme(legend.position ="none")
  
  # Plot Labels
  yleft <- textGrob("Output (MWh)", rot = 90, gp = gpar(fontsize = 20,fontface ='bold'))
  
  # Cheat way to put an x title in
  xtitle <- ggplot() +
    annotate("text", x = 10,  y = 10,
             size = 6, fontface ='bold',
             label = "Day of Month") + 
    theme_void()
  
  # Label the source and year
  xsubtitle <- ggplot() +
    annotate("text", x = 10,  y = 10,
             size = 4,
             label = paste("year:",year
                          # ", Database:",SourceDB
                           )) + 
    theme_void()
  
  #Create a big window
  #windows(18,12)
  
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
  
################################################################################
## FUNCTION: FourMonthSummary
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
  FourMonthSummary <- function(year,m1,m2,m3,m4,case) {
    
    grouped_font <- 26
    
    # Gather weekly output data (Feb,May,Aug,Nov)
    p1 <- Week12(year,m1,08,case) +
      theme(plot.title=element_text(family=Plot_Text_bf,size=grouped_font-6),
            axis.title.y=element_text(size=grouped_font,family=Plot_Text_bf),
            axis.title.x=element_blank(),
            legend.position = "bottom",
            text = element_text(size=grouped_font-6)) +
      guides(fill = guide_legend(nrow = 1)) +
      guides(linetype = guide_legend(nrow = 1))
    
    p2 <- Week12(year,m2,08,case) +
      theme(plot.title=element_text(family=Plot_Text_bf,size=grouped_font-6),
            legend.position ="none",
            axis.title.y=element_blank(),
            text = element_text(size=grouped_font-6),
            axis.title.x=element_blank())
    
    p3 <- Week12(year,m3,08,case) +
      theme(plot.title=element_text(family=Plot_Text_bf,size=grouped_font-6),
            legend.position ="none",
            axis.title.y=element_blank(),
            text = element_text(size=grouped_font-6),
            axis.title.x=element_blank())
    
    p4 <- Week12(year,m4,08,case) +
      theme(plot.title=element_text(family=Plot_Text_bf,size=grouped_font-6),
            legend.position ="none",
            axis.title.y=element_blank(),
            text = element_text(size=grouped_font-6),
            axis.title.x=element_blank())
    
    # Gather intertie info (Feb,May,Aug,Nov)
    # p5 <- Imp_ExpWk(year,m1,08,case) +
    #   theme(legend.position ="none",
    #         axis.title.y=element_text(size=10),
    #         plot.title = element_blank(),
    #         text = element_text(size= 8),
    #         axis.title.x=element_blank())+
    #   scale_x_datetime(expand=c(0,0),date_labels = "%e", breaks = "day")
    # 
    # p6 <- Imp_ExpWk(year,m2,08,case) +
    #   theme(legend.position ="none",
    #         plot.title = element_blank(),
    #         text = element_text(size= 8),
    #         axis.title.y.left=element_blank(),
    #         axis.title.x=element_blank())+
    #   scale_x_datetime(expand=c(0,0),date_labels = "%e", breaks = "day")
    # 
    # p7 <- Imp_ExpWk(year,m3,08,case) +
    #   theme(legend.position ="none",
    #         plot.title = element_blank(),
    #         text = element_text(size= 8),
    #         axis.title.y.left=element_blank(),
    #         axis.title.x=element_blank())+
    #   scale_x_datetime(expand=c(0,0),date_labels = "%e", breaks = "day")
    # 
    # p8 <- Imp_ExpWk(year,m4,08,case) +
    #   theme(legend.position ="none",
    #         plot.title = element_blank(),
    #         text = element_text(size= 8),
    #         axis.title.y.left=element_blank(),
    #         axis.title.x=element_blank())+
    #   scale_x_datetime(expand=c(0,0),date_labels = "%e", breaks = "day")
    
    # Price info  (Feb,May,Aug,Nov)
    p9 <- week_price(year,m1,08,case) +
      theme(legend.position ="none",
            plot.caption=element_blank(),
            axis.title.y=element_text(size=grouped_font,family=Plot_Text_bf),
            text = element_text(size=grouped_font-6),
            axis.title.x=element_blank())+
      scale_x_datetime(expand=c(0,0),date_labels = "%e", breaks = "day")
    
    p10 <- week_price(year,m2,08,case) +
      theme(legend.position ="none",
            axis.title.y=element_blank(),
            plot.caption=element_blank(),
            text = element_text(size=grouped_font-6),
            axis.title.x=element_blank())+
      scale_x_datetime(expand=c(0,0),date_labels = "%e", breaks = "day")
    
    p11 <- week_price(year,m3,08,case) +
      theme(legend.position ="none",
            axis.title.y=element_blank(),
            plot.caption=element_blank(),
            text = element_text(size=grouped_font-6),
            axis.title.x=element_blank())+
      scale_x_datetime(expand=c(0,0),date_labels = "%e", breaks = "day")
    
    p12 <- week_price(year,m4,08,case) +
      theme(legend.position ="none",
            plot.caption=element_blank(),
            axis.title.y=element_blank(),
            text = element_text(size=grouped_font-6),
            axis.title.x=element_blank())+
      scale_x_datetime(expand=c(0,0),date_labels = "%e", breaks = "day")
    
    # Get a common legend
    legend <- get_legend(p1) 
    p1 <- p1 + theme(legend.position ="none")
    
    # Plot Labels
    bottom <- textGrob("Day of Month", gp = gpar(fontsize = grouped_font))
    
    # Label the source and year
    xsubtitle <- ggplot() +
      annotate("text", x = 10,  y = 10,
               size = grouped_font/5,
               label = paste("year:",year,", Database:",SourceDB)) + 
      theme_void()
    
    #Create a big window
    #windows(18,12)
    
    # Set up plots
    p_m1=plot_grid(p1,p9,ncol=1, align="v", axis = "l", rel_heights = c(1,0.4))
    p_m2=plot_grid(p2,p10,ncol=1, align="v", axis = "l", rel_heights = c(1,0.4))
    p_m3=plot_grid(p3,p11,ncol=1, align="v", axis = "l", rel_heights = c(1,0.4))
    p_m4=plot_grid(p4,p12,ncol=1, align="v", axis = "l", rel_heights = c(1,0.4))
    
    #Arrange all the plots
    grid.arrange(plot_grid(p_m1, p_m2, p_m3, p_m4, ncol=4, align="v", axis = "l", rel_widths = c(1,1,1,1)),
                 plot_grid(bottom),
                 plot_grid(legend),
                 plot_grid(xsubtitle),
                 ncol=1,nrow=4,
                 heights=c(1, 0.05,0.05,0.05))
    
  }  
  
################################################################################
## FUNCTION: FourMonthSummary_rCurtail
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
  FourMonthSummary_rCurtail <- function(year,m1,m2,m3,m4,case) {
    
    grouped_font <- 26
    
    # Gather weekly output data (Feb,May,Aug,Nov)
    p1 <- Week12_rCURTAIL(year,m1,08,case) +
      theme(plot.title=element_text(family=Plot_Text_bf,size=grouped_font-6),
            axis.title.y=element_text(size=grouped_font,family=Plot_Text_bf),
            axis.title.x=element_blank(),
            text = element_text(size=grouped_font-6),
            legend.position = "bottom",) +
      guides(fill = guide_legend(nrow = 2)) +
      guides(linetype = guide_legend(nrow = 2))
    
    p2 <- Week12_rCURTAIL(year,m2,08,case) +
      theme(plot.title=element_text(family=Plot_Text_bf,size=grouped_font-6),
            legend.position ="none",
            axis.title.y=element_blank(),
            text = element_text(size=grouped_font-6),
            axis.title.x=element_blank())
    
    p3 <- Week12_rCURTAIL(year,m3,08,case) +
      theme(plot.title=element_text(family=Plot_Text_bf,size=grouped_font-6),
            legend.position ="none",
            axis.title.y=element_blank(),
            text = element_text(size=grouped_font-6),
            axis.title.x=element_blank())
    
    p4 <- Week12_rCURTAIL(year,m4,08,case) +
      theme(plot.title=element_text(family=Plot_Text_bf,size=grouped_font-6),
            legend.position ="none",
            axis.title.y=element_blank(),
            text = element_text(size=grouped_font-6),
            axis.title.x=element_blank())
    
    # Gather intertie info (Feb,May,Aug,Nov)
    # p5 <- Imp_ExpWk(year,m1,08,case) +
    #   theme(legend.position ="none",
    #         axis.title.y=element_text(size=10),
    #         plot.title = element_blank(),
    #         text = element_text(size= 8),
    #         axis.title.x=element_blank())+
    #   scale_x_datetime(expand=c(0,0),date_labels = "%e", breaks = "day")
    # 
    # p6 <- Imp_ExpWk(year,m2,08,case) +
    #   theme(legend.position ="none",
    #         plot.title = element_blank(),
    #         text = element_text(size= 8),
    #         axis.title.y.left=element_blank(),
    #         axis.title.x=element_blank())+
    #   scale_x_datetime(expand=c(0,0),date_labels = "%e", breaks = "day")
    # 
    # p7 <- Imp_ExpWk(year,m3,08,case) +
    #   theme(legend.position ="none",
    #         plot.title = element_blank(),
    #         text = element_text(size= 8),
    #         axis.title.y.left=element_blank(),
    #         axis.title.x=element_blank())+
    #   scale_x_datetime(expand=c(0,0),date_labels = "%e", breaks = "day")
    # 
    # p8 <- Imp_ExpWk(year,m4,08,case) +
    #   theme(legend.position ="none",
    #         plot.title = element_blank(),
    #         text = element_text(size= 8),
    #         axis.title.y.left=element_blank(),
    #         axis.title.x=element_blank())+
    #   scale_x_datetime(expand=c(0,0),date_labels = "%e", breaks = "day")
    
    # Price info  (Feb,May,Aug,Nov)
    p9 <- week_price(year,m1,08,case) +
      theme(legend.position ="none",
            plot.caption=element_blank(),
            text = element_text(size=grouped_font-6),
            axis.title.y=element_text(size=grouped_font,family=Plot_Text_bf),
            axis.title.x=element_blank())+
      scale_x_datetime(expand=c(0,0),date_labels = "%e", breaks = "day")
    
    p10 <- week_price(year,m2,08,case) +
      theme(legend.position ="none",
            axis.title.y=element_blank(),
            text = element_text(size=grouped_font-6),
            plot.caption=element_blank(),
            axis.title.x=element_blank())+
      scale_x_datetime(expand=c(0,0),date_labels = "%e", breaks = "day")
    
    p11 <- week_price(year,m3,08,case) +
      theme(legend.position ="none",
            axis.title.y=element_blank(),
            text = element_text(size=grouped_font-6),
            plot.caption=element_blank(),
            axis.title.x=element_blank())+
      scale_x_datetime(expand=c(0,0),date_labels = "%e", breaks = "day")
    
    p12 <- week_price(year,m4,08,case) +
      theme(legend.position ="none",
            plot.caption=element_blank(),
            text = element_text(size=grouped_font-6),
            axis.title.y=element_blank(),
            axis.title.x=element_blank())+
      scale_x_datetime(expand=c(0,0),date_labels = "%e", breaks = "day")
    
    # Get a common legend
    legend <- get_legend(p1)
    p1 <- p1 + theme(legend.position ="none")
    
    # Plot Labels
    bottom <- textGrob("Day of Month", gp = gpar(fontsize = grouped_font,family=Plot_Text_bf))
    
    # Label the source and year
    xsubtitle <- ggplot() +
      annotate("text", x = 10,  y = 10,
               size = grouped_font/5,
               label = paste("year:",year,", Database:",SourceDB)) + 
      theme_void()
    
    #Create a big window
    #windows(18,12)
    
    # Set up plots
    p_m1=plot_grid(p1,p9,ncol=1, align="v", axis = "l", rel_heights = c(1,0.4))
    p_m2=plot_grid(p2,p10,ncol=1, align="v", axis = "l", rel_heights = c(1,0.4))
    p_m3=plot_grid(p3,p11,ncol=1, align="v", axis = "l", rel_heights = c(1,0.4))
    p_m4=plot_grid(p4,p12,ncol=1, align="v", axis = "l", rel_heights = c(1,0.4))
    
    #Arrange all the plots
    grid.arrange(plot_grid(p_m1, p_m2, p_m3, p_m4, ncol=4, align="v", axis = "l", rel_widths = c(1,1,1,1)),
                 plot_grid(bottom),
                 plot_grid(legend),
                 plot_grid(xsubtitle),
                 ncol=1,nrow=4,
                 heights=c(1, 0.05,0.05,0.05))
    
  }  
################################################################################
## FUNCTION: CER_EM_hour_Res
## Plots annual avg hours run and capacity factor for CER plants
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupHr_sub - Filtered version of Resource Group Hour Table
##    ZoneHr_Avg - Average hourly info in zone
##    Export - Exports selected from Zone Hourly Table
################################################################################
  
CER_EM_hour_Res<-function(case){
  
  GenText_Sz<-16
  
  p1 <- Hours_CER_Res(BC) +
    theme(legend.position ="none", 
           plot.caption = element_blank(),
           text = element_text(size = GenText_Sz),       
           plot.title = element_text(size = GenText_Sz),
           axis.title.y = element_text(size = GenText_Sz+6),
           legend.text = element_text(size =GenText_Sz-6))          
  
  p2 <- Emissions_CER_Res(BC)+
    theme(plot.caption = element_blank(), 
          text = element_text(size = GenText_Sz),       
          plot.title = element_text(size = GenText_Sz),
          axis.title.y = element_text(size = GenText_Sz+6),
          legend.text = element_text(size =GenText_Sz-6)) 
  
  # Get a common legend
  legend <- get_legend(p2)
  p2 <- p2 + theme(legend.position ="none")
  
  
  #Arrange all the plots
  grid.arrange(plot_grid(p1,p2, ncol=2, align="v", axis = "l", rel_widths = c(1,1)),
               plot_grid(legend),
               ncol=1,nrow=2,
               heights=c(1, 0.05))
  
  
}
  
################################################################################
## FUNCTION: CER_EM_CF_Res
## Plots annual avg emissions and capacity factor for CER plants
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupHr_sub - Filtered version of Resource Group Hour Table
##    ZoneHr_Avg - Average hourly info in zone
##    Export - Exports selected from Zone Hourly Table
################################################################################

CER_EM_CF_Res<-function(case){
  
  GenText_Sz<-16
  
  p1 <- CF_CER_Res(BC) +
    theme(legend.position ="none", 
          plot.caption = element_blank(),
          text = element_text(size = GenText_Sz),       
          plot.title = element_text(size = GenText_Sz),
          axis.title.y = element_text(size = GenText_Sz+6),
          legend.text = element_text(size =GenText_Sz-6))          
  
  p2 <- Emissions_CER_Res(BC)+
    theme(plot.caption = element_blank(), 
          text = element_text(size = GenText_Sz),       
          plot.title = element_text(size = GenText_Sz),
          axis.title.y = element_text(size = GenText_Sz+6),
          legend.text = element_text(size =GenText_Sz-6)) 
  
  # Get a common legend
  legend <- get_legend(p2)
  p2 <- p2 + theme(legend.position ="none")
  
  
  #Arrange all the plots
  grid.arrange(plot_grid(p1,p2, ncol=2, align="v", axis = "l", rel_widths = c(1,1)),
               plot_grid(legend),
               ncol=1,nrow=2,
               heights=c(1, 0.05))
  
  
}

################################################################################
## FUNCTION: CER_EM_hour_group
## Plot CER resource emissions and capacity factors
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupHr_sub - Filtered version of Resource Group Hour Table
##    ZoneHr_Avg - Average hourly info in zone
##    Export - Exports selected from Zone Hourly Table
################################################################################

CER_EM_hour_group<-function(case){
  
  GenText_Sz<-16
  
  p1 <- CF_CER_groups(BC) +
    theme(legend.position ="none", 
          plot.caption = element_blank(), 
          text = element_text(size = GenText_Sz),       
          plot.title = element_text(size = GenText_Sz),
          axis.title.y = element_text(size = GenText_Sz+6),
          legend.text = element_text(size =GenText_Sz-6)) 
  
  p2 <-  Emissions_CER_group(BC)+
    theme(plot.caption = element_blank(), 
          text = element_text(size = GenText_Sz),       
          plot.title = element_text(size = GenText_Sz),
          axis.title.y = element_text(size = GenText_Sz+6),
          legend.text = element_text(size =GenText_Sz-6)) 
  
  # Get a common legend
  legend <- get_legend(p2)
  p2 <- p2 + theme(legend.position ="none")
  
  
  #Arrange all the plots
  grid.arrange(plot_grid(p1,p2, ncol=2, align="v", axis = "l", rel_widths = c(1,1)),
               plot_grid(legend),
               ncol=1,nrow=2,
               heights=c(1, 0.05))
  
  
} 
  