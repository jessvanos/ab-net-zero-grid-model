################################################################################  
## FUNCTION: CompDay_Season
## Weekly storage output
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupHr_sub - Filtered version of Resource Group Hour Table
################################################################################
CompDay_Season<-function(year1,day1,case) {

# SELET MONTHS AND DATA
  # Plot 1
    month1<-01

  # plot 2
    month2<-07

  # Get y-max, demand to meet + exports
    ZPriceYr <- ZoneHr_Avg %>%
      filter(Run_ID == case,
              year(date)==year1)
    ExpoYr <- Export %>%
      filter(Run_ID == case,
             year(date)==year1)
    
    # Set the max and min for the plot Output axis (y), Set above max ( round up to nearest 1000)
    MX <- round_any(max(ZPriceYr$Demand + ExpoYr$Output_MWH+1070), 100, f = ceiling)

# GENERATE PLOTS
  # Create a graph for each month of the year
    p1 <- Day2(year1,month1,day1,MX,case) +
      theme(axis.title.y=element_blank(),
            plot.title=element_blank())
  
  # Get a common legend
    legend <- get_legend(p1)
    p1 <- p1 + theme(legend.position ="none")
  
    p2 <- Day2(year1,month2,day1,MX,case)  +
      theme(legend.position ="none",
            axis.title.y=element_blank(),
            plot.title=element_blank())
  
  # Plot Labels
    yleft <- textGrob("Output (MWh)", rot = 90, gp = gpar(fontsize = 26))


# PLOT TOGETHER
    grid.arrange(plot_grid(p1, p2,legend, ncol=3, align="v", axis = "l", rel_widths = c(1,1,0.3)),
                 ncol=1,nrow=1, 
                 heights=c(1),
                 left=yleft)
}
################################################################################  
## FUNCTION: CompDay_AESO
## Weekly storage output
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupHr_sub - Filtered version of Resource Group Hour Table
################################################################################
CompDay_AESO<-function(year1,month1,day1,case) {
      
# SELET DATES
    
    # Get y-max, demand to meet + exports
    ZPriceYr <- ZoneHr_Avg %>%
      filter(Run_ID == case,
             year(date)==year1)
    ExpoYr <- Export %>%
      filter(Run_ID == case,
             year(date)==year1)
    
    # Set the max and min for the plot Output axis (y), Set above max ( round up to nearest 1000)
    MX <- round_any(max(ZPriceYr$Demand + ExpoYr$Output_MWH+1070), 100, f = ceiling)
    
    # GENERATE PLOTS
    # Create a graph for each month of the year
    p1 <- Day2(year1,month1,day1,MX,case) +
      theme(axis.title.y=element_blank())
    
    # Get a common legend
    legend <- get_legend(p1)
    p1 <- p1 + theme(legend.position ="none")
    
    p2 <- Day_AESO(year1,month1,day1,MX)  +
      theme(legend.position ="none",
            axis.title.y=element_blank())
    
    # Plot Labels
    yleft <- textGrob("Output (MWh)", rot = 90, gp = gpar(fontsize = 15))
    
    
    # PLOT TOGETHER
    grid.arrange(plot_grid(p1, p2,legend, ncol=3, align="v", axis = "l", rel_widths = c(1,1,0.4)),
                 ncol=1,nrow=1, 
                 heights=c(1),
                 left=yleft)  
    }
    
################################################################################  
## FUNCTION: CompDay_Wind
## Weekly storage output
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupHr_sub - Filtered version of Resource Group Hour Table
################################################################################
CompDay_Wind<-function(year1,case) {
      
# SELET DATES FROM YEAR CHOSEN
    # Get max and min wind data. Look at data (minus imports)
    data <- ResGroupHr_sub%>%
      sim_filt1(.) %>%
      filter(Run_ID == case,
             Report_Year == year1,
             ID == "Wind") %>%
      mutate(DateNorm=date(date)) %>%
      group_by(DateNorm,ID)%>%
      summarise(TotalGen=sum(Output_MWH))
    
    DateMin<-data$DateNorm[which.min(data$TotalGen)]
      year1<-year(DateMin)
      month1<-month(DateMin)
      day1<-day(DateMin)
    
    DateMax<-data$DateNorm[which.max(data$TotalGen)]
      year2<-year(DateMax)
      month2<-month(DateMax)
      day2<-day(DateMax)
  
    # Get y-max, demand to meet + exports
    ZPriceYr <- ZoneHr_Avg %>%
      filter(Run_ID == case,
             year(date)==year1)
    ExpoYr <- Export %>%
      filter(Run_ID == case,
             year(date)==year1)
    
    # Set the max and min for the plot Output axis (y), Set above max ( round up to nearest 1000)
    MX <- round_any(max(ZPriceYr$Demand + ExpoYr$Output_MWH+1070), 100, f = ceiling)
    
    # GENERATE PLOTS
    # Create a graph for each month of the year
    p1 <- Day2(year1,month1,day1,MX,case) +
      theme(axis.title.y=element_blank(),
            plot.title=element_blank())
    
    # Get a common legend
    legend <- get_legend(p1)
    p1 <- p1 + theme(legend.position ="none")
    
    p2 <- Day2(year2,month2,day2,MX,case)  +
      theme(legend.position ="none",
            axis.title.y=element_blank(),
            plot.title=element_blank())
    
    # Plot Labels
    yleft <- textGrob("Output (MWh)", rot = 90, gp = gpar(fontsize = 15))
    
    
    # PLOT TOGETHER
    grid.arrange(plot_grid(p1, p2,legend, ncol=3, align="v", axis = "l", rel_widths = c(1,1,0.4)),
                 ncol=1,nrow=1, 
                 heights=c(1),
                 left=yleft)  
}

################################################################################  
## FUNCTION: CompDay_Solar
## Weekly storage output
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupHr_sub - Filtered version of Resource Group Hour Table
################################################################################
CompDay_Solar<-function(year1,case) {
  
  # SELET DATES FROM YEAR CHOSEN
  # Get max and min wind data. Look at data (minus imports)
  data <- ResGroupHr_sub%>%
    sim_filt1(.) %>%
    filter(Run_ID == case,
           Report_Year == year1,
           ID == "Solar") %>%
    mutate(DateNorm=date(date)) %>%
    group_by(DateNorm,ID)%>%
    summarise(TotalGen=sum(Output_MWH))
  
  DateMin<-data$DateNorm[which.min(data$TotalGen)]
  year1<-year(DateMin)
  month1<-month(DateMin)
  day1<-day(DateMin)
  
  DateMax<-data$DateNorm[which.max(data$TotalGen)]
  year2<-year(DateMax)
  month2<-month(DateMax)
  day2<-day(DateMax)
  
  # Get y-max, demand to meet + exports
  ZPriceYr <- ZoneHr_Avg %>%
    filter(Run_ID == case,
           year(date)==year1)
  ExpoYr <- Export %>%
    filter(Run_ID == case,
           year(date)==year1)
  
  # Set the max and min for the plot Output axis (y), Set above max ( round up to nearest 1000)
  MX <- round_any(max(ZPriceYr$Demand + ExpoYr$Output_MWH+1070), 100, f = ceiling)
  
  # GENERATE PLOTS
  # Create a graph for each month of the year
  p1 <- Day2(year1,month1,day1,MX,case) +
    theme(axis.title.y=element_blank(),
          plot.title=element_blank())
  
  # Get a common legend
  legend <- get_legend(p1)
  p1 <- p1 + theme(legend.position ="none")
  
  p2 <- Day2(year2,month2,day2,MX,case)  +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          plot.title=element_blank())
  
  # Plot Labels
  yleft <- textGrob("Output (MWh)", rot = 90, gp = gpar(fontsize = 26))
  
  
  # PLOT TOGETHER
  grid.arrange(plot_grid(p1, p2,legend, ncol=3, align="v", axis = "l", rel_widths = c(1,1,0.4)),
               ncol=1,nrow=1, 
               heights=c(1),
               left=yleft)  
}

################################################################################  
## FUNCTION: CompDay_Years
## Weekly storage output
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupHr_sub - Filtered version of Resource Group Hour Table
################################################################################
CompDay_Years<-function(year1,year2,month1,day1,case) {
  
  # SELET MONTHS AND DATA
  
  # Get y-max, demand to meet + exports
  ZPriceYr <- ZoneHr_Avg %>%
    filter(Run_ID == case,
           year(date) %in% c(year1,year2))
  ExpoYr <- Export %>%
    filter(Run_ID == case,
           year(date) %in% c(year1,year2))
  
  
  # Set the max and min for the plot Output axis (y), Set above max ( round up to nearest 1000)
  MX <- round_any(max(ZPriceYr$Demand + ExpoYr$Output_MWH+1070), 100, f = ceiling)
  
  # GENERATE PLOTS
  # Create a graph for each month of the year
  p1 <- Day2(year1,month1,day1,MX,case) +
    theme(axis.title.y=element_blank(),
          plot.title=element_blank())
  
  # Get a common legend
  legend <- get_legend(p1)
  p1 <- p1 + theme(legend.position ="none")
  
  p2 <- Day2(year2,month1,day1,MX,case)  +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          plot.title=element_blank())
  
  # Plot Labels
  yleft <- textGrob("Output (MWh)", rot = 90, gp = gpar(fontsize = 15))
  
  
  # PLOT TOGETHER
  grid.arrange(plot_grid(p1, p2,legend, ncol=3, align="v", axis = "l", rel_widths = c(1,1,0.4)),
               ncol=1,nrow=1, 
               heights=c(1),
               left=yleft)
}