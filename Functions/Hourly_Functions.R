################################################################################
## FUNCTION: day2
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

day2 <- function(year, month, day, case, Restype, maxP) {
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
  # Filter data for WIND or ALL
  if (Restype=="WIND"){
    data<-data %>%
      filter(ID=="Wind")

    DY <- HrTime(data,year,month,day)
    
    # Set the max and min for the plot Output axis (y), Set slightly above max ( round up to nearest 500)
    MX <- maxP
    
    ## PLOT WITH AREA PLOT
    
    ggplot() +
      geom_area(data = DY, aes(x = date, y = Output_MWH, fill = ID, colour = ID), 
                alpha=0.7, size=0.5) +
      
      # Set the theme for the plot
      theme_bw() +
      theme(legend.position = "bottom") +
      
      theme(text=element_text(family=Plot_Text)) +
      
      theme(plot.title = element_text(size= Tit_Sz,hjust = 0.5)) +
      
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
            #panel.margin = unit(0,"null"),
            plot.margin = rep(unit(0,"null"),4),
            text = element_text(size= Overall_Sz)
      ) +
      guides(fill = guide_legend(nrow = 2)) +
      
      scale_x_datetime(expand=c(0,0),date_labels = "%H",breaks = "8 hours") +
      
      scale_y_continuous(expand=c(0,0), limits = c(0,MX), 
                         breaks = seq(0, MX, by = MX/4)) +
      
      labs(title=paste(format(DY$date[1],"%b-%e")),x = "Date", y = "Output (MWh)", fill = "Resource",colour = "Resource") +
      
      #Add colour
      scale_fill_manual(values=c(cOL_WIND)) +
      
      # Make outline the same as fill colors
      scale_colour_manual(values=c(cOL_WIND))
    
  # The ALL case  
  }else {
  
    ## SELECT A SINGLE DAY
    # Select only a single day from the zone Hourly, and Export data
    DY <- HrTime(data,year,month,day)
    ZPrice <- HrTime(ZoneHr_Avg,year,month,day) %>%
      filter(Run_ID == case)
  
    # Set the max and min for the plot Output axis (y), Set slightly above max ( round up to nearest 500)
    MX <- maxP
    
    ## PLOT WITH AREA PLOT
    
    ggplot() +
      geom_area(data = DY, aes(x = date, y = Output_MWH, fill = ID, colour = ID), 
                alpha=0.7, size=0.5) +
      
      # Add hourly load line (black line on the top)
      geom_line(data = ZPrice, 
                aes(x = date, y = Demand,colour = "Demand"), size=1.5, colour = "black") +
  
      # Set the theme for the plot
      theme_bw() +
      theme(legend.position = "bottom") +
      
      theme(text=element_text(family=Plot_Text)) +
      
      theme(plot.title = element_text(size= Tit_Sz,hjust = 0.5)) +
      
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
            #panel.margin = unit(0,"null"),
            plot.margin = rep(unit(0,"null"),4),
            text = element_text(size= Overall_Sz)
      ) +
      guides(fill = guide_legend(nrow = 2)) +
      
      scale_x_datetime(expand=c(0,0),date_labels = "%H",breaks = "8 hours") +
      
      scale_y_continuous(expand=c(0,0), limits = c(0,MX), 
                         breaks = seq(0, MX, by = MX/4)) +
      
      labs(title=paste(format(DY$date[1],"%b-%e")),x = "Date", y = "Output (MWh)", fill = "Resource",colour = "Resource") +
      
      #Add colour
      scale_fill_manual(values = colours1) +
      
      # Make outline the same as fill colors
      scale_colour_manual(values = colours1)
    }
}

################################################################################
## FUNCTION: YearOfDays
## Plots output for every first Tuesday of every month. 
## MAY NOT WORK if running more than one week per month.In which case - edit the plot day inputs directly
##
## INPUTS: 
##    year
##    day - Date to plot, 3 for tuesday
##    case - Run_ID which you want to plot
##    Restype - WIND or ALL
##    maxP - Output axis max
## TABLES REQUIRED: 
##    ResGroupHr_sub - Filtered version of Resource Group Hour Table
##    ZoneHr_Avg - Average hourly info in zone
##    Export - Exports selected from Zone Hourly Table
################################################################################

YearOfDays <-function(year,day,case,Restype,maxP) {
  
# Filters for the desired case study from the resource groups
data <- ResGroupHr_sub%>%
  sim_filt1(.) %>%
  subset(., select=-c(Report_Year,Capacity_Factor)) %>%
  rbind(.,Import) %>%
  filter(Run_ID == case)

# Find all the Tuesdays for the specified year
data2 <- YrDay_Time(data,year,day)
  # Reorder it is it is out of order
  data2$Day <-as.numeric(strftime(data2$date,format='%j'))
  data2 <- data2[order(data2$Day),]

# Get the unique days and months which have hourly data. Collect for later use
AllBreaks <- (unique(format(data2$date,format="%m-%d")))

  MonthBreaks <-as.numeric(substr(AllBreaks,1,2))
  DayBreaks <- as.numeric(substr(AllBreaks,4,5))
  

# Create a graph for each month of the year
p1 <- day2(year,MonthBreaks[1],DayBreaks[1],case,Restype,maxP) +
  theme(axis.title.x=element_blank())


p2 <-day2(year,MonthBreaks[2],DayBreaks[2],case,Restype,maxP) +
  theme(legend.position ="none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank())

p3 <- day2(year,MonthBreaks[3],DayBreaks[3],case,Restype,maxP) +
  theme(legend.position ="none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank())

p4 <-day2(year,MonthBreaks[4],DayBreaks[4],case,Restype,maxP) +
  theme(legend.position ="none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank())

p5 <- day2(year,MonthBreaks[5],DayBreaks[5],case,Restype,maxP) +
  theme(legend.position ="none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank())

p6 <- day2(year,MonthBreaks[6],DayBreaks[6],case,Restype,maxP) +
  theme(legend.position ="none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank())

p7 <- day2(year,MonthBreaks[7],DayBreaks[7],case,Restype,maxP) +
  theme(legend.position ="none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank())

p8 <- day2(year,MonthBreaks[8],DayBreaks[8],case,Restype,maxP) +
  theme(legend.position ="none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank())

p9 <-day2(year,MonthBreaks[9],DayBreaks[9],case,Restype,maxP) +
  theme(legend.position ="none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank())

p10 <-day2(year,MonthBreaks[10],DayBreaks[10],case,Restype,maxP) +
  theme(legend.position ="none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank())

p11 <-day2(year,MonthBreaks[11],DayBreaks[11],case,Restype,maxP) +
  theme(legend.position ="none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank())

p12 <- day2(year,MonthBreaks[12],DayBreaks[12],case,Restype,maxP) +
  theme(legend.position ="none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank())

# Get a common legend
legend <- get_legend(p1)
p1 <- p1 + theme(legend.position ="none")


# Cheat way to put an x title in
xtitle <- ggplot() +
  annotate("text", x = 10,  y = 10,
           size = 6,
           label = "Hour of Day") +
  theme_void()

# Label the source and year
xsubtitle <- ggplot() +
  annotate("text", x = 10,  y = 10,
           size = 4,
           label = paste("Running Every Second Week, Showing",wday(day,label=TRUE, abbr=FALSE), ", Year:",year,", Resource:",Restype,", Database:",SourceDB)) +
  theme_void()

#Create a big window
windows(26,8)


# Arrange all the plots in a line
grid.arrange(plot_grid(p1, p2, p3, p4, p5, p6,p7, p8, p9, p10, p11,p12, ncol=12, align="v", axis = "l", rel_widths = c(1.5,1,1,1,1,1,1,1,1,1,1,1)),
             plot_grid(xtitle),
             plot_grid(legend),
             plot_grid(xsubtitle),
             ncol=1,nrow=4,
             heights=c(1,0.1,0.2,0.1))

}
