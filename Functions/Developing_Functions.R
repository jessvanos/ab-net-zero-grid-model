################################################################################
# TITLE: Developing_Functions
# DESCRIPTION: Functions I am still working to build, not yet categorized. 

# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: January 6, 2023; LAST EDIT: January 6, 2023
#
################################################################################

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

###############################################################################

################################################################################
## FUNCTION: 
## 
##
## INPUTS: 
##    
## TABLES REQUIRED: 
##    
################################################################################
# Cap factor 
CF_AllR <- function(case) {
  
  #ResourceHr <- dbReadTable(con,'ResourceHour1')
  
  data <- ResHr %>%
    filter(Run_ID == case,
           #Output != 0,
           Primary_Fuel == "Wind",
           !is.na(Capacity_Factor)
    ) %>%
    mutate(Name = gsub("^.*?from ","",Name),
           #Name = gsub("^.*? ","",Name),
           #Name = case_when(str_detect(Name,"Pot")~paste("Hypothetical:",Name),
           #                  TRUE~Name),
           #Name = gsub("\\s*\\([^\\)]+\\)", "",Name)
           #time = as.POSIXct(as.character(ymd_h(gsub(" Hr ", "_",Time_Period))), 
           #                 tz = "MST")-(60*60)
    ) %>%
    #na.omit() %>%
    subset(.,select=c(Name,Time_Period,Output_MWH,Capacity,
                      #Percent_Marginal,Capacity,Capability,ID,Primary_Fuel,
                      Capacity_Factor,
                      #Beg_Date,End_Date,
                      Report_Year)) %>%
    group_by(Report_Year,Name) %>%
    summarise(Capacity = sum(Capacity),
              Output_MWH = sum(Output_MWH),
              Cap_Fac = Output_MWH/Capacity,
              #CF = mean(Capacity_Factor),
              #diff = Cap_Fac-CF
    ) %>%
    mutate(sit = case_when(str_detect(Name,"Pot")~"Hypo",
                           TRUE~"AESO"),
           Name = gsub("\\s*\\([^\\)]+\\)", "",Name),
           Name = case_when(str_detect(Name,"John D")~"John DOr",
                            TRUE~Name))
  
  ggplot() +
    geom_line(data = data, 
              aes(x = Report_Year, y = Cap_Fac,colour=Name,linetype=sit,
                  size = sit), 
    ) +
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          #axix.title.x = element_blank(),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          text = element_text(size= 15),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(x = "", y = "Capacity Factor (%)", fill = "Potential New Resource") +
    scale_x_continuous(expand=c(0,0)) +
    scale_color_manual("Hypothetical Site",
                       values = c("steelblue","seagreen","tan",
                                             "saddlebrown","khaki","firebrick",
                                             "goldenrod","black"),
                                             limits = c('Anzac','Bison Lake','Hinton','John DOr',
                                                        'Kehewin',
                                                        'Lesser Slave Lake','Pigeon Lake')) +
    scale_linetype_manual("",values = c("AESO"="dotdash","Hypo"="solid"),
                          labels=c("Wind farm from AESO queue",
                                   "Wind farm at hypothetical site")) +
    scale_size_manual("",values = c(0.5,2),
                      labels=c("Wind farm from AESO queue",
                               "Wind farm at hypothetical site")) +
    guides(color = guide_legend(override.aes = list(size = 3)),
    )
}


###########################################################################################

# Revenue


##########################################################################################
# Capture Prices

capture_price <- function(year1, year2, case) {
  # Plots the difference between the average capture price realized by each 
  # generation technology and the mean price for the same time period. 
  
  # Based on a plot designed by Dr. Andrew Leach
  
  # Filters data set for required timeframe for simulated data
  SampleSimZ <- ZoneHr_Avg  %>%
    filter(year(date) >= year1,
           year(date) <= year2,
           Run_ID == case,
    ) %>%
    subset(., select = c(date, Price, Imports, Exports))
  
  SampleSim <- ResGroupHr %>%
    filter(year(date) >= year1,
           year(date) <= year2,
           Output_MWH >= 0,
           Run_ID == case) %>%
    sim_filt(.) %>%
    mutate(Year = as.factor(Report_Year)) %>%
    subset(., select = c(date, ID, Output_MWH, Energy_Revenue, Year)) 
  
  SamSim <- merge(SampleSimZ, SampleSim, by = "date") %>%
    subset(., select = -c(Imports,Exports))
  
  # This section calculates the achieved prices for imports and exports
  Imp <- SampleSimZ %>%
    mutate(Energy_Revenue = Price*Imports/1000, 
           Year = as.factor(year(date)), 
           ID = "IMPORT",
           Output_MWH = Imports) %>%
    subset(., select = -c(Imports,Exports))
  
  Exp <- SampleSimZ %>%
    mutate(Energy_Revenue = Price*Exports/1000, 
           Year = as.factor(year(date)), 
           ID = "EXPORT",
           Output_MWH = Exports) %>%
    subset(., select = -c(Imports,Exports))
  
  Sim <- rbind(SamSim,Imp,Exp) %>%
    group_by(ID,Year,date) %>%
    summarise(total_rev = sum(Energy_Revenue*1000), 
              total_gen = sum(Output_MWH),
              price_mean=mean(Price)) %>%
    ungroup() %>%
    mutate(Plant_Type = ID) %>%
    group_by(Plant_Type,Year) %>%
    summarise(capture = sum(total_rev)/sum(total_gen),
              avg_rev = sum(total_rev)/sum(total_gen),
              p_mean=mean(price_mean, na.rm = TRUE)) %>%
    mutate(sit = "Simulation")
  
  sz <- 12
  
  # Plot the data
  ggplot(Sim,
         aes(Year,capture-p_mean),
         alpha=0.8)+
    geom_col(aes(Plant_Type,capture-p_mean),
             size=1.5,position = position_dodge(width = .9),width = .6)+
    geom_hline(yintercept=0, linetype="solid", color="gray",size=1)+
    scale_color_manual("",values=c("grey50","royalblue"))+
    scale_fill_manual("",values=c("grey50","royalblue"))+
    facet_grid(~Year) +
    scale_y_continuous(expand=c(0,0),
                       #                       limits = c(-50,100),
                       #                       breaks = seq(-40,100, by = 20)
    ) +
    labs(x="",y="Revenue Relative to \nMean Price ($/MWh)",
         title=paste0("Energy Price Capture Differential ($/MWh, ",year1,"-",year2,")"),
         caption=SourceDB) +
    
    theme(panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
          panel.grid.minor.y = element_line(color = "lightgray",linetype="dotted"),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          axis.text = element_text(size = sz),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title = element_text(size = sz),
          plot.subtitle = element_text(size = sz-2,hjust=0.5),
          plot.caption = element_text(face="italic",size = sz-4,hjust=0),
          plot.title = element_text(hjust=0.5,size = sz+2),
          #          plot.margin=unit(c(1,1,1.5,1.2),"cm"),
          
          # For transparent background
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          panel.border = element_rect(colour = "black", fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) 
}


#################################################################################################
# AChived pool 
ach_pool <- function(year1, year2, case) {
  # Plots the achieved premium-to-pool price realized by each generation 
  # technology. 
  #
  #The ratio of the achieved margin to the average pool price
  # Achieved price represents the average price realized in the wholesale energy 
  #
  # market for electricity delivered to the grid and is calculated as the 
  # weighted average of the hourly pool price, where the price in each settlement 
  # interval is weighted by the net-to-grid generation in that interval.
  #
  # Like AESO Market Report 2021 Figure 18
  
  
  # Filters data set for required timeframe for simulated data
  SampleSimZ <- ZoneHr_Avg  %>%
    filter(year(date) >= year1,
           year(date) <= year2,
           Run_ID == case,
    ) %>%
    subset(., select = c(date, Price, Imports, Exports))
  
  SampleSim <- ResGroupHr %>%
    filter(year(date) >= year1,
           year(date) <= year2,
           Run_ID == case) %>%
           sim_filt(.) %>%
    subset(., select = c(date, ID, Output))
  
  # Combine the two
  SampleSim <- merge(SampleSimZ, SampleSim, by = "date")
  SampleSim <- SampleSim %>%
    mutate(Year = as.factor(year(date)))
  
  # This section calculates the annual average pool prices for simulated data
  AvgSim <- SampleSim %>%
    group_by(Year) %>%
    summarise(Pool = mean(Price))
  
  # This section calculates the achieved prices for imports and exports
  Imp <- SampleSimZ %>%
    mutate(WeighPrice = Price*Imports, Year = as.factor(year(date))) %>%
    group_by(Year) %>%
    summarise(WeighPrice = sum(WeighPrice), gen = sum(Imports)) %>%
    mutate(AchPrice = WeighPrice/gen, Plant_Type = "IMPORT")
  
  Exp <- SampleSimZ %>%
    mutate(WeighPrice = Price*Exports, Year = as.factor(year(date))) %>%
    group_by(Year) %>%
    summarise(WeighPrice = sum(WeighPrice), gen = sum(Exports)) %>%
    mutate(AchPrice = WeighPrice/gen, Plant_Type = "EXPORT")
  
  # This section calculates the achieved prices
  AchSim <- SampleSim %>%
    mutate(WeighPrice = Price*Output, Plant_Type = ID) %>%
    #    ungroup() %>%
    #    mutate(Year = as.factor(year(time))) %>%
    group_by(Year, Plant_Type) %>%
    summarise(WeighPrice = sum(WeighPrice), gen = sum(Output)) %>%
    mutate(AchPrice = WeighPrice/gen)
  
  AchSim <- rbind(AchSim, Imp, Exp)
  
  # Combine the two
  Sim <- merge(AvgSim, AchSim, by = "Year")
  
  # Calculate the achieved margin and the achieved premium-to-pool price
  Sim <- Sim %>%
    mutate(Margin = AchPrice-Pool, Ratio = Margin/Pool, sit = "Simulation")
  
  Sim$Plant_Type <- factor(Sim$Plant_Type, levels=c("Coal-to-Gas", "Natural Gas","Natural Gas + CCS","Natual Gas and Hydrogen Blend","Hydrogen" , 
                                                    "Hydro","Other","Wind", "Solar", "Storage","Coal","Cogen", "EXPORT", "IMPORT"))
  
  levels(Sim$Plant_Type) <- c("Coal-to-Gas", "Natural Gas","Natural Gas + CCS","Natual Gas and Hydrogen Blend","Hydrogen" , 
                              "Hydro","Other","Wind", "Solar", "Storage","Coal","Cogen", "Export", "Import")
  

  sz <- 12
  
  # Plot the data
  ggplot() +
    geom_col(data = Sim, position = "dodge", alpha = 0.8, width = 0.7,
             aes(x = Plant_Type, y = Ratio)) +
    #    geom_text(total, aes(label = gen), vjust = -0.5, angle = 90) +
    facet_grid(~Year) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title.x = element_blank(),
          axis.text = element_text(size = sz),
          axis.title = element_text(size = sz),
          plot.title = element_text(size = sz+2),
          legend.text = element_text(size = sz),
          panel.grid.minor = element_blank(),
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
    labs(y = "Achieved Premium to Pool Price", 
         title = "Annual achieved premium to pool price",
         subtitle = SourceDB) +
    scale_fill_manual(values = AESO_colours) +
    scale_y_continuous(expand=c(0,0),
                       limits = c(-0.5,1.1),
                       breaks = c(-0.5, -0.3, -0.1, 0, 0.1, 0.3, 0.5, 0.7, 0.9, 1.1),
                       labels = percent
    )
}

##############################################################################################
# marginal tech
margin <- function(year1, year2, case) {
  # Plots the marginal price-setting technology for AESO and Sim
  # Like AESO Market Report 2021 Figure 19
  
  #  MargSim <- Year %>%
  #    filter(Run_ID == case,
  #           Condition == "Average",
  #           ID == "LTO_Coal" | ID == "AB_NGCONV" | ID == "AB_SCCT_noncogen" |
  #           ID == "AB_CCCT_noncogen" | ID == "LTO_Cogen" | ID == "LTO_Other" |
  #           ID == "LTO_Hydro" | ID == "LTO_Solar" | ID == "LTO_Storage" | 
  #           ID == "LTO_Wind" | ID == "Intertie") %>%
  #    subset(.,select=c(ID,Report_Year,Condition,Percent_Marginal))
  #    filter(Condition )
  
  cogen <- c("ALS1", "APS1", "BCR2", "BCRK", "BFD1", "CL01", "CNR5", "COD1", 
             "DOWG", "EC04", "FH1", "HMT1", "HRT1", "IOR1", "IOR2", "JOF1",
             "MEG1", "MKR1", "MKRC", "MUL1", "NX02", "PR1", "PW01", "RL1", 
             "SCL1", "SCR1", "SCR5", "SCR6", "SDH1", "TC01", "TC02", "TLM2", 
             "UOC1", "UOA1", "IOR3", "IOR4", "SHCG", "PEC1", "CRG1")
  
  ngcc <- c("CAL1", "CMH1", "EC01", "EGC1", "FNG1", "NX01", "Cascade")
  
  scgt <- c("ALP1", "ALP2", "ANC1", "BHL1", "CRS1", "CRS2", "CRS3", "DRW1", 
            "ENC1", "ENC2", "ENC3", "GEN5", "GEN6", "HSM1", "ME02", "ME03", 
            "ME04", "MFG1", "NAT1", "NPC1", "NPC2", "NPC3", "NPP1", "PH1", "PMB1",
            "RB5", "SET1",  "VVW1", "VVW2", "WCD1"
  )
  
  coal <- c("BR3", "BR4", "BR5", "GN1", "GN2", "GN3", "HRM", "KH1", "KH2", 
            "KH3", "SD2", "SD3", "SD4", "SD5", "SD6", "SH1", "SH2")
  
  hydro <- c("BIG", "BOW1", "BRA", "CHIN", "DKSN", "ICP1", "OMRH", "RYMD", "TAY1")
  
  ngconv <- c("Retrofit")
  
  other <- c("AFG1", "BON1", "CCMH", "DAI1", "DV1", "EAGL", "GPEC", "GOC1", "NRG3", 
             "SLP1", "SRL1", "WEY1", "WST1", "WWD1")
  
  solar <- c("BSC1", "HUL1", "INF1", "VXH1", "BRD1", "BUR1", "CLR1", "CLR2", 
             "SUF1", "WEF1", "JER1", "HYS1", "BRK1", "BRK2", "COL1", "CRD2", 
             "CRD1", "MON1", "NMK1", "STR1", "STR2", "TVS1", "EPS1", "VCN1", 
             "KKP1", "KKP2", "MIC1", "CLY1", "CLY2", "TRH1", "Tilley", "Coulee",
             "Enchant", "Stavely")
  
  wind <- c("CRE3", "CR1", "AKE1", "IEW1", "KHW1", "SCR2", "TAB1", "GWW1", "SCR3", 
            "BTR1", "OWF1", "IEW2", "SCR4", "CRR2", "NEP1", "HAL1", "BUL1", 
            "BUL2", "BSR1", "CRR1", "RIV1", "WHT1", "ARD1", "WRW1", "WHT2", 
            "RTL1", "WHE1", "FMG1", "JNR1", "JNR2", "JNR3", "HHW1", "Garden", 
            "Cypress", "Buffalo", "Grizzly", "Lanfine")
  
  storage <- c("*CRS", "*GN1&2", "ERV1", "ERV2", "ERV3", "SUM1")
  
  Sim <- ZoneHr_Avg %>%
    filter(Report_Year >= year1 & 
             Report_Year <= year2,
           Run_ID == case)  %>%
    mutate(Name = Marginal_Resource,
           Year = Report_Year,
           #           Plant_Type = "Group"
    ) %>%
    subset(.,select=c(date,Name,Price,Year)) %>%
    mutate(Plant_Type = case_when(
      grepl(paste(cogen,collapse="|"), x=Name) ~ "COGEN",
      grepl(paste(ngcc,collapse="|"), x=Name) ~ "NGCC",
      grepl(paste(scgt,collapse="|"), x=Name) ~ "SCGT",
      grepl(paste(coal,collapse="|"), x=Name) ~ "COAL",
      grepl(paste(hydro,collapse="|"), x=Name) ~ "HYDRO",
      grepl(paste(ngconv,collapse="|"), x=Name) ~ "NGCONV",
      grepl(paste(other,collapse="|"), x=Name) ~ "OTHER",
      grepl(paste(solar,collapse="|"), x=Name) ~ "SOLAR",
      grepl(paste(wind,collapse="|"), x=Name) ~ "WIND",
      grepl(paste(storage,collapse="|"), x=Name) ~ "STORAGE",
      grepl("Intertie", x=Name) ~ "INTERTIE"
    )) %>%
    group_by(Plant_Type,Year) %>%
    summarise(freq = n()) %>%
    ungroup() %>%
    group_by(Year) %>%
    mutate(tot = sum(freq),
           perc = freq/tot*100,
           sit = "Simulation") %>%
    ungroup() 
  
  Sim <- na.omit(Sim)  
  
  #  totHour <- RHour %>%
  #    filter(Report_Year >= year1 & 
  #             Report_Year <= year2,
  #           Percent_Marginal == 100,
  #           Run_ID == case) %>%
  #    subset(.,select=c(date,Name,Dispatch_Cost,Incr_Cost,Primary_Fuel,Percent_Marginal,Zone))
  
  #  Sim <- Hr %>%
  #    filter(Report_Year >= year1 & 
  #             Report_Year <= year2,
  #           Run_ID == case, 
  #           Name != "Alberta",
  #           Percent_Marginal == 100) %>%
  #    subset(.,select=c(Name,Time_Period,ID,date))
  
  #  SimComb <- merge(Sim,totZone, by=c("date",))
  
  #  data1 <- left_join(totZone, totHour, by=c("date","Name")) 
  
  #  data1 <- data1 %>%
  #    group_by(Name, Report_Year) %>%
  #    mutate(perc = 1-ecdf(Price)(Price)) #%>%
  
  Sample <- merit_filt %>%
    filter(year >= year1,
           year <= year2) %>%
    subset(.,select=c(date,year,he,asset_id,AESO_Name,Plant_Type,price,
                      actual_posted_pool_price,merit,dispatched_mw)) %>%
    mutate(Year = year)
  
  Act <- Sample %>%
    filter(dispatched_mw != 0) %>%
    group_by(date, he) %>%
    slice_max(n=1,merit) %>%
    ungroup() %>%
    group_by(Year,Plant_Type) %>%
    summarise(freq = n()) %>%
    ungroup() %>%
    group_by(Year) %>%
    mutate(tot = sum(freq), 
           perc = freq/tot*100,
           sit = "Actual")
  
  total <- rbind(Sim,Act)
  
  # Reorder the factors for plotting
  total$Plant_Type<-fct_relevel(total$Plant_Type, "NGCC",after=Inf)
  total$Plant_Type<-fct_relevel(total$Plant_Type, "SCGT",after=Inf)
  total$Plant_Type<-fct_relevel(total$Plant_Type, "COGEN",after=Inf)
  total$Plant_Type<-fct_relevel(total$Plant_Type, "INTERTIE",after=Inf)
  total$Plant_Type<-fct_relevel(total$Plant_Type, "NGCONV",after=Inf)
  total$Plant_Type<-fct_relevel(total$Plant_Type, "STORAGE",after=Inf)
  total$Plant_Type<-fct_relevel(total$Plant_Type, "HYDRO",after=Inf)
  total$Plant_Type<-fct_relevel(total$Plant_Type, "OTHER",after=Inf)
  total$Plant_Type<-fct_relevel(total$Plant_Type, "WIND",after=Inf)
  
  sz <- 12
  
  # Plot the data
  ggplot(total,
         aes(Plant_Type,perc,colour=sit,fill=sit),
         alpha=0.8)+
    geom_col(aes(Plant_Type,perc,colour=sit,fill=sit),
             size=1.5,position = position_dodge(width = .9),width = .6)+
    geom_hline(yintercept=0, linetype="solid", color="gray",size=1)+
    scale_color_manual("",values=c("grey50","royalblue"))+
    scale_fill_manual("",values=c("grey50","royalblue"))+
    facet_grid(~Year) +
    scale_y_continuous(expand=c(0,0),
                       #                       limits = c(-50,100),
                       #                       breaks = seq(-40,100, by = 20)
    ) +
    labs(x="",y="Percentage of Time",
         title="Annual marginal price-setting technology",
         subtitle = DB,
         caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach") +
    theme(panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
          panel.grid.minor.y = element_line(color = "lightgray",linetype="dotted"),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          axis.text = element_text(size = sz),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title = element_text(size = sz),
          plot.subtitle = element_text(size = sz-2,hjust=0.5),
          plot.caption = element_text(face="italic",size = sz-4,hjust=0),
          plot.title = element_text(hjust=0.5,size = sz+2),
          #          plot.margin=unit(c(1,1,1.5,1.2),"cm"),
          
          # For transparent background
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          panel.border = element_rect(colour = "black", fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) 
  
}

#########################################################################################
# revenue
################################################################################
# Annual revenue for resources
################################################################################

Revenue <- function(case) {
  data1 <- ResHr %>%
    filter(Run_ID == case,
           #grepl('New Resource',Name),
           #grepl("Pot",Name),
           Primary_Fuel == "Wind",
           !is.na(Capacity_Factor),
           #Report_Year >= 2024,
           Report_Year <= 2035
    ) %>%
    mutate(#Year = (Report_Year),
      Year = as.factor(Report_Year),
    ) %>%
    group_by(Year) %>%
    mutate(
      Fleet_AveRev = sum(Revenue*1000)/sum(Output_MWH),
    ) %>%
    ungroup() %>%
    filter(grepl('New Resource',Name),
           #grepl("Pot",Name),
    ) %>%
    mutate(Name = gsub("\\s*\\([^\\)]+\\)", "",Name),
           Name = gsub("^.*?P","",Name),
           Name = gsub("[0-9] *","",Name),
           Name = gsub("^.*?_","",Name),
           Name = gsub("Joss Wind","",Name)
    ) %>%
    group_by(Name,Year) %>% #Year
    summarise(Capacity = sum(Capacity),
              Fleet_AveRev = median(Fleet_AveRev),
              Output_MWH = sum(Output_MWH),
              Cap_Fac = Output_MWH/Capacity,
              Revenue = sum(Revenue)*1000,
              AveRev = Revenue/Output_MWH,
              diff = AveRev-Fleet_AveRev,
    ) %>%
    ungroup() %>%
    group_by(Name) %>%
    summarise(#Capacity = sum(Capacity),
      #Output_MWH = sum(Output_MWH),
      Cap_Fac = mean(Cap_Fac),
      #CF = Output_MWH/Capacity,
      Revenue = mean(Revenue),
      AveRev = mean(AveRev),
      diff = mean(diff)) %>%
    mutate(sit = case_when(grepl("Anzac",Name)~"Hypo",
                           grepl("Bison Lake",Name)~"Hypo",
                           grepl("Hinton",Name)~"Hypo",
                           grepl("John D'Or",Name)~"Hypo",
                           grepl("Kehewin",Name)~"Hypo",
                           grepl("Lesser",Name)~"Hypo",
                           grepl("Pigeon",Name)~"Hypo",
                           TRUE~"Active"))
  
  sz <- 12
  CF_color<-"grey40"
    AR_color<-"black"
      
    #    revmin <- min(floor(data1$AveRev),0)
    #    CFmax <- max(data1$Cap_Fac)
    #    revmax <- max(ceiling(data1$AveRev))
    #    multifact <- CFmax/revmax
    
    ggplot(data1,
           aes(x=fct_reorder(Name,AveRev)))+
      geom_col(aes(y=Cap_Fac/0.013,colour=sit,fill=sit),#fill=CF_color,
               width=0.7,color=CF_color,alpha = 0.6,#color="black"
      ) +
      geom_col(aes(y=AveRev,fill=sit),
               width=0.4, alpha=1, color="black" #position = position_dodge(width = .85),width = .6
                 #fill=AR_color
      )+
      
      geom_hline(yintercept=0) +
      #geom_hline(yintercept=0, linetype="solid", color="gray",size=1)+
      scale_color_manual("",values=c("Active"="grey","Hypo"="forestgreen"),
                         labels = c("Active"="AESO Queue",
                                    "Hypo"="Hypothetical Site"))+
      scale_fill_manual("",values=c("Active"="grey","Hypo"="forestgreen"),
                        labels = c("Active"="AESO Queue",
                                   "Hypo"="Hypothetical Site"))+
      #    facet_grid(~Year) +
      scale_y_continuous(expand=c(0,0),
                         limits = c(-20,45),
                         breaks = seq(-15,45, by = 10),
                         sec.axis = sec_axis(trans=~.*(0.013*100), 
                                             name ="Average Annual Capacity Factor (%)",
                                             breaks = seq(0,60,5))
      ) +
      labs(x="",y="Average Annual Energy Revenue ($/MWh)") +
      theme(axis.text = element_text(size = sz),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            axis.text.y.right = element_text(color = CF_color),
            axis.title.y = element_text(size = sz,color = AR_color,face="bold"),
            axis.title.y.right = element_text(size = sz,color = CF_color,
                                              face="bold",
                                              margin=unit(c(0,0,1,0.3), "cm")
            ),
            
            # For transparent background
            panel.background = element_rect(fill = "transparent"),
            panel.grid = element_blank(),
            panel.spacing = unit(1.5, "lines"),
            panel.border = element_rect(colour = "black", fill = "transparent"),
            plot.background = element_rect(fill = "transparent", color = NA),
            legend.position = "top",
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
      ) 
    
}

################################################################################
## FUNCTION: 
## 
##
## INPUTS: 
##    
## TABLES REQUIRED: 
##    
################################################################################
test<-function(case) {
  ## FOR NEW RESOURCES
DataHr <- ResHr %>%
  mutate(year = year(date),
         time = date) %>%
  filter(Run_ID == case,
         Condition == "Average",
         Zone == "WECC_Alberta") %>%
  filter(grepl('ANRN',ID)) %>%
  mutate(Report_Year=as.numeric(Report_Year)) %>%
  subset(.,select=c(ID,Name,Report_Year,date,Capability,Capacity,Dispatch_Cost,Output_MWH,Capacity_Factor,
                    Fuel_Usage,Primary_Fuel,
                    Net_Cost,Total_Cost_MWh,Fixed_Cost,Variable_OM_Cost,Total_Emission_Cost,Fuel_Cost,Startup_Cost,Build_Cost,
                    Revenue,Energy_Revenue_MWh,Value,Value_MWh,
                    Used_For_Op_Reserve, Forced_Outage,Maint_Outage,Total_Hours_Run,Beg_Date,End_Date))

gc()


DataYr <- ResYr %>%
  filter(Run_ID == case,
         Condition == "Average",
         Zone == "WECC_Alberta") %>%
  sim_filt5(.) %>%
  mutate(Report_Year=as.numeric(YEAR),
         Simulation_Name=paste(SourceDB)) %>%
  filter(Capacity>0) %>%
  subset(.,select=c(Name,Report_Year,Capability,Capacity,Dispatch_Cost,Output_MWH,Capacity_Factor,
                    Fuel_Usage,Primary_Fuel,
                    Net_Cost,Total_Cost_MWh,Fixed_Cost,Variable_OM_Cost,Total_Emission_Cost,Fuel_Cost,Startup_Cost,Build_Cost,
                    Revenue,Energy_Revenue_MWh,Value,Value_MWh,
                    Used_For_Op_Reserve, Forced_Outage,Maint_Outage,Total_Hours_Run,Beg_Date,End_Date,Simulation_Name))

################################################################################
## HOURLY RESOURCE  OUTPUT TABLES
## Resource Group annaul Information: 2022-2035. Gives info on resource group 
## costs, outputs, and emission costs. 
################################################################################
DataHr <- ResHr %>%
  mutate(year = year(date),
         time = date) %>%
  filter(Run_ID == case,
         Condition == "Average",
         Zone == "WECC_Alberta") %>%
  sim_filt5(.) %>%
  mutate(Report_Year=as.numeric(Report_Year),
         Simulation_Name=paste(SourceDB)) %>%
  subset(.,select=c(ID,Name,Report_Year,date,Capability,Capacity,Dispatch_Cost,Output_MWH,Capacity_Factor,
                    Fuel_Usage,Primary_Fuel,
                    Net_Cost,Total_Cost_MWh,Fixed_Cost,Variable_OM_Cost,Total_Emission_Cost,Fuel_Cost,Startup_Cost,Build_Cost,
                    Revenue,Energy_Revenue_MWh,Value,Value_MWh,
                    Used_For_Op_Reserve, Forced_Outage,Maint_Outage,Total_Hours_Run,Beg_Date,End_Date,Simulation_Name))  

## OTHER STUFF
  # REVENUE. in Can$000
  Data_Rev <-DataYr %>%
    subset(.,select=c(Name,Report_Year,
                      Revenue,Energy_Revenue_MWh)) %>%
    mutate(Type="Revenue")%>%
    rename(Total=Revenue,
           Total_Per_MWh=Energy_Revenue_MWh)
  # COST
  Data_Cost <-DataYr %>%
    subset(.,select=c(Name,Report_Year,
                      Net_Cost,Total_Cost_MWh)) %>%
    mutate(Type="Cost")%>%
    rename(Total=Net_Cost,
           Total_Per_MWh=Total_Cost_MWh)
  
}
################################################################################
## FUNCTION: WeekDSM
## Plots output for a week given study case, adjusts demand line for demand side curtailment.
##
## INPUTS: 
##    year, month, day - Date to plot
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupHr_sub - Filtered version of Resource Group Hour Table
##    ZoneHr_Avg - Average hourly info in zone
##    Export - Exports selected from Zone Hourly Table
################################################################################
WeekDSM <- function(year, month, day, case) {
  
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
  ZPrice <- WkTime(ZoneHr,year,month,day)  %>%
    filter(Name == "WECC_Alberta") %>%
    filter(Condition == "Average")%>%
    mutate(Demand_New=Demand-Demand_Side_Output)%>%
    subset(., select = c(date, Price, Baseline_Demand, Demand, Demand_Total,Demand_New,
                         Demand_Side_Output))
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
              alpha=0.7, size=0.5) +
    
    # Add hourly load line (black line on the top)
    geom_line(data = ZPrice, 
              aes(x = date, y = Demand_New), size=1.5, colour = "black") +
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
          
##############################################################################
# Bring in Resource Year Table and filter for relevant data. Format date columns
Add_Ret_data <- ResYr%>%
  sim_filt6(.) %>% #Filter to rename fuels
  subset(., select=c(Name,Condition,Capacity,Nameplate_Capacity,End_Date,Beg_Date,
                     Run_ID,Primary_Fuel,Time_Period,Capacity_Factor)) %>%
  filter(Run_ID == case)%>%
  mutate(Time_Period=as.numeric(Time_Period),
         End_Date=as.Date(End_Date,format = "%m/%d/%Y"),
         End_Year=year(End_Date),
         Beg_Date=as.Date(Beg_Date,format = "%m/%d/%Y"),
         Beg_Year=year(Beg_Date))%>%
  filter(Condition == "Average",
         Time_Period<=2035) 

# Set levels to each category in order specified
Add_Ret_data$Primary_Fuel <- factor(Add_Ret_data$Primary_Fuel, 
                                    levels=c("Coal","Coal-to-Gas", "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                             "Blended  Simple Cycle","Blended  Combined Cycle",
                                             "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                                             "Hydro", "Other",
                                             "Wind", "Solar", 
                                             "Storage - Battery", "Storage - Compressed Air", "Storage - Pumped Hydro", 
                                             "Cogeneration"))

# FILTER CAP RETIREMENTS
#Further filter peak capacity >0 (it is not yet retired), and end date = time period (to ensure you dont get doubles)
Retdata <- Add_Ret_data%>%
  group_by(Name)%>%
  mutate(In_Cap=max(Nameplate_Capacity))%>%
  ungroup()%>%
  filter(End_Year==Time_Period)%>%
  subset(select=c("Name","In_Cap","Primary_Fuel","Capacity_Factor","Beg_Date","Beg_Year","End_Date","End_Year"))%>%
  mutate(Type="Retirement")%>%
  arrange(.,End_Date)

# FILTER CAP ADDITIONS
Builddata <- Add_Ret_data %>%
  filter(Beg_Year >= 2022,
         Beg_Year==Time_Period) %>%
  group_by(Name)%>%
  mutate(In_Cap=max(Nameplate_Capacity))%>%
  ungroup()%>%
  select(., c("Name","In_Cap","Primary_Fuel","Capacity_Factor","Beg_Date","Beg_Year","End_Date","End_Year")) %>%
  mutate(Type="Addition")%>%
  arrange(.,Beg_Date)

# Add cap increases manual
Capinc<-data.frame(Name=c("Base Plant (SCR1)"),
                   In_Cap=c(800),
                   Primary_Fuel=c("Cogeneration"),
                   Capacity_Factor=NA,
                   Beg_Date=c(as.Date("07/01/2024", 
                                      format = "%m/%d/%Y")),
                   Beg_Year=c(2024),
                   End_Date=NA,
                   End_Year=NA,
                   Type="Addition")

# Add the manual plant to the rest
Builddata <-  rbind(Builddata,Capinc)%>%
  arrange(.,Beg_Date)

# NOW PUT IT ALL TOGETHER TO GET TOTALS BY RESOURCE TYPE
BuilddataTot <- Builddata%>%
  group_by(Primary_Fuel, Beg_Year) %>%
  summarise(Capacity_Added = sum(In_Cap))%>%
  mutate(Year=Beg_Year)%>%
  subset(select=c(Primary_Fuel,Year,Capacity_Added))

RetdatadataTot <- Retdata%>%
  group_by(Primary_Fuel, End_Year) %>%
  summarise(Capacity_Retired = sum(In_Cap))%>%
  mutate(Year=End_Year)%>%
  subset(select=c(Primary_Fuel,Year,Capacity_Retired))

# Get summary for each year!
Tot_Change<-merge(BuilddataTot,RetdatadataTot,by=c("Primary_Fuel","Year"), all.x = TRUE, all.y = TRUE)

# Replace NA values with 0
Tot_Change[is.na(Tot_Change)]=0

# Find the capacity difference in given year
Tot_Change <- Tot_Change %>%
  mutate(diff=Capacity_Added-Capacity_Retired)
          
          
  ##########

Tot_Change$Year <- as.factor(format(Tot_Change$Year, format="%Y"))

# Sum all up
Tot <- Tot_Change %>%
  group_by(Year) %>%
  summarise(maxy = sum(diff[which(diff>0)]), miny = sum(diff[which(diff<0)]))

# Capacity limits for plot
mny <- plyr::round_any(min(Tot$miny),1000, f=floor)
mxy <- plyr::round_any(max(Tot$maxy),1000, f=ceiling)

# Year limits for plot. Add 365 to get the year after the first
mnx <- format(min(ResGroupMn$Time_Period), format="%Y")
mxx <- format(max(ResGroupMn$Time_Period)-365*5, format="%Y")

# Plot it all
Tot_Change %>%
  ggplot() +
  aes(Year, (diff), fill = Primary_Fuel) +
  geom_col(alpha=0.7, size=.5, colour="black") +
  
  # Add line at y=0
  geom_hline(yintercept=0, color = "black")+
  
  theme_bw() +
  
  theme(
    # General Plot Settings
    panel.grid = element_blank(),
    # (t,r,b,l) margins, adjust to show full x-axis, default: (5.5,5.5,5.5,5.5)
    plot.margin = unit(c(6, 12, 5.5, 5.5), "points"),      # Plot margins
    panel.background = element_rect(fill = "transparent"), # Transparent background
    text = element_text(size = GenText_Sz),                # Text size
    plot.title = element_text(size = Tit_Sz,hjust = 0.5),  # Plot title size (if present)
    plot.subtitle = element_text(hjust = 0.5),             # Plot subtitle size (if present)
    #panel.grid.major.y = element_line(size=0.25,
    #linetype=1,color = 'gray90'),                         # Adds horizontal lines
    # X-axis
    axis.text.x = element_text(angle = 45,
                               vjust = 1, hjust = 1),          # Horizontal text
    axis.title.x = element_text(size = XTit_Sz),           # x-axis title text size
    # Y-axis
    axis.title.y = element_text(size = YTit_Sz),           # y-axis title text size
    # Legend
    legend.key.size = unit(1,"lines"),                     # Shrink legend boxes
    legend.position = "right",                             # Move legend to the bottom
    legend.justification = c(0.5,0.5),                     # Center the legend
    legend.text = element_text(size =Leg_Sz),              # Size of legend text
    legend.title=element_text()) +                         # Legend title
  
  scale_x_discrete(expand=c(0.05,0.05),
                   limits = as.character(mnx:mxx)) +
  scale_y_continuous(expand=c(0,0),
                     limits = c((mny),(mxy)),breaks=seq(mny,mxy,by=1000)) +
  scale_fill_manual(values=colours8,drop = FALSE) +
  
  labs(x = "Year", y = "Annual Change in Capacity (MW)", fill = "Resource Options",caption = paste(SourceDB))
  
