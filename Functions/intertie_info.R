################################################################################
# TITLE: intertie_info
# DESCRIPTION: Functions To use for plotting and evaluating intertie activities

# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: July 21, 2022; LAST EDIT: July 21, 2022
#
################################################################################
###############################################################################  
## FUNCTION: Imp_Exp 
## AB imports and exports plotted as yearly totals 
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    Import_Yr - All imports from AB
##    Export_Yr - All exports from AB
################################################################################

Imp_Exp <- function(case) {
  Imp <- Import_Yr %>%
    filter(Name == "WECC_Alberta") %>%
    filter(Run_ID == case) %>%
    mutate(Time_Period = format(.$Time_Period, format="%Y")) %>%
    mutate(ID = "Import")
  
  Exp <- Export_Yr %>%
    filter(Name == "WECC_Alberta") %>%
    filter(Run_ID == case) %>%
    mutate(Time_Period = format(.$Time_Period, format="%Y")) %>%
    mutate(ID = "Export") 
  
  
  # Filters for the desired case study
  data <- rbind(Imp,Exp) 
  
  # data$Time_Period  <- as.Date(as.character(data$Time_Period), 
  #                              format = "%Y")
  
  # # Get chosen dates
  # data$Time_Period <- format(data$Time_Period,format="%Y")
  # data <- data %>%
  #   filter(Time_Period %in% Years2Disp)
  
  # Set the max for the plot
  MX <- plyr::round_any(max(abs(data$Output_MWH+11)/1000), 1000, f = ceiling)
  
  # Plot
  data %>%
    ggplot() +
    aes(Time_Period, (Output_MWH/1000), fill = ID) +
    geom_bar(position="dodge",stat="identity",alpha=1) +
    
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(vjust = 1),
          axis.title.x = element_text(size = XTit_Sz),
          axis.title.y = element_text(size = YTit_Sz),
          plot.title = element_text(size = Tit_Sz),
          plot.subtitle = element_text(hjust = 0.5), 
          panel.background = element_rect(fill = NA),
          panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          legend.key.size = unit(1,"lines"), #Shrink legend
          legend.position = "bottom",
          legend.justification = c(0.5,0.5),
          legend.title=element_blank(),
          text = element_text(size = 15)) +
    
    scale_y_continuous(expand=c(0,0),limits = c(0,MX),breaks=pretty_breaks(6)) +
    
    labs(x = "Year", y = "Total Annual Imports and Exports (GWh)", fill = "Resource") +
    
    guides(fill = guide_legend(nrow = 1)) 
}

###############################################################################  
## FUNCTION: Imp_Exp2
## AB imports and exports plotted as monthly chart 
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    Import_Yr - All imports from AB
##    Export_Yr - All exports from AB
################################################################################

Imp_Exp2 <- function(year,case) {
  Imp <- Import %>%
    filter(Run_ID == case) %>%
    mutate(Year = format(.$date, format="%Y")) %>%
    mutate(ID = "Import") %>%
    filter(Year==year)
  
  Exp <- Export %>%
    filter(Run_ID == case) %>%
    mutate(Year = format(.$date, format="%Y")) %>%
    mutate(ID = "Export")  %>%
    filter(Year==year) %>%
    mutate(Output_MWH=Output_MWH*-1)
  
  
  # Filters for the desired case study
  data <- rbind(Imp,Exp) 
  
  # Set the max for the plot
  MX <- plyr::round_any(max(abs(Imp$Output_MWH+11)), 200, f = ceiling)
  MN <- plyr::round_any(max(abs(Exp$Output_MWH+11))*-1, 200, f = floor)
  
  # Plot
  ggplot() +
    geom_area(data = data, aes(x = date, y = Output_MWH, fill = ID), 
              alpha=1, size=0.5) +
    
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(vjust = 1),
          axis.title.x = element_text(size = XTit_Sz),
          axis.title.y = element_text(size = YTit_Sz),
          plot.title = element_text(size = Tit_Sz),
          plot.subtitle = element_text(hjust = 0.5), 
          panel.background = element_rect(fill = NA),
          panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          legend.key.size = unit(1,"lines"), #Shrink legend
          legend.position = "bottom",
          legend.justification = c(0.5,0.5),
          legend.title=element_blank(),
          text = element_text(size = 15)) +
    
    scale_y_continuous(expand=c(0,0),limits = c(-1700,1700),breaks=pretty_breaks(12)) +
    
    scale_x_datetime(expand=c(0,0),date_labels = "%b" ,breaks = "month") +
    
    labs(x = "Date", y = "AB Hourly Imports and Exports (MWh)",caption = SourceDB, title=year) +
    
    scale_fill_manual(values = c("Import"= cOL_IMPORT,"Export"=cOL_EXPORT)) +
    
    guides(fill = guide_legend(nrow = 1)) 
}

################################################################################  
## FUNCTION: BC_SK_IE 
## BC and SK imports and exports
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    Import - Import table derived of zone average table
##    Export - Export table dervied of zone average table
################################################################################

BC_SK_IE <- function(case) {
  EXP_BC <- Import_Yr %>%
    filter(Name == "WECC_BritishColumbia") %>%
    filter(Run_ID == case) %>%
    mutate(Time_Period = format(.$Time_Period, format="%Y")) %>%
    mutate(Output_MWH=Output_MWH*-1) %>%
    mutate(ID = "AB Exports to BC") 
  
  EXP_SK <- Import_Yr %>%
    filter(Name == "MRO_Saskatchewan") %>%
    filter(Run_ID == case) %>%
    mutate(Time_Period = format(.$Time_Period, format="%Y")) %>%
    mutate(Output_MWH=Output_MWH*-1) %>%
    mutate(ID = "AB Exports to SK") 
  
  IMP_BC <- Export_Yr %>%
    filter(Name == "WECC_BritishColumbia") %>%
    filter(Run_ID == case) %>%
    mutate(Time_Period = format(.$Time_Period, format="%Y"))  %>%
    mutate(ID = "AB Imports from BC") 
  
  IMP_SK <- Export_Yr %>%
    filter(Name == "MRO_Saskatchewan") %>%
    filter(Run_ID == case) %>%
    mutate(Time_Period = format(.$Time_Period, format="%Y")) %>%
    mutate(ID = "AB Imports from SK") 

  data <-rbind(EXP_SK,EXP_BC,IMP_BC,IMP_SK)
  
  # Re-arrange
  data$ID <- factor(data$ID,levels=c("AB Exports to SK","AB Exports to BC","AB Imports from BC","AB Imports from SK"),ordered=TRUE)
  
  
  # Set the max for the plot
  MX <- plyr::round_any((max(data$Output_MWH)/1000), 1000, f = ceiling)
  MN <- plyr::round_any((min(data$Output_MWH)/1000), 1000, f = floor)
  
  # Plot
  data %>%
    ggplot() +
    aes(Time_Period, (Output_MWH/1000), fill = ID) +
    geom_bar(position="dodge",stat="identity",alpha=1) +
    
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(vjust = 1),
          axis.title.x = element_text(size = XTit_Sz),
          axis.title.y = element_text(size = YTit_Sz),
          plot.title = element_text(size = Tit_Sz),
          plot.subtitle = element_text(hjust = 0.5), 
          panel.background = element_rect(fill = NA),
          panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          legend.key.size = unit(1,"lines"), #Shrink legend
          legend.position = "bottom",
          legend.justification = c(0.5,0.5),
          legend.title=element_blank(),
          text = element_text(size = 15)) +
    
    scale_y_continuous(expand=c(0,0),limits = c(MN,MX),breaks=pretty_breaks(8)) +
    
    labs(x = "Year", y = "Annual Total Imports/Exports from AB (GWh)", fill = "Resource",caption = SourceDB) +
    
    scale_fill_manual(values = c("AB Exports to BC"= "dodgerblue4",
                                 "AB Exports to SK" = "springgreen4",
                                 "AB Imports from BC"= "dodgerblue",
                                 "AB Imports from SK" ="springgreen")) +
    
    guides(fill = guide_legend(nrow = 1)) 
  
  
}

################################################################################
## FUNCTIONS: AESO_SimP_Int
## Plot comparison between actual and simulated data price for 1 week
##
## INPUTS:
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
################################################################################
AESO_SimP2_Int <- function(year,case) {
  
  #Firt get Sim Data
  # Filters for the desired case study
  data <- ZoneHr_Avg%>%
    filter(Run_ID == case)
  
  # Select only a single year using function WkTime
  ZPrice <- YrTime(data,year)
  
  # Set the max and min for the plot
  MXa <- plyr::round_any(max(abs(ZPrice$Price)+10), 10, f = ceiling)

  #Max min for date (x-axis)
  day_MN <- as.POSIXct(paste(01,01,year, sep = "/"), format="%d/%m/%Y")
  day_MX <- as.POSIXct(paste(31,12,year, sep = "/"), format="%d/%m/%Y")
  
  # Get intertie info 
  ZoneBC<- ZoneHr %>%
    filter(Name == "WECC_BritishColumbia") %>%
    filter(Condition == "Average") %>%
    subset(., select = c(date, Price, Baseline_Demand, Demand, Demand_Total,
                         Net_Load, Net_Load_Total, Marginal_Resource, 
                         Smp_Max_Date_Time, Smp_Max_Demand, Smp_Max_Capacity, 
                         Run_ID, Imports, Exports)) %>%
    filter(Run_ID == case)
  
      ZPriceBC <- YrTime(ZoneBC,year)
      MXb <- plyr::round_any(max(abs(ZPriceBC$Price)+10), 10, f = ceiling)
      
  
  ZoneSK<- ZoneHr %>%
    filter(Name == "MRO_Saskatchewan") %>%
    filter(Condition == "Average") %>%
    subset(., select = c(date, Price, Baseline_Demand, Demand, Demand_Total,
                         Net_Load, Net_Load_Total, Marginal_Resource, 
                         Smp_Max_Date_Time, Smp_Max_Demand, Smp_Max_Capacity, 
                         Run_ID, Imports, Exports)) %>%
    filter(Run_ID == case)
  
      ZPriceSK <- YrTime(ZoneSK,year)
      MXc <- plyr::round_any(max(abs(ZPriceSK$Price)+10), 10, f = ceiling)
      
  
  # Set the max for the plot
  MX <-max(MXa,MXb,MXc)
  
  # Plot the data    
  ggplot() +

    geom_line(data = ZPriceSK, 
              aes(x=date, y=Price, color="SK"), 
              size = 1) +
    
    geom_line(data = ZPriceBC, 
              aes(x=date, y=Price, color="BC"), 
              size = 1.5) +
    
    
    geom_line(data = ZPrice, 
              aes(x = date, y = Price,colour = "AB"), 
              size = 0.75) +
    

    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.background = element_rect(fill = "transparent"),
          axis.text.x=element_text(vjust=-1),
          axis.title.x = element_text(vjust=-1,size= XTit_Sz,face="bold"),
          axis.text.y=element_text(hjust=-0.5),
          axis.title.y = element_text(vjust=2,size= YTit_Sz,face="bold"),
          panel.grid.major.y = element_line(size=0.25,linetype=5,color = 'grey'),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          plot.title = element_text(size = Tit_Sz),
          text = element_text(size = 15),
          legend.position = "bottom",
          legend.background = element_rect(fill='transparent',colour ='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent")
          
    ) +
    
    scale_colour_manual("", 
                        breaks = c("AB","BC","SK"),
                        values = c("darkred","darkblue","chartreuse4")) +
    
    labs(title=year, y = "Average Hourly Pool Price ($/MWh)", x="Date",fill = "Resource",caption = SourceDB) +
    scale_x_datetime(expand=c(0,0),limits=c(day_MN,day_MX),breaks = "month",date_labels = "%b") +
    
    scale_y_continuous(expand=c(0,0), 
                       limits= c(0,MX),
                       breaks = seq(0, 1000, by = 100)
                       
    )
}
