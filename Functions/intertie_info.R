################################################################################
# TITLE: intertie_info
# DESCRIPTION: Functions To use for plotting and evaluating intertie activities

# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: July 21, 2022; LAST EDIT: August 7, 2022
#
################################################################################

###############################################################################  
################################################################################
## SIMULATION INTERTIE FUNCTIONS
################################################################################
###############################################################################  

###############################################################################  
## FUNCTION: Imp_Exp1
## AB imports and exports plotted as yearly totals 
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    Import_Yr - All imports from AB
##    Export_Yr - All exports from AB
################################################################################

Imp_Exp1 <- function(case) {
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
## AB imports and exports plotted as annual chart 
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

BC_SK_IE <- function(year,case) {
  # NOW TRADE INFO
  
  #Imports from BC
  Imp_BC <- LinkHr %>%
    filter(Condition=="Average") %>%
    filter(Zone_Out=="WECC_BritishColumbia") %>%
    filter(Zone_In=="WECC_Alberta") %>%
    filter(Run_ID == case) %>%
    mutate(Year = format(.$Time_Period, format="%Y")) %>%
    filter(Year==year)
  
  # Exports to BC
  Exp_BC <- LinkHr %>%
    filter(Condition=="Average") %>%
    filter(Zone_Out=="WECC_Alberta") %>%
    filter(Zone_In=="WECC_BritishColumbia") %>%
    filter(Run_ID == case) %>%
    mutate(Year = format(.$Time_Period, format="%Y")) %>%
    filter(Year==year) %>%
    mutate(Net_Load=Net_Load*-1)
  
  # Imports from SK
  Imp_SK <- LinkHr %>%
    filter(Condition=="Average") %>%
    filter(Zone_Out=="MRO_Saskatchewan") %>%
    filter(Zone_In=="WECC_Alberta") %>%
    filter(Run_ID == case) %>%
    mutate(Year = format(.$Time_Period, format="%Y")) %>%
    filter(Year==year)
  
  # Exports to SK
  Exp_SK <- LinkHr %>%
    filter(Condition=="Average") %>%
    filter(Zone_Out=="WECC_Alberta") %>%
    filter(Zone_In=="MRO_Saskatchewan") %>%
    filter(Run_ID == case) %>%
    mutate(Year = format(.$Time_Period, format="%Y")) %>%
    filter(Year==year)  %>%
    mutate(Net_Load=Net_Load*-1)
  
  
  # Put all data together and filter week
  data <-rbind(Exp_SK,Exp_BC,Imp_BC,Imp_SK) 
  
  # Set the max for the plot
  MX <- plyr::round_any(max(data$Net_Load+11), 200, f = ceiling)
  MN <- plyr::round_any(min(data$Net_Load+11)*-1, 200, f = floor)
  
  # Plot Trade
  TradePlot <- ggplot() +
    geom_area(data = data, aes(x = Time_Period, y = Net_Load, fill = ID), 
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
    
    scale_y_continuous(expand=c(0,0),limits = c(-1300,1300),breaks=pretty_breaks(8)) +
    
    scale_x_datetime(expand=c(0,0),limits=c(day_MN,day_MX),breaks = "week",date_labels = "%e") +
    
    labs(x = "Date", y = "AB Hourly Trade (MWh)") +
    
    scale_fill_manual(values = c("AB_BC"= "dodgerblue4","AB_SK"="springgreen4","BC_AB"="dodgerblue","SK_AB"="springgreen")) +
    
    guides(fill = guide_legend(nrow = 1)) 
  
  
}

###############################################################################  
## FUNCTION: MN_Trade_Price
## Get trade and price each month to compare
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    Import_Yr - All imports from AB
##    Export_Yr - All exports from AB
################################################################################

MN_Trade_Price <- function(year,month,case) {
 
  # Min for date (x-axis)
  day_MN <- as.POSIXct(paste(01,month,year, sep = "/"), format="%d/%m/%Y")
  
  # Max for date
  day_MX <- as.POSIXct(paste(01,month+1,year, sep = "/"), format="%d/%m/%Y")
  day_MX <- ceiling_date(day_MX, "month") - 1
  
  # Filters for the desired case study and data in that month
  data <- ZoneHr_Avg%>%
    filter(Run_ID == case) %>%
    filter(date>=day_MN) %>%
    filter(date<=day_MX) 
  
  # Select only a single year using function WkTime
  ZPrice <- YrTime(data,year)
  
  # Set the max and min for the plot
  MXp <- plyr::round_any(max(abs(ZPrice$Price)+13), 10, f = ceiling)
  MNp <- plyr::round_any(min(ZPrice$Price)-13, 10, f = floor)

  # # Color for plot background
  # rect_data <- data.frame(xmin=min(day_MN),
  #                         xmax=max(day_MX),
  #                         ymin=c(0,63.91,76.69),
  #                         ymax=c(63.91,76.69,MXp),
  #                         col=c(NA,"blue","green"))
  
  # Plot the data    
  PricePlot <- ggplot() +
    
    geom_line(data = ZPrice, 
              aes(x = date, y = Price,colour = "Simulated (AURORA)"), 
              size = 1) +
    
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.background = element_rect(fill = "transparent"),
          axis.text.x=element_text(vjust=-1),
          axis.title.x = element_blank(),
          axis.text.y=element_text(hjust=-0.5),
          axis.title.y = element_text(vjust=2,size= YTit_Sz,face="bold"),
          panel.grid.major.y = element_line(size=0.25,linetype=5,color = 'grey'),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          plot.title = element_text(size = Tit_Sz),
          text = element_text(size = 15),
          legend.position = "none",
          legend.text = element_blank(),
          legend.background = element_rect(fill='transparent',colour ='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent")
          
    ) +
    
    scale_colour_manual("", 
                        breaks = c("Simulated (AURORA)"),
                        values = c("darkred")) +
    
    labs(title=paste(year,",",month.name[month]), y = "Pool Price ($/MWh)") +
    scale_x_datetime(expand=c(0,0),limits=c(day_MN,day_MX),breaks = "week",date_labels = "%e") +
    
    scale_y_continuous(expand=c(0,0), 
                       limits= c(MNp,MXp),
                       breaks=pretty_breaks(6)) 
  # +
                         
    # geom_rect(data=rect_data, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=col),alpha=0.1)+
    #                   scale_fill_identity()                  
    
 # NOW TRADE INFO
  
  #Imports from BC
  Imp_BC <- LinkHr %>%
    filter(Condition=="Average") %>%
    filter(Zone_Out=="WECC_BritishColumbia") %>%
    filter(Zone_In=="WECC_Alberta") %>%
    filter(Run_ID == case) %>%
    mutate(Year = format(.$Time_Period, format="%Y")) %>%
    filter(Year==year)
  
  # Exports to BC
  Exp_BC <- LinkHr %>%
    filter(Condition=="Average") %>%
    filter(Zone_Out=="WECC_Alberta") %>%
    filter(Zone_In=="WECC_BritishColumbia") %>%
    filter(Run_ID == case) %>%
    mutate(Year = format(.$Time_Period, format="%Y")) %>%
    filter(Year==year) %>%
    mutate(Net_Load=Net_Load*-1)
  
  # Imports from SK
  Imp_SK <- LinkHr %>%
    filter(Condition=="Average") %>%
    filter(Zone_Out=="MRO_Saskatchewan") %>%
    filter(Zone_In=="WECC_Alberta") %>%
    filter(Run_ID == case) %>%
    mutate(Year = format(.$Time_Period, format="%Y")) %>%
    filter(Year==year)
  
  # Exports to SK
  Exp_SK <- LinkHr %>%
    filter(Condition=="Average") %>%
    filter(Zone_Out=="WECC_Alberta") %>%
    filter(Zone_In=="MRO_Saskatchewan") %>%
    filter(Run_ID == case) %>%
    mutate(Year = format(.$Time_Period, format="%Y")) %>%
    filter(Year==year)  %>%
    mutate(Net_Load=Net_Load*-1)
  
  
  # Put all data together and filter week
  data <-rbind(Exp_SK,Exp_BC,Imp_BC,Imp_SK) %>%
    filter(Time_Period>=day_MN) %>%
    filter(Time_Period<=day_MX) 
  
  # Set the max for the plot
  MX <- plyr::round_any(max(data$Net_Load+11), 200, f = ceiling)
  MN <- plyr::round_any(min(data$Net_Load+11)*-1, 200, f = floor)
  
  # Plot Trade
  TradePlot <- ggplot() +
    geom_area(data = data, aes(x = Time_Period, y = Net_Load, fill = ID), 
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
    
    scale_y_continuous(expand=c(0,0),limits = c(-1300,1300),breaks=pretty_breaks(8)) +
    
    scale_x_datetime(expand=c(0,0),limits=c(day_MN,day_MX),breaks = "week",date_labels = "%e") +
    
    labs(x = "Date", y = "AB Hourly Trade (MWh)") +
    
    scale_fill_manual(values = c("AB_BC"= "dodgerblue4","AB_SK"="springgreen4","BC_AB"="dodgerblue","SK_AB"="springgreen")) +

    guides(fill = guide_legend(nrow = 1)) 
  
  # Get Trade legend
  legend <- get_legend(TradePlot)
  TradePlot <- TradePlot + theme(legend.position ="none")
  
  gA <- ggplotGrob(PricePlot)
  gB<- ggplotGrob(TradePlot)
  
  # Arrange all the plots together
  maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
  gA$widths[2:5] <- as.list(maxWidth)
  gB$widths[2:5] <- as.list(maxWidth)
  grid.arrange(gA, gB, legend,ncol=1, 
               heights=c(1, 1,0.1))
  
}
###############################################################################  
################################################################################
## AESO INTERTIE FUNCTIONS
################################################################################
###############################################################################  

###############################################################################  
## FUNCTION: Trade_Mn_AESO 
## AB imports and exports and pool price for specific month
##
## INPUTS: 
##    year - Year to check
##    month - month to check
################################################################################  

Trade_Mn_AESO <- function(year,month,Imp_Exp) {
  
  # Min for date (x-axis)
  day_MN <- as.POSIXct(paste(01,month,year, sep = "/"), format="%d/%m/%Y")
  
  # Max for date
  day_MX <- as.POSIXct(paste(01,month+1,year, sep = "/"), format="%d/%m/%Y")
  day_MX <- ceiling_date(day_MX, "month") - 1
  
  # Filters for data in that month
  PData <- Imp_Exp %>%
    select(., c("ACTUAL_POOL_PRICE","Date_Begin_Local","date")) %>%
    filter(date>=day_MN) %>%
    filter(date<=day_MX) 
  
  
  # Set the max and min for the plot
  MXp <- plyr::round_any(max(abs(PData$ACTUAL_POOL_PRICE)+13), 10, f = ceiling)
  MNp <- plyr::round_any(min(PData$ACTUAL_POOL_PRICE)-13, 10, f = floor)
  if (MNp<0) {
    MNp <- 0 }
  
  # Plot the data    
  PricePlot <- ggplot() +
    
    geom_line(data = PData, 
              aes(x = date, y = ACTUAL_POOL_PRICE,colour = "Actual Pool Price"), 
              size = 1) +
    
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.background = element_rect(fill = "transparent"),
          axis.text.x=element_text(vjust=-1),
          axis.title.x = element_blank(),
          axis.text.y=element_text(hjust=0),
          axis.title.y = element_text(vjust=2,size= 15,face="bold"),
          panel.grid.major.y = element_line(size=0.25,linetype=5,color = 'grey'),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          plot.title = element_text(size = 15),
          text = element_text(size = 15),
          legend.position = "none",
          legend.text = element_blank(),
          legend.background = element_rect(fill='transparent',colour ='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent")
          
    ) +
    
    scale_colour_manual("", 
                        breaks = c("Actual Pool Price"),
                        values = c("darkred")) +
    
    labs(title=paste(year,",",month.name[month],"(AESO Data)"), y = "Pool Price ($/MWh)") +
    scale_x_datetime(expand=c(0,0),limits=c(day_MN,day_MX),breaks = "week",date_labels = "%e") +
    
    scale_y_continuous(expand=c(0,0), 
                       limits= c(MNp,MXp),
                       breaks=pretty_breaks(6))
  
  # NOW TRADE INFO
  
  #Imports from BC
  Imp_BC <- Imp_Exp %>%
    select(.,c("IMPORT_BC_MT","Date_Begin_Local","date")) %>%
    rename(Net_Load=IMPORT_BC_MT) %>%
    mutate(ID ="BC_AB")
  
  # Exports to BC
  Exp_BC <- Imp_Exp %>%
    select(.,c("EXPORT_BC_MT","Date_Begin_Local","date")) %>%
    rename(Net_Load=EXPORT_BC_MT) %>%
    mutate(Net_Load=Net_Load*-1) %>%
    mutate(ID ="AB_BC")
  
  Imp_SK <- Imp_Exp %>%
    select(.,c("IMPORT_SK","Date_Begin_Local","date")) %>%
    rename(Net_Load=IMPORT_SK) %>%
    mutate(ID ="SK_AB")
  
  # Exports to BC
  Exp_SK <- Imp_Exp %>%
    select(.,c("EXPORT_SK","Date_Begin_Local","date")) %>%
    rename(Net_Load=EXPORT_SK) %>%
    mutate(Net_Load=Net_Load*-1) %>%
    mutate(ID ="AB_SK")
  
  
  # Put all data together and filter week
  data <-rbind(Exp_SK,Exp_BC,Imp_BC,Imp_SK) %>%
    filter(date>=day_MN) %>%
    filter(date<=day_MX) 
  
  # Set the max for the plot
  MX <- plyr::round_any(max(data$Net_Load+11), 200, f = ceiling)
  MN <- plyr::round_any(min(data$Net_Load+11)*-1, 200, f = floor)
  
  # Plot Trade
  TradePlot <- ggplot() +
    geom_area(data = data, aes(x = date, y = Net_Load, fill = ID), 
              alpha=1, size=0.5) +
    
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(vjust = 1),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          plot.title = element_text(size = 15),
          plot.subtitle = element_text(hjust = 0.5), 
          panel.background = element_rect(fill = NA),
          panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          legend.key.size = unit(1,"lines"), #Shrink legend
          legend.position = "bottom",
          legend.justification = c(0.5,0.5),
          legend.title=element_blank(),
          text = element_text(size = 15)) +
    
    scale_y_continuous(expand=c(0,0),limits = c(-1300,1300),breaks=pretty_breaks(8)) +
    
    scale_x_datetime(expand=c(0,0),limits=c(day_MN,day_MX),breaks = "week",date_labels = "%e") +
    
    labs(x = "Date", y = "AB Hourly Trade (MWh)") +
    
    scale_fill_manual(values = c("AB_BC"= "dodgerblue4","AB_SK"="springgreen4","BC_AB"="dodgerblue","SK_AB"="springgreen")) +
    
    guides(fill = guide_legend(nrow = 1)) 
  
  # Get Trade legend
  legend <- get_legend(TradePlot)
  TradePlot <- TradePlot + theme(legend.position ="none")
  
  
  gA <- ggplotGrob(PricePlot)
  gB <- ggplotGrob(TradePlot)
  
  # Arrange all the plots together
  maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
  gA$widths[2:5] <- as.list(maxWidth)
  gB$widths[2:5] <- as.list(maxWidth)
  grid.arrange(gA, gB, legend,ncol=1, 
               heights=c(1, 1,0.1))
  
}

###############################################################################  
## FUNCTION: TradeOnly_Mn_AESO 
## AB imports and exports and pool price for specific month
##
## INPUTS: 
##    year - Year to check
##    month - month to check
################################################################################  

TradeOnly_Mn_AESO <- function(year,month,Imp_Exp) {
  
  # Min for date (x-axis)
  day_MN <- as.POSIXct(paste(01,month,year, sep = "/"), format="%d/%m/%Y")
  
  if (month <12) {
    # Max for date
    day_MX <- as.POSIXct(paste(01,month+1,year, sep = "/"), format="%d/%m/%Y")
    day_MX <- ceiling_date(day_MX, "month") - 1 
    
  }else {
    day_MX <- as.POSIXct(paste(31,month,year, sep = "/"), format="%d/%m/%Y") }
  
 
  #Imports from BC
  Imp_BC <- Imp_Exp %>%
    select(.,c("IMPORT_BC_MT","Date_Begin_Local","date")) %>%
    rename(Net_Load=IMPORT_BC_MT) %>%
    mutate(ID ="BC_AB")
  
  # Exports to BC
  Exp_BC <- Imp_Exp %>%
    select(.,c("EXPORT_BC_MT","Date_Begin_Local","date")) %>%
    rename(Net_Load=EXPORT_BC_MT) %>%
    mutate(Net_Load=Net_Load*-1) %>%
    mutate(ID ="AB_BC")
  
  Imp_SK <- Imp_Exp %>%
    select(.,c("IMPORT_SK","Date_Begin_Local","date")) %>%
    rename(Net_Load=IMPORT_SK) %>%
    mutate(ID ="SK_AB")
  
  # Exports to BC
  Exp_SK <- Imp_Exp %>%
    select(.,c("EXPORT_SK","Date_Begin_Local","date")) %>%
    rename(Net_Load=EXPORT_SK) %>%
    mutate(Net_Load=Net_Load*-1) %>%
    mutate(ID ="AB_SK")
  
  
  # Put all data together and filter week
  data <-rbind(Exp_SK,Exp_BC,Imp_BC,Imp_SK) %>%
    filter(date>=day_MN) %>%
    filter(date<=day_MX) 
  
  # Set the max for the plot
  MX <- plyr::round_any(max(data$Net_Load+11), 200, f = ceiling)
  MN <- plyr::round_any(min(data$Net_Load+11)*-1, 200, f = floor)
  
  # Plot Trade
  ggplot() +
    geom_area(data = data, aes(x = date, y = Net_Load, fill = ID), 
              alpha=1, size=0.5) +
    
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(vjust = 1),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          plot.title = element_text(size = 15),
          plot.subtitle = element_text(hjust = 0.5), 
          panel.background = element_rect(fill = NA),
          panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          legend.key.size = unit(1,"lines"), #Shrink legend
          legend.position = "bottom",
          legend.justification = c(0.5,0.5),
          legend.title=element_blank(),
          text = element_text(size = 15)) +
    
    scale_y_continuous(expand=c(0,0),limits = c(-1300,1300),breaks=pretty_breaks(8)) +
    
    scale_x_datetime(expand=c(0,0),limits=c(day_MN,day_MX),breaks = "7 day",date_labels = "%e") +
    
    labs(x = "Date", y = "AB Hourly Trade (MWh)",title=year) +
    
    scale_fill_manual(values = c("AB_BC"= "dodgerblue4","AB_SK"="springgreen4","BC_AB"="dodgerblue","SK_AB"="springgreen")) +
    
    guides(fill = guide_legend(nrow = 1)) 
  
}

###############################################################################  
## FUNCTION: Trade_Yr_AESO 
## AB imports and exports and pool price for specific year
##
## INPUTS: 
##    year - Year to check
################################################################################  

Trade_Yr_AESO <- function(year) {
  
  # Filters for data in that month
  PData <- Imp_Exp %>%
    select(., c("ACTUAL_POOL_PRICE","Date_Begin_Local","date","Year")) %>%
    filter(Year==year)
  
  
  # Set the max and min for the plot
  MXp <- plyr::round_any(max(abs(PData$ACTUAL_POOL_PRICE)+13), 10, f = ceiling)
  MNp <- plyr::round_any(min(PData$ACTUAL_POOL_PRICE)-13, 10, f = floor)
  if (MNp<0) {
    MNp <- 0 }
  
  #Imports from BC
  Imp_BC <- Imp_Exp %>%
    select(.,c("IMPORT_BC_MT","Date_Begin_Local","date","Year")) %>%
    rename(Net_Load=IMPORT_BC_MT) %>%
    mutate(ID ="BC_AB") %>%
    filter(Year==year)
  
  # Exports to BC
  Exp_BC <- Imp_Exp %>%
    select(.,c("EXPORT_BC_MT","Date_Begin_Local","date","Year")) %>%
    rename(Net_Load=EXPORT_BC_MT) %>%
    mutate(Net_Load=Net_Load*-1) %>%
    mutate(ID ="AB_BC")%>%
    filter(Year==year)
  
  Imp_SK <- Imp_Exp %>%
    select(.,c("IMPORT_SK","Date_Begin_Local","date","Year")) %>%
    rename(Net_Load=IMPORT_SK) %>%
    mutate(ID ="SK_AB")%>%
    filter(Year==year)
  
  # Exports to BC
  Exp_SK <- Imp_Exp %>%
    select(.,c("EXPORT_SK","Date_Begin_Local","date","Year")) %>%
    rename(Net_Load=EXPORT_SK) %>%
    mutate(Net_Load=Net_Load*-1) %>%
    mutate(ID ="AB_SK")%>%
    filter(Year==year)
  
  
  # Put all data together and filter week
  data <-rbind(Exp_SK,Exp_BC,Imp_BC,Imp_SK) 
  
  # Set the max for the plot
  MX <- plyr::round_any(max(data$Net_Load+11), 200, f = ceiling)
  MN <- plyr::round_any(min(data$Net_Load+11)*-1, 200, f = floor)
  
  # Plot Trade
  ggplot() +
    geom_area(data = data, aes(x = date, y = Net_Load, fill = ID), 
              alpha=1, size=0.5) +
    
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(vjust = 1),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          plot.title = element_text(size = 15),
          plot.subtitle = element_text(hjust = 0.5), 
          panel.background = element_rect(fill = NA),
          panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          legend.key.size = unit(1,"lines"), #Shrink legend
          legend.position = "bottom",
          legend.justification = c(0.5,0.5),
          legend.title=element_blank(),
          text = element_text(size = 15)) +
    
    scale_y_continuous(expand=c(0,0),limits = c(-1300,1300),breaks=pretty_breaks(8)) +
    
    scale_x_datetime(expand=c(0,0),breaks = "month",date_labels = "%b") +
    
    labs(x = "Date", y = "AB Hourly Trade (MWh)",title=year) +
    
    scale_fill_manual(values = c("AB_BC"= "dodgerblue4","AB_SK"="springgreen4","BC_AB"="dodgerblue","SK_AB"="springgreen")) 
  
}

###############################################################################  
## FUNCTION: Duration_AESO 
## AB imports and exports and pool price for specific year
##
## INPUTS: 
##    Years2See - Year to show in duration curve
################################################################################    

Duration_AESO <-function(Years2See) {
  # Get all the data by year and group to percents
  tot <- Imp_Exp%>%
    group_by(Year)%>%
    mutate(perc = 1-ecdf(ACTUAL_POOL_PRICE)(ACTUAL_POOL_PRICE))
  
  tot$Year <- as.factor(tot$Year)
  
  #Filter years to show
  tot <- tot %>%
    filter(Year %in% Years2See)
  
  #Plot
  ggplot() +
    geom_line(data = tot, 
              aes(x = perc, y = ACTUAL_POOL_PRICE, colour = Year), size = 1) +
    
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),
          panel.spacing = unit(2, "lines"),
          axis.title.x = element_text(size = 15,face="bold"),
          axis.title.y = element_text(size = 15,face="bold"),
          text = element_text(size = 15),
          legend.title = element_blank(),
          legend.position = "right",
          panel.grid.major.y = element_line(size=0.25,linetype=5,color = "gray70")) +
    
    labs(y = "Hourly Pool Price ($/MWh)", x = "Percentage of Time",colour = "Year",
         caption = "Hourly Metered Volumes and Pool Price and AIL data 2010 to 2022") +
    
    scale_x_continuous(expand=c(0,0), 
                       limits = c(0,1),
                       labels = percent) +
    
    scale_y_continuous(expand=c(0,0),limits = c(0,1000),breaks = pretty_breaks(5))  
}

###############################
T_month_all <- function(month) {
  
  
  # Create a graph for each month of the year
  p1 <- TradeOnly_Mn_AESO(2010,month,Imp_Exp) +
    theme(axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  
  p2 <- TradeOnly_Mn_AESO(2011,month,Imp_Exp) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p3 <- TradeOnly_Mn_AESO(2012,month,Imp_Exp) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p4 <- TradeOnly_Mn_AESO(2013,month,Imp_Exp) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p5 <- TradeOnly_Mn_AESO(2014,month,Imp_Exp) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p6 <- TradeOnly_Mn_AESO(2015,month,Imp_Exp) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p7 <- TradeOnly_Mn_AESO(2016,month,Imp_Exp) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p8 <- TradeOnly_Mn_AESO(2017,month,Imp_Exp) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p9 <- TradeOnly_Mn_AESO(2018,month,Imp_Exp) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p10 <- TradeOnly_Mn_AESO(2019,month,Imp_Exp) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p11 <- TradeOnly_Mn_AESO(2020,month,Imp_Exp) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p12 <- TradeOnly_Mn_AESO(2021,month,Imp_Exp) +
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
             label = paste("Month:",month.name[month],", AESO Data")) + 
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
