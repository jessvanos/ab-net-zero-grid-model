################################################################################
# TITLE: Intertie_Functions
# DESCRIPTION: Functions To use for plotting and evaluating intertie activitie. 
# Information on trade and what is happening in BC/MT/SK.
#
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
## AB imports and exports plotted as yearly totals. 
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
  
  # Get Year max for run
  MaxYr <- MaxYrStudy
  MinYr <- (min(data$Time_Period))
  
  # Filter to remove the final 5 years (as per AURORA, want to run 5 years past year of interest)
  data <- data%>%
    filter(Time_Period<=MaxYr)
  
  # Plot
  data %>%
    ggplot() +
    aes(Time_Period, (Output_MWH/1000), fill = ID) +
    geom_bar(position="dodge",stat="identity",alpha=1,color="black") +
    
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),
          axis.text = element_text(color="black"),
         # axis.title.x = element_text(size = XTit_Sz),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = YTit_Sz),
          plot.title = element_text(size = Tit_Sz),
          plot.subtitle = element_text(hjust = 0.5), 
          panel.background = element_rect(fill = NA),
         # panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          legend.key.size = unit(1,"lines"), #Shrink legend
          legend.position = c(.94,.95),
         legend.background = element_rect(fill = NA),
          legend.justification = c(0.5,0.5),
          legend.title=element_blank(),
          text = element_text(size = GenText_Sz )) +
    
    scale_y_continuous(expand=c(0,0),limits = c(0,MX),breaks=pretty_breaks(6),labels=comma) +
    
    labs(x = "Year", y = "Total Annual Imports and Exports (GWh)", fill = "Resource",caption=paste("Database: ",SourceDB)) +
    
    guides(fill = guide_legend(nrow = 2)) +
    
    scale_fill_manual(values=c("Export"="gray21","Import"="gray71"))
}

###############################################################################  
## FUNCTION: Imp_Exp2
## AB imports and exports plotted as annual chart, shows hourly patterns. 
##
## INPUTS: 
##    case - Run_ID which you want to plot
##    year - Year to show trade patterns
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
  MN <- plyr::round_any(min(abs(Exp$Output_MWH+11))*-1, 200, f = floor)
  
  # Plot
  ggplot() +
    geom_area(data = data, aes(x = date, y = Output_MWH, fill = ID), 
              alpha=1, size=0.5) +
    geom_hline(yintercept=0, linetype="solid", color="black",size=0.5)+
    
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),
          axis.text = element_text(color="black"),
          axis.title.x = element_text(size = XTit_Sz),
          axis.title.y = element_text(size = YTit_Sz),
          plot.title = element_text(size = Tit_Sz),
          plot.subtitle = element_text(hjust = 0.5), 
          panel.background = element_rect(fill = NA),
        #  panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          legend.key.size = unit(1,"lines"), #Shrink legend
          legend.position = "bottom",
          legend.justification = c(0.5,0.5),
          legend.title=element_blank(),
          text = element_text(size = 15)) +
    
    scale_y_continuous(expand=c(0,0),limits = c(MN,MX),breaks=pretty_breaks(12),labels=comma) +
    
    scale_x_datetime(expand=c(0,0),date_labels = "%b" ,breaks = "month") +
    
    labs(x = "Date", y = "AB Hourly Imports and Exports (MWh)",caption=paste("Database: ",SourceDB), title=year) +
    
    guides(fill = guide_legend(nrow = 1)) +
    
    scale_fill_manual(values=c("Export"="gray21","Import"="gray71"))
}

###############################################################################  
## FUNCTION: Imp_ExpWk
## AB imports and exports plotted as annual chart, shows hourly patterns. 
##
## INPUTS: 
##    case - Run_ID which you want to plot
##    year - Year to show trade patterns
## TABLES REQUIRED: 
##    Import_Yr - All imports from AB
##    Export_Yr - All exports from AB
################################################################################

Imp_ExpWk <- function(year, month, day, case) {
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
  Intdata <- rbind(Imp,Exp) 
    # Select only a single week
    WK_Int <- WkTime(Intdata,year,month,day)
  
  # Get price data
  PriceData <- ZoneHr_Avg%>%
    filter(Run_ID == case)
    # Select only a single week using function WkTime
    WK_Price <- WkTime(PriceData,year,month,day)

  # Set the max and min for the plot using annual max storage. Can opptionally add factor to move price plot up
  MX <- plyr::round_any(max(abs(Intdata$Output_MWH)), 20, f = ceiling)*1
  MN<- plyr::round_any(min(Intdata$Output_MWH), 20, f = floor)
  ylimint<-c(-MX,MX)
  ylimPrice<-c(-1020,1020)
  
  b <- diff(ylimint)/diff(ylimPrice)
  a <- ylimint[1] - b*ylimPrice[1] 
  
  # Plot
  ggplot() +

    geom_area_pattern(data = WK_Int, aes(x = date, y = Output_MWH, pattern_angle = ID), 
              alpha=1, size=0.5, 
              colour="black",fill="black",
              pattern_density = 0.3,
              pattern_fill    = "white",
              pattern_spacing=0.04) +
    
    geom_hline(yintercept=0, color = "black", size=1)+
    
    # geom_line(data = WK_Price, aes(x = date, y = a + Price*b,color="Pool Price"), 
    #           size = 1.25,linetype=1) +

    theme_bw() +
    theme(text=element_text(family=Plot_Text)) +
    
    # Set the theme for the plot
    theme(panel.grid = element_blank(),
          axis.text=element_text(color="black"),
          plot.title = element_text(size=Overall_Sz),
          legend.title = element_blank(),
          legend.position = "bottom",
          axis.title.y = element_text(size=YTit_Sz,face='bold'),
          axis.title.x=element_blank(),
          text = element_text(size= Overall_Sz),

    ) +
    scale_x_datetime(expand=c(0,0),date_labels = "%b-%e",breaks = "day") +
    
    scale_y_continuous(name="AB Hourly Intertie (MWh)",
                       breaks = pretty_breaks(8), 
                       limits = c(MN,MX),
                       labels=comma,
                       # sec.axis = sec_axis(~(. - a)/b, name="Wholesale Pool Price ($/MWh)",
                       #                     breaks = seq(0,1000,by=250),
                       #                     labels=comma)
                       ) +
    
    theme(axis.ticks.y.left = element_line(color = "black"),
          axis.text.y.left = element_text(color = "black"), 
          axis.title.y.left = element_text(color = "black")) +
    
    labs(x = paste("Date (",year,")"),title=month.abb[month]) +
    
    scale_color_manual(values=c("Pool Price"="black")) +
    scale_pattern_angle_manual(values=c(45,-45))
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
  
  if (month <12) {
    # Max for date
    day_MX <- as.POSIXct(paste(01,month+1,year, sep = "/"), format="%d/%m/%Y")
    day_MX <- ceiling_date(day_MX, "month") - 1 
    
  }else {
    day_MX <- as.POSIXct(paste(31,month,year, sep = "/"), format="%d/%m/%Y") }
  
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
              alpha=0.7, size=0.5) +
    
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
## FUNCTION: MN_TradeOnly 
## AB imports and exports and pool price for specific month.
##
## INPUTS: 
##    year - Year to check
##    month - month to check
################################################################################

MN_TradeOnly <- function(year,month,case) {

  # Min for date (x-axis)
  day_MN <- as.POSIXct(paste(01,month,year, sep = "/"), format="%d/%m/%Y")
  
  if (month <12) {
    # Max for date
    day_MX <- as.POSIXct(paste(01,month+1,year, sep = "/"), format="%d/%m/%Y")
    day_MX <- ceiling_date(day_MX, "month") - 1 
    
  }else {
    day_MX <- as.POSIXct(paste(31,month,year, sep = "/"), format="%d/%m/%Y") }
  
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
ggplot() +
  geom_area(data = data, aes(x = Time_Period, y = Net_Load, fill = ID), 
            alpha=0.7, size=0.5) +
  
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
  
  scale_x_datetime(expand=c(0,0),limits=c(day_MN,day_MX),breaks = "7 day",date_labels = "%e") +
  
  labs(x = "Date", y = "AB Hourly Trade (MWh)",title=month.abb[month]) +
  
  scale_fill_manual(values = c("AB_BC"= "dodgerblue4","AB_SK"="springgreen4","BC_AB"="dodgerblue","SK_AB"="springgreen")) +
  
  guides(fill = guide_legend(nrow = 1)) 
}

###############################################################################  
## FUNCTION: T_month_all_Sim 
## All trade for each month over one year.
##
## INPUTS: 
##    Years2See - Year to show in duration curve
################################################################################ 

T_month_all_Sim <- function(year,case) {
  
  
  # Create a graph for each month of the year
  p1 <- MN_TradeOnly(year,01,case) +
    theme(axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  
  p2 <- MN_TradeOnly(year,02,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p3 <- MN_TradeOnly(year,03,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p4 <- MN_TradeOnly(year,04,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p5 <- MN_TradeOnly(year,05,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p6 <- MN_TradeOnly(year,06,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p7 <- MN_TradeOnly(year,07,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p8 <- MN_TradeOnly(year,08,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p9 <- MN_TradeOnly(year,09,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p10 <- MN_TradeOnly(year,10,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p11 <- MN_TradeOnly(year,11,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p12 <- MN_TradeOnly(year,12,case) +
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
             size = 5,
             label = paste("Year:",year,",",SourceDB)) + 
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

###############################################################################  

###############################################################################  
################################################################################
## HR FIT FUNCTIONS
## Used to build trade pattrens
################################################################################
############################################################################### 

###############################################################################  
## FUNCTION: HR_Mn_BC 
##
## INPUTS: 
##    year - Year to check
##    month - month to check
################################################################################  

HR_Mn_BC <- function(year,month,HRcalc) {
  
  # Min for date (x-axis)
  day_MN <- as.POSIXct(paste(01,month,year, sep = "/"), format="%d/%m/%Y")
  
  if (month <12) {
    # Max for date
    day_MX <- as.POSIXct(paste(01,month+1,year, sep = "/"), format="%d/%m/%Y")
    day_MX <- ceiling_date(day_MX, "month") - 1 
    
  }else {
    day_MX <- as.POSIXct(paste(31,month,year, sep = "/"), format="%d/%m/%Y") }
  
  
  #Imports
  HR_BC_Imp <- HRcalc %>%
    select(.,c("date","Month","Year","IMPORT_BC_MT","T_HR_IMPORT_BC")) %>%
    rename(IMPORT_LOAD=IMPORT_BC_MT) %>%
    rename(Theo_HR=T_HR_IMPORT_BC) %>%
        mutate(ID ="BC_AB")%>%
    filter(date>=day_MN) %>%
    filter(date<=day_MX) 
  
  #Exports
  HR_BC_Exp <- HRcalc %>%
    select(.,c("date","Month","Year","EXPORT_BC_MT","T_HR_EXPORT_BC")) %>%
    rename(IMPORT_LOAD=EXPORT_BC_MT) %>%
    rename(Theo_HR=T_HR_EXPORT_BC) %>%
    mutate(ID ="AB_BC")%>%
    filter(date>=day_MN) %>%
    filter(date<=day_MX) 
  
  # Get mean values to plot
  h_line1 <- median(HR_BC_Imp$Theo_HR, na.rm=TRUE)
  h_line2 <- median(HR_BC_Exp$Theo_HR, na.rm=TRUE)
  
  # Plot Trade
  ggplot() +
    geom_point(data = HR_BC_Imp, aes(x = date, y = Theo_HR, colour = ID), 
              alpha=1, size=1) +

    geom_point(data = HR_BC_Exp, aes(x = date, y = Theo_HR, colour = ID), 
               alpha=1, size=1) +
    
    geom_hline(yintercept =h_line2 , color='darkred', lty='dashed', lwd=1.5) +
    geom_hline(yintercept = h_line1, color='dodgerblue4', lty='dashed', lwd=1.5) +
    
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
    
    scale_y_continuous(expand=c(0,0),limits = c(0,30000),breaks=pretty_breaks(8)) +
    
    scale_x_datetime(expand=c(0,0),limits=c(day_MN,day_MX),breaks = "7 day",date_labels = "%e") +
    
    labs(x = "Date", y = "Theoretical HR (Btu/kWh)",title=year) +
    
    scale_colour_manual(values = c("AB_BC"= "red","BC_AB"="dodgerblue")) +
    
    guides(fill = guide_legend(nrow = 1)) 
  
}

###############################################################################  
## FUNCTION: HR_Mn_SK 
##
## INPUTS: 
##    year - Year to check
##    month - month to check
################################################################################  

HR_Mn_SK <- function(year,month,HRcalc) {
  
  # Min for date (x-axis)
  day_MN <- as.POSIXct(paste(01,month,year, sep = "/"), format="%d/%m/%Y")
  day_lab <-as.POSIXct(paste(05,month,year, sep = "/"), format="%d/%m/%Y")
  
  if (month <12) {
    # Max for date
    day_MX <- as.POSIXct(paste(01,month+1,year, sep = "/"), format="%d/%m/%Y")
    day_MX <- ceiling_date(day_MX, "month") - 1 
    
  }else {
    day_MX <- as.POSIXct(paste(31,month,year, sep = "/"), format="%d/%m/%Y") }
  
  
  #Imports
  
  HR_SK_Imp <- HRcalc %>%
    select(.,c("date","Month","Year","IMPORT_SK","T_HR_IMPORT_SK")) %>%
    rename(IMPORT_LOAD=IMPORT_SK) %>%
    rename(Theo_HR=T_HR_IMPORT_SK) %>%
    mutate(ID ="SK_AB")%>%
    filter(date>=day_MN) %>%
    filter(date<=day_MX) 
  
  #Exports
  
  HR_SK_Exp <- HRcalc %>%
    select(.,c("date","Month","Year","EXPORT_SK","T_HR_EXPORT_SK")) %>%
    rename(IMPORT_LOAD=EXPORT_SK) %>%
    rename(Theo_HR=T_HR_EXPORT_SK) %>%
    mutate(ID ="AB_SK")%>%
    filter(date>=day_MN) %>%
    filter(date<=day_MX) 
  
  # Get mean values to plot
  h_line3 <- round(median(HR_SK_Imp$Theo_HR, na.rm=TRUE),digits=0)
  h_line4 <- round(median(HR_SK_Exp$Theo_HR, na.rm=TRUE),digits=0)
  
  # Plot Trade
  ggplot() +
    geom_point(data = HR_SK_Imp, aes(x = date, y = Theo_HR, colour = ID), 
              alpha=1, size=1) +
    
    # geom_text(
    #   aes(x = day_lab, y = h_line3+500, label = h_line3,colour='darkgreen',fontface="bold",size=3)) +

    geom_point(data = HR_SK_Exp, aes(x = date, y = Theo_HR, colour = ID), 
               alpha=1, size=1) +
    
    geom_hline(yintercept = h_line4, color='darkred', lty='dashed', lwd=1.5)  +
    geom_hline(yintercept = h_line3, color='darkgreen', lty='dashed', lwd=1.5) +
    # geom_text(
    #   aes(x = day_lab, y = h_line4+500, label = h_line4,colour='darkred',size=3)) +
    # 
    
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
    
    scale_y_continuous(expand=c(0,0),limits = c(0,30000),breaks=pretty_breaks(8)) +
    
    scale_x_datetime(expand=c(0,0),limits=c(day_MN,day_MX),breaks = "7 day",date_labels = "%e") +
    
    labs(x = "Date", y = "Theoretical HR (Btu/kWh)",title=year) +
    
    scale_colour_manual(values = c("AB_SK"="red","SK_AB"="springgreen")) +
    
    guides(fill = guide_legend(nrow = 1)) 
  
}

###############################################################################  
## FUNCTION: HR_YR_BC 
##
## INPUTS: 
##    year - Year to check
##    month - month to check
################################################################################  

HR_YR_BC <- function(year) {
  

  #Imports
  HR_BC_Imp <- HRcalc %>%
    select(.,c("date","Month","Year","IMPORT_BC_MT","T_HR_IMPORT_BC")) %>%
    rename(IMPORT_LOAD=IMPORT_BC_MT) %>%
    rename(Theo_HR=T_HR_IMPORT_BC) %>%
    mutate(ID ="BC_AB")%>%
    filter(Year==year) 
  
  #Exports
  HR_BC_Exp <- HRcalc %>%
    select(.,c("date","Month","Year","EXPORT_BC_MT","T_HR_EXPORT_BC")) %>%
    rename(IMPORT_LOAD=EXPORT_BC_MT) %>%
    rename(Theo_HR=T_HR_EXPORT_BC) %>%
    mutate(ID ="AB_BC")%>%
    filter(Year==year) 
  
  # Get mean values to plot
  h_line1 <- median(HR_BC_Imp$Theo_HR, na.rm=TRUE)
  h_line2 <- median(HR_BC_Exp$Theo_HR, na.rm=TRUE)
  
  # Plot Trade
  ggplot() +
    geom_point(data = HR_BC_Imp, aes(x = date, y = Theo_HR, colour = ID), 
               alpha=1, size=0.5) +
    
    geom_point(data = HR_BC_Exp, aes(x = date, y = Theo_HR, colour = ID), 
               alpha=1, size=0.5) +
    
    geom_hline(yintercept =h_line2 , color='darkred', lty='dashed', lwd=1) +
    geom_hline(yintercept = h_line1, color='dodgerblue4', lty='dashed', lwd=1) +
    
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
          legend.text=element_text(size = 15),
          text = element_text(size = 10)) +
    
    scale_y_continuous(expand=c(0,0),limits = c(0,30000),breaks=pretty_breaks(8)) +
    
    scale_x_datetime(expand=c(0,0),breaks = "month",date_labels = "%b") +
    
    labs(x = "Date", y = "Theoretical HR (Btu/kWh)",title=year) +
    
    scale_colour_manual(values = c("AB_BC"= "red","BC_AB"="dodgerblue")) +
    
    guides(fill = guide_legend(nrow = 1)) 
  
}

###############################################################################  
## FUNCTION: HR_Yr_SK 
##
## INPUTS: 
##    year - Year to check
##    month - month to check
################################################################################  

HR_Yr_SK <- function(year) {

  #Imports
  
  HR_SK_Imp <- HRcalc %>%
    select(.,c("date","Month","Year","IMPORT_SK","T_HR_IMPORT_SK")) %>%
    rename(IMPORT_LOAD=IMPORT_SK) %>%
    rename(Theo_HR=T_HR_IMPORT_SK) %>%
    mutate(ID ="SK_AB")%>%
    filter(Year==year)
  
  #Exports
  
  HR_SK_Exp <- HRcalc %>%
    select(.,c("date","Month","Year","EXPORT_SK","T_HR_EXPORT_SK")) %>%
    rename(IMPORT_LOAD=EXPORT_SK) %>%
    rename(Theo_HR=T_HR_EXPORT_SK) %>%
    mutate(ID ="AB_SK")%>%
    filter(Year==year)
  
  # Get mean values to plot
  h_line3 <- round(median(HR_SK_Imp$Theo_HR, na.rm=TRUE),digits=0)
  h_line4 <- round(median(HR_SK_Exp$Theo_HR, na.rm=TRUE),digits=0)
  
  # Plot Trade
  ggplot() +
    geom_point(data = HR_SK_Imp, aes(x = date, y = Theo_HR, colour = ID), 
               alpha=1, size=0.5) +
    
    # geom_text(
    #   aes(x = day_lab, y = h_line3+500, label = h_line3,colour='darkgreen',fontface="bold",size=3)) +
    
    geom_point(data = HR_SK_Exp, aes(x = date, y = Theo_HR, colour = ID), 
               alpha=1, size=0.5) +
    
    geom_hline(yintercept = h_line4, color='darkred', lty='dashed', lwd=1)  +
    geom_hline(yintercept = h_line3, color='darkgreen', lty='dashed', lwd=1) +
    # geom_text(
    #   aes(x = day_lab, y = h_line4+500, label = h_line4,colour='darkred',size=3)) +
    # 
    
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
          legend.text=element_text(size = 15),
          text = element_text(size = 10)) +
    
    scale_y_continuous(expand=c(0,0),limits = c(0,30000),breaks=pretty_breaks(8)) +
    
    scale_x_datetime(expand=c(0,0),breaks = "month",date_labels = "%b") +
    
    labs(x = "Date", y = "Theoretical HR (Btu/kWh)",title=year) +
    
    scale_colour_manual(values = c("AB_SK"="red","SK_AB"="springgreen")) +
    
    guides(fill = guide_legend(nrow = 1)) 
  
}
###############################################################################  
## FUNCTION: HR_month_BC_all 
##
## INPUTS: 
##    year - Year to check
##    month - month to check
################################################################################ 

HR_month_BC_all <- function(month) {
  
  
  # Create a graph for each month of the year
  p1 <- HR_Mn_BC(2010,month,HRcalc) +
    theme(axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  
  p2 <- HR_Mn_BC(2011,month,HRcalc) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p3 <- HR_Mn_BC(2012,month,HRcalc) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p4 <- HR_Mn_BC(2013,month,HRcalc) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p5 <- HR_Mn_BC(2014,month,HRcalc) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p6 <- HR_Mn_BC(2015,month,HRcalc) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p7 <- HR_Mn_BC(2016,month,HRcalc) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p8 <- HR_Mn_BC(2017,month,HRcalc) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p9 <- HR_Mn_BC(2018,month,HRcalc) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p10 <- HR_Mn_BC(2019,month,HRcalc) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p11 <- HR_Mn_BC(2020,month,HRcalc) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p12 <- HR_Mn_BC(2021,month,HRcalc) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  # Get a common legend
  legend <- get_legend(p1)
  p1 <- p1 + theme(legend.position ="none")
  
  # Plot Labels
  yleft <- textGrob("Theoretical HR (Btu/kWh", rot = 90, gp = gpar(fontsize = 15))
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

###############################################################################  
## FUNCTION: HR_month_SK_all 
##
## INPUTS: 
##    year - Year to check
##    month - month to check
################################################################################ 

HR_month_SK_all <- function(month) {
  
  
  # Create a graph for each month of the year
  p1 <- HR_Mn_SK(2010,month,HRcalc) +
    theme(axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  
  p2 <- HR_Mn_SK(2011,month,HRcalc) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p3 <- HR_Mn_SK(2012,month,HRcalc) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p4 <- HR_Mn_SK(2013,month,HRcalc) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p5 <- HR_Mn_SK(2014,month,HRcalc) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p6 <- HR_Mn_SK(2015,month,HRcalc) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p7 <- HR_Mn_SK(2016,month,HRcalc) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p8 <- HR_Mn_SK(2017,month,HRcalc) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p9 <- HR_Mn_SK(2018,month,HRcalc) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p10 <- HR_Mn_SK(2019,month,HRcalc) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p11 <- HR_Mn_SK(2020,month,HRcalc) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p12 <- HR_Mn_SK(2021,month,HRcalc) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  # Get a common legend
  legend <- get_legend(p1)
  p1 <- p1 + theme(legend.position ="none")
  
  # Plot Labels
  yleft <- textGrob("Theoretical HR (Btu/kWh", rot = 90, gp = gpar(fontsize = 15))
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

###############################################################################  
## FUNCTION: HR_year_BC_all 
##
## INPUTS: 
##    year - Year to check
##    month - month to check
################################################################################ 

HR_year_BC_all <- function() {
  
  
  # Create a graph for each month of the year
  p1 <- HR_YR_BC(2010) +
    theme(axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p2 <- HR_YR_BC(2011) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p3 <- HR_YR_BC(2012) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p4 <- HR_YR_BC(2013) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p5 <- HR_YR_BC(2014) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p6 <- HR_YR_BC(2015) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p7 <- HR_YR_BC(2016) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p8 <- HR_YR_BC(2017) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p9 <- HR_YR_BC(2018) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p10 <- HR_YR_BC(2019) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p11 <- HR_YR_BC(2020) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p12 <- HR_YR_BC(2021) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  # Get a common legend
  legend <- get_legend(p1)
  p1 <- p1 + theme(legend.position ="none")
  
  # Plot Labels
  yleft <- textGrob("Theoretical HR (Btu/kWh", rot = 90, gp = gpar(fontsize = 15))
  bottom <- textGrob("Date", gp = gpar(fontsize = 15))
  
  # Cheat way to put an x title in
  xtitle <- ggplot() +
    annotate("text", x = 10,  y = 10,
             size = 6,
             label = "Month") + 
    theme_void()
  
  # Label the source and year
  xsubtitle <- ggplot() +
    annotate("text", x = 10,  y = 10,
             size = 4,
             label = " AESO Data") + 
    theme_void()
  
  #Create a big window
  windows(20,12)
  
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

###############################################################################  
## FUNCTION: HR_year_SK_all 
##
## INPUTS: 
##    year - Year to check
##    month - month to check
################################################################################ 

HR_year_SK_all <- function() {
  
  
  # Create a graph for each month of the year
  p1 <- HR_Yr_SK(2010) +
    theme(axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p2 <- HR_Yr_SK(2011) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p3 <- HR_Yr_SK(2012) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p4 <- HR_Yr_SK(2013) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p5 <- HR_Yr_SK(2014) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p6 <- HR_Yr_SK(2015) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p7 <- HR_Yr_SK(2016) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p8 <- HR_Yr_SK(2017) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p9 <- HR_Yr_SK(2018) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p10 <- HR_Yr_SK(2019) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p11 <- HR_Yr_SK(2020) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p12 <- HR_Yr_SK(2021) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  # Get a common legend
  legend <- get_legend(p1)
  p1 <- p1 + theme(legend.position ="none")
  
  # Plot Labels
  yleft <- textGrob("Theoretical HR (Btu/kWh", rot = 90, gp = gpar(fontsize = 15))
  bottom <- textGrob("Date", gp = gpar(fontsize = 15))
  
  # Cheat way to put an x title in
  xtitle <- ggplot() +
    annotate("text", x = 10,  y = 10,
             size = 6,
             label = "Month") + 
    theme_void()
  
  # Label the source and year
  xsubtitle <- ggplot() +
    annotate("text", x = 10,  y = 10,
             size = 4,
             label = " AESO Data") + 
    theme_void()
  
  #Create a big window
  windows(20,12)
  
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

###############################################################################  
################################################################################
## AESO SIM COMPARE INTERTIE FUNCTIONS
################################################################################
############################################################################### 
mn_Trade_Comp <- function(year,month,case,HRcalc) {
  
  SimPlot <- MN_TradeOnly(year,month,case) +
    theme(axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          plot.title = element_blank())
  
  AESOPlot <- TradeOnly_Mn_AESO(year,month,HRcalc) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          plot.title = element_blank())
    
  # Get Trade legend
  legend <- get_legend(SimPlot)
  SimPlot <- SimPlot + theme(legend.position ="none")
  
  gA <- ggplotGrob(SimPlot)
  gB<- ggplotGrob(AESOPlot)
  
  # # Arrange all the plots together
  # maxHeight = grid::unit.pmax(gA$heights[2:5], gB$heights[2:5])
  # 
  # gA$heights[2:5] <- as.list(maxHeight)
  # gB$heights[2:5] <- as.list(maxHeight)
  
  # Plot Labels

  yleft <- ggplot() +
    annotate("text", x = 10,  y = 10,
             size = 5,
             angle = 90,
             label = "AB Hourly Trade (MWh)") + 
    theme_void()
  
  # Cheat way to put an x title in
  xtitle <- ggplot() +
    annotate("text", x = 10,  y = 10,
             size = 5,
             label = "Month") + 
    theme_void()
  
  # Label the source and year
  xsubtitle <- ggplot() +
    annotate("text", x = 10,  y = 10,
             size = 4,
             label = paste("Time Period:",month.name[month],",",year)) + 
    theme_void()
  
  # Title 1
  title1 <- ggplot() +
    annotate("text", x = 10,  y = 10,
             size = 5,
             label = SourceDB) + 
    theme_void()
  
  # Title 2
  title2 <- ggplot() +
    annotate("text", x = 10,  y = 10,
             size = 5,
             label = "AESO Data") + 
    theme_void()
  
  windows(12,6)
  
  grid.arrange(gA, gB, 
               plot_grid(xtitle),
               plot_grid(legend),
               plot_grid(xsubtitle),
               plot_grid(yleft),
               plot_grid(title1),
               plot_grid(title2),
               ncol=3,nrow=5, 
               layout_matrix = rbind(c(NA,7,8),c(6,1,2), c(3,3,3),c(4,4,4),c(5,5,5)),
               heights=c(0.1,1, 0.1,0.1,0.1),
               widths=c(0.1, 1,1))
  

}





