################################################################################
# TITLE: aeso_eval_1
# DESCRIPTION: Functions To used to plot and analyze AESO data
#
# AUTHOR: 
#   Codes adapted from Taylor Pawlenchuk include name in header block, 
#   modified since retrieval
#   If not specific, code is by Jessica Van Os
#
# EDITS & ADDITIONAL CONTENT: Jessica Van Os
# LAST EDIT: June 17, 2024

################################################################################
## FUNCTION: plnt_tr
## Identify Specific Plant traits
## Original Author: Taylor Pawlenchuk (Retrieved June 7, 2022)
################################################################################
plnt_tr <- function(Asset_ID) {
  plnt <- sub_samp %>%
    filter(ID == Asset_ID) %>%
    subset(., select = c(time, Price, gen, Capacity, Plant_Type, AESO_Name, CO2, Heat.Rate, Revenue, Cap_Fac))
}

################################################################################
## FUNCTION: Week_act
## Plots actual AESO output for a single week given the case study
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
## TABLES REQUIRED: 
##    df1a - Filtered version of nrgstream
################################################################################
Week_act <- function(year,month,day,MN=0,MX=13000) {
  
  wk_st <- as.POSIXct(paste(day,month,year, sep = "/"), format="%d/%m/%Y")
  wk_end <- as.POSIXct(paste(day+7,month,year, sep = "/"), format="%d/%m/%Y")
  
  df1a$time <- as.POSIXct(df1a$time, tz = "MST")
  
  # Select only a single week
  WKa <- df1a %>%
    filter(Plant_Type != "EXPORT" & Plant_Type != "IMPORT") %>%
    filter(time >= wk_st, time <= wk_end)%>%
    subset(.,select=c(Plant_Type,time,meancap,total_gen,total_rev,price_mean,heatrt_mean,Day,Year,Hour))
  
  WKIM <- df1a %>%
    filter(Plant_Type %in% c("EXPORT","IMPORT")) %>%
    filter(time >= wk_st & time <= wk_end)%>%
    group_by(time,Year,Day,Hour)%>%
    summarise(total_gen=sum(total_gen)*-1)%>%
    mutate(Plant_Type="TRADE",
           meancap="",
           total_rev="",
           price_mean="",
           heatrt_mean="")%>%
    ungroup()%>%
    subset(.,select=c(Plant_Type,time,meancap,total_gen,total_rev,price_mean,heatrt_mean,Day,Year,Hour))
  
  WK <- rbind(WKIM, WKa)
  
  {
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "SOLAR", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "WIND", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "HYDRO", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "OTHER", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "SCGT", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "NGCC", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "NGCONV", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "COAL", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "COGEN", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "STORAGE", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "TRADE", after = Inf)
    
  }
  
  WK$Plant_Type <- factor(WK$Plant_Type, levels=c("SOLAR","WIND", "HYDRO",  
                                                  "OTHER","SCGT","NGCC","NGCONV",
                                                  "COAL", "COGEN","STORAGE","TRADE"))
  
  levels(WK$Plant_Type) <- c("Solar","Wind","Hydro",
                             "Other","Natural Gas Simple Cycle", "Natural Gas Combined Cycle","Coal-to-Gas", 
                             "Coal", "Cogeneration","Storage","Trade")

  
  
  dmd <- Actdemand %>%
    filter(time >= wk_st & time <= wk_end)
  
  # Plot the data    
  ggplot() +
    geom_area(data = WK, aes(x = time, y = total_gen, fill = Plant_Type), colour = "black", 
              alpha=Plot_Trans, size=0.25) +
    
    # Add hourly load line
    geom_line(data = dmd, 
              aes(x = time, y = Demand), size=1.25, colour = "black") +
    
    scale_x_datetime(expand=c(0,0),date_labels = "%b-%e" ,breaks = "day") +
    
    geom_hline(yintercept = 0) +
    # Set the theme for the plot
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text,color="black",size= GenText_Sz)) +
    
    theme(panel.grid = element_blank(),
          legend.position = "bottom",
    ) +
    theme(axis.text.x = element_text(color="black",size= GenText_Sz),
          axis.text.y = element_text(color="black",size= GenText_Sz),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size= GenText_Sz+12,family = Plot_Text_bf),
          plot.title = element_text(size= GenText_Sz,family = Plot_Text_bf),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          #legend.key.size = unit(0.5,"lines"),
          legend.title=element_blank(),
          legend.text = element_text(size=GenText_Sz-12),
          legend.position = "right",
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    guides(fill = guide_legend(ncol = 1)) +
    scale_y_continuous(expand=c(0,0),labels=comma,limits=c(MN,MX),breaks = seq(MN,MX,by = 2500)) +
    labs(title = paste("NRG Stream Data,", year), x = "Date", y = "Output (MWh)", fill = "Plant_Type", colour = "Plant_Type") +

    #Add colour
    scale_fill_manual(values = colours1_exist,drop=FALSE) 
    
}

################################################################################
## FUNCTION: wkPrice
## Plots actual AESO pool price
## Original Author: Taylor Pawlenchuk (Retrieved June 7, 2022)
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
## TABLES REQUIRED: 
##    demand - Filtered version of nrgstream
################################################################################
wkPrice <- function(year,month,day) {
  
  wk_st <- as.POSIXct(paste(day,month,year, sep = "/"), format="%d/%m/%Y")
  wk_end <- as.POSIXct(paste(day+7,month,year, sep = "/"), format="%d/%m/%Y")
  
  price_WK <- Actdemand %>%
    filter(time >= wk_st & time <= wk_end)
  
  # Set the max for the plot
  MX <- plyr::round_any(max(abs(price_WK$Price)+100), 10, f = ceiling)
  
  ggplot() +
    geom_line(data = price_WK, 
              aes(x=time, y=Price), 
              size = 1.5, color="darkred") +
    scale_x_datetime(expand=c(0,0)) +
    
    theme_bw() +
    theme(text=element_text(family=Plot_Text)) +
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid = element_blank(),
          axis.title.x = element_text(size = XTit_Sz),
          axis.title.y = element_text(size = YTit_Sz),
          axis.text.y=element_text(hjust=-0.5),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'grey'),
          panel.grid.minor.y = element_blank(),
          text = element_text(size= 15)
    ) +
    scale_y_continuous(expand=c(0,0),limits=c(0,MX)) +
    labs(x = "Date", y = "Pool Price ($/MWh)")
}

################################################################################
## FUNCTION: AESO_Pr0t
## Price and output side by side
## Original Author: Taylor Pawlenchuk (Retrieved June 7, 2022)
##
## INPUTS: 
##    year, month, day - Date to look at
## FUNCTIONS REQUIRED: 
##    wkPrice - One week of AESO price data output
##    Week_act - One week of AESO data output
################################################################################

AESO_PrOt <- function(year,month,day) {
  plot_grid(wkPrice(year,month,day) + 
              theme(axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position ="bottom"),
            
            Week_act(year,month,day)+theme(legend.position ="none"), 
            ncol = 1, align="v", axis = "l",rel_heights = c(1,2.5)) 
}

################################################################################
## AESO INTERTIE FUNCTIONS
##  Functions use AESO data to visualize trade patterns
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
  
  if (month <12) {
    # Max for date
    day_MX <- as.POSIXct(paste(01,month+1,year, sep = "/"), format="%d/%m/%Y")
    day_MX <- ceiling_date(day_MX, "month") - 1 
    
  }else {
    day_MX <- as.POSIXct(paste(31,month,year, sep = "/"), format="%d/%m/%Y") }
  
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
              alpha=0.7, size=0.5) +
    
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
  
  Imp_Exp[is.na(Imp_Exp)] <- 0
  
  Imp_Exp <- Imp_Exp %>%
    filter(date>=day_MN) %>%
    filter(date<=day_MX) 
  
  #Imports from BC
  Imp_BC <- Imp_Exp %>%
    select(.,c("IMPORT_BC_MT","Date_Begin_Local","date")) %>%
    filter(IMPORT_BC_MT>-1) %>%
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
    filter(IMPORT_SK>-1) %>%
    rename(Net_Load=IMPORT_SK) %>%
    mutate(ID ="SK_AB")
  
  # Exports to BC
  Exp_SK <- Imp_Exp %>%
    select(.,c("EXPORT_SK","Date_Begin_Local","date")) %>%
    rename(Net_Load=EXPORT_SK) %>%
    mutate(Net_Load=Net_Load*-1) %>%
    mutate(ID ="AB_SK")
  
  
  # Put all data together and filter week
  data <-rbind(Exp_SK,Exp_BC,Imp_BC,Imp_SK) 
  
  # Plot Trade
  ggplot() +
    geom_area(data = data, aes(x = date, y = Net_Load, fill = ID), 
              alpha=0.7, size=0.5) +
    
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
    
    labs(x = "Date", y = "AB Hourly Trade (MWh)",title=paste(month.abb[month])) +
    
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

Trade_Yr_AESO <- function(year,Imp_Exp) {
  
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

################################################################################  
## FUNCTION: Wind_Dur_AESO
## Plot wind duration curve in chosen years as % Hours vs Fleet Output (MW)
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupHr -Yearly resource group data
################################################################################
Wind_Dur_AESO <- function() {
  
  # Bring in all data
  WindData <- df1a%>%
    filter(Plant_Type=="WIND") %>%
    group_by(Year) %>%
    # Get an empirical distribution for grouped data, 
    mutate(perc = 1-ecdf(total_gen)(total_gen),
           Output=total_gen) %>%
    rename(date=time)%>%
    subset(select=c(Output,perc,date,Year))
  
  # # filter out years of interest
  # WindData <- WindData %>%
  #   filter(Report_Year < 2036)
  
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
    
    labs(x = "Fleet Output (MW)", y = "Percent of Hours per Year",colour="ID",linetype="ID",caption = paste(SourceDB,',')) +
    
    scale_y_continuous(expand = c(0.01, 0),limits = c(0,MX),breaks=seq(0, MX, by=1000)) +
    
    scale_x_continuous(expand=c(0.01,0),breaks=seq(0, 1, by=0.2),labels = percent) 
  
}  

################################################################################  
## FUNCTION: Wind_DurNorm_AESO
## Plot wind duration curve in chosen years as % Hours vs output as Percent of max
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupHr -Yearly resource group data
################################################################################
Wind_DurNorm_AESO <- function() {
  
  # Bring in all data
  WindData <- df1a%>%
    filter(Plant_Type=="WIND") %>%
    group_by(Year) %>%
    mutate(Norm_Output=total_gen/max(total_gen),
           Output=total_gen)%>%
    rename(date=time,
           CF=group_CF)%>%
    # Get an empirical distribution for grouped data, 
    mutate(perc = 1-ecdf(CF)(CF))%>%
    subset(select=c(Output,CF,perc,date,Year))
  
  # Get the report year as a factor to plot
  WindData$Year <- as.factor(WindData$Year)
  
  # Plot
  ggplot() +
    geom_line(data = WindData, 
              aes(x = perc, y = CF, colour = Year), size = 1.25) +
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
    
    labs(x = "Percent of Hours per Year", y = "Fleet Capacity Factor (Output/Capacity)",colour="ID",linetype="ID",caption = paste("Source: NRGStream")) +
    
    scale_x_continuous(expand = c(0.01, 0),limits = c(0,1),breaks=seq(0, 1, by=0.2),labels = percent) +
    
    scale_y_continuous(expand=c(0,0),breaks=seq(0, 1, by=0.2),labels = percent) 
  
}

################################################################################
## FUNCTION: Day_AESO
## Plots actual AESO output for a single day.
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
## TABLES REQUIRED: 
##    df1a - Filtered version of nrgstream
################################################################################
Day_AESO <- function(year,month,day,MX) {
  
  # Get date to report
  day_filt <-as.POSIXct(paste(year,month,day," 00:00:00", sep = "-"),tz = "MST")
  day_report <- paste(month.abb[month]," ",day,", ",year," (",wday(day_filt,label=TRUE),")",sep="")
  
  # Format times
  df1a$time <- as.POSIXct(df1a$time, tz = "MST")
  
  # Select only a single day
  DYa <- df1a %>%
    filter(Plant_Type != "EXPORT" & Plant_Type != "IMPORT",
            Day == as.Date(paste(year,month,day, sep = "-")))
  
  # Select Imports
  DY_IM <- df1a %>%
    filter(Plant_Type == "IMPORT",
           Day == as.Date(paste(year,month,day, sep = "-")))%>%
    mutate(total_gen=total_gen*-1)
  
  # Put together
  DY_Tot <- rbind(DY_IM, DYa)
  
  {
    DY_Tot$Plant_Type<-fct_relevel(DY_Tot$Plant_Type, "IMPORT", after = Inf)
    DY_Tot$Plant_Type<-fct_relevel(DY_Tot$Plant_Type, "SOLAR", after = Inf)
    DY_Tot$Plant_Type<-fct_relevel(DY_Tot$Plant_Type, "WIND", after = Inf)
    DY_Tot$Plant_Type<-fct_relevel(DY_Tot$Plant_Type, "OTHER", after = Inf)
    DY_Tot$Plant_Type<-fct_relevel(DY_Tot$Plant_Type, "HYDRO", after = Inf)
    DY_Tot$Plant_Type<-fct_relevel(DY_Tot$Plant_Type, "SCGT", after = Inf)
    DY_Tot$Plant_Type<-fct_relevel(DY_Tot$Plant_Type, "NGCC", after = Inf)
    DY_Tot$Plant_Type<-fct_relevel(DY_Tot$Plant_Type, "NGCONV", after = Inf)
    DY_Tot$Plant_Type<-fct_relevel(DY_Tot$Plant_Type, "COAL", after = Inf)
    DY_Tot$Plant_Type<-fct_relevel(DY_Tot$Plant_Type, "COGEN", after = Inf)
    DY_Tot$Plant_Type<-fct_relevel(DY_Tot$Plant_Type, "STORAGE", after = Inf)
  }
  
  DY_Tot$Plant_Type <- factor(DY_Tot$Plant_Type, levels=c("IMPORT", "SOLAR","WIND",
                                                  "OTHER", "HYDRO",
                                                  "SCGT", "NGCC","NGCONV",  
                                                  "COAL", "COGEN","STORAGE"))
  
  levels(DY_Tot$Plant_Type) <- c("Import","Solar","Wind", 
                            "Other", "Hydro", 
                            "NGSC","NGCC", "Coal-to-Gas", 
                            "Coal", "Cogeneration","Storage")
  
  # Get demand line
  dmd <- Actdemand %>%
    filter(Day == as.Date(paste(year,month,day, sep = "-")))
  
  # Plot the data    
  ggplot() +
    geom_area(data = DY_Tot, aes(x = time, y = total_gen, fill = Plant_Type), colour = "black", 
              alpha=Plot_Trans, size=0.5) +
    
    # Add hourly load line
    geom_line(data = dmd, 
              aes(x = time, y = Demand), size=1.5, colour = "black") +
    
    # Set the theme for the plot
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),
          legend.position = "bottom",
    ) +
    theme(axis.text = element_text(size= GenText_Sz,colour = "black"),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.title.x = element_text(size= GenText_Sz+6),
          axis.title.y = element_text(size= GenText_Sz+6,family = Plot_Text_bf),
          #plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.position = 'right',
          legend.text = element_text(size=GenText_Sz-6),
          legend.title=element_blank(),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          text = element_text(size= GenText_Sz)
    ) +

    scale_y_continuous(expand=c(0,0), limits = c(0,MX), 
                       breaks = seq(0, MX, by = MX/4))+
    
    scale_x_datetime(expand=c(0,0),date_labels = "%H:%M",date_breaks = "4 hours") +
    
    labs(title = "NRG Stream Data", x = day_report, y = "Output (MWh)", fill = "Plant_Type", colour = "Plant_Type") +
    
    #Add colour
    scale_fill_manual(values = colours1_daily) 
  
}

###############################################################################  
## FUNCTION: T_month_all 
## All trade for each month over one year
##
## INPUTS: 
##    Years2See - Year to show in duration curve
################################################################################ 

T_month_all <- function(year,Imp_Exp) {
  
  
  # Create a graph for each month of the year
  p1 <- TradeOnly_Mn_AESO(year,01,Imp_Exp) +
    theme(axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  
  p2 <- TradeOnly_Mn_AESO(year,02,Imp_Exp) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p3 <- TradeOnly_Mn_AESO(year,03,Imp_Exp) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p4 <- TradeOnly_Mn_AESO(year,04,Imp_Exp) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p5 <- TradeOnly_Mn_AESO(year,05,Imp_Exp) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p6 <- TradeOnly_Mn_AESO(year,06,Imp_Exp) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p7 <- TradeOnly_Mn_AESO(year,07,Imp_Exp) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p8 <- TradeOnly_Mn_AESO(year,08,Imp_Exp) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p9 <- TradeOnly_Mn_AESO(year,09,Imp_Exp) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p10 <- TradeOnly_Mn_AESO(year,10,Imp_Exp) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p11 <- TradeOnly_Mn_AESO(year,11,Imp_Exp) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p12 <- TradeOnly_Mn_AESO(year,12,Imp_Exp) +
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
             label = paste("Year:",year,", AESO Data")) + 
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
## FUNCTION: AIL_Gen_Gaps
## Understand differences in AIL and generation
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ActaulDemand - Historical Demand
##    Df1a - Historical Generation
################################################################################
AIL_Gen_Gaps <- function(year,Imp_Exp) {
# LOOKING AT HOURLY DEMAND AND GENERATION TOTALS
# Re-format data
GenData<-df1a %>%
  filter(Plant_Type %in% c("COAL","NGCONV","COGEN","HYDRO","NGCC", "OTHER", "SCGT","SOLAR","WIND","STORAGE"))%>%
  group_by(time,Day,Year,Hour)%>%
  summarise(All_Gen=sum(total_gen))
ExportG<-df1a %>%
  filter(Plant_Type=="EXPORT")%>%
  group_by(time,Day)%>%
  summarise(Export=sum(total_gen))
ImportG<-df1a %>%
  filter(Plant_Type=="IMPORT")%>%
  group_by(time,Day)%>%
  summarise(Import=sum(total_gen)*-1)


# Put together
DFlist<-list(GenData,ExportG,ImportG,Actdemand)  

# Merge into one dataframe by date. Gap is AIL - Net Generation
Gen_Demand<- DFlist %>% 
  reduce(full_join, by=c("time","Day"),all=FALSE)%>%
  mutate(Gen_Net=All_Gen-Export+Import,
         Gap=AIL-Gen_Net)%>%
  subset(select=c("time","Day","Year","Hour","Price","Demand","AIL","Gen_Net","Gap"))

# Plot all 
PMin<-round_any(min(Gen_Demand$Gap,na.rm=TRUE)+10,50,f=floor)

ggplot(Gen_Demand) +
  geom_point(aes(x=time,y=Gap),size=0.5,na.rm=TRUE)+
  
  geom_hline(yintercept=-5, color = "blue", size=0.5)+
  geom_hline(yintercept=-25, color = "blue", size=0.5)+
  geom_hline(yintercept=-50, color = "blue", size=0.5)+
  geom_hline(yintercept=-100, color = "blue", size=0.5)+
  geom_hline(yintercept=-200, color = "blue", size=0.5)+
  
  
  theme_bw() +
  theme(panel.grid = element_blank()) +
  #theme(text=element_text(family=Plot_Text)) +
  
  scale_x_datetime(expand=c(0,0),date_labels = "%b %Y", breaks = "year")+
  scale_y_continuous(expand=c(0,0),limits=c(PMin,0)) +
  
  labs(x = "AIL - Net Gen (MW)", y = "Date", caption="From NRGStream data")
  

# Look at <0 only
Gen_Demand_Filt<-Gen_Demand%>%
  filter(Gap<0)

GroupedData<-cut_number(Gen_Demand_Filt$Gap,5)

# Historgram
ggplot(data = Gen_Demand_Filt, aes(x = Gap)) +
  geom_histogram(bins=100)+
  
  theme_bw() +
  theme(panel.grid = element_blank()) 
}

################################################################################  
## FUNCTION: Evalyr_AESO 
## Plotting year profiles of resource output
##
## INPUTS: 
##    input - ResgroupMnor ResGroupYr
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    Import - Import table derived of zone average table
################################################################################

Evalyr_AESO <- function(YrMin,Sep_cogen) {

  YrMax<-2023
  # Combine cogen 
  if (Sep_cogen == "n") {
    # Select only a single week
    YRall <- df1a %>%
      filter(Plant_Type != "EXPORT" & Plant_Type != "IMPORT") %>%
      mutate(across(Plant_Type,str_replace,'NGCONV|NGCC|SCGT|COGEN',"GAS"))%>%
      mutate(YEAR=as.numeric(year(Day)))%>%
      group_by(Plant_Type,Year,YEAR)%>%
      summarise(Gen=sum(total_gen),
                Gen_TWh=Gen/1000000)
  } else{
    # Select only a single week
    YRall <- df1a %>%
      filter(Plant_Type != "EXPORT" & Plant_Type != "IMPORT") %>%
      mutate(across(Plant_Type,str_replace,'NGCONV|NGCC|SCGT',"GAS"))%>%
      mutate(YEAR=as.numeric(year(Day)))%>%
      group_by(Plant_Type,Year,YEAR)%>%
      summarise(Gen=sum(total_gen),
                Gen_TWh=Gen/1000000)
  }
  
  YRtrade <- df1a %>%
    filter(Plant_Type %in% c("EXPORT","IMPORT")) %>%
    group_by(time,Year,Day,Hour)%>%
    summarise(total_gen=sum(total_gen)*-1)%>%
    mutate(Plant_Type="TRADE",
           meancap="",
           total_rev="",
           price_mean="",
           heatrt_mean="",
           YEAR=as.numeric(year(Day)))%>%
    ungroup()%>%
    group_by(Plant_Type,Year,YEAR)%>%
    summarise(Gen=sum(total_gen),
              Gen_TWh=Gen/1000000)
  
  YR_NRG <- rbind(YRall, YRtrade)
  
  # Add 2023
  gen2023 <- data.frame(Plant_Type=c("SOLAR","WIND","HYDRO",
                                     "OTHER","GAS","COGEN", 
                                     "COAL", "STORAGE","TRADE"),
                        Year=as.factor(2023),
                        YEAR=2023,
                        Gen=c(2310297.721,
                              10214688.89,
                              1697135.533,
                              2059662.312,
                              29869645.98,
                              33533379.56,
                              6514634.755,
                              NA,
                              0
                        )) %>%
    mutate(Gen_TWh=Gen/1000000)
  
  YR <- rbind(YR_NRG,gen2023)
  
  {
    YR$Plant_Type<-fct_relevel(YR$Plant_Type, "TRADE", after = Inf)
    YR$Plant_Type<-fct_relevel(YR$Plant_Type, "SOLAR", after = Inf)
    YR$Plant_Type<-fct_relevel(YR$Plant_Type, "WIND", after = Inf)
    YR$Plant_Type<-fct_relevel(YR$Plant_Type, "HYDRO", after = Inf)
    YR$Plant_Type<-fct_relevel(YR$Plant_Type, "OTHER", after = Inf)
    YR$Plant_Type<-fct_relevel(YR$Plant_Type, "GAS", after = Inf)
    YR$Plant_Type<-fct_relevel(YR$Plant_Type, "COGEN", after = Inf)
    YR$Plant_Type<-fct_relevel(YR$Plant_Type, "COAL", after = Inf)
    YR$Plant_Type<-fct_relevel(YR$Plant_Type, "STORAGE", after = Inf)
  }
  
  YR$Plant_Type <- factor(YR$Plant_Type, levels=c("TRADE","SOLAR","WIND", "HYDRO",  
                                                  "OTHER","GAS","COGEN",
                                                  "COAL", "STORAGE"))
  
  levels(YR$Plant_Type) <- c("Net Imports","Solar","Wind","Hydro",
                             "Biomass/Other","Natural Gas","Cogeneration", 
                             "Coal", "Storage")
  
  # Filter years
  YR <- YR%>%
    filter(YEAR >=YrMin,
           YEAR<=YrMax,
           Plant_Type !="Storage",
           Gen_TWh>=0)
  

  #GenText_Sz<-56
  # Plot
  YR %>%
    ggplot() +
    geom_area_pattern(aes(YEAR, Gen_TWh, fill = Plant_Type,pattern=Plant_Type),alpha=1, size=.5, colour="black",
                      pattern_density = 0.25,
                      pattern_fill = "black",
                      pattern_colour  = NA,
                      pattern_spacing=0.016) +
    
    theme_bw() +
    
    labs(x = "Year", y = "Alberta Annual Generation (TWh)", fill = "Resource",pattern="Resource",caption = "Data from NRGStream") +

    
    # Legend color scheme
    
    scale_fill_manual(values = c("Storage"='#cc79a7',"Solar"=cOL_SOLAR,"Wind"=cOL_WIND,"Hydro"=cOL_HYDRO,
                                 "Biomass/Other"='#D2DFFF',"Natural Gas"='#A6A6A6', 
                                 "Cogeneration"='#A6A6A6',#'#767171',
                                 "Coal"='#515151',"Net Imports"='#252323')) +
    
    scale_pattern_manual(values = c("Storage"='none',"Solar"='none',"Wind"='none',"Hydro"='none',
                                    "Biomass/Other"='none',"Natural Gas"='none', 
                                    "Cogeneration"='stripe',"Coal"='none',"Net Imports"='none')) + 
  
    theme(
      # General Plot Settings
      panel.grid = element_blank(),
      # (t,r,b,l) margins, adjust to show full x-axis, default: (5.5,5.5,5.5,5.5)
      plot.margin = unit(c(6, 12, 5.5, 5.5), "points"),      # Plot margins
      panel.background = element_blank(), # Transparent background
     # panel.border = element_blank(),
      text = element_text(size = GenText_Sz,family=Plot_Text),                # Text size
      plot.title = element_text(size = GenText_Sz),              # Plot title size (if present)
      axis.line.y = element_line(),
      axis.line.x = element_line(),
      plot.caption = element_text(hjust = 1,size = GenText_Sz-12,family=Plot_Text_it),             # Plot subtitle size (if present)

      # X-axis
      axis.text.x = element_text(vjust = 1,colour = "black",size = GenText_Sz),                 # Horizontal text
      #axis.title.x = element_text(size = XTit_Sz),           # x-axis title text size
      axis.title.x = element_blank(),
      # Y-axis
      axis.title.y = element_text(size = GenText_Sz+12,family=Plot_Text_bf),           # y-axis title text size
      axis.text.y = element_text(colour = "black",size = GenText_Sz),                 # Horizontal text
      
      # Legend
      legend.key.size = unit(1,"lines"),                     # Shrink legend boxes
      legend.position = "right",                            # Move legend to the bottom
      legend.justification = c(0.5,0.5),                     # Center the legend
      legend.text = element_text(size =GenText_Sz-6),              # Size of legend text
      legend.title=element_blank()) +                        # Remove legend title
    
    # Set axis scales
    scale_x_continuous(expand=c(0,0),limits = c(YrMin,2023),breaks=seq(YrMin, 2023, 2)) +
    
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,100),
                       breaks=pretty_breaks(5))
    
    #guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    # Plot labels
    

  
  # ggsave(
  #   filename = here(paste("Figures (Local)/","Historical Gen",".png", sep = "")),
  #   device = "png",
  #   plot = plotyr,
  #   width=6,
  #   height=3,
  #   dpi=300)
                        
}

################################################################################  
## FUNCTION: Evalcap_AESO 
## Plotting year profiles of resource capacity
##
## INPUTS: 
##    input - ResgroupMnor ResGroupYr
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    Import - Import table derived of zone average table
################################################################################

Evalcap_AESO <- function(YrMin,Sep_cogen) {
  
  # Read AESO capacity data
  AESO_Cap <- readRDS(here("Data Files","Alberta Data","AESO_Annual_Capacity.RData")) 
  
  # Combine cogen 
  if (Sep_cogen == "n") {
    # Select only a single week
    YR <- AESO_Cap  %>%
      replace(is.na(.), 0) %>%
      filter(Year >=YrMin) %>%
      mutate(across(Plant_Type,str_replace,'Gas Fired Steam|Combined Cycle|Simple Cycle|Cogeneration|Dual Fuel',"GAS"))%>%
      group_by(Plant_Type,Year)%>%
      summarise(Cap=sum(Capacity))
  } else{
    # Select only a single week
    YR <- AESO_Cap %>%
      replace(is.na(.), 0) %>%
      filter(Year >=YrMin) %>%
      mutate(across(Plant_Type,str_replace,'Gas Fired Steam|Combined Cycle|Simple Cycle|Dual Fuel',"GAS"))%>%
      group_by(Plant_Type,Year)%>%
      summarise(Cap=sum(Capacity))
  }
  
  {
    YR$Plant_Type<-fct_relevel(YR$Plant_Type, "Storage", after = Inf)
    YR$Plant_Type<-fct_relevel(YR$Plant_Type, "Solar", after = Inf)
    YR$Plant_Type<-fct_relevel(YR$Plant_Type, "Wind", after = Inf)
    YR$Plant_Type<-fct_relevel(YR$Plant_Type, "Hydro", after = Inf)
    YR$Plant_Type<-fct_relevel(YR$Plant_Type, "Other", after = Inf)
    YR$Plant_Type<-fct_relevel(YR$Plant_Type, "GAS", after = Inf)
    YR$Plant_Type<-fct_relevel(YR$Plant_Type, "Cogeneration", after = Inf)
    YR$Plant_Type<-fct_relevel(YR$Plant_Type, "Coal", after = Inf)
  }
  
  
  YR$Plant_Type <- factor(YR$Plant_Type, levels=c("Storage","Solar","Wind", "Hydro",  
                                                  "Other","GAS","Cogeneration",
                                                  "Coal"))
  
  levels(YR$Plant_Type) <- c("Storage","Solar","Wind","Hydro",
                             "Other","Natural Gas","Cogeneration", 
                             "Coal")
  
  GenText_Sz<-56
  # Plot
  YR %>%
    ggplot() +
    geom_area(aes(Year, Cap, fill = Plant_Type),alpha=1, size=.25, colour="black") +
    
    theme_bw() +
    
    # Changes the font type
    #theme(text=element_text(family=Plot_Text)) +             
    
    theme(
      # General Plot Settings
      panel.grid = element_blank(),
      # (t,r,b,l) margins, adjust to show full x-axis, default: (5.5,5.5,5.5,5.5)
      plot.margin = unit(c(6, 12, 5.5, 5.5), "points"),      # Plot margins
      panel.background = element_blank(), # Transparent background
      panel.border = element_blank(),
      text = element_text(size = GenText_Sz),                # Text size
      plot.title = element_text(size = GenText_Sz),              # Plot title size (if present)
      axis.line.y = element_line(),
      axis.line.x = element_line(),
      plot.caption = element_text(hjust = 1,size = GenText_Sz-10,face = "italic"),             # Plot subtitle size (if present)
      
      # X-axis
      axis.text.x = element_text(vjust = 1,colour = "black"),                 # Horizontal text
      #axis.title.x = element_text(size = XTit_Sz),           # x-axis title text size
      axis.title.x = element_blank(),
      # Y-axis
      axis.title.y = element_text(size = GenText_Sz+8),           # y-axis title text size
      axis.text.y = element_text(colour = "black"),                 # Horizontal text
      
      # Legend
      legend.key.size = unit(1,"lines"),                     # Shrink legend boxes
      legend.position = "bottom",                            # Move legend to the bottom
      legend.justification = c(0.5,0.5),                     # Center the legend
      legend.text = element_text(size =GenText_Sz-6),              # Size of legend text
      legend.title=element_blank()) +                        # Remove legend title
    
    # Set axis scales
    scale_x_continuous(expand=c(0,0),limits = c(2015,2023),breaks=seq(2015, 2023, 1)) +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,20000),
                       breaks=pretty_breaks(6),
                       label=comma) +
    
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    # Plot labels
    labs(x = "Year", y = "Capacity (MW)", fill = "Resource",colour="Resource",caption = "Data from AESO Annual Market Statistics Report") +
    
    
    # Legend color scheme
    scale_fill_manual(values = c("Storage"='#cc79a7',"Solar"="darkgoldenrod2","Wind"="#238b45","Hydro"="#4472C4",
                                 "Other"='#e6e6e6',"Natural Gas"='#A6A6A6', 
                                 "Cogeneration"='#767171',"Coal"='#515151',"Net Imports"='#252323'),
                      drop = TRUE) 
  
  # ggsave(
  #   filename = here(paste("Figures (Local)/","Historical Gen",".png", sep = "")),
  #   device = "png",
  #   plot = plotyr,
  #   width=6,
  #   height=3,
  #   dpi=300)
  
}

################################################################################  
## FUNCTION: Evalcap_AESO2 
## Plotting year profiles of resource output
##
## INPUTS: 
##    input - ResgroupMnor ResGroupYr
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    Import - Import table derived of zone average table
################################################################################

Evalcap_AESO2 <- function(YrMin,Sep_cogen) {
  
  # Read AESO capacity data
  AESO_Cap <- readRDS(here("Data Files","Alberta Data","AESO_Annual_Capacity.RData")) 
  
  # Combine cogen 
  if (Sep_cogen == "n") {
    # Select only a single week
    YR <- AESO_Cap  %>%
      filter(Year >2016) %>%
      replace(is.na(.), 0) %>%
      mutate(across(Plant_Type,str_replace,'Gas Fired Steam|Combined Cycle|Simple Cycle|Cogeneration|Dual Fuel',"Natural Gas"),
             across(Plant_Type,str_replace,'Hydro|Other',"Hydro & Other"),
             Year_fact = as.factor(Year))%>%
      group_by(Plant_Type,Year,Year_fact)%>%
      summarise(Cap=sum(Capacity)) %>%
      mutate(Year_fact=if_else(Year == 2025,"2025 Under Construction",paste(Year_fact)))
      ymax<-16000
  } else{
    # Select only a single week
    YR <- AESO_Cap %>%
      filter(Year >2016) %>%
      replace(is.na(.), 0) %>%
      mutate(across(Plant_Type,str_replace,'Gas Fired Steam|Combined Cycle|Simple Cycle|Dual Fuel',"Natural Gas"),
             Year_fact = as.factor(Year))%>%
      group_by(Plant_Type,Year,Year_fact)%>%
      summarise(Cap=sum(Capacity))%>%
      mutate(Year_fact=if_else(Year == 2025,"2025 Under Construction",paste(Year_fact)))
    ymax<-10000
    
  }
  
  {
    YR$Plant_Type<-fct_relevel(YR$Plant_Type, "Coal", after = Inf)
    YR$Plant_Type<-fct_relevel(YR$Plant_Type, "Cogeneration", after = Inf)
    YR$Plant_Type<-fct_relevel(YR$Plant_Type, "Natural Gas", after = Inf)
    YR$Plant_Type<-fct_relevel(YR$Plant_Type, "Hydro", after = Inf)
    YR$Plant_Type<-fct_relevel(YR$Plant_Type, "Other", after = Inf)
    YR$Plant_Type<-fct_relevel(YR$Plant_Type, "Hydro & Other", after = Inf)
    YR$Plant_Type<-fct_relevel(YR$Plant_Type, "Wind", after = Inf)
    YR$Plant_Type<-fct_relevel(YR$Plant_Type, "Solar", after = Inf)
    YR$Plant_Type<-fct_relevel(YR$Plant_Type, "Storage", after = Inf)
    
    
  }

  # cols_choice <- c("2017"='#252323',"2018"="#515151","2019"="#767171","2020"="#A6A6A6",
  #                  "2021"='#e6e6e6',"2022"='#4472C4',
  #                  "2023"='#238b45',"2024"='darkgoldenrod2',"2025 Expected"='#cc79a7')
  
  # cols_choice <- c("2017"='#252323',"2018"="#515151","2019"="#767171","2020"="#A6A6A6",
  #                  "2021"='#e6e6e6',"2022"='#203764',
  #                  "2023"='#305496',"2024"='#4472C4',"2025 Under Construction"='#8EA9DB')
  
  # Set colors and patterns with string wrap
  cols_choice <- setNames(c('#252323', '#515151', '#767171', '#A6A6A6', '#e6e6e6', '#203764', '#305496', '#4472C4', '#8EA9DB'), 
                              c("2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025 Under Construction"))
  
  patterns_choice <- setNames(c('none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'stripe'), 
                       c("2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025 Under Construction"))
  
  # Plot
  YR %>%
    ggplot() +
    aes(Plant_Type, Cap, fill = Year_fact,pattern=Year_fact) +
    geom_col_pattern(position="dodge",size=.5, colour="black",
                     pattern_density = 0.3,
                     pattern_fill = "black",
                     pattern_colour  = NA,
                     pattern_spacing=0.01) +

    theme_bw() +

    theme(
      # General Plot Settings
      panel.grid = element_blank(),
      # (t,r,b,l) margins, adjust to show full x-axis, default: (5.5,5.5,5.5,5.5)
      plot.margin = unit(c(7, 12, 5.5, 5.5), "points"),      # Plot margins
      
      panel.background = element_blank(), # Transparent background
     # panel.border = element_blank(),
      text = element_text(size = GenText_Sz,family=Plot_Text),                # Text size
      axis.line.y = element_line(),
      axis.line.x = element_line(),
      plot.caption = element_text(hjust = 1,size = GenText_Sz-12,family=Plot_Text_it),             # Plot subtitle size (if present)
      
      # X-axis
      axis.text.x = element_text(vjust = 1,colour = "black",size = GenText_Sz+2),                 # Horizontal text
      axis.title.x = element_blank(),
      # Y-axis
      axis.title.y = element_text(size = GenText_Sz+18,family=Plot_Text_bf),           # y-axis title text size
      axis.text.y = element_text(colour = "black",size = GenText_Sz+2),                 # Horizontal text
      
      # Legend
      #legend.key.size = unit(1,"lines"),                     # Shrink legend boxes
      legend.key.width = unit(1, "lines"),
      legend.key.height = unit(1, "lines"),
      legend.position = c(0.88,0.86),                            # Move legend to the bottom
      legend.background = element_rect(fill = "transparent"),
      legend.text = element_text(size =GenText_Sz-4, lineheight = 0.8),              # Size of legend text
      legend.title=element_blank()) +                        # Remove legend title
    
    # Set axis scales
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,ymax),
                       breaks=pretty_breaks(6),
                       label=comma) +
    
    #guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    # Plot labels
    labs(x = "Year", y = "Year Start Capacity (MW)", fill = "Year",pattern= "Year",caption = "Data from AESO Annual Market Statistics Report and Long-Term Adequacy Report (Feb 2024)") +
    
    guides(fill = guide_legend(ncol = 1)) +
    guides(pattern = guide_legend(ncol = 1)) +
  
    scale_pattern_manual(values = patterns_choice,drop = FALSE) +
  
    # Legend color scheme
    scale_fill_manual(values = cols_choice,drop = FALSE) 
    
  
  # ggsave(
  #   filename = here(paste("Figures (Local)/Hist Figs NRG/","Historical Gen",".png", sep = "")),
  #   device = "png",
  #   plot = plotyr,
  #   width=6,
  #   height=3,
  #   dpi=300)
  
}

################################################################################
## FUNCTIONS: Resource_Ridge_AESO
## Plot ridgeline for capacity factor of selected resource
##
## INPUTS:
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
################################################################################
Resource_Ridge_AESO <- function(RType,MinYr) {
  
  # Bring in hitorical data
  Res_data <- df1a%>%
    filter(Plant_Type==RType,
           Day<"2023-01-01",
           Day>=paste(MinYr,"-01-01",sep=""),
           Year %in% c(2009,2011,2013,2015,2017,2019,2021,2022)) %>%
    rename(YearA=Year,
           CF=group_CF)%>%
    group_by(YearA) %>%
    mutate(Output=total_gen,
           YearA=paste(YearA))%>%
    rename(date=time)%>%
    subset(select=c(Output,CF,date,YearA))
  Res_data[is.na(Res_data)] <- 0
  
  
  # Plot max
  #PMax <- round_any(max(Res_data$CF),0.1,f=ceiling)
  PMax=1
  
  # Plot
  ggplot() +
    geom_density_ridges_gradient(data = Res_data, 
                                 aes(x = CF, y = YearA, fill = stat(x)),calc_ecdf = TRUE,vline_color="black",vline_linetype = 2,
                                 quantile_lines=TRUE, quantile_fun=function(CF,...)mean(CF),
                                 alpha = 0.8,scale=1.2) +
    scale_fill_viridis_c(option = "viridis",name = "Capacity Factor") +
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
    
    labs(x = "Hourly Capacity Factor (Output/Capacity)", y = "Frequency",caption = paste('Source: NRGStream',", Type: ",RType)) +
    
    scale_x_continuous(expand=c(0,0),limits = c(0,PMax),breaks=seq(0, PMax, by=0.2),labels = percent) 
}

################################################################################  
## FUNCTION: Eval_gas_AESO 
## Plot gas generation from NRG
##
################################################################################

Eval_gas_AESO <- function(inputdata,short_names=FALSE) {
  
Gas_stats <- melt(inputdata, id="Year") %>%
    rename("Plant_Type"=variable,
           "total_gen"=value) %>%
    mutate(Plant_Type = as.factor(Plant_Type))
  

Gas_stats$Plant_Type <- factor(Gas_stats$Plant_Type, levels=c("Gas Fired Steam","Dual Fuel",
                                                              "NGSC","NGCC","Cogen",
                                                              "NGSC BTF","NGCC BTF","Cogen BTF"
                                                              ))

if (short_names == FALSE ){
  new_names <- c("Cogeneration"="Cogen",
                 "Cogeneration (BTF)"="Cogen BTF",
                 "Simple-Cycle Natural Gas"="NGSC",
                 "Combined-Cycle Natural Gas"="NGCC",
                 "Combined-Cycle Natural Gas (BTF)"="NGCC BTF",
                 "Simple-Cycle Natural Gas (BTF)"="NGSC BTF"
                 )
  Gas_stats$Plant_Type <- fct_recode(Gas_stats$Plant_Type, !!!new_names)
  
  Col_gas_types <- c("Dual Fuel"='#e6e6e6',"Gas Fired Steam"="#2F2D41",
                     "Simple-Cycle Natural Gas"='#b4AEFC',"Combined-Cycle Natural Gas"="#515151",
                     "Cogeneration"='#A6A6A6',
                     "Simple-Cycle Natural Gas (BTF)"='#b4AEFC',"Combined-Cycle Natural Gas (BTF)"="#515151",
                     "Cogeneration (BTF)"='#A6A6A6')
  
  pat_gas_types <-c("Dual Fuel"='none',"Gas Fired Steam"='none',
                    "Simple-Cycle Natural Gas"='none',"Combined-Cycle Natural Gas"='none',
                    "Cogeneration"='none',
                    "Simple-Cycle Natural Gas (BTF)"='stripe',"Combined-Cycle Natural Gas (BTF)"='stripe',
                    "Cogeneration (BTF)"='stripe')
  }else{
    new_names <- c("Cogen (BTF)"="Cogen BTF",
                   "NGCC (BTF)"="NGCC BTF",
                   "NGSC (BTF)"="NGSC BTF"
    )
    Gas_stats$Plant_Type <- fct_recode(Gas_stats$Plant_Type, !!!new_names)
    
    Col_gas_types <- c("Dual Fuel"='#e6e6e6',"Gas Fired Steam"="#2F2D41",
                       "NGSC"='#b4AEFC',"NGCC"="#515151",
                       "Cogen"='#A6A6A6',
                       "NGSC (BTF)"='#b4AEFC',"NGCC (BTF)"="#515151",
                       "Cogen (BTF)"='#A6A6A6')
    
    
    pat_gas_types <-c("Dual Fuel"='none',"Gas Fired Steam"='none',
                      "NGSC"='none',"NGCC"='none',
                      "Cogen"='none',
                      "NGSC (BTF)"='stripe',"NGCC (BTF)"='stripe',
                      "Cogen (BTF)"='stripe')
  }


# Plot
Gas_stats %>%
    ggplot() +
    geom_area_pattern(aes(Year, total_gen, fill = Plant_Type,pattern=Plant_Type),alpha=1, size=.5, colour="black",
                      pattern_density = 0.25,
                      pattern_fill = "black",
                      pattern_colour  = NA,
                      pattern_spacing=0.016) +
    
    theme_bw() +
    
    labs(x = "Year", y = "Alberta Annual Gas Generation (TWh)", fill = "Resource",pattern="Resource",
         caption = "Data from AESO's Annual Market Statistics Report") +
    
    
    # Legend color scheme
    
    scale_fill_manual(values = Col_gas_types) +
    
    scale_pattern_manual(values = pat_gas_types) + 
    
    theme(
      # General Plot Settings
      panel.grid = element_blank(),
      # (t,r,b,l) margins, adjust to show full x-axis, default: (5.5,5.5,5.5,5.5)
      plot.margin = unit(c(6, 12, 5.5, 5.5), "points"),      # Plot margins
      panel.background = element_blank(), # Transparent background
      # panel.border = element_blank(),
      text = element_text(size = GenText_Sz,family=Plot_Text),                # Text size
      plot.title = element_text(size = GenText_Sz),              # Plot title size (if present)
      axis.line.y = element_line(),
      axis.line.x = element_line(),
      plot.caption = element_text(hjust = 1,size = GenText_Sz-12,family=Plot_Text_it),             # Plot subtitle size (if present)
      
      # X-axis
      axis.text.x = element_text(vjust = 1,colour = "black",size = GenText_Sz),                 # Horizontal text
      #axis.title.x = element_text(size = XTit_Sz),           # x-axis title text size
      axis.title.x = element_blank(),
      # Y-axis
      axis.title.y = element_text(size = GenText_Sz+12,family=Plot_Text_bf),           # y-axis title text size
      axis.text.y = element_text(colour = "black",size = GenText_Sz),                 # Horizontal text
      
      # Legend
      legend.key.size = unit(1,"lines"),                     # Shrink legend boxes
      legend.position = "right",                            # Move legend to the bottom
      legend.justification = c(0.5,0.5),                     # Center the legend
      legend.text = element_text(size =GenText_Sz-6),              # Size of legend text
      legend.title=element_blank()) +                        # Remove legend title
    
    # Set axis scales
    scale_x_continuous(expand=c(0,0),limits = c(2015,2023),breaks=seq(2015, 2023, 1)) +
    
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,70),
                       breaks=pretty_breaks(7))
  

  
}

