################################################################################
# TITLE: Net_Zero_eval
# DESCRIPTION: Functions to evaluate the electricity grid as it approaches possible net zero states

# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: July 4, 2022; LAST EDIT: July 20, 2022

################################################################################
################################################################################  
## FUNCTION: Retirecol
## Plotting the resources retired as a bar chart
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResYr - Resource Year table describing all resources start and end dates
################################################################################

# Stacked Area showing totals for Fuel Types
Retirecol <- function(case) {
  
  # Bring in Resource Year Table and filter columns
  Retdata <- ResYr%>%
    sim_filt2(.) %>% #Filter to rename fuels
    subset(., select=c(Name,Condition,YEAR,Capacity,End_Date,Run_ID,Primary_Fuel,Time_Period,Peak_Capacity)) %>%
    filter(Run_ID == case) %>%
    filter(Condition == "Average") 

  
  # Set levels to each category in order specified
  Retdata$Primary_Fuel <- factor(Retdata$Primary_Fuel, levels=c("Coal", "Cogen", "NG - NonCycling", 
                                                                "NG", "NG - Peaking", "Hydro","Solar",
                                                                "Wind", "Storage", "Other"))
  
  #Get Year max for run and filter for end dates BEFORE this date
  MaxYr <- max(Retdata$YEAR)
  Retdata$End_Date  <- as.Date(Retdata$End_Date, 
                              format = "%m/%d/%Y")
  Retdata$End_Date <- format(Retdata$End_Date,format="%Y")
  
  # Now filter data for resources that end before the study is over
  #Further filter peak capacity >0 (it is not yet retired), and end date = time period (to ensure you dont get doubles)
  Retdata <- Retdata%>%
    filter(.,End_Date <= MaxYr) %>%
    filter(.,Capacity>0) %>% 
    filter(End_Date==Time_Period)
  
  # Add a column to describe the new resources 
  Retdata$RetUnits <- 1  
  
  #Now group everything together
  Retdata <- Retdata%>%
  group_by(Primary_Fuel, End_Date) %>%
    summarise(Units = sum(RetUnits), Capacity = sum(Peak_Capacity))

  #Max Units Built
  mxu <- round_any(max(Retdata$Units),10,f=ceiling)
  mxc <- max(Retdata$Capacity)
  
  #Plot data
  ggplot(Retdata) +
    aes(x=End_Date, y=Units, fill = Primary_Fuel, group = Primary_Fuel) +
    geom_bar(position="stack", stat="identity", alpha=1) +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),  
          axis.title.x = element_text(size = XTit_Sz,hjust=0.5),
          axis.title.y = element_text(size = YTit_Sz, vjust=0),
          panel.background = element_rect(fill = "transparent"),
          plot.title = element_blank(),
          legend.justification = c(0.5,0.5),
          legend.position = ("bottom"),
          legend.title=element_blank(), 
          legend.key.size = unit(1,"lines"),
          plot.caption=element_text(size=10),
          panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          text = element_text(size = 15)) +
    
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    
    labs(x = "End Date", y = "Units Retired", fill = "Fuel Type",
         caption="Note: Retirement is classified by fuel type here")  +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,(mxu)),breaks=breaks_pretty(6)) +
    
    scale_fill_manual(values=c("Coal" =cOL_COAL, "Cogen"=cOL_COGEN, "NG - NonCycling"=cOL_Gas, 
                               "NG"=COL_Gas1, "NG - Peaking"=COL_Gas2,
                               "Hydro"=cOL_HYDRO, "Solar"=cOL_SOLAR, 
                               "Wind"=cOL_WIND, "Storage"=cOL_STORAGE,"Other"=cOL_OTHER))
    
}

################################################################################  
## FUNCTION: RetireMW 
## Plotting the resource capacity retired as a bar chart
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResYr - Resource Year table describing all resources start and end dates
################################################################################

# Stacked Area showing totals for Fuel Types
RetireMW <- function(case) {
  
  # Bring in Resource Year Table and filter columns
  Retdata <- ResYr%>%
    sim_filt2(.) %>% #Filter to rename fuels
    subset(., select=c(Name,Condition,YEAR,Capacity,End_Date,Run_ID,Primary_Fuel,Time_Period,Peak_Capacity)) %>%
    filter(Run_ID == case) %>%
    filter(Condition == "Average") 
  
  
  # Set levels to each category in order specified
  Retdata$Primary_Fuel <- factor(Retdata$Primary_Fuel, levels=c("Coal", "Cogen", "NG - NonCycling", 
                                                                "NG", "NG - Peaking", "Hydro","Solar",
                                                                "Wind", "Storage", "Other"))
  
  #Get Year max for run and filter for end dates BEFORE this date
  MaxYr <- max(Retdata$YEAR)
  MinYr <- min(Retdata$YEAR)
  Retdata$End_Date  <- as.Date(Retdata$End_Date, 
                               format = "%m/%d/%Y")
  Retdata$End_Date <- format(Retdata$End_Date,format="%Y")
  
  # Now filter data for resources that end before the study is over
  #Further filter peak capacity >0 (it is not yet retired), and end date = time period (to ensure you dont get doubles)
  Retdata <- Retdata%>%
    filter(.,End_Date <= MaxYr) %>%
    filter(.,Capacity>0) %>% 
    filter(End_Date==Time_Period)
  
  # Add a column to describe the new resources 
  Retdata$RetUnits <- 1  
  
  #Now group everything together
  Retdata <- Retdata%>%
    group_by(Primary_Fuel, End_Date) %>%
    summarise(Units = sum(RetUnits), Capacity = sum(Peak_Capacity))
  
  #Max Units Built
  dyMX <- aggregate(Retdata["Capacity"], by=Retdata["End_Date"], sum)
  mxc <- round_any(max(dyMX$Capacity+11),500,f=ceiling)
  
  Retdata$End_Date <- as.numeric(Retdata$End_Date)
  
  #Plot data
  ggplot(Retdata) +
    aes(x=End_Date, y=Capacity, fill = Primary_Fuel, group = Primary_Fuel) +
    geom_bar(position="stack", stat="identity", alpha=1) +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),  
          axis.title.x = element_text(size = XTit_Sz,hjust=0.5),
          axis.title.y = element_text(size = YTit_Sz, vjust=0),
          panel.background = element_rect(fill = "transparent"),
          plot.title = element_blank(),
          legend.justification = c(0.5,0.5),
          legend.position = ("bottom"),
          legend.title=element_blank(), 
          legend.key.size = unit(1,"lines"),
          plot.caption=element_text(size=10),
          panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          text = element_text(size = 15)) +
    
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    
    labs(x = "End Date", y = "Capacity Retired", fill = "Fuel Type",caption=SourceDB)  +
    
    scale_x_continuous(expand = c(0.01, 0.01),limits = NULL,breaks=seq(MinYr, MaxYr, by=1)) +
    
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,(mxc)),breaks=breaks_pretty(6)) +
    
    scale_fill_manual(values=c("Coal" =cOL_COAL, "Cogen"=cOL_COGEN, "NG - NonCycling"=cOL_Gas, 
                               "NG"=COL_Gas1, "NG - Peaking"=COL_Gas2,
                               "Hydro"=cOL_HYDRO, "Solar"=cOL_SOLAR, 
                               "Wind"=cOL_WIND, "Storage"=cOL_STORAGE,"Other"=cOL_OTHER))
  
}

################################################################################  
## FUNCTION: Builtcol
## Plotting the resources built as a bar chart
## This is not the best visual when partial builds are allowed! Partial builds limit 
## the capacity but not the actual number of units 
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    Build - Build table describing all new resources
################################################################################

# Stacked Area showing totals for Fuel Types
Builtcol <- function(case) {
  MaxIt <- max(Build$LT_Iteration)
  
  data <- Build %>%
    filter(Run_ID == case & LT_Iteration == MaxIt 
           &  Time_Period != "Study" )%>%
    # filter(Fuel_Type=="WND") %>%
    # filter(Units_Built>=1)
    group_by(Fuel_Type, Time_Period) %>%
    summarise(Units = sum(Units_Built), Capacity = sum(Capacity_Built)) 
  
  data$Fuel_Type <- factor(data$Fuel_Type, levels=c("Gas0","Gas1","OT", "WND", "SUN","PS"))
  
  levels(data$Fuel_Type) <- c( "CCCT gas/oil", "SCCT","Other","Wind", "Solar", "Storage")
  
  Tot <- data %>%
    group_by(Time_Period) %>%
    summarise(totu = sum(Units), totc = sum(Capacity))
  
  dyMX <- aggregate(Tot["totu"], by=Tot["Time_Period"], sum)
  mxu <- round_any(max(dyMX$totu+1),10,f=ceiling)

  ggplot(data) +
    aes(Time_Period, Units, fill = Fuel_Type, group = Fuel_Type) +
    geom_bar(position="stack", stat="identity", alpha=1) +
    theme_bw() +
    
    #geom_text(position = position_stack(vjust = 0.5),colour="black") +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),  
          axis.title.x = element_text(size = XTit_Sz,hjust=0.5),
          axis.title.y = element_text(size = YTit_Sz, vjust=0),
          panel.background = element_rect(fill = "transparent"),
          plot.title = element_text(size = Tit_Sz),
          legend.justification = c(0.5,0.5),
          legend.position = ("bottom"),
          legend.title=element_blank(), 
          legend.key.size = unit(1,"lines"),
          plot.caption=element_text(size=10),
          panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          text = element_text(size = 20)) +
    
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    
    labs(x = "Date", y = "Units Built by Aurora", fill = "Fuel Type",
         caption="Note: Units may be partially built to a certain capacity which is why numbers are not all even") +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,(mxu)),breaks=breaks_pretty(6)) +
    
    scale_fill_manual(values=c("CCCT gas/oil"=cOL_NGCC, "SCCT"=cOL_SCGT,"Other"=cOL_OTHER,
                               "Wind"=cOL_WIND,"Solar"=cOL_SOLAR, "Storage"=cOL_STORAGE))
}

################################################################################  
## FUNCTION: Build_A_MW 
## Plotting the capacity of resources built by Aurora (does not include AESO que projects)
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    Build - Build table describing all new resources
################################################################################

# Stacked Area showing totals for Fuel Types
Build_A_MW <- function(case) {
  data <- Build %>%
    filter(Run_ID == case & LT_Iteration == max(LT_Iteration) & 
             Time_Period != "Study")%>%
    group_by(Fuel_Type, Time_Period) %>%
    summarise(Units = sum(Units_Built), Capacity = sum(Capacity_Built)) 
  
  data$Fuel_Type <- factor(data$Fuel_Type, levels=c("Gas0","Gas1","OT", "WND", "SUN","PS"))
  
  levels(data$Fuel_Type) <- c( "CCCT gas/oil", "SCCT","Other","Wind", "Solar", "Storage")
  
  Tot <- data %>%
    group_by(Time_Period) %>%
    summarise(totc = sum(Capacity))
  
  dyMX <- aggregate(Tot["totc"], by=Tot["Time_Period"], sum)
  mxc <- plyr::round_any(max(abs(Tot$totc)), 1000, f = ceiling)
  
  ggplot(data) +
    aes(Time_Period, Capacity, fill = Fuel_Type, group = Fuel_Type) +
    geom_bar(position="stack", stat="identity", alpha=1) +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(), 
          axis.title.x = element_text(size = XTit_Sz,face="bold"),
          axis.title.y = element_text(size = YTit_Sz,face="bold"),
          plot.title = element_text(size = Tit_Sz),
          panel.background = element_rect(fill = "transparent"),
          legend.justification = c(0.5,0.5),
          legend.title=element_blank(),
          legend.position = ("bottom"),
          legend.key.size = unit(1,"lines"),
          panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          text = element_text(size = 20)) +
    
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    
    labs(x = "Date", y = "Capacity Built (MW)", fill = "Fuel Type") +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,mxc)) +
    #    scale_x_discrete(expand=c(0,0)) +
    scale_fill_manual(values=c("CCCT gas/oil"=cOL_NGCC, "SCCT"=cOL_SCGT,"Other"=cOL_OTHER,
                               "Wind"=cOL_WIND,"Solar"=cOL_SOLAR, "Storage"=cOL_STORAGE))
}

################################################################################  
## FUNCTION: BuildMW 
## Plotting the built capacity for ALL new resources (resource table and new resource table)
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResYr - Resource Year table describing all resources start and end dates
################################################################################

BuildMW <- function(case) 
{
  # Bring in Resource Year Table and filter columns
  Builddata <- ResSt%>%
    sim_filt3(.) %>% #Filter to rename fuels
    subset(., select=c(Name,Condition,Capacity,Peak_Capacity,End_Date,Beg_Date,Run_ID,Primary_Fuel,Capacity_Factor)) %>%
    filter(Run_ID == case) %>%
    filter(Condition == "Average") 
  
  
  # Set levels to each category in order specified
  Builddata$Primary_Fuel <- factor(Builddata$Primary_Fuel, levels=c("Coal", "Cogen", "Coal-to-Gas", "SCCT", "NGCC", "Hydro","Solar",
                                                                    "Wind", "Storage", "Other") )
  
  #Get Year max for run and filter for end dates BEFORE this date
  MaxYr <- max(ResYr$YEAR)
  MinYr <- min(ResYr$YEAR)
  
  Builddata$Beg_Date  <- as.Date(Builddata$Beg_Date, 
                                 format = "%m/%d/%Y")
  Builddata$Beg_Date <- format(Builddata$Beg_Date,format="%Y")
  
  Builddata <- Builddata%>%
    filter(.,Beg_Date <= MaxYr) %>%
    filter(.,Beg_Date >= MinYr) #%>%
   # filter(.,Capacity>0)
  
  #Now group everything together
  Builddata <- Builddata%>%
    group_by(Primary_Fuel, Beg_Date) %>%
    summarise(Capacity = sum(Capacity))
  
  #Max Units Built
  dyMX <- aggregate(Builddata["Capacity"], by=Builddata["Beg_Date"], sum)
  mxc <- round_any(max(dyMX$Capacity+11),1000,f=ceiling)
  
  Builddata$Beg_Date <- as.numeric(Builddata$Beg_Date)
  
  #Plot data
  ggplot(Builddata) +
    aes(x=Beg_Date, y=Capacity, fill = Primary_Fuel, group = Primary_Fuel) +
    geom_bar(position="stack", stat="identity", alpha=1) +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),  
          axis.title.x = element_text(size = XTit_Sz,hjust=0.5),
          axis.title.y = element_text(size = YTit_Sz, vjust=0),
          panel.background = element_rect(fill = "transparent"),
          plot.title = element_blank(),
          legend.justification = c(0.5,0.5),
          legend.position = ("bottom"),
          legend.title=element_blank(), 
          legend.key.size = unit(1,"lines"),
          plot.caption=element_text(size=10),
          panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          text = element_text(size = 15)) +
    
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    
    labs(x = "Year", y = "New Capacity (MW)", fill = "Fuel Type",caption=SourceDB)  +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,(mxc)),breaks=breaks_pretty(6)) +
    scale_x_continuous(expand = c(0.01, 0.01),limits = NULL,breaks=seq(MinYr, MaxYr, by=1)) +
    
    scale_fill_manual(values=c("Cogen"=cOL_COGEN, "Coal-to-Gas"=cOL_NGConv, 
                               "SCCT"=cOL_SCGT, "NGCC"=cOL_NGCC,
                               "Hydro"=cOL_HYDRO, "Other"=cOL_OTHER,
                               "Wind"=cOL_WIND, "Solar"=cOL_SOLAR,"Storage"=cOL_STORAGE))
  
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

  # Get chosen years
  data$Time_Period <- format(data$Time_Period,format="%Y")
  data <- data %>%
    filter(Time_Period %in% Years2Disp)
  
  # re-order teh bars for asthetics
  data$ID <- factor(data$ID,levels=c("Coal", "Import","Coal-to-Gas", "Cogen", "Natural Gas",
                       "Wind","Other","Solar", "Hydro","Storage"),ordered=TRUE)
  
  # Set the max for the plot
  MX <- plyr::round_any(max(abs(data$Output_MWH)/1000000), 10, f = ceiling)
  
    # Plot
  data %>%
    ggplot() +
    aes(Time_Period, (Output_MWH/1000000), fill = ID) +
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
    
    labs(x = "Year", y = "Annual Generation (TWh)", fill = "Resource") +
    
    guides(fill = guide_legend(nrow = 1)) +
    
    scale_fill_manual(values = colours4) 
  
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

###############################################################################  
## FUNCTION: Imp_Exp2 
## Old version of yearly total plot imp/exp
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    Import - All imports from AB
##    Export - All exports from AB
################################################################################

Imp_Exp2 <- function(case) {
  Imp <- Import %>%
    filter(Run_ID == case) %>%
    mutate(date = format(.$date, format="%Y")) %>%
    group_by(date) %>%
    summarise(Output_MWH = sum(Output_MWH)) %>%
    mutate(ID = "Import") 
  
  Exp <- Export %>%
    filter(Run_ID == case) %>%
    mutate(date = format(.$date, format="%Y")) %>%
    group_by(date) %>%
    summarise(Output_MWH = sum(Output_MWH)) %>%
    mutate(ID = "Export") 
  

  # Filters for the desired case study
  data <- rbind(Imp,Exp) 
  
  # date$date  <- as.Date(as.character(Imp$date), 
  #                             format = "%Y")
  
  # # Get chosen dates
  # data$Time_Period <- format(data$Time_Period,format="%Y")
  # data <- data %>%
  #   filter(Time_Period %in% Years2Disp)
  
  # Set the max for the plot
  MX <- plyr::round_any(max(abs(data$Output_MWH+11)/1000), 10, f = ceiling)
  
  # Plot
  data %>%
    ggplot() +
    aes(date, (Output_MWH/1000), fill = ID) +
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
    
    scale_y_continuous(expand=c(0,0),limits = c(0,3000),breaks=pretty_breaks(6)) +
    
    labs(x = "Year", y = "Total Annual Imports and Exports (GWh)", fill = "Resource") +
    
    guides(fill = guide_legend(nrow = 1)) 
}

###############################################################################  
## FUNCTION:  
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
################################################################################

# EmData <- ResGroupEmYr %>%
#   filter(Run_ID == case & Condition == "Average") %>%
#   sim_filt1(.) %>%
#   filter(Type =="CO2")
# 
# EmResData <- ResEmYr %>%
#   filter(Run_ID == case & Condition == "Average") %>%
#   sim_filt1(.) %>%
#   filter(Type =="CO2")
