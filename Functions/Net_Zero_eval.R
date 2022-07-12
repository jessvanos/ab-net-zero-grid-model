################################################################################
# TITLE: Net_Zero_eval
# DESCRIPTION: Functions to evaluate electricity grid as it approaches possible net zero states

# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: July 4, 2022; LAST EDIT: July 12, 2022

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
    subset(., select=c(Name,Condition,YEAR,Capacity,End_Date,Run_ID,Primary_Fuel,Time_Period)) %>%
    filter(Run_ID == case) %>%
    filter(Condition == "Average") %>%
    filter(Time_Period == "2035")

  
  # Set levels to each category in order specified
  Retdata$Primary_Fuel <- factor(Retdata$Primary_Fuel, levels=c("Coal", "Cogen", "NG - NonCycling", 
                                                                "NG", "NG - Peaking", "Hydro","Solar",
                                                                "Wind", "Storage", "Other"))
  
  #Get Year max for run and filter for end dates BEFORE this date
  MaxYr <- max(Retdata$YEAR)
  Retdata$End_Date  <- as.Date(Retdata$End_Date, 
                              format = "%m/%d/%Y")
  Retdata$End_Date <- format(Retdata$End_Date,format="%Y")
  Retdata <- Retdata%>%
    filter(.,End_Date <= MaxYr) 
  
  # Add a column to describe the new resources 
  Retdata$RetUnits <- 1  
  
  #Now group everything together
  Retdata <- Retdata%>%
  group_by(Primary_Fuel, End_Date) %>%
    summarise(Units = sum(RetUnits), Capacity = sum(Capacity))

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
          text = element_text(size = 20)) +
    
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    
    labs(x = "End Date", y = "Units Retired", fill = "Fuel Type")  +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,(mxu)),breaks=breaks_pretty(6)) +
    
    scale_fill_manual(values=c("Coal" =cOL_COAL, "Cogen"=cOL_COGEN, "NG - NonCycling"=cOL_Gas, 
                               "NG"=COL_Gas1, "NG - Peaking"=COL_Gas2,
                               "Hydro"=cOL_HYDRO, "Solar"=cOL_SOLAR, 
                               "Wind"=cOL_WIND, "Storage"=cOL_STORAGE,"Other"=cOL_OTHER))
    
}

################################################################################  
## FUNCTION: Builtcol
## Plotting the resources built as a bar chart
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
    group_by(Fuel_Type, Time_Period) %>%
    summarise(Units = sum(Units_Built), Capacity = sum(Capacity_Built)) 
  
  data$Fuel_Type <- factor(data$Fuel_Type, levels=c("Gas0","Gas1","OT", "WND", "SUN","PS"))
  
  levels(data$Fuel_Type) <- c( "CCCT gas/oil", "SCCT","Other","Wind", "Solar", "Storage")
  
  Tot <- data %>%
    group_by(Time_Period) %>%
    summarise(totu = sum(Units), totc = sum(Capacity))
  
  mxu <- round_any(max(Tot$totu),10,f=ceiling)
  mxc <- max(Tot$totc)
  
  ggplot(data) +
    aes(Time_Period, Units, fill = Fuel_Type, group = Fuel_Type) +
    geom_bar(position="stack", stat="identity", alpha=1) +
    theme_bw() +
    
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
    
    labs(x = "Date", y = "Units Built", fill = "Fuel Type",
         caption="Note: Units may be partially built to a certain capacity which is why numbers are not all even") +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,(mxu)),breaks=breaks_pretty(6)) +
    
    scale_fill_manual(values=c("CCCT gas/oil"=cOL_NGCC, "SCCT"=cOL_SCGT,"Other"=cOL_OTHER,
                               "Wind"=cOL_WIND, "Storage"=cOL_STORAGE,"Solar"=cOL_SOLAR))
}

################################################################################  
## FUNCTION: BuiltMW 
## Plotting the capacity of resources built
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    Build - Build table describing all new resources
################################################################################

# Stacked Area showing totals for Fuel Types
BuiltMW <- function(case) {
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
                               "Wind"=cOL_WIND, "Storage"=cOL_STORAGE,"Solar"=cOL_SOLAR))
}

################################################################################  
## FUNCTION: Output_Comp 
## Plotting the capacity of resources built
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    Build - Build table describing all new resources
################################################################################

Output_Comp <- function(input,case) {
  Imp <- Import %>%
    filter(Run_ID == case) %>%
    mutate(Time_Period = format(.$date, format="%Y")) %>%
    group_by(Time_Period) %>%
    summarise(Output_MWH = sum(Output_MWH)) %>%
    mutate(ID = "Import") 
  
  Imp$Time_Period  <- as.Date(as.character(Imp$Time_Period), 
                              format = "%Y")
  
  # Filters for the desired case study
  data <- input %>%
    filter(Run_ID == case & Condition == "Average") %>%
    select(ID, Time_Period, Output_MWH) %>%
    sim_filt(.) %>%
    rbind(.,Imp) 
  
  data$ID<-fct_relevel(data$ID, "Import")

  # Get chosen dates
  data$Time_Period <- format(data$Time_Period,format="%Y")
  data <- data %>%
    filter(Time_Period %in% Years2Disp)
  
  # Set the max for the plot
  MX <- plyr::round_any(max(abs(data$Output_MWH)/1000000), 10, f = ceiling)
  
 # data$Time_Period <- toString(data$Time_Period)
  
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

###############################################################################
### ISSUES UNDER HERE 
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
RetirecMW <- function(case) {
  
  # Bring in Resource Year Table and filter columns
  Retdata <- ResSt%>%
    sim_filt2(.) %>% #Filter to rename fuels
    subset(., select=c(Name,Condition,Capacity,End_Date,Run_ID,Primary_Fuel,Time_Period)) %>%
    filter(Run_ID == case) %>%
    filter(Condition == "Average") 
  
  
  # Set levels to each category in order specified
  Retdata$Primary_Fuel <- factor(Retdata$Primary_Fuel, levels=c("Coal", "Cogen", "Gas", "Gas1", "Gas2", "Hydro","Solar",
                                                                "Wind", "Storage", "Other"))
  
  #Get Year max for run and filter for end dates BEFORE this date
  MaxYr <- max(ResYr$YEAR)
  Retdata$End_Date  <- as.Date(Retdata$End_Date, 
                               format = "%m/%d/%Y")
  Retdata$End_Date <- format(Retdata$End_Date,format="%Y")
  Retdata <- Retdata%>%
    filter(.,End_Date <= MaxYr) 
  
  #Now group everything together
  Retdata <- Retdata%>%
    group_by(Primary_Fuel, End_Date) %>%
    summarise(Capacity = sum(Capacity))
  
  #Max Units Built
  mxc <- round_any(max(Retdata$Capacity),10,f=ceiling)
  
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
          text = element_text(size = 20)) +
    
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    
    labs(x = "Year", y = "Capacity Retired", fill = "Fuel Type")  +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,(mxc)),breaks=breaks_pretty(6)) +
    
    scale_fill_manual(values=c("Coal" =cOL_COAL, "Cogen"=cOL_COGEN, "Gas"=cOL_Gas, 
                               "Gas1"=COL_Gas1, "Gas2"=COL_Gas2,
                               "Hydro"=cOL_HYDRO, "Solar"=cOL_SOLAR, 
                               "Wind"=cOL_WIND, "Storage"=cOL_STORAGE,"Other"=cOL_OTHER))
  
}


 #################################################################################
# NEW TEST
Buildcol <- function(case) 
 {
  # Bring in Resource Year Table and filter columns
  Builddata <- ResSt%>%
    sim_filt2(.) %>% #Filter to rename fuels
    subset(., select=c(Name,Condition,Capacity,End_Date,Beg_Date,Run_ID,Primary_Fuel)) %>%
    filter(Run_ID == case) %>%
    filter(Condition == "Average") 
  
  
  # Set levels to each category in order specified
  Builddata$Primary_Fuel <- factor(Builddata$Primary_Fuel, levels=c("Coal", "Cogen", "Gas", "Gas1", "Gas2", "Hydro","Solar",
                                                                "Wind", "Storage", "Other"))
  
  #Get Year max for run and filter for end dates BEFORE this date
  MaxYr <- max(ResYr$YEAR)
  MinYr <- min(ResYr$YEAR)
  
  Builddata$Beg_Date  <- as.Date(Builddata$Beg_Date, 
                               format = "%m/%d/%Y")
  Builddata$Beg_Date <- format(Builddata$Beg_Date,format="%Y")
  
  Builddata <- Builddata%>%
    filter(.,Beg_Date <= MaxYr) %>%
    filter(.,Beg_Date >= MinYr)
  
  # Add a column to describe the new resources 
  Builddata$RetUnits <- 1  
  
  #Now group everything together
  Builddata <- Builddata%>%
    group_by(Primary_Fuel, Beg_Date) %>%
    summarise(Units = sum(RetUnits), Capacity = sum(Capacity))
  
  #Max Units Built
  mxu <- round_any(max(Builddata$Units),20,f=ceiling)
  mxc <- max(Builddata$Capacity)
  
  Builddata$Beg_Date <- as.numeric(Builddata$Beg_Date)
  
  #Plot data
  ggplot(Builddata) +
    aes(x=Beg_Date, y=Units, fill = Primary_Fuel, group = Primary_Fuel) +
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
    
    labs(x = "Year", y = "Units Built", fill = "Fuel Type")  +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,(mxu)),breaks=breaks_pretty(6)) +
    scale_x_continuous(expand = c(0.01, 0.01),limits = NULL,breaks=seq(MinYr, MaxYr, by=1)) +
    
    scale_fill_manual(values=c("Coal" =cOL_COAL, "Cogen"=cOL_COGEN, "Gas"=cOL_Gas, 
                               "Gas1"=COL_Gas1, "Gas2"=COL_Gas2,
                               "Hydro"=cOL_HYDRO, "Solar"=cOL_SOLAR, 
                               "Wind"=cOL_WIND, "Storage"=cOL_STORAGE,"Other"=cOL_OTHER))
  
}

#####
BuildMW <- function(case) 
{
  # Bring in Resource Year Table and filter columns
  Builddata <- ResSt%>%
    sim_filt2(.) %>% #Filter to rename fuels
    subset(., select=c(Name,Condition,Capacity,End_Date,Beg_Date,Run_ID,Primary_Fuel)) %>%
    filter(Run_ID == case) %>%
    filter(Condition == "Average") 
  
  
  # Set levels to each category in order specified
  Builddata$Primary_Fuel <- factor(Builddata$Primary_Fuel, levels=c("Coal", "Cogen", "Gas", "Gas1", "Gas2", "Hydro","Solar",
                                                                    "Wind", "Storage", "Other"))
  
  #Get Year max for run and filter for end dates BEFORE this date
  MaxYr <- max(ResYr$YEAR)
  MinYr <- min(ResYr$YEAR)
  
  Builddata$Beg_Date  <- as.Date(Builddata$Beg_Date, 
                                 format = "%m/%d/%Y")
  Builddata$Beg_Date <- format(Builddata$Beg_Date,format="%Y")
  
  Builddata <- Builddata%>%
    filter(.,Beg_Date <= MaxYr) %>%
    filter(.,Beg_Date >= MinYr)
  
  #Now group everything together
  Builddata <- Builddata%>%
    group_by(Primary_Fuel, Beg_Date) %>%
    summarise(Capacity = sum(Capacity))
  
  #Max Units Built
  mxc <- round_any(max(Builddata$Capacity),100,f=ceiling)
  
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
    
    labs(x = "Year", y = "Capacity Built (MW)", fill = "Fuel Type")  +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,(mxc)),breaks=breaks_pretty(6)) +
    scale_x_continuous(expand = c(0.01, 0.01),limits = NULL,breaks=seq(MinYr, MaxYr, by=1)) +
    
    scale_fill_manual(values=c("Coal" =cOL_COAL, "Cogen"=cOL_COGEN, "Gas"=cOL_Gas, 
                               "Gas1"=COL_Gas1, "Gas2"=COL_Gas2,
                               "Hydro"=cOL_HYDRO, "Solar"=cOL_SOLAR, 
                               "Wind"=cOL_WIND, "Storage"=cOL_STORAGE,"Other"=cOL_OTHER))
  
}