################################################################################
# TITLE: Build_Retire_Functions
# DESCRIPTION: Functions to evaluate the electricity grid as it approaches possible net zero states

# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: July 4, 2022; LAST EDIT: January 6, 2023

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
  Retdata$Primary_Fuel <- factor(Retdata$Primary_Fuel, levels=c("Coal-to-Gas", "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                                                "Blended  Simple Cycle","Blended  Combined Cycle",
                                                                "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                                                                "Hydro", "Other",
                                                                "Wind", "Solar", "Storage","Coal", "Cogeneration"))
  
  #Get Year max for run and filter for end dates BEFORE this date
  # Filter to remove the final 5 years (as per AURORA, want to run 5 years past year of interest)
  MaxYr <- max(Retdata$YEAR)-5
  Retdata$End_Date  <- as.Date(Retdata$End_Date, 
                              format = "%m/%d/%Y")
  Retdata$End_Date <- format(Retdata$End_Date,format="%Y")
  
  # Replace the capacity with the peak / actual capacity and not just what is available
  for (i in 1:length(Retdata$Capacity)) {
    if (Retdata[i,"Capacity"]<Retdata[i,"Peak_Capacity"]) {
      Retdata[i,"Capacity"] <- Retdata[i,"Peak_Capacity"]
    }
  }
  
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
    geom_bar(position="stack", stat="identity", alpha=0.7,color="black") +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),  
          axis.title.x = element_text(size = XTit_Sz,hjust=0.5),
          axis.title.y = element_text(size = YTit_Sz, vjust=0),
          panel.background = element_rect(fill = "transparent"),
          plot.title = element_blank(),
          legend.justification = c(0.5,0.5),
          legend.position = ("right"),
          legend.title=element_blank(), 
          legend.key.size = unit(1,"lines"),
          plot.caption=element_text(size=10),
          panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          text = element_text(size = 15)) +
    
    #guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    
    labs(x = "End Date", y = "Units Retired", fill = "Fuel Type",
         caption="Note: Retirement is classified by fuel type here")  +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,(mxu)),breaks=breaks_pretty(6)) +
    
    scale_fill_manual(values=colours3)
    
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
    filter(Condition == "Average") %>%
    mutate(Time_Period=as.numeric(Time_Period))
  
  
  # Set levels to each category in order specified
  Retdata$Primary_Fuel <- factor(Retdata$Primary_Fuel, levels=c("Coal-to-Gas", "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                                                "Blended  Simple Cycle","Blended  Combined Cycle",
                                                                "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                                                                "Hydro", "Other",
                                                                "Wind", "Solar", "Storage","Coal", "Cogeneration"))
  
  #Get Year max for run and filter for end dates BEFORE this date
  # Filter to remove the final 5 years (as per AURORA, want to run 5 years past year of interest)
  MaxYr <- max(Retdata$Time_Period)-5
  MinYr <- min(Retdata$Time_Period)
  
  Retdata$End_Date  <- as.Date(Retdata$End_Date, 
                               format = "%m/%d/%Y")
  Retdata$End_Date <- format(Retdata$End_Date,format="%Y")
  
  # Replace the capacity with the peak / actual capacity and not just what is available
  for (i in 1:length(Retdata$Capacity)) {
    if (Retdata[i,"Capacity"]<Retdata[i,"Peak_Capacity"]) {
      Retdata[i,"Capacity"] <- Retdata[i,"Peak_Capacity"]
    }
  }
  
  # Now filter data for resources that end before the study is over
  #Further filter end date = time period (to ensure you don't get doubles)
  Retdata <- Retdata%>%
    filter(.,End_Date <= MaxYr) %>%
    filter(End_Date==Time_Period)
  
  # Pull out the names of retired units
  RetiredUnits <- Retdata[,c("Name","Capacity","Primary_Fuel","End_Date")]
  RetiredUnits <- RetiredUnits[order(RetiredUnits$End_Date),]
  print(RetiredUnits)
  
  # Add a column to describe the new resources 
  Retdata$RetUnits <- 1  
  
  #Now group everything together
  Retdata2 <- Retdata%>%
    group_by(Primary_Fuel, End_Date) %>%
    summarise(Units = sum(RetUnits), Capacity = sum(Capacity)) %>%
    mutate(Capacity=Capacity*-1)
  
  #Max Units Built
  dyMX <- aggregate(Retdata2["Capacity"], by=Retdata2["End_Date"], sum)
  mxc <- round_any(min(dyMX$Capacity+11),500,f=ceiling)
  
  Retdata2$End_Date <- as.numeric(Retdata2$End_Date)
  
  #Plot data
  ggplot(Retdata2) +
    aes(x=End_Date, y=Capacity, fill = Primary_Fuel, group = Primary_Fuel) +
    geom_bar(position="stack", stat="identity", alpha=0.7,color='black') +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),  
          axis.title.x = element_text(size = XTit_Sz,hjust=0.5),
          axis.title.y = element_text(size = YTit_Sz, vjust=0),
          panel.background = element_rect(fill = "transparent"),
          plot.title = element_blank(),
          legend.justification = c(0.5,0.5),
          legend.position = ("right"),
          legend.title=element_blank(), 
          legend.key.size = unit(1,"lines"),
          plot.caption=element_text(size=10),
          #panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          text = element_text(size = 15)) +
    
    #guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
    
    labs(x = "End Date", y = "Capacity Retired", fill = "Fuel Type",caption=SourceDB)  +
    
    scale_x_continuous(expand = c(0.05, 0.05),limits=c(as.numeric(MinYr), as.numeric(MaxYr)),
                       breaks=seq(MinYr, MaxYr, by=1),position = "top") +
    
    scale_y_continuous(expand=c(0,0),
                       limits = c((mxc),0),breaks=breaks_pretty(5)) +
    
    scale_fill_manual(values=colours3,drop = FALSE)
  
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

# Stacked Bars showing totals for Fuel Types
Build_A_MW <- function(case) {
  
  data <- Build %>%
    filter(Run_ID == case & LT_Iteration == max(LT_Iteration) & 
             Time_Period != "Study")%>%
    group_by(Fuel_Type, Time_Period) %>%
    summarise(Units = sum(Units_Built), Capacity = sum(Capacity_Built)) %>%
    sim_filt4(.)

  # levels(data$Fuel_Type) <- c("Hydrogen","Natual Gas and Hydrogen Blend","Natural Gas", 
  #                             "Hydro", "Other",
  #                             "Wind", "Solar", 
  #                             "Storage - Battery", "Storage - Compressed Air", "Storage - Pumped Hydro")
  
  Tot <- data %>%
    group_by(Time_Period) %>%
    summarise(totc = sum(Capacity)) %>%
    ungroup()
  
  dyMX <- aggregate(Tot["totc"], by=Tot["Time_Period"], sum)
  mxc <- plyr::round_any(max(abs(Tot$totc)), 1000, f = ceiling)
  
  ggplot(data) +
    aes(Time_Period, Capacity, fill = Fuel_Type, group = Fuel_Type) +
    geom_bar(position="stack", stat="identity", alpha=0.6,color='black') +
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
          #panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          text = element_text(size = 20)) +
    
    guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
    
    labs(x = "Date", y = "Capacity Built (MW)", fill = "Fuel Type") +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,mxc)) +
    #    scale_x_discrete(expand=c(0,0)) +
    scale_fill_manual(values=colours5,drop = FALSE)
  
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
  data <- ResYr%>%
    sim_filt3(.) %>% #Filter to rename fuels
    subset(., select=c(Name,Condition,Capacity,Peak_Capacity,End_Date,Beg_Date,Run_ID,Primary_Fuel,Time_Period,Capacity_Factor)) %>%
    filter(Run_ID == case) %>%
    filter(Condition == "Average") %>%
    mutate(Time_Period=as.numeric(Time_Period))
  
  
  # Set levels to each category in order specified
  data$Primary_Fuel <- factor(data$Primary_Fuel, levels=c("Coal-to-Gas", "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                                          "Blended  Simple Cycle","Blended  Combined Cycle",
                                                          "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                                                          "Hydro", "Other",
                                                          "Wind", "Solar", 
                                                          "Storage - Battery", "Storage - Compressed Air", "Storage - Pumped Hydro", 
                                                          "Cogeneration") )
 
   # Replace the capacity with the peak / actual capacity and not just what is available
  for (i in 1:length(data$Capacity)) {
    if (data[i,"Capacity"] < data[i,"Peak_Capacity"]) {
      data[i,"Capacity"] <- data[i,"Peak_Capacity"]
    }
  }
  
  # Get Year max for run and filter for end dates BEFORE this date
  MaxYr <- max(data$Time_Period)-5
  MinYr <- min(data$Time_Period)
  
  # Give start date so that already built are not listed as new additions
  CurDate <- as.Date("01/01/2022", 
                     format = "%m/%d/%Y")
  data$FiltDate  <- as.Date(data$Beg_Date, 
                            format = "%m/%d/%Y")
  
  # Format start date as year
  data$Beg_Date  <- as.Date(data$Beg_Date, 
                                 format = "%m/%d/%Y")
  data$Beg_Date <- format(data$Beg_Date,format="%Y")
  
  #Filter
  Builddata <- data %>%
    filter(.,Beg_Date <= MaxYr) %>%
    filter(.,FiltDate > CurDate) %>%
    filter(.,Capacity>0) %>%
    filter(Beg_Date==Time_Period) %>%
    select(., c("Name","Capacity","Primary_Fuel","Beg_Date"))

  # Add cap increases manual
  Capinc<-data.frame(Name=c("Base Plant (SCR1)"),
                     Capacity=c(800),
                     Primary_Fuel=c("Cogeneration"),
                     Beg_Date=c(2024))

  Builddata <-  rbind(Builddata,Capinc)
  
  
    
  # Pull out the names of built units
  BuiltUnits <- Builddata[order(Builddata$Beg_Date),]
  print(BuiltUnits)
  
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
    geom_bar(position="stack", stat="identity", alpha=0.7,color='black') +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),  
          axis.title.x = element_text(size = XTit_Sz,hjust=0.5),
          axis.title.y = element_text(size = YTit_Sz, vjust=0),
          panel.background = element_rect(fill = "transparent"),
          plot.title = element_blank(),
          legend.justification = c(0.5,0.5),
          legend.position = ("right"),
          legend.title=element_blank(), 
          legend.key.size = unit(1,"lines"),
          plot.caption=element_text(size=10),
          #panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          text = element_text(size = 15)) +
    
    #guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
    
    labs(x = "Year", y = "New Capacity (MW)", fill = "Fuel Type",caption=SourceDB)  +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,(mxc)),breaks=breaks_pretty(6)) +
    scale_x_continuous(expand = c(0.01, 0.01),limits = NULL,breaks=seq(MinYr, MaxYr, by=1)) +
    
    scale_fill_manual(values=colours5,drop = FALSE)
  
  # BuiltUnitsList <- list(BuiltUnits)
  # return(BuiltUnitsList)
}

################################################################################  
## FUNCTION: Eval_diffcap (original author: Taylor Pawlenchuk)
## The first year of data does not have a prior capacity to compare to, so it is not used.
##
## INPUTS: 
##    input - ResGroupYear
## TABLES REQUIRED: 
##    ResGroupYear -Yearly resoruce group emissions
################################################################################

# **NOTE: Issue with first year - need to reformat?**

Eval_diffcap <- function(case) {
  
  # Filters for the desired case study
  data <- ResGroupYr %>%
    filter(Run_ID == case & Condition == "Average") %>%
    subset(.,select=c(ID, Time_Period, Capacity)) %>%
    sim_filt5(.) %>%
    group_by(ID) %>%
    arrange(Time_Period) %>%
    mutate(diff = Capacity - lag(Capacity, default = first(Capacity)))
  
  # data<-data %>%
  #   filter(Time_Period<=MaxYr)
  
  data$Time_Period <- as.factor(format(data$Time_Period, format="%Y"))

  # Sum all up
  Tot <- data %>%
    group_by(Time_Period) %>%
    summarise(maxy = sum(diff[which(diff>0)]), miny = sum(diff[which(diff<0)]))
  
  # Capacity limits for plot
  mny <- plyr::round_any(min(Tot$miny),1000, f=floor)
  mxy <- plyr::round_any(max(Tot$maxy),1000, f=ceiling)
  
  # Year limits for plot. Add 365 to get the year after the first
  mnx <- format(min(ResGroupMn$Time_Period)+365, format="%Y")
  mxx <- format(max(ResGroupMn$Time_Period)-365*5, format="%Y")
  
  # Plot it all
  data %>%
    ggplot() +
    aes(Time_Period, (diff), fill = ID) +
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
}

################################################################################  
## FUNCTION: Units
## Unit specific bar chart showing builds by unit for certain resource type.
## ex: "WND"
##
## INPUTS: 
##    case - Run_ID which you want to plot
##    Fuel - Fuel type matching Fuel_ID 
## TABLES REQUIRED: 
##    Build - Build table describing all new resources
################################################################################

Units <- function(case, Fuel) {
  
  data <- Build %>%
    filter(Run_ID == case & LT_Iteration == max(LT_Iteration) & 
             Time_Period == "Study" & Fuel_Type == Fuel) 
  
  data %>%
    ggplot() +
    aes(Name, Units_Built,fill = Fuel_Type) + 
    geom_col() +
    labs(x = "Plant Name", y = "Units Built") +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,(max(data$Units_Built)+1))) +
    
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.x = element_text(size = XTit_Sz,face="bold"),
          axis.title.y = element_text(size = YTit_Sz,face="bold"),
          plot.title = element_text(size = Tit_Sz),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent"),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.key.size = unit(1,"lines"), 
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          text = element_text(size = 15),
          panel.border = element_rect(colour = "black", fill = "transparent"), 
          panel.grid.major.y = element_line(size=0.25,linetype=5,color = "gray36")) +
    
    
    scale_fill_manual(values="gray") +
    
    theme(text=element_text(family=Plot_Text))
}

################################################################################  
## FUNCTION: Slack
## Unit specific bar chart showing units not built (or available) for certain resource type.
##
## INPUTS: 
##    case - Run_ID which you want to plot
##    Fuel - Fuel type matching Fuel_ID 
## TABLES REQUIRED: 
##    Build - Build table describing all new resources
################################################################################

Slack <- function(case, Fuel) {
  data <- Build %>%
    filter(Run_ID == case & LT_Iteration == max(LT_Iteration) & 
             Time_Period == "Study" & Fuel_Type == Fuel) 
  
  data %>%
    ggplot() +
    aes(Name, Max_Limit_Slack,fill = Fuel_Type) + 
    geom_col() +
    labs(x = "Plant Name", y = "Units Available") +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,(max(data$Max_Limit_Slack)+1))) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.x = element_text(size = XTit_Sz,face="bold"),
          axis.title.y = element_text(size = YTit_Sz,face="bold"),
          plot.title = element_text(size = Tit_Sz),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.key.size = unit(1,"lines"), 
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          text = element_text(size = 15),
          panel.border = element_rect(colour = "black", fill = "transparent"), 
          panel.grid.major.y = element_line(size=0.25,linetype=5,color = "gray36")) +  
    
    scale_fill_manual(values="gray") +
    
    theme(text=element_text(family=Plot_Text))
}

################################################################################
## FUNCTION: BuildUnits
## Show units built compared to available ones for a resource type
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
################################################################################  

BuildUnits <- function(case, Fuel) {
  p1 <- Units(case,Fuel)+
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          legend.position ="none")
  
  p2 <- Slack(case,Fuel)  +
    theme(legend.position ="none")
  
  p3 <- plot_grid(p1, p2, ncol = 1, align="v", axis = "l", rel_heights = c(1,1))
  
  ggdraw(add_sub(p3,paste("Simulation: ",SourceDB,"; ", "Fuel Type:",Fuel, sep = "")))
}

