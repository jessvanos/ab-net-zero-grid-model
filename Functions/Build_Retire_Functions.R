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
    subset(., select=c(Name,Condition,YEAR,Capacity,End_Date,Run_ID,Primary_Fuel,Time_Period,Nameplate_Capacity)) %>%
    filter(Run_ID == case) %>%
    filter(Condition == "Average") 

  
  # Set levels to each category in order specified
  Retdata$Primary_Fuel <- factor(Retdata$Primary_Fuel, levels=c("Solar","Wind","Hydro", "Other",
                                                                "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                                                "Blended  Simple Cycle","Blended  Combined Cycle",
                                                                "Natural Gas Combined Cycle + CCS","Natural Gas Simple Cycle", "Natural Gas Combined Cycle", 
                                                                "Coal-to-Gas","Coal", "Cogeneration", "Storage"))
  
  #Get Year max for run and filter for end dates BEFORE this date
  # Filter to remove the final 5 years (as per AURORA, want to run 5 years past year of interest)
  MaxYr <- max(as.numeric(Retdata$YEAR))-5
  Retdata$End_Date  <- as.Date(Retdata$End_Date, 
                              format = "%m/%d/%Y")
  Retdata$End_Date <- format(Retdata$End_Date,format="%Y")
  
  # Replace the capacity with the peak / actual capacity and not just what is available
  # for (i in 1:length(Retdata$Capacity)) {
  #   if (Retdata[i,"Capacity"]<Retdata[i,"Peak_Capacity"]) {
  #     Retdata[i,"Capacity"] <- Retdata[i,"Peak_Capacity"]
  #   }
  # }
  
  # Now filter data for resources that end before the study is over
  #Further filter peak capacity >0 (it is not yet retired), and end date = time period (to ensure you dont get doubles)
  Retdata <- Retdata%>%
    filter(.,End_Date <= MaxYr) %>%
    filter(.,Nameplate_Capacity>0) %>% 
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
    geom_bar(position="stack", stat="identity", alpha=Plot_Trans,color="black") +
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
    sim_filt3(.) %>% #Filter to rename fuels
    subset(., select=c(Name,Condition,YEAR,Capacity,End_Date,Run_ID,Primary_Fuel,Time_Period,Nameplate_Capacity)) %>%
    filter(Run_ID == case) %>%
    filter(Condition == "Average") %>%
    mutate(Time_Period=as.numeric(Time_Period))
  
  
  # Set levels to each category in order specified
  Retdata$Primary_Fuel <- factor(Retdata$Primary_Fuel, levels=c("Cogeneration","Coal-to-Gas", "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                                                #"Blended  Simple Cycle","Blended  Combined Cycle",
                                                                "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS",
                                                                "Natural Gas Combined Cycle CCS Retrofit","Natural Gas Combined Cycle", 
                                                                "Hydro", "Other",
                                                                "Wind", "Solar", 
                                                                "Storage - Battery", "Storage - Compressed Air", "Storage - Pumped Hydro",
                                                                "Nuclear" ) )
  
  #Get Year max for run and filter for end dates BEFORE this date
  # Filter to remove the final 5 years (as per AURORA, want to run 5 years past year of interest)
  MaxYr <- MaxYrStudy
  MinYr <- 2022
  
  Retdata$End_Date  <- as.Date(Retdata$End_Date, 
                               format = "%m/%d/%Y")
  Retdata$End_Date <- format(Retdata$End_Date,format="%Y")
  
  # Now filter data for resources that end before the study is over
  #Further filter end date = time period (to ensure you don't get doubles)
  Retdata <- Retdata%>%
    filter(.,End_Date <= MaxYr) %>%
    group_by(Name)%>%
    mutate(maxcap=max(Nameplate_Capacity))%>%
    ungroup()%>%
    filter(End_Date==Time_Period)
  
  # Pull out the names of retired units
  RetiredUnits <- Retdata[,c("Name","maxcap","Primary_Fuel","End_Date")]
  RetiredUnits <- RetiredUnits[order(RetiredUnits$End_Date),]
  print(RetiredUnits)
  
  # Add a column to describe the new resources 
  Retdata$RetUnits <- 1  
  
  #Now group everything together
  Retdata2 <- Retdata%>%
    group_by(Primary_Fuel, End_Date) %>%
    summarise(Units = sum(RetUnits), Capacity = sum(maxcap)) %>%
    mutate(Capacity=Capacity*-1)
  
  #Max Units Built
  dyMX <- aggregate(Retdata2["Capacity"], by=Retdata2["End_Date"], sum)
  mxc <- round_any(min(dyMX$Capacity+11),500,f=floor)
  
  Retdata2$End_Date <- as.numeric(Retdata2$End_Date)
  
  #Plot data
  ggplot(Retdata2,aes(x=End_Date, y=Capacity, fill = Primary_Fuel)) +
   
    # Plot added
   geom_bar_pattern(aes(pattern = Primary_Fuel),
                     position="stack", stat="identity",
                     na.rm=TRUE, alpha=Plot_Trans,color='black',
                     pattern_density = 0.3,
                     pattern_fill = "black",
                     pattern_colour  = NA,
                     pattern_spacing=0.01) +
    
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),
          axis.text = element_text(color="black"),
          #axis.title.x = element_text(size = XTit_Sz,hjust=0.5),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45,color="black",
                                     vjust = 0, hjust = 0),
          axis.title.y = element_text(size = GenText_Sz+6, vjust=0),
          panel.background = element_rect(fill = "transparent"),
          plot.title = element_blank(),
          legend.justification = c(0.5,0.5),
          legend.position = ("right"),
          legend.title=element_blank(), 
          legend.key.size = unit(1,"lines"),
          plot.caption=element_text(size=30),
          #panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          text = element_text(size = GenText_Sz)) +
    
    #guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
    
    labs(x = "End Date", y = "Capacity Retired", fill = "Primary_Fuel",pattern="Primary_Fuel",caption=SourceDB)  +
    
    scale_x_continuous(expand = c(0, 0),
                       limits = c(MinYr-1,MaxYr+1),
                       breaks=seq(MinYr, MaxYr, by=1),position = "top") +
    
    scale_y_continuous(expand=c(0,0),
                       limits = c((mxc),0),breaks=breaks_pretty(5),labels=comma) +
    
    scale_fill_manual(values=colours5,drop = FALSE) +
    
    scale_pattern_manual(values=Patterns5,drop = FALSE)
  
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
    filter(Run_ID == case,
           LT_Iteration == 0,
           Time_Period != "Study",
           Units_Built>0) %>%
    mutate(YEAR=as.numeric(Time_Period))%>%
    group_by(Fuel_Type, Time_Period,YEAR) %>%
    summarise(Units = sum(Units_Built), Capacity = sum(Capacity_Built)) %>%
    sim_filt4(.)

   levels(data$Fuel_Type) <- c("Hydrogen","Natual Gas and Hydrogen Blend","Natural Gas", "Natural Gas + CCS",
                               "Hydro", "Other","Wind", "Solar","Storage","Nuclear")
  
  YrMN <-min(data$YEAR)
  YrMX <-MaxYrStudy
   
  Tot <- data %>%
    group_by(Time_Period) %>%
    summarise(totc = sum(Capacity)) %>%
    ungroup()
  
  dyMX <- aggregate(Tot["totc"], by=Tot["Time_Period"], sum)
  mxc <- plyr::round_any(max(abs(Tot$totc)), 1000, f = ceiling)
  
  ggplot(data) +
    aes(YEAR, Capacity, fill = Fuel_Type, group = Fuel_Type) +
    geom_bar(position="stack", stat="identity", alpha=Plot_Trans,color='black') +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(), 
          axis.title.x = element_blank(),
          axis.text.y=element_text(color="black"),
          axis.text.x=element_text(angle=45,vjust=0.5,color="black"),
          axis.title.y = element_text(size = YTit_Sz,face="bold"),
          plot.title = element_text(size = Tit_Sz),
          panel.background = element_rect(fill = "transparent"),
          legend.justification = c(0.5,0.5),
          legend.title=element_blank(),
          legend.position = ("bottom"),
          legend.key.size = unit(1,"lines"),
          #panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          text = element_text(size = 20)) +
    
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    
    labs(x = "Date", y = "Capacity Built (MW)", fill = "Fuel Type") +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,mxc),label=comma) +
    scale_x_continuous(expand=c(0,0),
                       limits = c(YrMN-1,YrMX+1),breaks=seq(YrMN,YrMX,by=1)) +
    
    scale_fill_manual(values=colours2,drop = FALSE) 
  
  
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
    subset(., select=c(Name,Condition,Capacity,Nameplate_Capacity,End_Date,Beg_Date,Run_ID,Primary_Fuel,Time_Period,Capacity_Factor)) %>%
    filter(Run_ID == case) %>%
    filter(Condition == "Average") %>%
    mutate(Time_Period=as.numeric(Time_Period))
  
  
  # Set levels to each category in order specified
  data$Primary_Fuel <- factor(data$Primary_Fuel, levels=c("Cogeneration","Coal-to-Gas", "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                                          #"Blended  Simple Cycle","Blended  Combined Cycle",
                                                          "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS",
                                                          "Natural Gas Combined Cycle CCS Retrofit","Natural Gas Combined Cycle", 
                                                          "Hydro", "Other",
                                                          "Wind", "Solar", 
                                                          "Storage - Battery", "Storage - Compressed Air", "Storage - Pumped Hydro",
                                                          "Nuclear"
                                                          ) )
  
  # Get Year max for run and filter for end dates BEFORE this date
  MaxYr <- MaxYrStudy
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
    group_by(Name)%>%
    mutate(maxcap=max(Nameplate_Capacity))%>%
    ungroup()%>%
    filter(Beg_Date==Time_Period) %>%
    select(., c("Name","maxcap","Primary_Fuel","Beg_Date"))

  # Add cap increases manual - this change is in capacity of plant, not a new build
  Capinc<-data.frame(Name=c("Base Plant (SCR1)"),
                     maxcap=c(800),
                     Primary_Fuel=c("Cogeneration"),
                     Beg_Date=c(2024))

  Builddata <-  rbind(Builddata,Capinc)
  
  # Pull out the names of built units
  BuiltUnits <- Builddata[order(Builddata$Beg_Date),]
  print(BuiltUnits)
  
  #Now group everything together
  Builddata <- Builddata%>%
    group_by(Primary_Fuel, Beg_Date) %>%
    summarise(Capacity = sum(maxcap))
  
  #Max Units Built
  dyMX <- aggregate(Builddata["Capacity"], by=Builddata["Beg_Date"], sum)
  mxc <- round_any(max(dyMX$Capacity+11),500,f=ceiling)
  
  Builddata$Beg_Date <- as.numeric(Builddata$Beg_Date)
  
  #Plot data
  ggplot(Builddata,aes(x=Beg_Date, y=Capacity, fill = Primary_Fuel)) +
    geom_bar_pattern(aes(pattern = Primary_Fuel),
                       position="stack", stat="identity",
                       na.rm=TRUE, alpha=Plot_Trans,color='black',
                       pattern_density = 0.3,
                       pattern_fill = "black",
                       pattern_colour  = NA,
                       pattern_spacing=0.01) +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),  
          #axis.title.x = element_text(size = XTit_Sz,hjust=0.5),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = GenText_Sz+6, vjust=0),
          panel.background = element_rect(fill = "transparent"),
          axis.text.x=element_text(angle=45,vjust = 0.5, hjust = 0.5,color="black"),
          axis.text.y=element_text(color="black"),
          plot.title = element_blank(),
          legend.justification = c(0.5,0.5),
          legend.position = ("right"),
          legend.text = element_text(size = GenText_Sz-6),
          legend.title=element_blank(), 
          legend.key.size = unit(1,"lines"),
          plot.caption=element_text(size=30),
          #panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          text = element_text(size = GenText_Sz)) +
    
    #guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
    
    labs(x = "Year", y = "New Capacity (MW)", fill = "Primary_Fuel",pattern="Primary_Fuel",caption=SourceDB)  +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,(mxc)),breaks=breaks_pretty(6),label=comma) +
    scale_x_continuous(expand = c(0, 0),limits = c(MinYr-1, MaxYr+1),breaks=seq(MinYr, MaxYr)) +
    
    # guides(fill = guide_legend(ncol = 1)) +
    # guides(pattern=guide_legend(ncol = 1)) +
    
    scale_fill_manual(values=colours5,drop=FALSE) +
  
    scale_pattern_manual(values=Patterns5,drop=FALSE)
  
  # BuiltUnitsList <- list(BuiltUnits)
  # return(BuiltUnitsList)
}

################################################################################  
## FUNCTION: TotalCapChange
## Gives capacity added and retired each year in the same plot
##
## INPUTS: 
##    input - ResGroupYear
## TABLES REQUIRED: 
##    ResGroupYear -Yearly resoruce group emissions
################################################################################
TotalCapChange <- function(case) {
  
  # Color choice
  col_scale = colours8

  
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
           Time_Period<=MaxYrStudy) 
  
  # Set levels to each category in order specified
  Add_Ret_data$Primary_Fuel <- factor(Add_Ret_data$Primary_Fuel, 
                                      levels=c("Coal","Coal-to-Gas", "Hydrogen Simple Cycle",
                                               #"Hydrogen Combined Cycle",
                                               #"Blended  Simple Cycle","Blended  Combined Cycle",
                                               "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle CCS Retrofit",
                                               "Natural Gas Combined Cycle", 
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
    mutate(Year=End_Year,
           Capacity_Retired=Capacity_Retired*-1)%>%
    subset(select=c(Primary_Fuel,Year,Capacity_Retired))
  
  # Get summary for each year!
  Tot_Change<-merge(BuilddataTot,RetdatadataTot,by=c("Primary_Fuel","Year"), all.x = TRUE, all.y = TRUE)
  
  # Replace NA values with 0
  Tot_Change[is.na(Tot_Change)]=0
  
  # Sum all up
  Tot <- Tot_Change %>%
    group_by(Year) %>%
    summarise(maxy = sum(Capacity_Added), miny = sum(Capacity_Retired))
  
  # Capacity limits for plot
  mny <- plyr::round_any(min(Tot$miny),1000, f=floor)
  mxy <- plyr::round_any(max(Tot$maxy),1000, f=ceiling)
  
  # Year limits for plot. Add 365 to get the year after the first
  mnx <- format(min(ResGroupMn$Time_Period), format="%Y")
  mxx <- format(MaxYrStudy, format="%Y")
  
  # Plot it all
  Tot_Change %>%
    ggplot() +
    
    # Plot added
    geom_col_pattern(aes(Year, (Capacity_Added), fill = Primary_Fuel,pattern=Primary_Fuel),
             alpha=Plot_Trans, size=.5, colour="black",
             pattern_density = 0.35,
             pattern_fill = "black",
             pattern_colour  = NA,
             pattern_spacing=0.01) +
    
    # Plot retired
    geom_col_pattern(aes(Year, (Capacity_Retired), fill = Primary_Fuel,pattern=Primary_Fuel),
             alpha=Plot_Trans, size=.5, colour="black",
             pattern_density = 0.3,
             pattern_fill = "black",
             pattern_colour  = NA,
             pattern_spacing=0.01) +
    
    # Add line at y=0
    geom_hline(yintercept=0, color = "black",size=0.75)+
    
    theme_bw() +
  
    # Changes the font type
    theme(text=element_text(family=Plot_Text)) + 
    
    theme(
      # General Plot Settings
      panel.grid = element_blank(),
      # (t,r,b,l) margins, adjust to show full x-axis, default: (5.5,5.5,5.5,5.5)
      plot.margin = unit(c(6, 12, 5.5, 5.5), "points"),      # Plot margins
      panel.background = element_rect(fill = "transparent"), # Transparent background
      text = element_text(size = GenText_Sz),                # Text size
      plot.title = element_text(size = GenText_Sz,hjust = 0.5),  # Plot title size (if present)
      plot.subtitle = element_text(hjust = 0.5),             # Plot subtitle size (if present)
      #panel.grid.major.y = element_line(size=0.25,
      #linetype=1,color = 'gray90'),                         # Adds horizontal lines
      # X-axis
      axis.text.x = element_text(angle = 45,color="black",
                                 vjust = 1, hjust = 1),          # Horizontal text
     # axis.title.x = element_text(size = GenText_Sz+6),           # x-axis title text size
      axis.title.x =element_blank(), 
      # Y-axis
      axis.title.y = element_text(size = GenText_Sz + 6),           # y-axis title text size
      axis.text.y = element_text(color="black"),
      # Legend
      legend.key.size = unit(1,"lines"),                     # Shrink legend boxes
      legend.position = "right",                             # Move legend to the bottom
      legend.justification = c(0.5,0.5),                     # Center the legend
      legend.text = element_text(size = GenText_Sz -6),              # Size of legend text
      legend.title=element_blank()) +                         # Legend title
    
    scale_x_discrete(expand=c(0.01,0.01),
                     limits = c(mnx:mxx)) +
    scale_y_continuous(expand=c(0,0),
                       limits = c((mny),(mxy)),breaks=seq(mny,mxy,by=1000),
                       label=comma) +
    scale_fill_manual(values=col_scale,drop = FALSE) +
    scale_pattern_manual(values=Patterns8,drop = FALSE) +
    
    labs(x = "Year", y = "Change in Capacity (MW)", fill = "Resource Options",pattern="Resource Options",caption = paste(SourceDB))
}

################################################################################  
## FUNCTION: Eval_CapChange 
## Shows the net capacity change each year.
##
## INPUTS: 
##    input - ResGroupYear
## TABLES REQUIRED: 
##    ResGroupYear -Yearly resoruce group emissions
################################################################################
Eval_CapChange <- function(case) {
  
  # Col choice
  col_scale = colours8
  
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
           Time_Period<=MaxYrStudy) 

# Set levels to each category in order specified
Add_Ret_data$Primary_Fuel <- factor(Add_Ret_data$Primary_Fuel, 
                                    levels=c("Coal","Coal-to-Gas", "Hydrogen Simple Cycle",
                                             #"Hydrogen Combined Cycle",
                                             #"Blended  Simple Cycle","Blended  Combined Cycle",
                                             "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle CCS Retrofit",
                                             "Natural Gas Combined Cycle", 
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
mxx <- format(MaxYrStudy, format="%Y")

# Plot it all
Tot_Change %>%
  ggplot() +
  aes(Year, (diff), fill = Primary_Fuel) +
  geom_col_pattern(aes(pattern=Primary_Fuel),alpha=Plot_Trans, size=.5, colour="black",
                   pattern_density = 0.3,
                   pattern_fill = "black",
                   pattern_colour  = NA,
                   pattern_spacing=0.01) +
  
  # Add line at y=0
  geom_hline(yintercept=0, color = "black")+
  
  theme_bw() +
  theme(text=element_text(family=Plot_Text)) +
  theme(
    # General Plot Settings
    panel.grid = element_blank(),
    # (t,r,b,l) margins, adjust to show full x-axis, default: (5.5,5.5,5.5,5.5)
    plot.margin = unit(c(6, 12, 5.5, 5.5), "points"),      # Plot margins
    panel.background = element_rect(fill = "transparent"), # Transparent background
    text = element_text(size = GenText_Sz),                # Text size
    plot.title = element_text(size = GenText_Sz,hjust = 0.5),  # Plot title size (if present)
    plot.subtitle = element_text(hjust = 0.5),             # Plot subtitle size (if present)
    #panel.grid.major.y = element_line(size=0.25,
    #linetype=1,color = 'gray90'),                         # Adds horizontal lines
    # X-axis
    axis.text.x = element_text(angle = 45,
                               vjust = 1, hjust = 1,color="black"),          # Horizontal text
    #axis.title.x = element_text(size = GenText_Sz+6),           # x-axis title text size
    axis.title.x = element_blank(),
    # Y-axis
    axis.title.y = element_text(size = GenText_Sz+6),           # y-axis title text size
    axis.text.y = element_text(color = "black"),
    # Legend
    legend.key.size = unit(1,"lines"),                     # Shrink legend boxes
    legend.position = "right",                             # Move legend to the bottom
    legend.justification = c(0.5,0.5),                     # Center the legend
    legend.text = element_text(size =GenText_Sz-6),              # Size of legend text
    legend.title=element_blank()) +                        # Legend title
  
  scale_x_discrete(expand=c(0.05,0.05),
                   limits = as.character(mnx:mxx)) +
  scale_y_continuous(expand=c(0,0),
                     limits = c((mny),(mxy)),breaks=seq(mny,mxy,by=1000),labels=comma) +
  scale_fill_manual(values=col_scale,drop = FALSE) +
  scale_pattern_manual(values=Patterns8,drop = FALSE) +
  labs(x = "Year", y = "Net Change in Capacity (MW)", fill = "Resource Options",pattern="Resource Options",caption = paste(SourceDB))
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
    filter(Run_ID == case & LT_Iteration == 0 & 
             Time_Period == "Study" & Fuel_Type == Fuel) 
  
  data %>%
    ggplot() +
    aes(Name, Units_Built,fill = Fuel_Type) + 
    geom_col(color="black") +
    labs(x = "Plant Name", y = "Units Built") +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,(max(data$Units_Built)+1))) +
    theme(text=element_text(family=Plot_Text)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.x = element_text(size = 26,face="bold"),
          axis.title.y = element_text(size = 26,face="bold"),
          axis.text=element_text(color="black"),
          plot.title = element_text(size = 20),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent"),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.key.size = unit(1,"lines"), 
          legend.background = element_rect(fill='transparent'),
          legend.title = element_blank(),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          text = element_text(size = 18),
          panel.border = element_rect(colour = "black", fill = "transparent"), 
          #panel.grid.major.y = element_line(size=0.25,linetype=5,color = "gray36")
          ) +
    
    
    scale_fill_manual(values="lightblue") +
    
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
    filter(Run_ID == case & LT_Iteration == 0 & 
             Time_Period == "Study" & Fuel_Type == Fuel) 
  
  data %>%
    ggplot() +
    aes(Name, Max_Limit_Slack,fill = Fuel_Type) + 
    geom_col(color="black") +
    labs(x = "Plant Name", y = "Units Still Available") +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,(max(data$Max_Limit_Slack)+1))) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          #axis.title.x = element_text(size = XTit_Sz,face="bold"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 26,face="bold"),
          plot.title = element_text(size = 20),
          axis.text=element_text(color="black"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.key.size = unit(1,"lines"), 
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          text = element_text(size = 18),
          panel.border = element_rect(colour = "black", fill = "transparent"), 
          #panel.grid.major.y = element_line(size=0.25,linetype=5,color = "gray36")
          ) +  
    
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
  
  if (Fuel=="WND"){
    AddedText<-"; Each Unit Capacity = 200 MW"
  } else if (Fuel=="SUN"){
    AddedText<-"; Each Unit Capacity = 100 MW"
  }else{
    AddedText<-"; Each Unit Capacity = Varries"
  }
  
  ggdraw(add_sub(p3,paste("Simulation: ",SourceDB,"; ", "Fuel Type: ",Fuel, sep = "",AddedText)))
}

################################################################################
## FUNCTION: Build_CCSRet
## Show units were retrofited with CCS and the year.
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
################################################################################  

Build_CCSRet <- function(case) {
  
  # Get the CCS bulid data first
  CCSBuild <- Build %>%
    filter(Run_ID == case,
           LT_Iteration == 0,
           Fuel_Type == "GasCCS",
           Time_Period != "Study",
           grepl("Retrofit",Name)) %>%
    mutate(BuildYear=if_else(Units_Built==1,as.numeric(Time_Period),0))%>%
    group_by(Name)%>%
    summarise(Units_Built=sum(Units_Built),
              BuildYear=as.character(max(BuildYear))) %>%
    ungroup() 
      
  # Replace 0's
  CCSBuild$BuildYear[CCSBuild$BuildYear == 0]<-NA

  # CCS Retrofit Graph
    ggplot(CCSBuild) +
    geom_col(aes(Name, Units_Built,fill = BuildYear),color="black") +
    labs(x = "Plant Name", y = "CCS Retrofit Units Added",fill="Start Year") +
    
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,1.5),breaks=1) +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.x = element_text(size = 26,face="bold"),
          axis.title.y = element_text(size = 26,face="bold"),
          axis.text=element_text(color="black"),
          plot.title = element_text(size = 20),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent"),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.key.size = unit(1,"lines"), 
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          text = element_text(size = 20),
          panel.border = element_rect(colour = "black", fill = "transparent"), 
          #panel.grid.major.y = element_line(size=0.25,linetype=5,color = "gray36")
    ) +
    
    # Remove NA from legend
    #  scale_fill_brewer(palette= "RdYlBu",na.translate = F) +
      scale_fill_manual(values=c("2026"="#313695",
                                 "2027"="#4575b4",
                                 "2028"="#74add1",
                                 "2029"="#abd9e9",
                                 "2030"="#e0f3f8",
                                 "2031"="#ffffbf",
                                 "2032"="#fee090",
                                 "2033"="#fdae61",
                                 "2034"="#f46d43",
                                 "2035"="#d73027",
                                 "2036"="#a50026"),na.translate = F,drop=TRUE)+
    
    theme(text=element_text(family=Plot_Text))
  
}

################################################################################
## FUNCTION: Build_CCSRet2
## Show units were retrofited with CCS and the year.
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
################################################################################  

Build_CCSRet2 <- function(case) {
  
  # Get the CCS retrofit data
  CCSBuild <- Build %>%
    filter(Run_ID == case,
           LT_Iteration == 0,
           Fuel_Type == "GasCCS",
           Time_Period != "Study",
           grepl("Retrofit",Name)) %>%
    mutate(BuildYear=if_else(Units_Built==1,as.numeric(Time_Period),0))%>%
    group_by(Name)%>%
    summarise(Units_Built=sum(Units_Built),
              BuildYear=as.character(max(BuildYear))) %>%
    ungroup() 
  
  # Replace 0's
  CCSBuild$BuildYear[CCSBuild$BuildYear == 0]<-NA
  
  # Get the retrofits that occured
  CCSTemp<-CCSBuild %>%
    filter(Units_Built >0)%>%
    mutate(Name=word(Name,1,2),
           Name=gsub("-","",Name))
  
  # Extract list of names from retorifts that occurred
  CCSNames<-list(unique(CCSTemp$Name))
  
  
  # Get all combined cycle data
  CC_all<-ResYr %>%
    filter(Primary_Fuel == "WECC-Alberta NaturalGas",
           Run_ID == case,
           Condition == "Average") %>%
    mutate(Time_Period=as.numeric(Time_Period),
           End_Date=as.Date(End_Date,format = "%m/%d/%Y"),
           Beg_Date=as.Date(Beg_Date,format = "%m/%d/%Y"),
           End_Date=year(End_Date), 
           Beg_Date=year(Beg_Date)) %>% 
    filter(Beg_Date<=2025)%>% # Filter out any new resources
    subset(., select=c(Name,Condition,Capacity,Nameplate_Capacity,End_Date,Beg_Date,
                       Run_ID,Primary_Fuel,Time_Period,Capacity_Factor))
  
  CC_fate<-CC_all %>%
    group_by(Name)%>%
    summarise(Capacity=max(Capacity),
              End=max(End_Date)) %>%
    ungroup()%>%
    mutate(Retro=if_else(grepl(paste(unlist(CCSNames),collapse="|"),Name),1,0),
           End=as.character(End))
  
  CC_fate$End[CC_fate$End == 0]<-NA
  
  
  # PLOT
  ggplot(CC_fate) +
    geom_col(aes(Name, Retro,fill = End),color="black") +
    labs(x = "Plant Name", y = "Units Retrofited",fill="End Year") +
    
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,1.5),breaks=1) +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 26,face="bold"),
          axis.text=element_text(color="black"),
          plot.title = element_text(size = 20),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent"),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.key.size = unit(1,"lines"), 
          legend.background = element_rect(fill='transparent'),
          legend.position="right",
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          text = element_text(size = 20),
          panel.border = element_rect(colour = "black", fill = "transparent"), 
          #panel.grid.major.y = element_line(size=0.25,linetype=5,color = "gray36")
    ) +
    
    # Remove NA from legend
    #scale_fill_brewer(palette= "RdYlBu",na.translate = F,drop=TRUE) +
    
    scale_fill_manual(values=c("2026"="#313695",
                               "2027"="#4575b4",
                               "2028"="#74add1",
                               "2029"="#abd9e9",
                               "2030"="#e0f3f8",
                               "2031"="#ffffbf",
                               "2032"="#fee090",
                               "2033"="#fdae61",
                               "2034"="#f46d43",
                               "2035"="#d73027",
                               "2036"="#a50026"),na.translate = F,drop=TRUE)+
  
    theme(text=element_text(family=Plot_Text))
  
}


################################################################################
## FUNCTION: CC_Fate_study
## Show units were retrofited with CCS and the year.
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
################################################################################  

CC_Fate_study <- function(case) {
  
  # Get the CCS retrofit data
  CCSBuild <- Build %>%
    filter(Run_ID == case,
           LT_Iteration == 0,
           Fuel_Type == "GasCCS",
           Time_Period != "Study",
           grepl("Retrofit",Name)) %>%
    mutate(BuildYear=if_else(Units_Built==1,as.numeric(Time_Period),0))%>%
    group_by(Name)%>%
    summarise(Units_Built=sum(Units_Built),
              BuildYear=as.character(max(BuildYear))) %>%
    ungroup() 
  
  # Replace 0's
  CCSBuild$BuildYear[CCSBuild$BuildYear == 0]<-NA
  
  # Get the retrofits that occured
  CCSTemp<-CCSBuild %>%
    filter(Units_Built >0)%>%
    mutate(Name=word(Name,1,2),
           Name=gsub("-","",Name))
  
  # Extract list of names from retorifts that occurred
  CCSNames<-list(unique(CCSTemp$Name))
  
  
  # Get all combined cycle data
  CC_all<-ResYr %>%
    filter(Primary_Fuel == "WECC-Alberta NaturalGas",
           Run_ID == case,
           Condition == "Average") %>%
    mutate(Time_Period=as.numeric(Time_Period),
           End_Date=as.Date(End_Date,format = "%m/%d/%Y"),
           Beg_Date=as.Date(Beg_Date,format = "%m/%d/%Y"),
           End_Date=year(End_Date), 
           Beg_Date=year(Beg_Date)) %>% 
    filter(Beg_Date<=2025)%>% # Filter out any new resources
    subset(., select=c(Name,Condition,Capacity,Nameplate_Capacity,End_Date,Beg_Date,
                       Run_ID,Primary_Fuel,Time_Period,Capacity_Factor))
  
  CC_fate<-CC_all %>%
    group_by(Name)%>%
    summarise(Capacity=max(Capacity),
              End=max(End_Date)) %>%
    ungroup()%>%
    mutate(Fate=if_else(grepl(paste(unlist(CCSNames),collapse="|"),Name),"CCS Retrofit",
                        if_else(End<MaxYrStudy,"Retired",
                                "Opperating")))
  
  # PLOT
  ggplot(CC_fate) +
    geom_col(aes(Name, Capacity,fill = Fate),color="black") +
    labs(x = "Plant Name", y = "Plant Capacity") +
    
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,max(CC_fate$Capacity)+50)) +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = GenText_Sz+6,face="bold"),
          axis.text=element_text(color="black"),
          plot.title = element_text(size = GenText_Sz),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent"),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.key.size = unit(1,"lines"), 
          legend.background = element_rect(fill='transparent'),
          legend.title = element_blank(),
          legend.position="bottom",
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          text = element_text(size = GenText_Sz),
          panel.border = element_rect(colour = "black", fill = "transparent"), 
          #panel.grid.major.y = element_line(size=0.25,linetype=5,color = "gray36")
    ) +
    
    # Remove NA from legend
    scale_fill_manual(na.translate = F,
                      values=c("CCS Retrofit"="lightsteelblue","Opperating"="steelblue2","Retired"="steelblue4")) +
    
    theme(text=element_text(family=Plot_Text))
  
}
  
################################################################################
## FUNCTION: CC_Fate_year
## Show units were retrofited with CCS and the year.
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
################################################################################  

CC_Fate_year <- function(case) {
  
  # Get the CCS retrofit data
  CCSBuild <- Build %>%
    filter(Run_ID == case,
           LT_Iteration == 0,
           Fuel_Type == "GasCCS",
           Time_Period != "Study",
           grepl("Retrofit",Name)) %>%
    mutate(BuildYear=if_else(Units_Built==1,as.numeric(Time_Period),0))%>%
    group_by(Name)%>%
    summarise(Units_Built=sum(Units_Built),
              BuildYear=as.character(max(BuildYear))) %>%
    ungroup() 
  
  # Replace 0's
  CCSBuild$BuildYear[CCSBuild$BuildYear == 0]<-NA
  
  # Get the retrofits that occured
  CCSTemp<-CCSBuild %>%
    filter(Units_Built >0)%>%
    mutate(Name=word(Name,1,2),
           Name=gsub("-","",Name))
  
  # Extract list of names from retorifts that occurred
  CCSNames<-list(unique(CCSTemp$Name))
  
  
  # Get all combined cycle data
  CC_all<-ResYr %>%
    filter(Primary_Fuel == "WECC-Alberta NaturalGas",
           Run_ID == case,
           Condition == "Average") %>%
    mutate(Time_Period=as.numeric(Time_Period),
           End_Date=as.Date(End_Date,format = "%m/%d/%Y"),
           Beg_Date=as.Date(Beg_Date,format = "%m/%d/%Y"),
           End_Date=year(End_Date), 
           Beg_Date=year(Beg_Date)) %>% 
    subset(., select=c(Name,Condition,Capacity,Nameplate_Capacity,End_Date,Beg_Date,
                       Run_ID,Primary_Fuel,Time_Period,Capacity_Factor))
  
  CC_fate<-CC_all %>%
    group_by(Name)%>%
    mutate(MaxCap=max(Capacity))%>%
    ungroup()%>%
    mutate(Fate=if_else(End_Date<Time_Period & grepl(paste(unlist(CCSNames),collapse="|"),Name),"CCS Retrofit",
                        if_else(End_Date<Time_Period,"Retired",
                                "Opperating"))) %>%
    group_by(Time_Period,Fate) %>%
    summarise(Capacity=sum(MaxCap))%>%
    mutate(Capacity=if_else(Fate == "Retired",Capacity*-1,Capacity),
           YR=as.character(Time_Period),
           YEAR_trim=as.numeric(format(YR)))%>%
    ungroup() %>%
    filter(YEAR_trim<=MaxYrStudy) %>%
    # Optional line to remove retire instead of showing on graph
    filter(!Fate=="Retired")

  # PLOT
  ggplot(CC_fate) +
    geom_col(aes(YR, Capacity,fill = Fate),color="black") +
    labs(x = "Year", y = "NGCC Annual Capacity (MW)") +
    
    geom_hline(yintercept=0, color = "black",size=0.5,linetype=1)+
    
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,
                         #min(CC_fate$Capacity)-500,
                         max(CC_fate$Capacity)+500),breaks=pretty_breaks(8)) +

    theme(text=element_text(family=Plot_Text)) +
    
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = GenText_Sz+6,face="bold"),
          axis.text=element_text(color="black"),
          plot.title = element_text(size = GenText_Sz),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent"),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.key.size = unit(1,"lines"), 
          legend.background = element_rect(fill='transparent'),
          legend.title = element_blank(),
          legend.position="bottom",
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          text = element_text(size = GenText_Sz),
          panel.border = element_rect(colour = "black", fill = "transparent"), 
          #panel.grid.major.y = element_line(size=0.25,linetype=5,color = "gray36")
    ) +
    
    # Remove NA from legend
    scale_fill_manual(na.translate = F,
                      values=c("CCS Retrofit"=cOL_NGCC_CCS,"Opperating"=cOL_NGCC,"Retired"="steelblue4")) +
    
    theme(text=element_text(family=Plot_Text))
  
}