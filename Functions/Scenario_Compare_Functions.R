################################################################################
# TITLE: Scenario_Compare_Functions
# DESCRIPTION: Functions for plots with data from multiple scenarios
#
# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: January 22, 2024; 
#
################################################################################

###############################################################################
## FUNCTION: compare_rename
## Rename scenarios for comparison
###############################################################################
compare_rename <-function(data,type){
  
  if (type == "l"){
    input_name <-c("Draft CER","Current Policy","Emissions Limit","TIER 2050","TIER 2035","No ITCs","No ITCs with CER")
  }else{
    input_name<-c("CER","CP","EL","TIER2050","TIER2035","noITCs","CERnoITCs")
  }
  
  # Rename if needed to make up for poor initial coding
  if (!"Scenario" %in% colnames(data)){
    data <-data %>%
      rename(Scenario=Sim_Name)
  }
  
  data <- data %>%
    mutate(Scenario = if_else(grepl("CER_12",Scenario)==TRUE,input_name[1],
                              if_else(grepl("CP_11",Scenario)==TRUE,input_name[2],
                                      if_else(grepl("EL_",Scenario)==TRUE,input_name[3],
                                        if_else(grepl("TIER2050_",Scenario)==TRUE,input_name[4],
                                                if_else(grepl("TIER2035",Scenario)==TRUE,input_name[5],
                                                        if_else(grepl("CP_noITC",Scenario)==TRUE,input_name[6],
                                                                if_else(grepl("CER_noITC",Scenario)==TRUE,input_name[7],"unknown"))))))))
  
  if (any(data$Scenario == "unknown")==TRUE) {
    print("Unknown scenario detected")
  } else{
    print("Scenario names:")
    print(unique(data$Scenario))
  }
  
  return(data)
    
}

################################################################################
## FUNCTION: AvgYr_price_COMPARE
## Plots annual average pool price, with optional historical
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
AvgYr_price_COMPARE <- function(name_type,AESO_include) {
  
  # Filter and prepare Simulation data
  Sim <- Zone %>%
    mutate(Year=as.numeric(Year)) %>%
    select(.,c(Year,Avg_Price,Scenario)) %>%
    compare_rename(.,name_type)
  
  # Plot color
  if (name_type == "l"){
    scenario_colors<-sn_colors_l
    scenario_lines<-sn_line_l
  }else{
    scenario_colors<-sn_colors_s
    scenario_lines<-sn_line_s
  }
  
  # Include AESO data
  if (AESO_include=="Y"){
      # Define AESO data (from market statistcs reports)
      AESO_Year <-c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023)
      AESO_pool <- c(70.36,80.79,66.95,89.95,47.81,50.88,76.22,64.32,80.19,49.42,33.34,18.28,22.19,50.35,54.88,46.72,101.93,162.46,88.59)
      AESO_Zone <-data.frame(Year =AESO_Year,
                             Avg_Price =AESO_pool,
                             Scenario = "Historic")
      # Merge result
      all_pool <- rbind(Sim,AESO_Zone) %>%
        mutate(Scenario = as.factor(Scenario))
      all_pool$Scenario<-fct_relevel(all_pool$Scenario, "Historic",after=Inf)

  }else{
    all_pool<-Sim
  }
  
  # Get plot max/mins
  YearMX<-max(all_pool$Year)
  YearMN<-min(all_pool$Year)
  Upplim <- round_any(max(all_pool$Avg_Price)+51,50)
  
  # Plot
  ggplot(all_pool) +
    geom_line(aes(x = Year, y = Avg_Price, colour = Scenario,linetype= Scenario), 
              size = 1.25) +
    theme_bw() +
    theme(text=element_text(family=Plot_Text)) +
    theme(axis.text = element_text(color="black"),
          axis.title = element_text(size = GenText_Sz+6),
          axis.text.x = element_text(angle = 0, hjust=0.5,color="black"),
          plot.title = element_blank(),
          text = element_text(size=GenText_Sz),
          axis.title.x=element_blank(),
          legend.text = element_text(size = GenText_Sz-6),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.y = element_line(size=0.25,linetype=2,color = 'gray70'),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(y = "Annaul Average Pool Price ($/MWh)", x="Year",colour="Condition",linetype="Condition") +
    
    #scale_colour_grey() +
    scale_linetype_manual(name="Guide1",values = scenario_lines,drop=TRUE,limits = force)+
    scale_colour_manual(name="Guide1",values = scenario_colors,drop = TRUE,limits = force) +
    
    scale_x_continuous(expand=c(0,0),limits = c(YearMN-1,YearMX+1),breaks=seq(YearMN, YearMX, 5)) +
    
    scale_y_continuous(expand=c(0,0),limits=c(0,Upplim),n.breaks = 5, 
    )
    
}

################################################################################
## FUNCTION: AnnualEmLine_COMPARE
## Plots annual average emissions
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
AnnualEm_COMPARE <- function(name_type,cogen_include) {
  
  # Plot color
  if (name_type == "l"){
    scenario_colors<-sn_colors2_l
    scenario_lines<-sn_line2_l
  }else{
    scenario_colors<-sn_colors2_s
    scenario_lines<-sn_line2_s
  }
  
  # Filter emissions data
  if (cogen_include=="Y"){
    # Filter and prepare Simulation data
    Sim <- Zone %>%
      mutate(Year=as.numeric(Year))%>%
      select(.,c(Year,Emissions,Scenario))%>%
      compare_rename(.,name_type)%>%
      mutate(Scenario=as.factor(Scenario))
    add_note<-""
  }else{
    Sim <- Zone %>%
      mutate(Year=as.numeric(Year))%>%
      select(.,c(Year,NonCogen_Emissions,Scenario))%>%
      compare_rename(.,name_type)%>%
      rename(Emissions=NonCogen_Emissions)%>%
      mutate(Scenario=as.factor(Scenario))
    
    add_note<-"Cogeneration emissions excluded"
  }

  # Get plot max/mins
  YearMX<-max(Sim$Year)
  YearMN<-min(Sim$Year)
  Upplim <- round_any(max(Sim$Emissions)+3,5)
  
  # Plot
  ggplot(Sim) +
    geom_line(aes(x = Year, y = Emissions, colour = Scenario,linetype= Scenario), 
              size = 1.25) +
    theme_bw() +
    theme(text=element_text(family=Plot_Text)) +
    theme(axis.text = element_text(color="black"),
          axis.title = element_text(size = GenText_Sz+6),
          axis.text.x = element_text(angle = 0, hjust=0.5,color="black"),
          plot.title = element_blank(),
          text = element_text(size=GenText_Sz),
          axis.title.x=element_blank(),
          legend.text = element_text(size = GenText_Sz-6),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          panel.grid.major.y = element_line(size=0.25,linetype=2,color = 'gray70'),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(y = "Annual Emissions (Mt CO2e)", x="Year",caption=add_note) +
    
    #scale_colour_grey() +
    scale_linetype_manual(name="Guide1",values = scenario_lines,drop=TRUE,limits = force)+
    scale_colour_manual(name="Guide1",values = scenario_colors,drop = TRUE,limits = force) +
    
    scale_x_continuous(expand=c(0,0),limits = c(YearMN-0.2,YearMX+0.2),breaks=seq(YearMN, YearMX, 2)) +
    
    scale_y_continuous(expand=c(0,0),limits=c(0,Upplim),n.breaks = 6)
  
}

################################################################################
## FUNCTION: AnnualEm_Cum_COMPARE
## Plots annual average emissions in cummulative bar chart.
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
AnnualEm_Cum_COMPARE <- function(name_type,cogen_include) {
  
  # Plot color
  if (name_type == "l"){
    scenario_colors<-sn_colors2_l
  }else{
    scenario_colors<-sn_colors2_s
  }
  
  # Filter emissions data
  if (cogen_include=="Y"){
    # Filter and prepare Simulation data
    Sim <- Zone %>%
      mutate(Year=as.numeric(Year))%>%
      select(.,c(Year,Emissions,Scenario))%>%
      compare_rename(.,name_type)%>%
      mutate(Scenario=as.factor(Scenario))%>%
      group_by(Scenario)%>%
      summarise(Year = Year,
                AnnualEm=Emissions,
                Emissions=cumsum(Emissions))
    add_note<-""
  }else{
    Sim <- Zone %>%
      mutate(Year=as.numeric(Year))%>%
      select(.,c(Year,NonCogen_Emissions,Scenario))%>%
      compare_rename(.,name_type)%>%
      rename(Emissions=NonCogen_Emissions)%>%
      mutate(Scenario=as.factor(Scenario))%>%
      group_by(Scenario)%>%
      summarise(Year = Year,
                AnnualEm=Emissions,
                Emissions=cumsum(Emissions))
    
    add_note<-"Cogeneration emissions excluded"
  }
  
  # Get plot max/mins
  YearMX<-max(Sim$Year)
  YearMN<-min(Sim$Year)
  Upplim <- round_any(max(Sim$Emissions)+11,25)
  
  # Filter by breaks
  nbreaks=1
  plot_breaks = seq(YearMN,YearMX,by=nbreaks)
  Sim <- Sim %>%
    filter(Year %in% plot_breaks)
  
  # Plot
  ggplot(Sim) +
    geom_bar(aes(x = Year, y = Emissions, fill = Scenario), 
              size = 1.25,stat="identity",position = "dodge") +
    theme_bw() +
    #facet_wrap(~Scenario)
    theme(text=element_text(family=Plot_Text)) +
    theme(axis.text = element_text(color="black"),
          axis.title = element_text(size = GenText_Sz+6),
          axis.text.x = element_text(angle = 0, hjust=0.5,color="black"),
          plot.title = element_blank(),
          text = element_text(size=GenText_Sz),
          axis.title.x=element_blank(),
          legend.text = element_text(size = GenText_Sz-6),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          panel.grid.major.y = element_line(size=0.25,linetype=2,color = 'gray70'),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(y = "Cummulative Emissions (Mt CO2e)", x="Year",caption=add_note) +
    
    #scale_colour_grey() +
    scale_fill_manual(name="Guide1",values = scenario_colors,drop = TRUE,limits = force) +
    
    scale_x_continuous(breaks=seq(YearMN, YearMX, nbreaks), expand = c(0.01,0.01)) +
    
    scale_y_continuous(expand=c(0,0),limits=c(0,Upplim),n.breaks = 6)
  
}

################################################################################
## FUNCTION: Total_Cap_COMPARE
## Plots total capacity added.
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
Total_Cap_Add_COMPARE <- function(name_type) {

  # Filter emissions data
  Builddata <- Tot_Cap %>%
      compare_rename(.,name_type)%>%
    mutate(Scenario=as.factor(Scenario),
           New_Cap_Total=New_Cap_Total/1000)%>%
    filter(New_Cap_Total>0)

  #Max Units Built
  max_groups <-Builddata %>%group_by(Scenario)%>%
    summarise(max = sum(New_Cap_Total))
  mxc <- round_any(max(max_groups$max)+3,5,f=ceiling)
  
  
  #Plot data
  ggplot(Builddata,aes(x=New_Cap_Total, y=Scenario)) +
    geom_bar_pattern(aes(pattern = Plant_Type, fill = Plant_Type),
                     position="stack", stat="identity",
                     na.rm=TRUE, alpha=Plot_Trans,color='black',
                     pattern_density = 0.3,
                     pattern_fill = "black",
                     pattern_colour  = NA,
                     pattern_spacing=0.01) +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),  
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = GenText_Sz+6, vjust=0),
          panel.background = element_rect(fill = "transparent"),
          axis.text.x=element_text(angle=0,vjust = 0.5, hjust = 0.5,color="black"),
          axis.text.y=element_text(color="black"),
          plot.title = element_blank(),
          legend.justification = c(0.5,0.5),
          legend.position = ("right"),
          legend.text = element_text(size = GenText_Sz-6),
          legend.title=element_blank(), 
          #legend.key.size = unit(1,"lines"),
          text = element_text(size = GenText_Sz)) +
    
    guides(fill = guide_legend(ncol=1)) +
    
    scale_fill_manual(name="Plant Type",values=colours5,drop = TRUE,limits = force) +
    scale_pattern_manual(name="Plant Type",values=Patterns5,drop = TRUE,limits = force) +
    
    scale_x_continuous(expand = c(0, 0),limits = c(0, mxc),breaks=seq(0, mxc,by=5),labels=comma) +
    
    labs(x = "Total Capacity Added (GW)") 

  
}

################################################################################
## FUNCTION: Total_Cap_Ret_COMPARE
## Plots total capacity retired.
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
Total_Cap_Ret_COMPARE <- function(name_type) {
  
  # Filter emissions data
  Builddata <- Tot_Cap %>%
    compare_rename(.,name_type)%>%
    mutate(Scenario=as.factor(Scenario),
           Cap_Retired=-1*Cap_Retired/1000)%>%
    filter(Cap_Retired<0)
  
  #Max Units Built
  max_groups <-Builddata %>%group_by(Scenario)%>%
    summarise(min = sum(Cap_Retired))
  mnc <- round_any(min(max_groups$min)-3,5,f=floor)
  
  
  #Plot data
  ggplot(Builddata,aes(x=Cap_Retired, y=Scenario)) +
    geom_bar_pattern(aes(pattern = Plant_Type, fill = Plant_Type),
                     position="stack", stat="identity",
                     na.rm=TRUE, alpha=Plot_Trans,color='black',
                     pattern_density = 0.3,
                     pattern_fill = "black",
                     pattern_colour  = NA,
                     pattern_spacing=0.01) +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),  
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = GenText_Sz+6, vjust=0),
          panel.background = element_rect(fill = "transparent"),
          axis.text.x=element_text(angle=0,vjust = 0.5, hjust = 0.5,color="black"),
          axis.text.y=element_text(color="black"),
          plot.title = element_blank(),
          legend.justification = c(0.5,0.5),
          legend.position = ("right"),
          legend.text = element_text(size = GenText_Sz-6),
          legend.title=element_blank(), 
          #legend.key.size = unit(1,"lines"),
          text = element_text(size = GenText_Sz)) +
    
    guides(fill = guide_legend(ncol=1)) +
    
    scale_fill_manual(name="Plant Type",values=colours8,drop = TRUE,limits = force) +
    scale_pattern_manual(name="Plant Type",values=Patterns8,drop = TRUE,limits = force) +
    
    scale_x_continuous(expand = c(0, 0),limits = c(mnc, 0),breaks=pretty_breaks(5),labels=comma) +
    
    labs(x = "Total Capacity Retired (GW)") 
  
  
}

################################################################################
## FUNCTION: Cap_Relative_COMPARE
## Plots capacity additions relative to CP.
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
Cap_Relative_COMPARE <- function(name_type) {
  
  # Gather all data first
  Builddata <- Tot_Cap %>%
    compare_rename(.,name_type)%>%
    mutate(Scenario=as.factor(Scenario),
           New_Cap_Total=New_Cap_Total)%>%
    filter(New_Cap_Total>0) %>%
    subset(.,select=c(Scenario,Plant_Type,New_Cap_Total,Cap_Retired))
  
  # Format to compare against CP
  CP_Cap <- Tot_Cap %>%
    compare_rename(.,name_type)%>%
      filter(Scenario %in% c("CP","Current Policy"))%>%
    group_by(Plant_Type) %>%
    summarise(CP_Add = New_Cap_Total,
              CP_Ret = Cap_Retired)
  
  # Join them
  AllData <- merge(Builddata,CP_Cap,by=c("Plant_Type"), all.x = TRUE) %>%
    replace(is.na(.), 0) %>%
    mutate(Cap_Diff = New_Cap_Total-CP_Add,
           Ret_Diff = Cap_Retired-CP_Ret) %>%
  # Replace this to keep in figure
    mutate(Cap_Diff=replace(Cap_Diff,(Scenario %in% c("CP","Current Policy") & Plant_Type == "Wind"),0.00001)) %>%
    filter(!(Cap_Diff == 0))

  
  #Max Units Built
  max_groups <-AllData %>%group_by(Scenario)%>%
    summarise(max = sum(Cap_Diff[Cap_Diff>0]),
              min = sum(Cap_Diff[Cap_Diff<0]))
  mxc <- round_any(max(max_groups$max)+51,100,f=ceiling)
  mnc <-round_any(min(max_groups$min)-51,100,f=floor)
  
  #Plot data
  ggplot(AllData,aes(x=Cap_Diff, y=Scenario)) +
    geom_bar_pattern(aes(pattern = Plant_Type, fill = Plant_Type),
                     position="stack", stat="identity",
                     na.rm=TRUE, alpha=Plot_Trans,color='black',
                     pattern_density = 0.3,
                     pattern_fill = "black",
                     pattern_colour  = NA,
                     pattern_spacing=0.01) +
    theme_bw() +
    geom_vline(xintercept =0)+
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),  
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = GenText_Sz+6, vjust=0),
          panel.background = element_rect(fill = "transparent"),
          axis.text.x=element_text(angle=0,vjust = 0.5, hjust = 0.5,color="black"),
          axis.text.y=element_text(color="black"),
          plot.title = element_blank(),
          legend.justification = c(0.5,0.5),
          legend.position = ("right"),
          legend.text = element_text(size = GenText_Sz-6),
          legend.title=element_blank(), 
          #legend.key.size = unit(1,"lines"),
          text = element_text(size = GenText_Sz)) +
    
    guides(fill = guide_legend(ncol=1)) +
    
    scale_fill_manual(name="Plant Type",values=colours5,drop = TRUE,limits = force) +
    scale_pattern_manual(name="Plant Type",values=Patterns5,drop = TRUE,limits = force) +
    
    scale_x_continuous(expand = c(0, 0),limits = c(mnc, mxc),breaks=pretty_breaks(5),labels=comma) +
    
    labs(x = "Capacity Additions Relative to Current Policy (MW)") 
  
  
}

################################################################################
## FUNCTION: Cap_Year_Relative_COMPARE
## Plots capacity relative to CP in chosen year.
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
Cap_Year_Relative_COMPARE <- function(name_type,year_in) {
  
  # Gather all data first
  Filt_data <- ResGrYr %>%
    compare_rename(.,name_type)%>%
    mutate(Scenario=as.factor(Scenario),
           Ptype = as.character(Plant_Type),
           Ptype=if_else(Ptype %in% c("Storage - Compressed Air","Storage - Pumped Hydro","Storage - Battery"),"Storage",Ptype))%>%
    filter(Year == year_in) %>%
    group_by(Scenario,Ptype)%>%
    summarise(Capacity_MW=sum(Capacity_MW),
              Output_MWH=sum(Output_MWH),
              Emissions_Tonne=sum(Emissions_Tonne),
              Total_Hours_Run=mean(Total_Hours_Run),
              Percent_Marginal=mean(Percent_Marginal),
              Capacity_Factor=mean(Capacity_Factor))
  
  # Format to compare against CP
  CP_data <- Filt_data %>%
    filter(Scenario %in% c("CP","Current Policy"))%>%
    group_by(Ptype) %>%
    summarise(CP_Cap = Capacity_MW,
              CP_Out = Output_MWH,
              CP_Em = Emissions_Tonne,
              CP_Hours = Total_Hours_Run,
              CP_Marginal = Percent_Marginal,
              CP_CF = Capacity_Factor)
  
  # Join them
  AllData <- merge(Filt_data,CP_data,by=c("Ptype"), all.x = TRUE) %>%
    replace(is.na(.), 0) %>%
    mutate(Cap_Diff = Capacity_MW-CP_Cap,
           Out_Diff = Output_MWH-CP_Out,
           Em_Diff= Emissions_Tonne-CP_Em,
           Hours_Diff = Total_Hours_Run-CP_Hours,
           Marg_Diff = Percent_Marginal-CP_Marginal,
           CF_Diff = Capacity_Factor-CP_CF) %>%
    # Replace this to keep in figure
    mutate(Cap_Diff=replace(Cap_Diff,(Scenario %in% c("CP","Current Policy") & Ptype == "Wind"),0.00001)) %>%
    filter(!(Cap_Diff == 0))

  
  #Max Capacity
  max_groups <-AllData %>%group_by(Scenario)%>%
    summarise(max = sum(Cap_Diff[Cap_Diff>0]),
              min = sum(Cap_Diff[Cap_Diff<0]))
  mxc <- round_any(max(max_groups$max)+151,200,f=ceiling)
  mnc <-round_any(min(max_groups$min)-151,200,f=floor)
  
  
  #Plot data
  ggplot(AllData,aes(x=Cap_Diff, y=Scenario)) +
    geom_bar(aes(fill = Ptype),
                     position="stack", stat="identity",
                     na.rm=TRUE, alpha=Plot_Trans,color='black',
                    ) +
    theme_bw() +
    geom_vline(xintercept =0)+
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),  
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = GenText_Sz+6, vjust=0),
          panel.background = element_rect(fill = "transparent"),
          axis.text.x=element_text(angle=0,vjust = 0.5, hjust = 0.5,color="black"),
          axis.text.y=element_text(color="black"),
          plot.title = element_blank(),
          legend.justification = c(0.5,0.5),
          legend.position = ("right"),
          legend.text = element_text(size = GenText_Sz-6),
          legend.title=element_blank(), 
          #legend.key.size = unit(1,"lines"),
          text = element_text(size = GenText_Sz)) +
    
    guides(fill = guide_legend(ncol=1)) +
    
    scale_fill_manual(name="Plant Type",values=colours3b,drop = TRUE,limits = force) +

    scale_x_continuous(expand = c(0, 0),limits = c(mnc, mxc),breaks=pretty_breaks(5),labels=comma) +
    
    labs(x = paste(year_in,"Capacity Relative to Current Policy (MW)"))
  
  
}

################################################################################
## FUNCTION: Annual_Cap_COMPARE
## Plots annual capacity.
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
Annual_Cap_COMPARE <- function(name_type) {
  
  # Filter data & aggregate years
  Builddata <- Ann_Cap %>%
    compare_rename(.,name_type)%>%
    mutate(Scenario=as.factor(Scenario),
           report_group = if_else(between(Year,2023,2025),"2023 - 2025",
                                  if_else(between(Year,2026,2030),"2026 - 2030",
                                          if_else(between(Year,2031,2035),"2031 - 2035",
                                                  if_else(between(Year,2036,2040),"2036 - 2040",
                                                          if_else(between(Year,2041,2045),"2041 - 2045","na")))))) %>%
    group_by(report_group,Scenario,Plant_Type) %>%
    summarise(Add_MW = sum(Capacity_Added),
              Ret_MW = sum(Capacity_Retired),
              diff_MW= sum(Difference_MW)) %>%
    ungroup()
  

  
  #Max Units Built
  max_groups <-Builddata %>%group_by(Scenario,report_group)%>%
    summarise(max = sum(Add_MW),
              min = sum(Ret_MW))
  mxc <- round_any(max(max_groups$max)+51,100,f=ceiling)
  mnc <- round_any(min(max_groups$min)-51,100,f=floor)*-1
  
  #Plot data
  ggplot(Builddata,aes(x=Scenario, y=Add_MW)) +
    geom_bar_pattern(aes(pattern = Plant_Type, fill = Plant_Type),
                     position="stack", stat="identity",width=1,
                     na.rm=TRUE, alpha=Plot_Trans,color='black',
                     pattern_density = 0.3,
                     pattern_fill = "black",
                     pattern_colour  = NA,
                     pattern_spacing=0.01) +
    facet_wrap(~report_group, strip.position = "bottom",nrow = 1) +
    theme_classic() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(
      #panel.grid = element_blank(),  
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_text(size = GenText_Sz+6, vjust=0),
          panel.background = element_rect(fill = "transparent"),
          axis.text.x=element_text(angle=90,vjust = 0.5, hjust = 1,color="black",size = GenText_Sz-10),
          axis.text.y=element_text(color="black"),
          plot.title = element_blank(),
          
          legend.justification = c(0.5,0.5),
          legend.key.size = unit(0.3, "cm"),
          legend.position = ("right"),
          legend.text = element_text(size = GenText_Sz-12),
          legend.title=element_blank(), 
          legend.spacing.y = unit(0.1, 'cm'),

          strip.placement = "outside",
          strip.text = element_text(size = GenText_Sz, color = "black"),
          strip.background = element_rect(colour=NA, fill=NA),
          panel.spacing = unit(0,'lines'),
          
          text = element_text(size = GenText_Sz)) +
    
    guides(fill = guide_legend(ncol=1,byrow = TRUE)) +
    
    scale_fill_manual(name="Plant Type",values=colours8,drop = TRUE,limits = force) +
    scale_pattern_manual(name="Plant Type",values=Patterns8,drop = TRUE,limits = force) +
    
    scale_y_continuous(expand = c(0, 0),limits=c(0,mxc),labels=comma) +
    scale_x_discrete(expand = c(0.3, 0.3))+
    
    labs(y = "Capacity Added (MW)") 
  
  
}

################################################################################
## FUNCTION: Total_Gen_COMPARE
## Plots total study generation.
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
Total_Gen_COMPARE <- function(name_type) {
  
  # Filter emissions data
  DataGen <- ResGrYr %>%
    rename(Scenario=Sim_Name)%>%
    compare_rename(.,name_type)%>%
    mutate(Scenario=as.factor(Scenario))%>%
    group_by(Scenario,Plant_Type)%>%
    summarise(Totalgen = sum(Output_MWH)/10^6)
  
  #Max Units Built
  max_groups <-DataGen %>%group_by(Scenario)%>%
    summarise(max = sum(Totalgen))
  mxc <- round_any(max(max_groups$max)+51,100,f=ceiling)

  #Plot data
  ggplot(DataGen,aes(x=Totalgen, y=Scenario)) +
    geom_bar(aes(fill = Plant_Type),
                     position="stack", stat="identity",color="black") +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),  
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = GenText_Sz+6, vjust=0),
          panel.background = element_rect(fill = "transparent"),
          axis.text.x=element_text(angle=0,vjust = 0.5, hjust = 0.5,color="black"),
          axis.text.y=element_text(color="black"),
          plot.title = element_blank(),
          legend.justification = c(0.5,0.5),
          legend.position = ("right"),
          legend.text = element_text(size = GenText_Sz-6),
          legend.title=element_blank(), 
          #legend.key.size = unit(1,"lines"),
          text = element_text(size = GenText_Sz)) +
    
    guides(fill = guide_legend(ncol=1)) +
    
    scale_fill_manual(name="Plant Type",values=colours3b,drop = TRUE,limits = force) +
    
    scale_x_continuous(expand = c(0, 0),limits = c(0, mxc),breaks=seq(0, mxc,by=250),labels=comma) +
    
    labs(x = "Total Generation (TWh)") 
  
  
}

################################################################################
## FUNCTION: Annual_Gen_COMPARE
## Plots annual generation.
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
Annual_Gen_COMPARE <- function(name_type) {
  
  # Filter data
  Gendata <- ResGrYr %>%
    compare_rename(.,name_type)%>%
    mutate(Ptype = as.character(Plant_Type),
           Ptype=if_else(Ptype %in% c("Storage - Compressed Air","Storage - Pumped Hydro","Storage - Battery"),"Storage",Ptype),
           Scenario=as.factor(Scenario))%>%
    group_by(Year,Scenario,Ptype) %>%
    summarise(Output_MWH = sum(Output_MWH))
    

   # Imports
    netimport <- Zone %>%
      compare_rename(.,name_type)%>%
      group_by(Scenario,Year)%>%
      summarise(Ptype = "Net Imports",
                Output_MWH = sum(Imports_Total-Exports_Total))
    
    DataGen <- rbind(Gendata,netimport)
  
    # Aggregate
    DataGen <-DataGen %>%
      mutate(report_group = if_else(between(Year,2023,2025),"2023 - 2025",
                             if_else(between(Year,2026,2030),"2026 - 2030",
                                     if_else(between(Year,2031,2035),"2031 - 2035",
                                             if_else(between(Year,2036,2040),"2036 - 2040",
                                                     if_else(between(Year,2041,2045),"2041 - 2045","na"))))))%>%
      group_by(report_group,Scenario,Ptype) %>%
      summarise(Output_TWh = sum(Output_MWH)/10^6) %>%
      ungroup() 
      
  #Max Units Built
  max_groups <-DataGen %>%group_by(Scenario,report_group)%>%
    summarise(max = sum(Output_TWh),
              min = sum(Output_TWh[Output_TWh<0]))
  mxc <- round_any(max(max_groups$max)+30,50,f=ceiling)
  mnc <- round_any(min(max_groups$min),50,f=floor)
  
  
  # Order resources
  # Set levels to each category in order specified
  DataGen$Ptype <- factor(DataGen$Ptype, levels=c("Storage","Net Imports","Solar","Wind", "Other", "Hydro", 
                                      "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                      "Blended  Simple Cycle","Blended  Combined Cycle",
                                      "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                                      "Coal-to-Gas", 
                                      "Coal", "Cogeneration"))
  
  #Plot data
  ggplot(DataGen,aes(x=Scenario, y=Output_TWh)) +
    geom_bar(aes(fill = Ptype),
             position="stack", stat="identity",width=1,color="black",size=0.5)+
    
    facet_wrap(~report_group, strip.position = "bottom",nrow = 1) +
    theme_classic() +
    
    geom_hline(yintercept =0) +
  
    theme(text=element_text(family=Plot_Text)) +
    
    theme(
      #panel.grid = element_blank(),  
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_text(size = GenText_Sz+6, vjust=0),
      panel.background = element_rect(fill = "transparent"),
      axis.text.x=element_text(angle=90,vjust = 0.5, hjust = 1,color="black",size = GenText_Sz-10),
      axis.text.y=element_text(color="black"),
      plot.title = element_blank(),
      
      legend.justification = c(0.5,0.5),
      legend.key.size = unit(0.3, "cm"),
      legend.position = ("right"),
      legend.text = element_text(size = GenText_Sz-12),
      legend.title=element_blank(), 
      legend.spacing.y = unit(0.1, 'cm'),
      
      strip.placement = "outside",
      strip.text = element_text(size = GenText_Sz, color = "black"),
      strip.background = element_rect(colour=NA, fill=NA),
      panel.spacing = unit(0,'lines'),
      
      text = element_text(size = GenText_Sz)) +
    
    guides(fill = guide_legend(ncol=1,byrow = TRUE)) +
    
    scale_fill_manual(name="Plant Type",values=colours3b,drop = TRUE,limits = force) +

    scale_y_continuous(expand = c(0, 0),limits=c(mnc,mxc),labels=comma) +
    scale_x_discrete(expand = c(0.3, 0.3))+
    
    labs(y = "Generation (TWh)") 
  
  
}

################################################################################
## FUNCTION: Total_Gen_Treemap_COMPARE
## Plots total study generation using percentages
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
Total_Gen_Treemap_COMPARE <- function(name_type,cogen_include) {
  
  # Filter emissions data
  if (!cogen_include=="Y"){
    DataGen <- ResGrYr %>%
      filter(!Plant_Type == "Cogeneration")
    name_add = "- Cogen Removed"
  }else{
    DataGen <- ResGrYr
    name_add=""
  }
  
  DataGen <- DataGen %>%
    rename(Scenario=Sim_Name)%>%
    compare_rename(.,name_type)%>%
    mutate(Scenario=as.factor(Scenario))%>%
    mutate(P_group=if_else(Plant_Type %in% c("Wind","Solar","Hydro"),"Renewables",
                           if_else(Plant_Type %in% c("Coal-to-Gas","Natural Gas Combined Cycle","Natural Gas Simple Cycle","Cogeneration"),"Natural Gas",
                                   if_else(Plant_Type %in% c("Storage - Compressed Air","Storage - Pumped Hydro","Storage - Battery"),"Storage",
                                           if_else(Plant_Type %in% c('Other'),"Other", 
                                              if_else(Plant_Type %in% c("Coal"),"Coal",
                                                if_else(Plant_Type %in% c("Hydrogen Combined Cycle","Hydrogen Simple Cycle"),"Hydrogen",
                                                    if_else(Plant_Type %in% c("Natural Gas Combined Cycle + CCS"),"Abated Natural Gas","UNKNOWN"))))))))%>%
    group_by(Scenario,P_group)%>%
    # THIS FILTERS OUT STORAGE
    filter(Output_MWH>0)%>%
    summarise(Totalgen = sum(Output_MWH)/10^6)%>%
    ungroup()%>%
    group_by(Scenario)%>%
    mutate(perc_gen=round(100*Totalgen/sum(Totalgen),0),
           label_gr = if_else(perc_gen>1,paste(P_group,perc_gen,'%'),paste(P_group,"<1%")))%>%
    ungroup()
    
  #Plot data
  ggplot(DataGen,aes(area=Totalgen,fill = P_group,label=label_gr)) +
    geom_treemap(color="black")+
    geom_treemap_text(colour = "white",
                      place = "centre",grow = FALSE) +
    facet_wrap(.~Scenario, 
               #ncol = 1
               ) +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),  
          panel.background = element_rect(fill = "transparent"),
          text = element_text(size = GenText_Sz),
          legend.text = element_text(size = GenText_Sz-25),
          legend.title = element_blank(), 
          legend.position = "bottom",
          legend.key.size = unit(0.3, "cm"),
          plot.title = element_text(size = GenText_Sz-16, hjust = 0.5),
          
    # Facet grids
    strip.text = element_text(size = GenText_Sz-16, color = "black"),
    strip.background = element_rect(colour=NA, fill=NA),
    panel.border = element_rect(fill = NA, color = "black")) +

    guides(fill = guide_legend(nrow=1)) +
    scale_fill_manual(values=colorsgroup_1,drop=TRUE,limits = force) +
    # Title
    labs(title = paste("Cummulative Study Generation",name_add)) 
      
  
}

################################################################################
## FUNCTION: Year_Gen_COMPARE
## Plots generation for a single year.
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
Year_Gen_COMPARE <- function(name_type,year_look,show_neg) {
  
  # Filter emissions data
  DataGen <- ResGrYr %>%
    rename(Scenario=Sim_Name)%>%
    compare_rename(.,name_type)%>%
    filter(Year == year_look) %>%
    mutate(Ptype = as.character(Plant_Type),
           Ptype=if_else(Ptype %in% c("Storage - Compressed Air","Storage - Pumped Hydro","Storage - Battery"),"Storage",Ptype))%>%
    mutate(Scenario=as.factor(Scenario))%>%
    group_by(Scenario,Ptype)%>%
    summarise(Totalgen = Output_MWH/10^6)
  
  if (show_neg == "Y"){
    # import
    netimport <- Zone %>%
      compare_rename(.,name_type)%>%
      filter(Year == year_look) %>%
      group_by(Scenario)%>%
      summarise(Ptype = "Net Imports",
                Totalgen = sum(Imports_Total-Exports_Total)/10^6)
    
    DataGen <- rbind(DataGen,netimport)
    
    #Max Units Built
    max_groups <-DataGen %>%group_by(Scenario)%>%
      summarise(max = sum(Totalgen[Totalgen>0]),
                Min = sum(Totalgen[Totalgen<0]))
    mxc <- round_any(max(max_groups$max)+5,10,f=ceiling)
    mnc <- round_any(min(max_groups$Min)-2.6,5,f=floor)
  } else{
    #Max Units Built
    max_groups <-DataGen %>%group_by(Scenario)%>%
      summarise(max = sum(Totalgen[Totalgen>0]))
    mxc <- round_any(max(max_groups$max)+5,10,f=ceiling)
    mnc <- 0
  }

  #Plot data
  ggplot(DataGen,aes(x=Totalgen, y=Scenario)) +
    geom_bar(aes(fill = Ptype),
             position="stack", stat="identity",color="black") +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),  
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = GenText_Sz+6, vjust=0),
          panel.background = element_rect(fill = "transparent"),
          axis.text.x=element_text(angle=0,vjust = 0.5, hjust = 0.5,color="black"),
          axis.text.y=element_text(color="black"),
          plot.title = element_blank(),
          legend.justification = c(0.5,0.5),
          legend.position = ("right"),
          legend.text = element_text(size = GenText_Sz-6),
          legend.title=element_blank(), 
          #legend.key.size = unit(1,"lines"),
          text = element_text(size = GenText_Sz)) +
    
    guides(fill = guide_legend(ncol=1)) +
    
    scale_fill_manual(name="Plant Type",values=colours3b,drop = TRUE,limits = force) +
    
    scale_x_continuous(expand = c(0, 0),limits = c(mnc, mxc),breaks=seq(mnc, mxc,by=10),labels=comma) +
    
    labs(x = paste(year_look,"Total Generation (TWh)")) +
    geom_vline(xintercept =0)
  
}

################################################################################
## FUNCTION: Cost_Cum_COMPARE
## Plots generation for a single year.
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
Cost_Cum_COMPARE <- function(name_type) {
  
  # ASSUMPTIONS
  #   Capital - Based on annualized capital cost
  #   Fuel - Fuel cost for standard resources, charging cost for storage
  #   Emissions - Net emissions cost (Based on pay and credits)
  Costs_all <- ResGrYr %>%
    compare_rename(.,name_type)%>%
    group_by(Scenario)%>%
    summarise("CAPEX"=sum(CAPEX)/1000000,
              "VOM" = (sum(Variable_OM_Cost)+sum(Misc_Costs))/1000000,
              "FOM" = sum(Fixed_OM_Cost)/1000000,
              "Emissions Net" = sum(Emissions_Cost)/1000000,
              #OPEX_B=sum(OPEX)/1000000,
              "Fuel" = sum(Total_Fuel_Cost)/1000000,
              "Storage Charging" =sum(Storage_Charging_Cost)/1000000,                                  
              #Total_B = sum(Total_Cost)/1000000,
              #Value_B = sum(Value)/1000000,
              #Revenue_B = sum(Revenue)/1000000,
              #sum_check=Fuel_B+Emissions_B+Capital_B+VOM_M+FOM_B
    )
  
  #Plot max
  mxc <- 125
  GenText_Sz <-GenText_Sz-4
  
  # Get scenarios input
  num_scn <- nrow(Costs_all)
  df_list <- list()
  counter <- 1
  
  # Create each plot
  for (scn in Costs_all$Scenario) {

    # Fitler data
    Costs_temp = melt(Costs_all,id='Scenario')%>%
      filter(Scenario==scn)%>%
      select(.,c(variable,value))%>%
      mutate(value=round(value,2))
    
    # Generate plot
    plot_temp<- waterfall(Costs_temp,aes(values=value,labels =variable),
                            fill_by_sign = FALSE, 
                            fill_colours = c("#003399","#4472C4",'#515151','#767171','#cc79a7','gray80'),
                            total_rect_color ="black",
                            calc_total = TRUE) +
      theme_bw() + 
      theme(text=element_text(family=Plot_Text)) +
      theme(panel.grid = element_blank(),  
            
            axis.title.x = element_text(size = GenText_Sz-26, vjust=0),
            panel.background = element_rect(fill = "transparent"),
            axis.text.x=element_text(angle=90,vjust = 0.5, hjust = 1,color="black",size = GenText_Sz-30),
            plot.title = element_blank(),
            text = element_text(size = GenText_Sz),
            panel.grid.major.y = element_line(size=0.25,linetype=2,color = 'gray70'),
            strip.background = element_rect(colour=NA, fill=NA)) +
      
      scale_y_continuous(expand = c(0, 0),limits=c(0,mxc),breaks=pretty_breaks(6),labels=comma)
      
      if (counter==1){
        plot_temp <- plot_temp +
        theme(axis.title.y = element_text(size = GenText_Sz-26, vjust=0),
              axis.text.x=element_text(angle=90,vjust = 0.5, hjust = 1,color="black",size = GenText_Sz-30),
              axis.text.y=element_text(color="black",size = GenText_Sz-30)) +
        labs(y="Total Cost ($B)", x=paste(scn)) 
          
      }else{
        plot_temp <- plot_temp +
          theme(axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank() ) +
          labs(x=paste(scn)) 
          
      }
      
      
    # Save plot
    df_list[[counter]]<-plot_temp
    counter = counter+1
  }
  
  grid.arrange(grobs = df_list, ncol = num_scn,align="v", axis = "l")
  
}
  
################################################################################
## FUNCTION: AnnualCost_Cum_COMPARE
## Plots annual average emissions in cummulative bar chart.
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
AnnualCost_Cum_COMPARE <- function(name_type,emissions_include) {
  
  # Plot color
  if (name_type == "l"){
    scenario_colors<-sn_colors2_l
  }else{
    scenario_colors<-sn_colors2_s
  }
  
  # Filter emissions data
  if (emissions_include=="N"){
    # Values in billions
    Sim <- ResGrYr %>%
      compare_rename(.,name_type)%>%
      group_by(Scenario,Year)%>%
      summarise(Capital_Costs=sum(CAPEX)/1000000,
                Operational_Costs=(sum(OPEX)-sum(Emissions_Cost))/1000000,
      ) %>%
      ungroup() %>%
      group_by(Scenario)%>%
      summarise(Year,
                OPEX = cumsum(Operational_Costs),
                CAPEX = cumsum(Capital_Costs))
    add_note<-"Emission costs / profits removed"
  }else{
    Sim <- ResGrYr %>%
      compare_rename(.,name_type)%>%
      group_by(Scenario,Year)%>%
      summarise(Capital_Costs=sum(CAPEX)/1000000,
                Operational_Costs=sum(OPEX)/1000000,
                #"Emissions Net" = sum(Emissions_Cost)/1000000,
                #"Fuel" = sum(Total_Fuel_Cost)/1000000,
                # Total_Costs = sum(Total_Cost)/1000000,
                #Value_B = sum(Value)/1000000,
                #Revenue_B = sum(Revenue)/1000000,
      ) %>%
      ungroup() %>%
      group_by(Scenario)%>%
      summarise(Year,
                OPEX = cumsum(Operational_Costs),
                CAPEX = cumsum(Capital_Costs))
    add_note<-""
  }
  
  # Make one column
  Cost_T <- melt(Sim,id=c("Scenario","Year"))
  
  # Get plot max/mins
  YearMX<-max(Sim$Year)
  YearMN<-min(Sim$Year)
  
  upplim_df <- Cost_T %>%
    group_by(Year,Scenario)%>%
    summarise(Total_C=sum(value))
  
  Upplim <- round_any(max(upplim_df$Total_C)+11,25)
  
  # Filter by breaks
  nbreaks=1
  plot_breaks = seq(YearMN,YearMX,by=nbreaks)
  Sim <- Sim %>%
    filter(Year %in% plot_breaks)
  
  # Plot
  ggplot(Cost_T) +
    geom_bar(aes(x = Year, y = value, fill=variable), 
             size = 1.25,stat="identity",position = "stack") +
    facet_grid(~Scenario) +
    theme_bw() +
    theme(text=element_text(family=Plot_Text)) +
    theme(axis.text = element_text(color="black"),
          axis.title = element_text(size = GenText_Sz+6),
          axis.text.x = element_text(angle = 90, hjust=0,vjust=0.5,color="black"),
          plot.title = element_blank(),
          text = element_text(size=GenText_Sz),
          axis.title.x=element_blank(),
          legend.text = element_text(size = GenText_Sz-6),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = "right",
          panel.grid.major.y = element_line(size=0.25,linetype=2,color = 'gray70'),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          
          strip.placement = "outside",
          strip.text = element_text(size = GenText_Sz, color = "black"),
          strip.background = element_rect(colour=NA, fill=NA),
          panel.spacing = unit(1.5,'lines')
    ) +
    labs(y = "Cummulative Cost ($B)", x="Year",caption=add_note) +
    
    scale_fill_manual(values = c("CAPEX"="#4472C4","OPEX"='gray80')) +

    scale_x_continuous(breaks=seq(YearMN, YearMX, nbreaks), expand = c(0,0)) +
    
    scale_y_continuous(expand=c(0,0),limits=c(0,Upplim),n.breaks = 6)
  
}