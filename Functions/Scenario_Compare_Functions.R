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
    input_name <-c("Draft CER","Current Policy","Emissions Limit","TIER 2050")
  }else{
    input_name<-c("CER","CP","EL","TIER2050")
  }
  
  data <- data %>%
    mutate(Scenario = if_else(grepl("CER_",Scenario)==TRUE,input_name[1],
                              if_else(grepl("CP_",Scenario)==TRUE,input_name[2],
                                      if_else(grepl("EL_",Scenario)==TRUE,input_name[3],
                                        if_else(grepl("TIER2050_",Scenario)==TRUE,input_name[4],"unknown")))))
  
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
    scale_linetype_manual(name="Guide1",values = scenario_lines,drop=TRUE)+
    scale_colour_manual(name="Guide1",values = scenario_colors,drop = TRUE) +
    
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
    scale_linetype_manual(name="Guide1",values = scenario_lines,drop=TRUE)+
    scale_colour_manual(name="Guide1",values = scenario_colors,drop = TRUE) +
    
    scale_x_continuous(expand=c(0,0),limits = c(YearMN-0.2,YearMX+0.2),breaks=seq(YearMN, YearMX, 2)) +
    
    scale_y_continuous(expand=c(0,0),limits=c(0,Upplim),n.breaks = 6)
  
}

################################################################################
## FUNCTION: AnnualEm_Cum_COMPARE
## Plots annual average emissions
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
    scale_fill_manual(name="Guide1",values = scenario_colors,drop = TRUE) +
    
    scale_x_continuous(limits = c(YearMN-0.2,YearMX+0.2),breaks=seq(YearMN, YearMX, 2)) +
    
    scale_y_continuous(expand=c(0,0),limits=c(0,Upplim),n.breaks = 6)
  
}

################################################################################
## FUNCTION: Total_Cap_COMPARE
## Plots annual average emissions
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
## FUNCTION: Total_Gen_COMPARE
## Plots annual average emissions
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
## FUNCTION: Total_Gen_Treemap_COMPARE
## Plots annual average emissions
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
  }else{
    DataGen <- ResGrYr
  }
  
  DataGen <- DataGen %>%
    rename(Scenario=Sim_Name)%>%
    compare_rename(.,name_type)%>%
    mutate(Scenario=as.factor(Scenario))%>%
    mutate(P_group=if_else(Plant_Type %in% c("Wind","Solar","Hydro"),"Renewables",
                           if_else(Plant_Type %in% c("Coal-to-Gas","Natural Gas Combined Cycle","Natural Gas Simple Cycle","Cogeneration"),"Natural Gas",
                                   if_else(Plant_Type %in% c("Storage - Compressed Air","Storage - Pumped Hydro","Storage - Battery"),"Storage",
                                           if_else(Plant_Type %in% c("Coal",'Other'),"Coal & Other", 
                                                if_else(Plant_Type %in% c("Hydrogen Combined Cycle","Hydrogen Simple Cycle"),"Hydrogen",
                                                    if_else(Plant_Type %in% c("Natural Gas Combined Cycle + CCS"),"Abated Natural Gas","UNKNOWN")))))))%>%
    group_by(Scenario,P_group)%>%
    summarise(Totalgen = sum(Output_MWH)/10^6)%>%
    ungroup()%>%
    group_by(Scenario)%>%
    mutate(perc_gen=round(100*Totalgen/sum(Totalgen),0),
           label_gr = paste(P_group,perc_gen,'%'))%>%
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
          legend.position = "none",
          plot.title = element_text(size = GenText_Sz-16, hjust = 0.5),
          
    # Facet grids
    strip.text = element_text(size = GenText_Sz-16, color = "black"),
    strip.background = element_rect(colour=NA, fill=NA),
    panel.border = element_rect(fill = NA, color = "black")) +

    scale_fill_manual(values=colorsgroup_1) +
    # Title
    labs(title = "Cummulative Study Generation") 
      
  
}

################################################################################
## FUNCTION: Year_Gen_COMPARE
## Plots annual average emissions
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
    
    labs(x = "Total Generation (TWh)") +
    geom_vline(xintercept =0)
  
}
