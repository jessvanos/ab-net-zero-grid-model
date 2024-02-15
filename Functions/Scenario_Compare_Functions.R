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
    input_name <-c("Draft CER","Current Policy","Emissions Limit")
  }else{
    input_name<-c("CER","CP","EL")
  }
  
  data <- data %>%
    mutate(Scenario = if_else(grepl("CER_",Scenario)==TRUE,input_name[1],
                              if_else(grepl("CP_",Scenario)==TRUE,input_name[2],
                                      if_else(grepl("EL_",Scenario)==TRUE,input_name[3],"unknown"))))
  
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