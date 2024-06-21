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
    input_name <-c("Draft CER","Current Policy","Emissions Limit", # 1,2,3
                   "TIER 2050","TIER 2035",                        # 4,5
                   "Current Policy No ITCs","Draft CER No ITCs",                   # 6,7
                   "Absolute Zero","No H2 Absolute Zero",          # 8,9
                   "No Emissions Credits", "50% Credit Value",         # 10,11
                   "CP_txmod","CER_txmod","EL_txmod",              # 12,13,14
                   "Increased Transmission","No CCUS",              # 15, 16
                   "30% Credit Value", "70% Credit Value")               # 17, 18
  }else{
    input_name<-c("CER","CP","EL",
                  "TIER2050","TIER2035",
                  "noITCs","CERnoITCs",
                  "AZ","AZ_noH2",
                  "noEPCs", "50EPC",
                  "CP_txmod","CER_txmod","EL_txmod",
                  "CP_2TX","no_CCUS",
                  "30EPC","70EPC")
  }
  

  # Rename if needed to make up for poor initial coding
  if (!"Scenario" %in% colnames(data)){
    data <-data %>%
      rename(Scenario=Sim_Name)
  }
  
  data <- data %>%
    mutate(Scenario = if_else(grepl("CER_02",Scenario)==TRUE,input_name[1],
                              if_else(grepl("CP_04",Scenario)==TRUE,input_name[2],
                                      if_else(grepl("EL_25",Scenario)==TRUE,input_name[3],
                                        if_else(grepl("TIER2050_",Scenario)==TRUE,input_name[4],
                                                if_else(grepl("TIER2035",Scenario)==TRUE,input_name[5],
                                                        if_else(grepl("CP_noITC",Scenario)==TRUE,input_name[6],
                                                                if_else(grepl("CER_noITC",Scenario)==TRUE,input_name[7],
                              if_else(grepl("AZ_",Scenario)==TRUE,input_name[8],
                                      if_else(grepl("AZ_noH2",Scenario)==TRUE,input_name[9],
                                              if_else(grepl("CP_noEPC_",Scenario)==TRUE,input_name[10],
                                                      if_else(grepl("CP_50EPCs",Scenario)==TRUE,input_name[11],
                                                              if_else(grepl("CP_12",Scenario)==TRUE,input_name[12],
                                      if_else(grepl("CER_14",Scenario)==TRUE,input_name[13],
                                              if_else(grepl("EL_19",Scenario)==TRUE,input_name[14],
                                                      if_else(grepl("CP_2tx",Scenario)==TRUE,input_name[15],
                                                              if_else(grepl("CP_noCCS",Scenario)==TRUE,input_name[16],
                                      if_else(grepl("CP_30EPCs",Scenario)==TRUE,input_name[17],
                                              if_else(grepl("CP_70EPCs",Scenario)==TRUE,input_name[18],"unknown")))))))))))))))))))
  
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
    cp_case = "Current Policy"
  }else{
    scenario_colors<-sn_colors_s
    scenario_lines<-sn_line_s
    cp_case = "CP"
    
  }
  
  # Include AESO data
  if (AESO_include=="Y"){
      # Define AESO data (from market statistcs reports)
      AESO_Year <-c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023)
      AESO_pool <- c(70.36,80.79,66.95,89.95,47.81,50.88,76.22,64.32,80.19,49.42,33.34,18.28,22.19,50.35,54.88,46.72,101.93,162.46,133.63)
      AESO_Zone <-data.frame(Year =AESO_Year,
                             Avg_Price =AESO_pool,
                             Scenario = "Historic")
      # Merge result
      all_pool <- rbind(Sim,AESO_Zone) %>%
        mutate(Scenario = as.factor(Scenario))
      all_pool$Scenario<-fct_relevel(all_pool$Scenario, cp_case)
      all_pool$Scenario<-fct_relevel(all_pool$Scenario, "Historic")

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
          axis.title = element_text(size = GenText_Sz+6,famil=Plot_Text_bf),
          axis.text.x = element_text(angle = 0, hjust=0.5,color="black"),
          plot.title = element_blank(),
          text = element_text(size=GenText_Sz),
          axis.title.x=element_blank(),
          legend.text = element_text(size = GenText_Sz-6),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = c(0.9,0.88),
          #legend.position = "right",
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.y = element_line(size=0.25,linetype=2,color = 'gray70'),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(y = "Average Annual Pool Price ($/MWh)", x="Year",colour="Condition",linetype="Condition") +
    
    #scale_colour_grey() +
    scale_linetype_manual(name="Guide1",values = scenario_lines,drop=TRUE,limits = force)+
    scale_colour_manual(name="Guide1",values = scenario_colors,drop = TRUE,limits = force) +
    
    scale_x_continuous(expand=c(0,0),limits = c(YearMN-1,YearMX+1),breaks=seq(YearMN, YearMX, 5)) +
    
    scale_y_continuous(expand=c(0,0),limits=c(0,Upplim),n.breaks = 5, 
    )
    
}

################################################################################
## FUNCTION: AvgYr_price_COMPARE2
## Plots annual average pool price, with optional historical
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
AvgYr_price_COMPARE2 <- function(name_type,AESO_include) {
  
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
    theme(text=element_text(size=GenText_Sz,family=Plot_Text),
          axis.text = element_text(color="black"),
          axis.text.x = element_text(angle = 0, hjust=0.5,color="black"),
          plot.title = element_blank(),
          axis.title.x=element_blank(),
          legend.text = element_text(size = GenText_Sz-8),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = c(0.84,0.97),
          panel.background = element_rect(fill = "transparent"),
          #panel.grid.major.y = element_line(size=0.25,linetype=2,color = 'gray70'),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(y = "Average Annual Pool Price ($/MWh)", x="Year",colour="Condition",linetype="Condition") +
    
    #scale_colour_grey() +
    scale_linetype_manual(name="Guide1",values = scenario_lines,drop=TRUE,limits = force)+
    scale_colour_manual(name="Guide1",values = scenario_colors,drop = TRUE,limits = force) +
    
    scale_x_continuous(expand=c(0,0),limits = c(YearMN-1,YearMX+1),breaks=seq(YearMN, YearMX, 5)) +
    
    scale_y_continuous(expand=c(0,0),limits=c(0,Upplim),n.breaks = 8, 
    ) +
    
    theme(axis.title.y = element_text(size = GenText_Sz+6, family=Plot_Text_bf))
  
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
AnnualEm_COMPARE <- function(name_type,cogen_include,bw_color=FALSE) {
  
  # Plot color
  if (name_type == "l"){
    scenario_colors<-sn_colors2_l
    scenario_lines<-sn_line2_l

    # ITC
    #scenario_colors<-sn_col_ITC
    #scenario_lines<-sn_line_ITC
    
    # CP
    #scenario_colors<-sn_colors_CP
    #scenario_lines<-sn_line2_CP
    
  }else{
    scenario_colors<-sn_colors2_s
    scenario_lines<-sn_line2_s
  }
  
  # Overwrite for EPC
  if (bw_color==TRUE){
    scenario_colors<-EPC_colors
  }
  
  # Filter emissions data
  if (cogen_include=="Y"){
    # Filter and prepare Simulation data
    Sim <- Zone %>%
      mutate(Year=as.numeric(Year))%>%
      select(.,c(Year,Emissions,Scenario))%>%
      compare_rename(.,name_type)%>%
      mutate(Scenario=as.factor(Scenario)) %>%
      arrange(Emissions)
    add_note<-""
  }else{
    Sim <- Zone %>%
      mutate(Year=as.numeric(Year))%>%
      select(.,c(Year,NonCogen_Emissions,Scenario))%>%
      compare_rename(.,name_type)%>%
      rename(Emissions=NonCogen_Emissions)%>%
      mutate(Scenario=as.factor(Scenario)) %>%
      arrange(Emissions)
    
    add_note<-"Cogeneration emissions excluded"
  }
  
  # Get plot max/mins
  YearMX<-max(Sim$Year)
  YearMN<-min(Sim$Year)
  Upplim <- round_any(max(Sim$Emissions)+3,5)
  
 # try(Sim$Scenario<-fct_relevel(Sim$Scenario, "No Emissions Credits"))
 # try(Sim$Scenario<-fct_relevel(Sim$Scenario, "Current Policy",after=Inf))
  
  # Plot
  ggplot(Sim) +
    geom_line(aes(x = Year, y = Emissions, colour = Scenario,linetype= Scenario,#size=Scenario
                  ), 
              size = 1.25
              ) +
    theme_bw() +
    theme(text=element_text(family=Plot_Text)) +
    theme(axis.text = element_text(color="black",size=GenText_Sz),
          axis.title = element_text(size = GenText_Sz+6,family=Plot_Text_bf),
          axis.text.x = element_text(angle = 0, hjust=0.5,color="black",size=GenText_Sz),
          plot.title = element_blank(),
          plot.margin =unit(c(0.5, 0.5, 0.5, 0.5),"cm"), 
          text = element_text(size=GenText_Sz),
          axis.title.x=element_blank(),
          legend.text = element_text(size = GenText_Sz-6),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",#c(0.87,0.88),
          #panel.grid.major.y = element_line(size=0.25,linetype=2,color = 'gray70'),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          plot.caption = element_text(family = Plot_Text_it,size=GenText_Sz-6),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(y = expression("Annual GHG Emissions (Mt"[CO2e]*")"), x="Year",caption=add_note) +
    
    #scale_colour_grey() +
    scale_linetype_manual(name="Guide1",values = scenario_lines,drop=TRUE,limits = force)+
    scale_colour_manual(name="Guide1",values = scenario_colors,drop = TRUE,limits = force) +
    #scale_size_manual(name="Guide1",values = sn_size_CP,drop = TRUE,limits = force) +
    
    scale_x_continuous(expand=c(0,0),limits = c(YearMN-0.2,YearMX+0.2),breaks=seq(YearMN, YearMX, 2)) +
    
    scale_y_continuous(expand=c(0,0),limits=c(0,18),breaks = pretty_breaks(8),labels = function(x) sprintf("%.1f", x))
  
}

################################################################################
## FUNCTION: AnnualEm_Cum_COMPARE
## Plots annual average emissions in Cumulative bar chart.
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
      reframe(Year = Year,
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
      reframe(Year = Year,
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
  
  try(Sim$Scenario<-fct_relevel(Sim$Scenario, "No Emissions Credits"))
  
  # Plot
  ggplot(Sim) +
    geom_bar(aes(x = Year, y = Emissions, fill = Scenario), 
              size = 0.25,stat="identity",position = "dodge",color="black",width=0.8) +
    theme_bw() +
    #facet_wrap(~Scenario)
    theme(text=element_text(family=Plot_Text)) +
    theme(axis.text = element_text(color="black",size = GenText_Sz),
          axis.title = element_text(size = GenText_Sz+6,family=Plot_Text_bf),
          axis.text.x = element_text(angle = 0, hjust=0.5,color="black",size = GenText_Sz),
          plot.title = element_blank(),
          text = element_text(size=GenText_Sz),
          axis.title.x=element_blank(),
          legend.text = element_text(size = GenText_Sz),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = c(0.1,0.91),
          #panel.grid.major.y = element_line(size=0.25,linetype=2,color = 'gray70'),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.key.size = unit(0.3, "cm"),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(y = expression("Cumulative GHG Emissions (Mt"[CO2e]*")"), x="Year",caption=add_note) +
    
    #scale_colour_grey() +
    scale_fill_manual(name="Guide1",values = scenario_colors,drop = TRUE,limits = force) +
    
    scale_x_continuous(breaks=seq(YearMN, YearMX, nbreaks), expand = c(0.01,0.01)) +
    
    scale_y_continuous(expand=c(0,0),limits=c(0,Upplim),n.breaks = 8,labels = function(x) sprintf("%.0f", x))
  
}

################################################################################
## FUNCTION: AnnualEm_Cum_Dots
## Plots annual average emissions in Cumulative bar chart.
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
AnnualEm_Cum_Dots <- function(name_type,cogen_include) {
  
  # Plot color
  if (name_type == "l"){
    scenario_colors<-sn_colors2_l
    group_ITC <- c("Current Policy No ITCs")
    group_TIER <-c("TIER 2050","TIER 2035")
    group_EPC <-c("No Emissions Credits","70% Credit Value","50% Credit Value","30% Credit Value")
    group_CCS <-c("No CCUS")
    group_policy <- c("Draft CER","Draft CER No ITCs","Emissions Limit")
    
    BAU <- "Current Policy"
    
  }else{
    scenario_colors<-sn_colors2_s
    group_ITC <- c("noITCs")
    group_TIER <-c("TIER2050","TIER2035")
    group_EPC <-c("noEPCs","70EPC","50EPC","30EPC")
    group_CCS <-c("no_CCS")
    group_policy <- c("CER","CERnoITCs","EL")
    
    BAU <- "CP"
    
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
                "2045 Annual Emissions"=Emissions,
                "Cumulative Emissions"=cumsum(Emissions)) %>%
      filter(Year==2045)%>%
      mutate(subtype = if_else(Scenario %in% group_ITC, "ITC",
                               if_else(Scenario %in% group_TIER, "TIER",
                                       if_else(Scenario %in% group_EPC, "EPCs",
                                               if_else(Scenario %in% group_CCS, "CCS",
                                                       if_else(Scenario %in% group_policy, "Policy",Scenario))))))
    
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
                "2045 Annual Emissions"=Emissions,
                "Cumulative Emissions"=cumsum(Emissions)) %>%
      filter(Year==2045) %>%
      mutate(subtype = if_else(Scenario %in% group_ITC, "ITC",
                               if_else(Scenario %in% group_TIER, "TIER",
                                       if_else(Scenario %in% group_EPC, "EPCs",
                                               if_else(Scenario %in% group_CCS, "CCS",
                                                       if_else(Scenario %in% group_policy, "Policy",Scenario))))))
    
    add_note<-"Cogeneration emissions excluded"
  }
  
  em_2045 <- melt(Sim,id=c("Scenario","Year","subtype"))
  
  # Get current policy values & remove from data
  cp_2045 <- em_2045$value[em_2045$Scenario==BAU & em_2045$variable == "2045 Annual Emissions"]
  cp_cum <- em_2045$value[em_2045$Scenario==BAU & em_2045$variable == "Cumulative Emissions"]
  em_2045 <- em_2045 %>%
    filter(Scenario != BAU)
  
  # reorder
  em_2045$Scenario <- reorder(em_2045$Scenario, em_2045$value)
  
  # Plot
  ggplot(em_2045) +
    # geom_point(aes(x = Scenario, y = value, shape=Scenario),
    #          size = 2) +
    geom_point(aes(x = Scenario, y = value),
                         size = 2,shape=16) +
    theme_bw() +
    facet_wrap(~variable,scales="free_y") +
    
    scale_shape_manual(values = sn_shape_l) +
    
    geom_hline(data = subset(em_2045, variable == "2045 Annual Emissions"), 
               aes(yintercept = cp_2045,colour  = BAU), 
               linetype = "dashed") +
    geom_text(data = subset(em_2045, variable == "2045 Annual Emissions"), 
              aes(x = "No Emissions Credits", y = cp_2045, label = "Current Policy Scenario"), 
              hjust = 1,vjust=-0.5,family=Plot_Text,size=GenText_Sz-34) +
    
    geom_hline(data = subset(em_2045, variable == "Cumulative Emissions"), 
               aes(yintercept = cp_cum,colour  = BAU), 
               linetype = "dashed") +
    geom_text(data = subset(em_2045, variable == "Cumulative Emissions"), 
              aes(x = "No Emissions Credits", y = cp_cum, label = "Current Policy Scenario"), 
              hjust = 1,vjust=-0.5,family=Plot_Text,size=GenText_Sz-34) +
    
    scale_color_manual(name = NULL, values = c("black")) +
    
    theme(text=element_text(family=Plot_Text)) +
    theme(axis.text = element_text(color="black"),
          axis.title = element_text(size = GenText_Sz+6,family=Plot_Text_bf),
          axis.text.x = element_text(angle = 90, hjust=1,vjust=0.5,color="black",size = GenText_Sz-6),
          plot.title = element_blank(),
          text = element_text(size=GenText_Sz),
          axis.title.x=element_blank(),
          legend.text = element_text(size = GenText_Sz-6),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = "none",
          #panel.grid.major.y = element_line(size=0.25,linetype=2,color = 'gray70'),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          
          strip.placement = "outside",
          strip.text = element_text(size = GenText_Sz-6, color = "black"),
          strip.background = element_rect(colour=NA, fill=NA),
          #panel.spacing = unit(0,'lines'),
          
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.key.size = unit(0.3, "cm"),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(y = expression("GHG Emissions (Mt"[CO2e]*")")) +

    scale_y_continuous(expand=expansion(c(0,0.1)),limits=c(0,NA),
                       n.breaks = 8,labels = function(x) sprintf("%.1f", x))
  
}

################################################################################
## FUNCTION: Annual_Em_group
## Plots annual capacity for selected resource group
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
Annual_Em_group <- function(name_type,list_groups,shorten_names=FALSE) {
  
  years_cap = seq.int(2023, 2045) 

  # Filter data & aggregate years
  Emdata <- ResGrYr %>%
    compare_rename(.,name_type)%>%
    mutate(Scenario=as.factor(Scenario),
           Ptype = as.character(Plant_Type),
           Ptype=if_else(Ptype %in% c("Storage - Compressed Air","Storage - Pumped Hydro","Storage - Battery"),"Storage",Ptype),
           Ptype=as.factor(Ptype)) %>%
    filter(Year %in% years_cap,
           Ptype %in% list_groups) %>%
    group_by(Year,Scenario,Ptype) %>%
    summarise(Em_Mt = sum(Emissions_Tonne)/10^6) %>%
    ungroup()
  
  # Order resources
  # Set levels to each category in order specified
  Emdata$Ptype <- factor(Emdata$Ptype, levels=c("Storage","Net Imports","Solar","Wind", "Other", "Hydro", 
                                                  "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                                  "Blended  Simple Cycle","Blended  Combined Cycle",
                                                  "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                                                  "Coal-to-Gas", 
                                                  "Coal", "Cogeneration"))
  
  #Max Units Built
  max_groups <-Emdata %>%group_by(Scenario,Year)%>%
    summarise(max = sum(Em_Mt))
  mxc <- round_any(max(max_groups$max)+3,5,f=ceiling)
  
  if (shorten_names==TRUE){
    # Map short names
    new_names <- c("Cogen"="Cogeneration",
                   "H2SC"="Hydrogen Simple Cycle",
                   "NGCC"="Natural Gas Combined Cycle",
                   "NGCC+CCUS"="Natural Gas Combined Cycle + CCS",
                   "NGSC"= "Natural Gas Simple Cycle")
    
    Emdata$Ptype <- fct_recode(Emdata$Ptype, !!!new_names)
    col_scale = colours3d
    leg_txt_sz=GenText_Sz-6
  } else{
    
    # Choose color scale
    col_scale = colours3b
    leg_txt_sz=GenText_Sz-12
  }
  
  #Plot data
  ggplot(Emdata,aes(x=Year, y=Em_Mt)) +
    geom_bar(aes(fill = Ptype),
             position="stack", stat="identity",width=1,
             na.rm=TRUE, alpha=Plot_Trans,color='black') +
    facet_wrap(~Scenario, strip.position = "top",nrow = 1) +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(
      panel.grid = element_blank(),  
      axis.title.x = element_blank(),
      panel.background = element_rect(fill = "transparent"),
      axis.text.x=element_text(angle=90,vjust = 0.5, hjust = 1,color="black",size = GenText_Sz-12),
      axis.text.y=element_text(color="black"),
      plot.title = element_blank(),
      
      legend.justification = c(0.5,0.5),
      legend.key.size = unit(0.3, "cm"),
      legend.position = ("right"),
      legend.text = element_text(size = leg_txt_sz),
      legend.title=element_blank(), 
      legend.spacing.y = unit(0.1, 'cm'),
      
      strip.placement = "outside",
      strip.text = element_text(size = GenText_Sz, color = "black"),
      strip.background = element_rect(colour=NA, fill=NA),
      #panel.spacing = unit(0,'lines'),
      
      text = element_text(size = GenText_Sz)) +
    
    #guides(fill = guide_legend(ncol=1,byrow = TRUE)) +
    
    scale_fill_manual(name="Plant Type",values=col_scale,drop = TRUE,limits = force) +
    
    scale_y_continuous(expand = c(0, 0),limits=c(0,mxc),breaks = pretty_breaks(8),labels=comma) +
    scale_x_continuous(expand = c(0,0), breaks=seq(2023,2045,1))+
    
    labs(y = expression("Annual GHG Emissions (Mt"[CO2e]*")")) +
    theme(axis.title.y = element_text(size = GenText_Sz+6, vjust=0,family=Plot_Text_bf))
  
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

# Color
  col_scale = colours5b

  
  # Filter emissions data
  Builddata <- Tot_Cap %>%
      compare_rename(.,name_type)%>%
    mutate(Scenario=as.factor(Scenario),
           Ptype = as.character(Plant_Type),
           Ptype=if_else(Ptype %in% c("Storage - Compressed Air","Storage - Pumped Hydro","Storage - Battery"),"Storage",Ptype),
           Ptype=as.factor(Ptype),
           New_Cap_Total=New_Cap_Total/1000)%>%
    group_by(Scenario,Ptype)%>%
    summarise(New_Cap_Total=sum(New_Cap_Total)) %>%
    filter(New_Cap_Total>0)

  #Max Units Built
  max_groups <-Builddata %>%group_by(Scenario)%>%
    summarise(max = sum(New_Cap_Total))
  mxc <- round_any(max(max_groups$max)+3,5,f=ceiling)
  
  
  #Plot data
  ggplot(Builddata,aes(x=New_Cap_Total, y=Scenario)) +
    geom_bar_pattern(aes(pattern = Ptype, fill = Ptype),
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
          axis.title.x = element_text(size = GenText_Sz+6, vjust=0,family = Plot_Text_bf),
          panel.background = element_rect(fill = "transparent"),
          axis.text.x=element_text(angle=0,vjust = 0.5, hjust = 0.5,color="black"),
          axis.text.y=element_text(color="black"),
          plot.title = element_blank(),
          legend.justification = c(0.5,0.5),
          legend.position = ("right"),
          legend.text = element_text(size = GenText_Sz-12),
          legend.title=element_blank(), 
          #legend.key.size = unit(1,"lines"),
          text = element_text(size = GenText_Sz)) +
    
    guides(fill = guide_legend(ncol=1)) +
    
    scale_fill_manual(name="Plant Type",values=col_scale,drop = TRUE,limits = force) +
    scale_pattern_manual(name="Plant Type",values=Patterns5b,drop = TRUE,limits = force) +
    
    scale_x_continuous(expand = c(0, 0),limits = c(0, 28),breaks=seq(0, mxc,by=2),labels=comma) +
    
    labs(x = "Total Capacity Added (GW)") 

  
}

################################################################################
## FUNCTION: Total_Cap_add_ret_COMPARE
## Plots total capacity added.
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
Total_Cap_add_ret_COMPARE <- function(name_type) {
  
  # Color
  col_scale = colours5b
  
  
  # Filter emissions data
  Builddata <- Tot_Cap %>%
    compare_rename(.,name_type)%>%
    mutate(Scenario=as.factor(Scenario),
           Ptype = as.character(Plant_Type),
           Ptype=if_else(Ptype %in% c("Storage - Compressed Air","Storage - Pumped Hydro","Storage - Battery"),"Storage",Ptype),
           Ptype=as.factor(Ptype),
           New_Cap_Total=New_Cap_Total/1000)%>%
    group_by(Scenario,Ptype)%>%
    summarise(New_Cap_Total=sum(New_Cap_Total),
              Ret_cap=) %>%
    filter(New_Cap_Total>0)
  
  #Max Units Built
  max_groups <-Builddata %>%group_by(Scenario)%>%
    summarise(max = sum(New_Cap_Total))
  mxc <- round_any(max(max_groups$max)+3,5,f=ceiling)
  
  
  #Plot data
  ggplot(Builddata,aes(x=New_Cap_Total, y=Scenario)) +
    geom_bar_pattern(aes(pattern = Ptype, fill = Ptype),
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
          axis.title.x = element_text(size = GenText_Sz+6, vjust=0,family = Plot_Text_bf),
          panel.background = element_rect(fill = "transparent"),
          axis.text.x=element_text(angle=0,vjust = 0.5, hjust = 0.5,color="black"),
          axis.text.y=element_text(color="black"),
          plot.title = element_blank(),
          legend.justification = c(0.5,0.5),
          legend.position = ("right"),
          legend.text = element_text(size = GenText_Sz-12),
          legend.title=element_blank(), 
          #legend.key.size = unit(1,"lines"),
          text = element_text(size = GenText_Sz)) +
    
    guides(fill = guide_legend(ncol=1)) +
    
    scale_fill_manual(name="Plant Type",values=col_scale,drop = TRUE,limits = force) +
    scale_pattern_manual(name="Plant Type",values=Patterns5b,drop = TRUE,limits = force) +
    
    scale_x_continuous(expand = c(0, 0),limits = c(0, 28),breaks=seq(0, mxc,by=2),labels=comma) +
    
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
  
  # Choose color scale
    col_scale = colours8

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
    
    scale_fill_manual(name="Plant Type",values=col_scale,drop = TRUE,limits = force) +
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
  
  # Choose color scale
    col_scale = colours5

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
    
    scale_fill_manual(name="Plant Type",values=col_scale,drop = TRUE,limits = force) +
    scale_pattern_manual(name="Plant Type",values=Patterns5,drop = TRUE,limits = force) +
    
    scale_x_continuous(expand = c(0, 0),limits = c(mnc, mxc),breaks=pretty_breaks(8),labels=comma) +
    
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
  
  # Choose color scale
    col_scale = colours3b

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

  new_names <- c("Natural Gas Combined Cycle + CCUS"="Natural Gas Combined Cycle + CCS")
  AllData$Ptype <- fct_recode(AllData$Ptype, !!!new_names)
  
  try(AllData$Scenario<-fct_relevel(AllData$Scenario, "No Emissions Credits",after=Inf))
  
  
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
          axis.title.x = element_text(size = GenText_Sz+6, vjust=0,family = Plot_Text_bf),
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
    
    scale_fill_manual(name="Plant Type",values=col_scale,drop = TRUE,limits = force) +

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
Annual_Cap_Add_COMPARE <- function(name_type) {
  
  # Choose color scale
  col_scale = colours8
  
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
    
    scale_fill_manual(name="Plant Type",values=col_scale,drop = TRUE,limits = force) +
    scale_pattern_manual(name="Plant Type",values=Patterns8,drop = TRUE,limits = force) +
    
    scale_y_continuous(expand = c(0, 0),limits=c(0,mxc),labels=comma) +
    scale_x_discrete(expand = c(0.3, 0.3))+
    
    labs(y = "Capacity Added (MW)") 
  
  
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
  
  years_cap = c(2025,2030,2035,2040,2045)
  
  # Choose color scale
    col_scale = colours3b

  
  # Filter data & aggregate years
  Capdata <- ResGrYr %>%
    compare_rename(.,name_type)%>%
    mutate(Scenario=as.factor(Scenario),
           Ptype = as.character(Plant_Type),
           Ptype=if_else(Ptype %in% c("Storage - Compressed Air","Storage - Pumped Hydro","Storage - Battery"),"Storage",Ptype),
           Ptype=as.factor(Ptype)) %>%
    filter(Year %in% years_cap) %>%
    group_by(Year,Scenario,Ptype) %>%
    summarise(Cap_MW = sum(Capacity_MW)) %>%
    ungroup()
  
  # Order resources
  # Set levels to each category in order specified
  Capdata$Ptype <- factor(Capdata$Ptype, levels=c("Storage","Net Imports","Solar","Wind", "Other", "Hydro", 
                                                  "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                                  "Blended  Simple Cycle","Blended  Combined Cycle",
                                                  "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                                                  "Coal-to-Gas", 
                                                  "Coal", "Cogeneration"))
  
  #Max Units Built
  max_groups <-Capdata %>%group_by(Scenario,Year)%>%
    summarise(max = sum(Cap_MW))
  mxc <- round_any(max(max_groups$max)+51,100,f=ceiling)

  #Plot data
  ggplot(Capdata,aes(x=Scenario, y=Cap_MW)) +
    geom_bar(aes(fill = Ptype),
                     position="stack", stat="identity",width=1,
                     na.rm=TRUE, alpha=Plot_Trans,color='black') +
    facet_wrap(~Year, strip.position = "bottom",nrow = 1) +
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
    
    scale_fill_manual(name="Plant Type",values=col_scale,drop = TRUE,limits = force) +

    scale_y_continuous(expand = c(0, 0),limits=c(0,mxc),labels=comma) +
    scale_x_discrete(expand = c(0.3, 0.3))+
    
    labs(y = "Capacity (MW)") 
  
  
}

################################################################################
## FUNCTION: Annual_Cap_group
## Plots annual capacity for selected resource group
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
Annual_Cap_group <- function(name_type,list_groups) {
  
  years_cap = seq.int(2023, 2045) 

  # Choose color scale
    col_scale = colours3b
  
  # Filter data & aggregate years
  Capdata <- ResGrYr %>%
    compare_rename(.,name_type)%>%
    mutate(Scenario=as.factor(Scenario),
           Ptype = as.character(Plant_Type),
           Ptype=if_else(Ptype %in% c("Storage - Compressed Air","Storage - Pumped Hydro","Storage - Battery"),"Storage",Ptype),
           Ptype=as.factor(Ptype)) %>%
    filter(Year %in% years_cap,
           Ptype %in% list_groups) %>%
    group_by(Year,Scenario,Ptype) %>%
    summarise(Cap_MW = sum(Capacity_MW)) %>%
    ungroup()
  
  # Order resources
  # Set levels to each category in order specified
  Capdata$Ptype <- factor(Capdata$Ptype, levels=c("Storage","Net Imports","Solar","Wind", "Other", "Hydro", 
                                                  "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                                  "Blended  Simple Cycle","Blended  Combined Cycle",
                                                  "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                                                  "Coal-to-Gas", 
                                                  "Coal", "Cogeneration"))
  
  #Max Units Built
  max_groups <-Capdata %>%group_by(Scenario,Year)%>%
    summarise(max = sum(Cap_MW))
  mxc <- round_any(max(max_groups$max)+501,1000,f=ceiling)
  
  #Plot data
  ggplot(Capdata,aes(x=Year, y=Cap_MW)) +
    geom_bar(aes(fill = Ptype),
             position="stack", stat="identity",width=1,
             na.rm=TRUE, alpha=Plot_Trans,color='black') +
    facet_wrap(~Scenario, strip.position = "top",nrow = 1) +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(
      panel.grid = element_blank(),  
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = GenText_Sz+6, vjust=0,family=Plot_Text_bf),
      panel.background = element_rect(fill = "transparent"),
      axis.text.x=element_text(angle=90,vjust = 0.5, hjust = 1,color="black",size = GenText_Sz-12),
      axis.text.y=element_text(color="black"),
      plot.title = element_blank(),

      legend.justification = c(0.5,0.5),
      legend.key.size = unit(0.3, "cm"),
      legend.position = "none",#("right"),
      legend.text = element_text(size = GenText_Sz-12),
      legend.title=element_blank(), 
      legend.spacing.y = unit(0.1, 'cm'),
      
      strip.placement = "outside",
      strip.text = element_text(size = GenText_Sz, color = "black"),
      strip.background = element_rect(colour=NA, fill=NA),
      #panel.spacing = unit(0,'lines'),
      
      text = element_text(size = GenText_Sz)) +
    
    guides(fill = guide_legend(ncol=1,byrow = TRUE)) +
    
    scale_fill_manual(name="Plant Type",values=col_scale,drop = TRUE,limits = force) +
    
    scale_y_continuous(expand = c(0, 0),limits=c(0,mxc),breaks = pretty_breaks(8),labels=comma) +
    scale_x_continuous(expand = c(0,0), breaks=seq(2023,2045,1))+
    
    labs(y = "Capacity (MW)") 
  
}

################################################################################
## FUNCTION: Annual_Cap_group_area
## Plots annual capacity for selected resource group
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
Annual_Cap_group_area <- function(name_type,list_groups,nrg_include,EPC_rename=FALSE) {
  
  years_cap = seq.int(2023, 2045) 
  
  # Choose color scale
    col_scale = colours3b
  
  # Filter data & aggregate years
  Capdata <- ResGrYr %>%
    compare_rename(.,name_type)%>%
    mutate(Scenario=as.factor(Scenario),
           Ptype = as.character(Plant_Type),
           Ptype=if_else(Ptype %in% c("Storage - Compressed Air","Storage - Pumped Hydro","Storage - Battery"),"Storage",Ptype),
           Ptype=as.factor(Ptype)) %>%
    filter(Year %in% years_cap,
           Ptype %in% list_groups) %>%
    group_by(Year,Scenario,Ptype) %>%
    summarise(Cap_MW = sum(Capacity_MW)) %>%
    ungroup()
  
  if (nrg_include==TRUE){
    # Get NRG data if required
    capNRG <- df1a %>%
      mutate(Year = as.numeric(year(Day)))%>%
      filter(Plant_Type %in% list_groups,
             Year > 2014) %>%
      rename(Ptype=Plant_Type)%>%
      group_by(Year,Ptype) %>%
      summarise(Cap_MW = mean(capacity))
    
    # Add scenarios
    unique_scenarios <- unique(Capdata$Scenario)
    all_combinations <- expand.grid(Year = unique(capNRG$Year), Scenario = unique_scenarios)
    genNRG_expanded <- full_join(all_combinations, capNRG, by = "Year")
    
    # Combine data
    Capdata <-full_join(Capdata, genNRG_expanded, by = c("Year","Ptype","Scenario")) %>%
      mutate(Cap_MW = coalesce(Cap_MW.y, Cap_MW.x)) %>%
      select(-Cap_MW.x, -Cap_MW.y) 
    
    Yr_min <- min(Capdata$Year)
    Yr_gap <- 2
    
    # Fix gaps in area plot caused by missing 0 data
    first_years <- Capdata %>%
      group_by(Ptype, Scenario) %>%
      summarise(Year = min(Year) - 1) %>%
      filter(Year>2014-1) %>%
      mutate(Cap_MW = 0)
    
    Capdata <- bind_rows(Capdata, first_years)
    
  } else{
    Yr_min <- 2023
    Yr_gap <- 2
  }
  
  # COnvert to GW
  Capdata <- Capdata%>%
    mutate(Cap_GW=Cap_MW/10^3)
  
  # Order resources
  # Set levels to each category in order specified
  Capdata$Ptype <- factor(Capdata$Ptype, levels=c("Storage","Net Imports","Solar","Wind", "Other", "Hydro", 
                                                  "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                                  "Blended  Simple Cycle","Blended  Combined Cycle",
                                                  "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                                                  "Coal-to-Gas", 
                                                  "Coal", "Cogeneration"))
  
  
  # RENAME EPC SCENARIOS
  if (EPC_rename==TRUE){
    Capdata <- Capdata %>%
      mutate(Scenario = if_else(Scenario=="Current Policy","90%",
                                if_else(grepl('No Emissions Credits',Scenario),"0%",
                                        if_else(grepl('30%',Scenario),"30%",
                                                if_else(grepl('50%',Scenario),"50%",
                                                        if_else(grepl('70%',Scenario),"70%","unknown"))))))
    Capdata$Scenario <- factor(Capdata$Scenario, levels=c("0%","30%","50%","70%","90%"))
    line_size_plot = 0.25} 
  else{
    line_size_plot=0.5
  }
  
  # Map CCS
  new_names <- c("Natural Gas Combined Cycle + CCUS"="Natural Gas Combined Cycle + CCS")
  Capdata$Ptype <- fct_recode(Capdata$Ptype, !!!new_names)

  #Max Units Built
  max_groups <-Capdata %>%group_by(Scenario,Year)%>%
    summarise(max = sum(Cap_GW))
  mxc <- round_any(max(max_groups$max)+5,10,f=ceiling)
  
  #Plot data
  ggplot(Capdata,aes(x=Year, y=Cap_GW)) +
    geom_area(aes(fill = Ptype),size=line_size_plot,
             na.rm=TRUE, alpha=Plot_Trans,color='black') +
    facet_wrap(~Scenario, strip.position = "top",nrow = 1) +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    geom_vline(xintercept = 2023, linetype="dashed",size=0.5,color="white") +
    annotate("text", x=2022.5, y=4200, size=10,label="end of actual data", angle=90,color="white") +
    
    theme(
      panel.grid = element_blank(),  
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = GenText_Sz+6, family=Plot_Text_bf),
      panel.background = element_rect(fill = "transparent"),
      axis.text.x=element_text(angle=90,vjust = 0.5, hjust = 1,color="black",size = GenText_Sz-12),
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
      panel.spacing = unit(1,'lines'),
      
      text = element_text(size = GenText_Sz)) +
    
   # guides(fill = guide_legend(nrow=2,byrow = TRUE)) +
    
    scale_fill_manual(name="Plant Type",values=col_scale,drop = TRUE,limits = force) +
    
    scale_y_continuous(expand = c(0, 0),limits=c(0,mxc),breaks = pretty_breaks(8),labels=comma) +
    scale_x_continuous(expand = c(0,0), limits=c(Yr_min,2045),breaks=seq(Yr_min,2045,Yr_gap))+
    
    labs(y = "Capacity (GW)") 
  
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
  
  # Choose color scale
    col_scale = colours3b
  
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
    
    scale_fill_manual(name="Plant Type",values=col_scale,drop = TRUE,limits = force) +
    
    scale_x_continuous(expand = c(0, 0),limits = c(0, mxc),breaks=seq(0, mxc,by=250),labels=comma) +
    
    labs(x = "Total Generation (TWh)") 
  
  
}

################################################################################
## FUNCTION: Annual_Gen_group
## Plots annual generation for selected resource group
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
Annual_Gen_group <- function(name_type,list_groups) {
  
  years_cap = seq.int(2023, 2045) 
  
  # Choose color scale
    col_scale = colours3b
  
  # Filter data & aggregate years
  Gendata <- ResGrYr %>%
    compare_rename(.,name_type)%>%
    mutate(Scenario=as.factor(Scenario),
           Ptype = as.character(Plant_Type),
           Ptype=if_else(Ptype %in% c("Storage - Compressed Air","Storage - Pumped Hydro","Storage - Battery"),"Storage",Ptype),
           Ptype=as.factor(Ptype)) %>%
    filter(Year %in% years_cap,
           Ptype %in% list_groups) %>%
    group_by(Year,Scenario,Ptype) %>%
    summarise(Gen_MW = sum(Output_MWH)/10^6) %>%
    ungroup()
  
  # Order resources
  # Set levels to each category in order specified
  Gendata$Ptype <- factor(Gendata$Ptype, levels=c("Storage","Net Imports","Solar","Wind", "Other", "Hydro", 
                                                  "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                                  "Blended  Simple Cycle","Blended  Combined Cycle",
                                                  "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                                                  "Coal-to-Gas", 
                                                  "Coal", "Cogeneration"))
  
  #Max Units Built
  max_groups <-Gendata %>%group_by(Scenario,Year)%>%
    summarise(max = sum(Gen_MW))
  mxc <- round_any(max(max_groups$max)+5,10,f=ceiling)
  
  #Plot data
  ggplot(Gendata,aes(x=Year, y=Gen_MW)) +
    geom_bar(aes(fill = Ptype),
             position="stack", stat="identity",width=1,
             na.rm=TRUE, alpha=Plot_Trans,color='black') +
    facet_wrap(~Scenario, strip.position = "top",nrow = 1) +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(
      panel.grid = element_blank(),  
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = GenText_Sz+6, vjust=0,family=Plot_Text_bf),
      panel.background = element_rect(fill = "transparent"),
      axis.text.x=element_text(angle=90,vjust = 0.5, hjust = 1,color="black",size = GenText_Sz-12),
      axis.text.y=element_text(color="black"),
      plot.title = element_blank(),
      
      legend.justification = c(0.5,0.5),
      legend.key.size = unit(0.3, "cm"),
      legend.position = "none",#("right"),
      legend.text = element_text(size = GenText_Sz-12),
      legend.title=element_blank(), 
      legend.spacing.y = unit(0.1, 'cm'),
      
      strip.placement = "outside",
      strip.text = element_text(size = GenText_Sz, color = "black"),
      strip.background = element_rect(colour=NA, fill=NA),
      #panel.spacing = unit(0,'lines'),
      
      text = element_text(size = GenText_Sz)) +
    
    guides(fill = guide_legend(ncol=1,byrow = TRUE)) +
    
    scale_fill_manual(name="Plant Type",values=col_scale,drop = TRUE,limits = force) +
    
    scale_y_continuous(expand = c(0, 0),limits=c(0,mxc),breaks = pretty_breaks(8),labels=comma) +
    scale_x_continuous(expand = c(0,0), breaks=seq(2023,2045,1))+
    
    labs(y = "Annual Generation (TWh)") 
  
}

################################################################################
## FUNCTION: Annual_Gen_group_area
## Plots annual generation for selected resource group in stacked area.
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
Annual_Gen_group_area <- function(name_type,list_groups,nrg_include,EPC_rename=FALSE) {
  
  years_cap = seq.int(2023, 2045) 
  
  # Choose color scale
    col_scale = colours3b
  
  # Filter data & aggregate years
  Gendata <- ResGrYr %>%
    compare_rename(.,name_type)%>%
    mutate(Scenario=as.factor(Scenario),
           Ptype = as.character(Plant_Type),
           Ptype=if_else(Ptype %in% c("Storage - Compressed Air","Storage - Pumped Hydro","Storage - Battery"),"Storage",Ptype),
           Ptype=as.factor(Ptype)) %>%
    filter(Year %in% years_cap,
           Ptype %in% list_groups) %>%
    group_by(Year,Scenario,Ptype) %>%
    summarise(Gen_TWh = sum(Output_MWH)/10^6) %>%
    ungroup()
  
  if (nrg_include==TRUE){
    # Get NRG data if required
    genNRG <- df1a %>%
      mutate(Year = as.numeric(year(Day)))%>%
      filter(Plant_Type %in% list_groups,
             Year > 2014) %>%
      rename(Ptype=Plant_Type)%>%
      group_by(Year,Ptype) %>%
      summarise(Gen_TWh = sum(total_gen)/10^6)
    
    # Add scenarios
    unique_scenarios <- unique(Gendata$Scenario)
    all_combinations <- expand.grid(Year = unique(genNRG$Year), Scenario = unique_scenarios)
    genNRG_expanded <- full_join(all_combinations, genNRG, by = "Year")
    
    # Combine data
    Gendata <-full_join(Gendata, genNRG_expanded, by = c("Year","Ptype","Scenario")) %>%
      mutate(Gen_TWh = coalesce(Gen_TWh.y, Gen_TWh.x)) %>%
      select(-Gen_TWh.x, -Gen_TWh.y) 
    
    Yr_min <- min(Gendata$Year)
    Yr_gap <- 2
    
    # Fix gaps in area plot caused by missing 0 data
    first_years <- Gendata %>%
      group_by(Ptype, Scenario) %>%
      summarise(Year = min(Year) - 1) %>%
      filter(Year>2014-1) %>%
      mutate(Gen_TWh = 0)

    Gendata <- bind_rows(Gendata, first_years)

  } else{
    Yr_min <- 2023
    Yr_gap <- 2
  }
  
  # Order resources
  # Set levels to each category in order specified
  Gendata$Ptype <- factor(Gendata$Ptype, levels=c("Storage","Net Imports","Solar","Wind", "Other", "Hydro", 
                                                  "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                                  "Blended  Simple Cycle","Blended  Combined Cycle",
                                                  "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                                                  "Coal-to-Gas", 
                                                  "Coal", "Cogeneration"))
  # RENAME EPC SCENARIOS
  if (EPC_rename==TRUE){
    Gendata <- Gendata %>%
      mutate(Scenario = if_else(Scenario=="Current Policy","90%",
                                if_else(grepl('No Emissions Credits',Scenario),"0%",
                                        if_else(grepl('30%',Scenario),"30%",
                                                if_else(grepl('50%',Scenario),"50%",
                                                        if_else(grepl('70%',Scenario),"70%","unknown"))))))
    Gendata$Scenario <- factor(Gendata$Scenario, levels=c("0%","30%","50%","70%","90%"))
    line_size_plot = 0.25} 
  else{
    line_size_plot=0.5
    }
  
  # Map CCS
  new_names <- c("Natural Gas Combined Cycle + CCUS"="Natural Gas Combined Cycle + CCS")
  Gendata$Ptype <- fct_recode(Gendata$Ptype, !!!new_names)
    
  #Max Units Built
  max_groups <-Gendata %>%group_by(Scenario,Year)%>%
    summarise(max = sum(Gen_TWh))
  mxc <- round_any(max(max_groups$max)+5,10,f=ceiling)
  
  #Plot data
  ggplot(Gendata,aes(x=Year, y=Gen_TWh)) +
    geom_area(aes(fill = Ptype),na.rm=TRUE, alpha=Plot_Trans,color='black',size=line_size_plot) +
    facet_wrap(~Scenario, strip.position = "top",nrow = 1) +
    theme_bw() +
    
    geom_vline(xintercept = 2023, linetype="dashed",size=0.5,color="white") +
    annotate("text", x=2022.5, y=15, size=10,label="end of actual data", angle=90,color="white") +    
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(
      panel.grid = element_blank(),  
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = GenText_Sz+6, vjust=0,family=Plot_Text_bf),
      panel.background = element_rect(fill = "transparent"),
      axis.text.x=element_text(angle=90,vjust = 0.5, hjust = 1,color="black",size = GenText_Sz-12),
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
      panel.spacing = unit(1,'lines'),
      
      text = element_text(size = GenText_Sz)) +
    
    guides(fill = guide_legend(ncol=1,byrow = TRUE)) +
    
    scale_fill_manual(name="Plant Type",values=col_scale,drop = TRUE,limits = force) +
    
    scale_y_continuous(expand = c(0, 0),limits=c(0,mxc),breaks = pretty_breaks(8),labels=comma) +
    scale_x_continuous(expand = c(0,0),limits=c(Yr_min,2045), breaks=seq(Yr_min,2045,Yr_gap))+
    
    labs(y = "Annual Generation (TWh)") 
  
}

################################################################################
## FUNCTION: Annual_CF_group
## Plots annual capacity factor for selected resource group
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
Annual_CF_group <- function(EPC_rename=FALSE) {
  
  years_cap = seq.int(2023, 2045) 
  
  # Choose color scale
  col_scale = colours3b
  
  # Filter data & aggregate years
  CFdata <- ResGrYr %>%
    compare_rename(.,"l")%>%
    mutate(Scenario=as.factor(Scenario),
           Ptype = as.character(Plant_Type),
           Ptype=if_else(Ptype %in% c("Storage - Compressed Air","Storage - Pumped Hydro","Storage - Battery"),"Storage",Ptype),
           Ptype=as.factor(Ptype)) %>%
    filter(Year %in% years_cap,
           Ptype != "Storage") %>%
    mutate(Capacity_Factor = if_else(Capacity_Factor==0,NA,Capacity_Factor)) %>%
    arrange(Capacity_Factor)
  
  order_CFss <- CFdata %>%
    filter(Year == 2045,
           Scenario=="Current Policy") %>%
    arrange(desc(Capacity_Factor)) %>%
    pull(Ptype)
  
  # Convert 'color' column to a factor with levels ordered by final year's values
  CFdata$Ptype <- factor(CFdata$Ptype, levels = order_CFss)
  
  # RENAME EPC SCENARIOS
  if (EPC_rename==TRUE){
    CFdata <- CFdata %>%
      mutate(Scenario = if_else(Scenario=="Current Policy","90%",
                                if_else(grepl('No Emissions Credits',Scenario),"0%",
                                        if_else(grepl('30%',Scenario),"30%",
                                                if_else(grepl('50%',Scenario),"50%",
                                                        if_else(grepl('70%',Scenario),"70%","unknown"))))))
    CFdata$Scenario <- factor(CFdata$Scenario, levels=c("0%","30%","50%","70%","90%"))
    line_size_plot = 0.25} 
  else{
    line_size_plot=0.5
  }
  
  # Map CCS
  new_names <- c("Natural Gas Combined Cycle + CCUS"="Natural Gas Combined Cycle + CCS")
  CFdata$Ptype <- fct_recode(CFdata$Ptype, !!!new_names)

  try(CFdata$Scenario<-fct_relevel(CFdata$Scenario, "No Emissions Credits"))
  
  #Plot data
  ggplot(CFdata,aes(x=Year, y=Capacity_Factor)) +
    geom_line(aes(color = Ptype),na.rm=TRUE, size=1) +
    facet_wrap(~Scenario, strip.position = "top",nrow = 1) +
    theme_bw() +

    theme(text=element_text(family=Plot_Text,size = GenText_Sz)) +
    
    theme(
      panel.grid = element_blank(),  
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = GenText_Sz+6, vjust=0,family=Plot_Text_bf),
      panel.background = element_rect(fill = "transparent"),
      axis.text.x=element_text(angle=90,vjust = 0.5, hjust = 1,color="black",size = GenText_Sz-12),
      axis.text.y=element_text(color="black"),
      plot.title = element_blank(),
      
      legend.justification = c(0.5,0.5),
      legend.key.size = unit(0.3, "cm"),
      legend.position = ("right"),
      legend.text = element_text(size = GenText_Sz-6),
      legend.title=element_blank(), 
      legend.spacing.y = unit(0.1, 'cm'),
      
      strip.placement = "outside",
      strip.text = element_text(size = GenText_Sz-6, color = "black"),
      strip.background = element_rect(colour=NA, fill=NA),
      panel.spacing = unit(1,'lines')) +
      
    guides(fill = guide_legend(ncol=1,byrow = TRUE)) +
    
    scale_color_manual(name="Plant Type",values=col_scale,drop = TRUE,limits = force) +
    
    scale_y_continuous(expand = c(0, 0),limits=c(0,1),breaks = pretty_breaks(10),labels=percent) +
    scale_x_continuous(expand = c(0,0),limits=c(2023,2045), breaks=seq(2023,2045,2))+
    
    labs(y = "Average Annual Capacity Factor") 
  
}

################################################################################
## FUNCTION: Annual_Gen_group_perc
## Plots annual generation percentage for selected resource group.
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
Annual_Gen_group_perc <- function(name_type,list_groups,nrg_include) {
  
  years_cap = seq.int(2023, 2045) 
  
  # Choose color scale
    col_scale = colours3b
  
  # Filter data & aggregate years
  Gendata <- ResGrYr %>%
    compare_rename(.,name_type)%>%
    mutate(Scenario=as.factor(Scenario),
           Ptype = as.character(Plant_Type),
           Ptype=if_else(Ptype %in% c("Storage - Compressed Air","Storage - Pumped Hydro","Storage - Battery"),"Storage",Ptype),
           Ptype=as.factor(Ptype)) %>%
    filter(Year %in% years_cap,
           Ptype %in% list_groups) %>%
    group_by(Year,Scenario,Ptype) %>%
    summarise(Gen_TWh = sum(Output_MWH)/10^6) %>%
    ungroup()
  
  if (nrg_include==TRUE){
    # Get NRG data if required
    genNRG <- df1a %>%
      mutate(Year = as.numeric(year(Day)))%>%
      filter(Plant_Type %in% list_groups,
             Year > 2014) %>%
      rename(Ptype=Plant_Type)%>%
      group_by(Year,Ptype) %>%
      summarise(Gen_TWh = sum(total_gen)/10^6)
    
    # Add scenarios
    unique_scenarios <- unique(Gendata$Scenario)
    all_combinations <- expand.grid(Year = unique(genNRG$Year), Scenario = unique_scenarios)
    genNRG_expanded <- full_join(all_combinations, genNRG, by = "Year")
    
    # Combine data
    Gendata <-full_join(Gendata, genNRG_expanded, by = c("Year","Ptype","Scenario")) %>%
      mutate(Gen_TWh = coalesce(Gen_TWh.y, Gen_TWh.x)) %>%
      select(-Gen_TWh.x, -Gen_TWh.y) 
    
    Yr_min <- min(Gendata$Year)
    Yr_gap <- 2
    
    # Fix gaps in area plot caused by missing 0 data
    first_years <- Gendata %>%
      group_by(Ptype, Scenario) %>%
      summarise(Year = min(Year) - 1) %>%
      filter(Year>2014-1) %>%
      mutate(Gen_TWh = 0)
    
    Gendata <- bind_rows(Gendata, first_years) 

  } else{
    Yr_min <- 2023
    Yr_gap <- 1
  }

  # Order resources
  # Set levels to each category in order specified
  Gendata$Ptype <- factor(Gendata$Ptype, levels=c("Storage","Net Imports","Solar","Wind", "Other", "Hydro", 
                                                  "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                                  "Blended  Simple Cycle","Blended  Combined Cycle",
                                                  "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                                                  "Coal-to-Gas", 
                                                  "Coal", "Cogeneration"))
  
  #Plot data
  ggplot(Gendata,aes(x=Year, y=Gen_TWh)) +
    geom_area(aes(fill = Ptype),na.rm=TRUE, alpha=Plot_Trans,color='black',position = "fill",size=0.5,) +
    facet_wrap(~Scenario, strip.position = "top",nrow = 1) +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    geom_vline(xintercept = 2023, linetype="dashed",size=0.5,color="white") +
    annotate("text", x=2022.5, y=0.2, size=10,label="end of actual data", angle=90,color="white") +    
    
    theme(
      panel.grid = element_blank(),  
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = GenText_Sz+6, vjust=0),
      panel.background = element_rect(fill = "transparent"),
      axis.text.x=element_text(angle=90,vjust = 0.5, hjust = 1,color="black",size = GenText_Sz-12),
      axis.text.y=element_text(color="black"),
      plot.title = element_blank(),
      
      legend.justification = c(0.5,0.5),
      legend.key.size = unit(0.3, "cm"),
      legend.position = ("bottom"),
      legend.text = element_text(size = GenText_Sz-12),
      legend.title=element_blank(), 
      legend.spacing.y = unit(0.1, 'cm'),
      
      strip.placement = "outside",
      strip.text = element_text(size = GenText_Sz, color = "black"),
      strip.background = element_rect(colour=NA, fill=NA),
      panel.spacing = unit(1,'lines'),
      
      text = element_text(size = GenText_Sz)) +
    
    #guides(fill = guide_legend(ncol=1,byrow = TRUE)) +
    
    scale_fill_manual(name="Plant Type",values=col_scale,drop = TRUE,limits = force) +
    
    scale_y_continuous(expand=c(0,0),
                       labels = scales::percent, 
                       breaks = sort(c(seq(0,1,length.out=5)))) +
    scale_x_continuous(expand = c(0,0), limits = c(Yr_min,2045),breaks=seq(Yr_min,2045,Yr_gap))+
    
    labs(y = "Annual Generation (MW)") 
  
}


################################################################################
## FUNCTION: Total_Gen_Relative_COMPARE
## Plots generation relative to CP.
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
Total_Gen_Relative_COMPARE <- function(name_type) {
  
  # Choose color scale
    col_scale = colours3b

  # Filter emissions data
  DataGen <- ResGrYr %>%
    rename(Scenario=Sim_Name)%>%
    compare_rename(.,name_type)%>%
    mutate(Scenario=as.factor(Scenario),
           Ptype = as.character(Plant_Type),
           Ptype=if_else(Ptype %in% c("Storage - Compressed Air","Storage - Pumped Hydro","Storage - Battery"),"Storage",Ptype),
           Ptype = as.factor(Ptype))%>%
    group_by(Scenario,Ptype)%>%
    summarise(Scn_Gen = sum(Output_MWH)/10^6)

  # Format to compare against CP
  CP_Gen <- DataGen %>%
    filter(Scenario %in% c("CP","Current Policy"))%>%
    group_by(Ptype)%>%
    summarise(CP_gen = sum(Scn_Gen))
  
  # Join them
  AllData <- merge(DataGen,CP_Gen,by=c("Ptype"), all.x = TRUE) %>%
    replace(is.na(.), 0) %>%
    mutate(gen_Diff = Scn_Gen-CP_gen) %>%
    # Replace this to keep in figure
    mutate(gen_Diff=replace(gen_Diff,(Scenario %in% c("CP","Current Policy") & Ptype == "Wind"),0.0000001)) %>%
    filter(!(gen_Diff == 0)) 
  
  AllData$Ptype <- factor(AllData$Ptype,levels=c("Net Imports","Coal","Cogeneration", 
                 "Coal-to-Gas","Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                 "Natural Gas Combined Cycle + CCS",
                 "Natural Gas Simple Cycle", "Natural Gas Combined Cycle", 
                 "Hydro", "Other", "Wind", 
                 "Solar", "Storage"))

  #Max Units Built
  mxc <- round_any(max(AllData$gen_Diff[AllData$gen_Diff>0])+5,10,f=ceiling)
  mnc <-round_any(min(AllData$gen_Diff[AllData$gen_Diff<0])-5,10,f=floor)
  
  #Plot data
  ggplot(AllData,aes(x=gen_Diff, y=Scenario)) +
    geom_bar(aes(fill = Ptype),
                     position="stack", stat="identity",
                     na.rm=TRUE, alpha=Plot_Trans,color='black') +
    theme_bw() +
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),  
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = GenText_Sz+6, vjust=0,family=Plot_Text_bf),
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
    
    guides(fill = guide_legend(ncol=1,reverse = TRUE)) +
    
    scale_fill_manual(name="Plant Type",values=col_scale,drop = TRUE,limits = force) +

    scale_x_continuous(expand = c(0, 0),limits = c(mnc, mxc),breaks=pretty_breaks(8),labels=comma) +
    
    labs(x = "Total Generation Relative to Current Policy (TWh)") 
  
  
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
  
  # Choose color scale
    col_scale = colours3b
  
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
    
    scale_fill_manual(name="Plant Type",values=col_scale,drop = TRUE,limits = force) +

    scale_y_continuous(expand = c(0, 0),limits=c(mnc,mxc),labels=comma) +
    scale_x_discrete(expand = c(0.3, 0.3))+
    
    labs(y = "Generation (TWh)") 
  
  
}

################################################################################
## FUNCTION: Total_Gen_Treemap_COMPARE
## Plots total study generation using percentages.
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
    labs(title = paste("Cumulative Study Generation",name_add)) 
      
  
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
  
  # Choose color scale
    col_scale = colours3b
  
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
    
    scale_fill_manual(name="Plant Type",values=col_scale,drop = TRUE,limits = force) +
    
    scale_x_continuous(expand = c(0, 0),limits = c(mnc, mxc),breaks=seq(mnc, mxc,by=10),labels=comma) +
    
    labs(x = paste(year_look,"Total Generation (TWh)")) +
    geom_vline(xintercept =0)
  
}

################################################################################
## FUNCTION: Cost_Cum_COMPARE
## Plots cumulative costs.
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
  Costs_sum <- Costs_all %>%
    group_by(Scenario)%>%
    summarise(Total=CAPEX+VOM+FOM+`Emissions Net`+Fuel+`Storage Charging`)
  
  mxc <- round_any(max(Costs_sum$Total)+3,5,f=ceiling)
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
## Plots total cost compare.
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
    labs(y = "Cumulative Cost ($B)", x="Year",caption=add_note) +
    
    scale_fill_manual(values = c("CAPEX"="#4472C4","OPEX"='gray80')) +

    scale_x_continuous(breaks=seq(YearMN, YearMX, nbreaks), expand = c(0,0)) +
    
    scale_y_continuous(expand=c(0,0),limits=c(0,Upplim),n.breaks = 6)
  
}

################################################################################
## FUNCTION: AnnualCost_Cum_COMPARE
## Plots total cost compare, relative to CP.
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
AnnualCost_Cum_rel_COMPARE <- function(name_type,emissions_include) {
  
  # Plot color
  if (name_type == "l"){
    scenario_colors<-sn_colors2_l
    sn_base = "Current Policy"
  }else{
    scenario_colors<-sn_colors2_s
    sn_base = "CP"
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
    
    # Subtract from current policy
    df_relative <- Sim %>%
      group_by(Year) %>%
      mutate_at(vars(OPEX, CAPEX), list(~ifelse(Scenario == sn_base, ., . - .[Scenario == sn_base]))) %>%
      ungroup() %>%
      filter(!Scenario == sn_base)
    
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
    
    # Subtract from current policy
    df_relative <- Sim %>%
      group_by(Year) %>%
      mutate_at(vars(OPEX, CAPEX), list(~ifelse(Scenario == sn_base, ., . - .[Scenario == sn_base]))) %>%
      ungroup() %>%
      filter(!Scenario == sn_base)
    
  }
  
  # Make one column
  Cost_T <- melt(df_relative,id=c("Scenario","Year"))
  
  # Get plot max/mins
  YearMX<-max(df_relative$Year)
  YearMN<-min(df_relative$Year)
  
  upplim_df <- Cost_T %>%
    group_by(Year,Scenario)%>%
    summarise(Total_C=sum(value),
              MinC = min(value))
  
  Upplim <- round_any(max(upplim_df$Total_C)+5,10)
  Lowlim <- round_any(min(upplim_df$MinC)-5,10)
  
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
    labs(y = "Cumulative Cost Difference from CP ($B)", x="Year",caption=add_note) +
    
    scale_fill_manual(values = c("CAPEX"="#4472C4","OPEX"='gray80')) +
    
    scale_x_continuous(breaks=seq(YearMN, YearMX, nbreaks), expand = c(0,0)) +
    
    scale_y_continuous(expand=c(0,0),limits=c(Lowlim,Upplim),n.breaks = 10)
  
}

################################################################################
## FUNCTION: Cost_Cum_rel_COMPARE
## Plots cumulative cost compare relative to CP.
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
Cost_Cum_rel_COMPARE <- function(name_type) {
  
  # Base case
  if (name_type == "l"){
    sn_base = "Current Policy"
  }else{
    sn_base = "CP"
  }
  
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
    ) %>%
    mutate_at(vars(CAPEX, VOM,FOM,`Emissions Net`,Fuel,`Storage Charging`), list(~ifelse(Scenario == sn_base, ., . - .[Scenario == sn_base]))) %>%
    ungroup() %>%
    filter(!Scenario == sn_base)
  
  #Plot max
  Costs_sum <- Costs_all %>%
    group_by(Scenario)%>%
    summarise(Total=CAPEX+VOM+FOM+`Emissions Net`+Fuel+`Storage Charging`,
              min_total = (min(CAPEX[CAPEX<0],0)+min(VOM[VOM<0],0)+min(`Emissions Net`[`Emissions Net`<0],0)+
                             min(Fuel[Fuel<0],0)+min(`Storage Charging`[`Storage Charging`<0],0)))
  
  mxc <- round_any(max(Costs_sum$Total)+3,5,f=ceiling)
  mnc <- round_any(min(Costs_sum$min_total)-1,5,f=floor)
  GenText_Sz <-GenText_Sz
  
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
      
      scale_y_continuous(expand = c(0, 0),limits=c(mnc,mxc),breaks=pretty_breaks(10),labels=comma)
    
    if (counter==1){
      plot_temp <- plot_temp +
        theme(axis.title.y = element_text(size = GenText_Sz-26, vjust=0),
              axis.text.x=element_text(angle=90,vjust = 0.5, hjust = 1,color="black",size = GenText_Sz-30),
              axis.text.y=element_text(color="black",size = GenText_Sz-30)) +
        labs(y="Total Cost Relative to Current Policy (nominal $B)", x=paste(scn)) 
      
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
## FUNCTION: AnnualValue_Cum_norm
## Plots total nominal resource group value.
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
AnnualValue_Cum_norm <- function(name_type) {
  
  # Plot color
  if (name_type == "l"){
    scenario_colors<-sn_colors2_l
    sn_base = "Current Policy"
  }else{
    scenario_colors<-sn_colors2_s
    sn_base = "CP"
  }
  
  Sim <- ResGrYr %>%
    compare_rename(.,name_type)%>%
    group_by(Scenario,Year)%>%
    summarise(Total = sum(Total_Cost)/1000000,
              Value = sum(Value)/1000000,
              Revenue = sum(Revenue)/1000000
    ) %>%
    ungroup() %>%
    group_by(Scenario)%>%
    summarise(Year,
              Total = cumsum(Total),
              Value = cumsum(Value),
              Revenue = cumsum(Revenue)) %>%
    filter(Year == 2045)
  
  # Get totals
  base_total = as.numeric(Sim[Sim$Scenario == sn_base, "Total"])
  base_value = as.numeric(Sim[Sim$Scenario == sn_base, "Value"])
  base_rev = as.numeric(Sim[Sim$Scenario == sn_base, "Revenue"])

  # Normalize
  Sim <- Sim %>%
    mutate("Total Cost" = Total/base_total,
           "Total Value" = Value/base_value,
           "Total Revenue" = Revenue/base_rev,)%>%
    select(.,c(Scenario,`Total Cost`,`Total Revenue`,`Total Value`))
  
  # Make one column
  Cost_T <- melt(Sim,id=c("Scenario"))
  
  Upplim <- round_any(max(Cost_T$value)+0.3,0.5)
  
  # Plot
  ggplot(Cost_T) +
    geom_bar(aes(x = Scenario, y = value, fill=Scenario), 
             size = 0.5,stat="identity",position = "stack",color="black")+
    geom_text(aes(x = Scenario,y = value, label = sprintf("%.2f",value), vjust=-0.5),size =GenText_Sz/4)  +
    facet_grid(~variable) +
    theme_bw() +
    theme(text=element_text(family=Plot_Text)) +
    theme(axis.text = element_text(color="black"),
          axis.title = element_text(size = GenText_Sz+6),
          axis.text.x = element_text(angle = 0, hjust=0.5,vjust=0,color="black"),
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
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          
          strip.placement = "outside",
          strip.text = element_text(size = GenText_Sz, color = "black"),
          strip.background = element_rect(colour=NA, fill=NA),
          panel.spacing = unit(1.5,'lines')
    ) +
    labs(y = "Cost Normalized to CP", x="Year") +
    
    scale_fill_manual(values = scenario_colors,drop=TRUE,limits = force) +
    
    scale_y_continuous(expand=c(0,0),limits=c(0,Upplim),n.breaks = 6)
  
}

################################################################################
# COMPARING METRICS 
################################################################################
################################################################################
## FUNCTION: Cap_Year_Relative_COMPARE
## Plots capacity relative to CP in chosen year.
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
Cap_Year_Diff_COMPARE <- function(name_type,year_in,base_case,txt_sz=GenText_Sz) {
  
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
    filter(Scenario %in% base_case)%>%
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
    mutate(Diff_type = if_else(Cap_Diff>0,"Increased",
                               if_else(Cap_Diff<0,"Decreased","Same")),
           Ptype = as.factor(Ptype)) %>%
    filter(!Scenario %in% base_case)
  
  # Map short names
  new_names <- c("NGConv"="Coal-to-Gas",
                 "Cogen"="Cogeneration",
                 "H2SC"="Hydrogen Simple Cycle",
                 "NGCC"="Natural Gas Combined Cycle",
                 "NGCCS"="Natural Gas Combined Cycle + CCS",
                 "NGSC"= "Natural Gas Simple Cycle")
  AllData$Ptype <- fct_recode(AllData$Ptype, !!!new_names)
  
  
  #Max Capacity
  mxc <- round_any(max(AllData$Cap_Diff[AllData$Cap_Diff>0])+151,500,f=ceiling)
  mnc <-round_any(min(AllData$Cap_Diff[AllData$Cap_Diff<0])-151,500,f=floor)
  
  
  #Plot data
  ggplot(AllData,aes(x=Ptype, y=Cap_Diff)) +
    geom_bar(aes(fill = Diff_type),
             position="dodge", stat="identity",
             na.rm=TRUE, alpha=Plot_Trans,color='black',
    ) +
    facet_wrap(~Scenario, strip.position = "top",nrow = 1) +
    theme_bw() +
    geom_vline(xintercept =0)+
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),  
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_text(size = txt_sz+6, vjust=0.5,family=Plot_Text_bf),
          panel.background = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", size = 1),
          axis.text.x=element_text(angle=90,vjust = 0.5, hjust = 1,color="black"),
          axis.text.y=element_text(color="black"),
          plot.title = element_blank(),
          legend.justification = c(0.5,0.5),
          legend.position = ("none"),
          legend.text = element_text(size = txt_sz-6),
          legend.title=element_blank(), 
          #legend.key.size = unit(1,"lines"),
          text = element_text(size = txt_sz)) +
    
    geom_text(aes(x = "Storage",y = 2*mxc/3, label = "Increased from CP", vjust=-0.5,hjust=1),size =txt_sz/4,family=Plot_Text,color="#3A3A3A")  +
    geom_text(aes(x = "Storage",y = 2*mnc/3, label = "Decreased from CP", vjust=-0.5,hjust=1),size =txt_sz/4,family=Plot_Text,color="#747474")  +
    
    theme(strip.placement = "outside",
          strip.text = element_text(size = txt_sz-6, color = "black"),
          strip.background = element_rect(colour="black", fill=NA, size = 1),
          panel.spacing = unit(0,'lines')) +
  
    scale_fill_manual(values = c("#3A3A3A", "#747474", "black"),
                      breaks = c("Increased", "Decreased")) +
    guides(fill = guide_legend(ncol=1)) +
    
    scale_y_continuous(expand = c(0, 0),limits=c(mnc,mxc),breaks = seq(mnc,mxc,500),labels=comma) +
    
    #scale_x_discrete(expand = c(0, 0)) +

    labs(y = paste(year_in," Capacity Difference (MW)"))
  
  
}

################################################################################
## FUNCTION: Gen_Diff_COMPARE
## Plots generation relative to CP in chosen year.
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
Gen_Diff_COMPARE <- function(name_type,base_case,txt_sz=GenText_Sz) {
  
  # Gather all data first
  Filt_data <- ResGrYr %>%
    compare_rename(.,name_type)%>%
    mutate(Scenario=as.factor(Scenario),
           Ptype = as.character(Plant_Type),
           Ptype=if_else(Ptype %in% c("Storage - Compressed Air","Storage - Pumped Hydro","Storage - Battery"),"Storage",Ptype))%>%
    filter(Ptype != "Storage") %>%
    group_by(Scenario,Ptype)%>%
    summarise(Output_GWH=sum(Output_MWH)/1000000,
              Emissions_MT=sum(Emissions_Tonne)/1000000)
  
  # Format to compare against CP
  CP_data <- Filt_data %>%
    filter(Scenario %in% base_case)%>%
    group_by(Ptype) %>%
    summarise(CP_Out = Output_GWH,
              CP_Em = Emissions_MT)
  
  # Join them
  AllData <- merge(Filt_data,CP_data,by=c("Ptype"), all.x = TRUE) %>%
    replace(is.na(.), 0) %>%
    mutate(Out_Diff = (Output_GWH-CP_Out),
           Em_Diff= Emissions_MT-CP_Em) %>%
    # Replace this to keep in figure
    mutate(Diff_type = if_else(Out_Diff>0,"Increased",
                               if_else(Out_Diff<0,"Decreased","Same")),
           Ptype = as.factor(Ptype)) %>%
    filter(!Scenario %in% base_case)
  
  # Map short names
  new_names <- c("NGConv"="Coal-to-Gas",
                 "Cogen"="Cogeneration",
                 "H2SC"="Hydrogen Simple Cycle",
                 "NGCC"="Natural Gas Combined Cycle",
                 "NGCCS"="Natural Gas Combined Cycle + CCS",
                 "NGSC"= "Natural Gas Simple Cycle")
  AllData$Ptype <- fct_recode(AllData$Ptype, !!!new_names)
  
  
  #Max Capacity
  mxc <- round_any(max(AllData$Out_Diff[AllData$Out_Diff>0])+11,20,f=ceiling)
  mnc <-round_any(min(AllData$Out_Diff[AllData$Out_Diff<0])-11,20,f=floor)
  
  
  #Plot data
  ggplot(AllData,aes(x=Ptype, y=Out_Diff)) +
    geom_bar(aes(fill = Diff_type),
             position="dodge", stat="identity",
             na.rm=TRUE, alpha=Plot_Trans,color='black',
    ) +
    facet_wrap(~Scenario, strip.position = "top",nrow = 1) +
    theme_bw() +
    geom_vline(xintercept =0)+
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),  
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_text(size = txt_sz+6, vjust=0.5,family=Plot_Text_bf),
          panel.background = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", size = 1),
          axis.text.x=element_text(angle=90,vjust = 0.5, hjust = 1,color="black"),
          axis.text.y=element_text(color="black"),
          plot.title = element_blank(),
          legend.justification = c(0.5,0.5),
          legend.position = ("none"),
          legend.text = element_text(size = txt_sz-6),
          legend.title=element_blank(), 
          #legend.key.size = unit(1,"lines"),
          text = element_text(size = txt_sz)) +
    
    geom_text(aes(x = "Solar",y = 2*mxc/3, label = "Increased from CP", vjust=-0.5,hjust=1),size =txt_sz/4,family=Plot_Text,color="#3A3A3A")  +
    geom_text(aes(x = "Solar",y = 2*mnc/3, label = "Decreased from CP", vjust=-0.5,hjust=1),size =txt_sz/4,family=Plot_Text,color="#747474")  +
    
    theme(strip.placement = "outside",
          strip.text = element_text(size = txt_sz-6, color = "black"),
          strip.background = element_rect(colour="black", fill=NA, size = 1),
          panel.spacing = unit(0,'lines')) +
    
    scale_fill_manual(values = c("#3A3A3A", "#747474", "black"),
                      breaks = c("Increased", "Decreased")) +
    guides(fill = guide_legend(ncol=1)) +
    
    scale_y_continuous(expand = c(0, 0),limits=c(mnc,mxc),breaks = seq(mnc,mxc,20),labels = function(x) sprintf("%.1f", x)) +
    
   # scale_x_discrete(expand = c(0, 0)) +
    
    labs(y = paste("Total Generation Difference (MW)"))
  
  
}

################################################################################
## FUNCTION: Em_Diff_COMPARE
## Plots total GHG emissions relative to CP
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
Em_Diff_COMPARE <- function(name_type,base_case,em_diff_groups,txt_sz=GenText_Sz) {
  
  # Gather all data first
  Filt_data <- ResGrYr %>%
    compare_rename(.,name_type)%>%
    mutate(Scenario=as.factor(Scenario),
           Ptype = as.character(Plant_Type)) %>%
    filter(Ptype %in% em_diff_groups) %>%
    group_by(Scenario,Ptype)%>%
    summarise(Output_GWH=sum(Output_MWH)/1000000,
              Emissions_MT=sum(Emissions_Tonne)/1000000)
  
  # Format to compare against CP
  CP_data <- Filt_data %>%
    filter(Scenario %in% base_case)%>%
    group_by(Ptype) %>%
    summarise(CP_Out = Output_GWH,
              CP_Em = Emissions_MT)
  
  # Join them
  AllData <- merge(Filt_data,CP_data,by=c("Ptype"), all.x = TRUE) %>%
    replace(is.na(.), 0) %>%
    mutate(Out_Diff = (Output_GWH-CP_Out),
           Em_Diff= Emissions_MT-CP_Em) %>%
    # Replace this to keep in figure
    mutate(Diff_type = if_else(Em_Diff>0,"Increased",
                               if_else(Em_Diff<0,"Decreased","Same")),
           Ptype = as.factor(Ptype)) %>%
    filter(!Scenario %in% base_case)
  
  # Map short names
  new_names <- c("NGConv"="Coal-to-Gas",
                 "Cogen"="Cogeneration",
                 "NGCC"="Natural Gas Combined Cycle",
                 "NGCC+CCUS"="Natural Gas Combined Cycle + CCS",
                 "NGSC"= "Natural Gas Simple Cycle")
  AllData$Ptype <- fct_recode(AllData$Ptype, !!!new_names)
  
  
  #Max Em
  mxc <- round_any(max(AllData$Em_Diff[AllData$Em_Diff>0])+5,10,f=ceiling)
  mnc <-round_any(min(AllData$Em_Diff[AllData$Em_Diff<0])-5,10,f=floor)
  
  try(AllData$Scenario<-fct_relevel(AllData$Scenario, "No Emissions Credits"))
  
  #Plot data
  ggplot(AllData,aes(x=Ptype, y=Em_Diff)) +
    geom_bar(aes(fill = Diff_type),
             position="dodge", stat="identity",
             na.rm=TRUE, alpha=Plot_Trans,color='black',
    ) +
    facet_wrap(~Scenario, strip.position = "top",nrow = 1) +
    theme_bw() +
    geom_vline(xintercept =0)+
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),  
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_text(size = txt_sz+6, vjust=0.5,family=Plot_Text_bf),
          panel.background = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", size = 1),
          axis.text.x=element_text(angle=90,vjust = 0.5, hjust = 1,color="black"),
          axis.text.y=element_text(color="black"),
          plot.title = element_blank(),
          legend.justification = c(0.5,0.5),
          legend.position = ("none"),
          legend.text = element_text(size = txt_sz-6),
          legend.title=element_blank(), 
          #legend.key.size = unit(1,"lines"),
          text = element_text(size = txt_sz)) +
    
    #geom_text(aes(x = "Other",y = 2.9*mxc/3, label = "Increased from CP", vjust=-0.5,hjust=1),size =txt_sz/4,family=Plot_Text,color="#3A3A3A")  +
    #geom_text(aes(x = "Other",y = 2*mnc/3, label = "Decreased from CP", vjust=-0.5,hjust=1),size =txt_sz/4,family=Plot_Text,color="#747474")  +
    
    theme(strip.placement = "outside",
          strip.text = element_text(size = txt_sz-6, color = "black"),
          strip.background = element_rect(colour="black", fill=NA, size = 1),
          panel.spacing = unit(0,'lines')) +
    
    scale_fill_manual(values = c("#3A3A3A", "#747474", "black"),
                      breaks = c("Increased", "Decreased")) +
    guides(fill = guide_legend(ncol=1)) +
    
    scale_y_continuous(expand = c(0, 0),limits=c(mnc,mxc),breaks = seq(mnc,mxc,10),labels = function(x) sprintf("%.1f", x)) +
    
    #scale_x_discrete(expand = c(0, 0)) +
    
    labs(y = expression("GHG Emissions Difference (Mt"[CO2e]*")"))
  
}

################################################################################
## FUNCTION: Cost_Diff_COMPARE
## Plots cost relative to CP in each year.
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
Cost_Diff_COMPARE <- function(name_type,base_case,axis_space,txt_sz=GenText_Sz) {
  
  # Plot color
  if (name_type == "l"){
    scenario_colors<-sn_colors2_l
    scenario_lines<-sn_line2_l
  }else{
    scenario_colors<-sn_colors2_s
    scenario_lines<-sn_line2_s
  }
  
  # Gather all data first
  Filt_data <- ResGrYr %>%
    compare_rename(.,name_type)%>%
    mutate(Scenario=as.factor(Scenario)) %>%
    group_by(Scenario,Year)%>%
    summarise(Cost=sum(Total_Cost)/1000000) %>%
    ungroup() %>%
    group_by(Scenario) %>%
    mutate(Year,
           cum_cost = cumsum(Cost)) %>%
    ungroup()
  
  # Format to compare against CP
  CP_data <- Filt_data %>%
    filter(Scenario %in% base_case)%>%
    group_by(Year) %>%
    summarise(CP_Cost = Cost,
              CP_cum = cum_cost)
  
  # Join them
  AllData <- merge(Filt_data,CP_data,by=c("Year"), all.x = TRUE) %>%
    replace(is.na(.), 0) %>%
    mutate(cost_diff = (Cost-CP_Cost),
           cum_cost_diff = (cum_cost-CP_cum),
           cum_cost_diff_perc = cum_cost_diff/CP_cum,
           Diff_type = if_else(cost_diff>0,"Increased",
                               if_else(cost_diff<0,"Decreased","Same"))) %>%
    filter(!Scenario %in% base_case) 
  
  # Get plot max/mins
  YearMX<-max(AllData$Year)
  YearMN<-min(AllData$Year)
  mxc <- round_any(max(AllData$cum_cost_diff_perc[AllData$cum_cost_diff_perc>0])+0.1,0.2,f=ceiling)
  mnc <-round_any(min(AllData$cum_cost_diff_perc)-0.1,0.2,f=floor)
  
  #Plot data
  ggplot(AllData) +
    geom_line(aes(x = Year, y = cum_cost_diff_perc, colour = Scenario,linetype= Scenario), 
              size = 1) +
    geom_hline(yintercept =0) +
    theme_bw() +
    theme(text=element_text(family=Plot_Text)) +
    theme(axis.text = element_text(color="black"),
          axis.title = element_text(size = txt_sz+6,family=Plot_Text_bf),
          axis.text.x = element_text(angle = 90, hjust=0,color="black"),
          plot.title = element_blank(),
          text = element_text(size=txt_sz),
          axis.title.x=element_blank(),
          panel.border = element_rect(color = "black", size = 1),
          legend.text = element_text(size = txt_sz-8),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          #panel.grid.major.y = element_line(size=0.25,linetype=2,color = 'gray70'),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(y = "Cumulative Cost Difference", x="Year") +
    
    #scale_colour_grey() +
    scale_linetype_manual(name="Guide1",values = scenario_lines,drop=TRUE,limits = force)+
    scale_colour_manual(name="Guide1",values = scenario_colors,drop = TRUE,limits = force) +
    
    scale_x_continuous(expand=c(0,0),limits = c(YearMN-0.2,YearMX+0.2),breaks=seq(YearMN, YearMX, 2)) +
    
    scale_y_continuous(expand=c(0,0),limits=c(mnc,mxc),breaks = seq(mnc,mxc,by=axis_space),labels = percent)
  
}
  
################################################################################
## FUNCTION: compare_metrics
## Plot 4 metrics in one.
##
################################################################################
compare_metrics <- function(name_type,capyr,base_case,em_diff_groups,axis_space,txt_sz=GenText_Sz) {


p1<-Cap_Year_Diff_COMPARE(name_type,capyr,base_case,txt_sz) + 
  theme(plot.title = element_text(margin = margin(b = 5)),
        strip.text = element_blank(),
        axis.title.y = element_text(size = txt_sz+2,family=Plot_Text_bf))+ 
        ggtitle("(A)")
p2<-Gen_Diff_COMPARE(name_type,base_case,txt_sz)  + 
  theme(plot.title = element_text(margin = margin(b = 5)),
        strip.text = element_blank(),
        axis.title.y = element_text(size = txt_sz+2,family=Plot_Text_bf))+   
  ggtitle("(B)")

p3<-Em_Diff_COMPARE(name_type,base_case,em_diff_groups,txt_sz)  + 
  theme(plot.title = element_text(margin = margin(b = 5)),
        strip.text = element_blank(),
        axis.title.y = element_text(size = txt_sz+2,family=Plot_Text_bf))+   
  ggtitle("(C)")

# p4<-Cost_Diff_COMPARE(name_type,base_case,axis_space,txt_sz) + 
#   theme(plot.title = element_text(margin = margin(b = 5)),
#         axis.title.y = element_text(size = txt_sz+2,family=Plot_Text_bf))+   
#   ggtitle("(D)") 

# Arrange all the plots
grid.arrange(plot_grid(p1, p2, p3,nrow=1),nrow=1,family=Plot_Text)

}

################################################################################
## FUNCTION: compare_cap_gen_em
## Plot all metrics together
##
################################################################################
compare_cap_gen_em <- function(name_type) {
  
  em_groups <-  c("Coal", "Coal-to-Gas","Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                  "Natural Gas Combined Cycle + CCS", "Natural Gas Simple Cycle", "Natural Gas Combined Cycle","Other")
  
  years_cap = seq.int(2023, 2045) 
  
  # Filter data & aggregate years
  RawData <- ResGrYr %>%
    compare_rename(.,name_type)%>%
    mutate(Scenario=as.factor(Scenario),
           Ptype = as.character(Plant_Type),
           Ptype=if_else(Ptype %in% c("Storage - Compressed Air","Storage - Pumped Hydro","Storage - Battery"),"Storage",Ptype),
           Ptype=as.factor(Ptype)) %>%
    filter(Year %in% years_cap) %>%
    group_by(Year,Scenario,Ptype) %>%
    summarise("Capacity (GW)" = sum(Capacity_MW)/10^3,
              "Generation (TWh)" = sum(Output_MWH)/10^6,
              Em_Mt = sum(Emissions_Tonne)/10^6) %>%
    ungroup()%>%
    mutate(Em_Mt = if_else(Ptype %in% em_groups,Em_Mt,0)) %>%
    rename("GHG Emissions (Mt CO2e)"=Em_Mt)
  
  # Imports
  # netimport <- Zone %>%
  #   compare_rename(.,name_type)%>%
  #   group_by(Scenario,Year)%>%
  #   summarise(Ptype = "Net Imports",
  #             "Capacity (GW)" = 0,
  #             "Generation (TWh)" = sum(Imports_Total-Exports_Total)/10^6,
  #             "Emissions (Mt CO2)"=0)
  # 
  # RawData <- rbind(RawData,netimport)
  
  # Order resources
  # Set levels to each category in order specified
  RawData$Ptype <- factor(RawData$Ptype, levels=c("Storage","Net Imports","Solar","Wind", "Other", "Hydro", 
                                                  "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                                  "Blended  Simple Cycle","Blended  Combined Cycle",
                                                  "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                                                  "Coal-to-Gas", 
                                                  "Coal", "Cogeneration"))
  
  # Map short names
  new_names <- c("Cogen"="Cogeneration",
                 "NGCC"="Natural Gas Combined Cycle",
                 "NGCC+CCUS"="Natural Gas Combined Cycle + CCS",
                 "H2SC"="Hydrogen Simple Cycle",
                 "NGSC"= "Natural Gas Simple Cycle")
  RawData$Ptype <- fct_recode(RawData$Ptype, !!!new_names)
  
  col_scale = colours3c
  
  # Set up plot limits
  Yr_gap <- 2
  
  max_groups <-RawData %>%group_by(Scenario,Year)%>%
    summarise(max_cap = sum(`Capacity (GW)`),
              max_gen = sum(`Generation (TWh)`[`Generation (TWh)`>0]),
              min_gen = sum(`Generation (TWh)`[`Generation (TWh)`<=0]),
              max_em = sum(`GHG Emissions (Mt CO2e)`))
  
  mx_cap <- round_any(max(max_groups$max_cap)+3,5,f=ceiling)
  mx_gen <- round_any(max(max_groups$max_gen)+5,10,f=ceiling)
  mx_em <- round_any(max(max_groups$max_em)+3,5,f=ceiling)
  mn_gen <- round_any(min(max_groups$min_gen)-5,10,f=ceiling)
  
  # Melt the data
  ModData <- melt(RawData,id=c("Year","Scenario","Ptype"))
  
  #Plot data
  ggplot(ModData,aes(x=Year, y=value)) +
    geom_area(aes(fill = Ptype),size=0.25,
              na.rm=TRUE, alpha=Plot_Trans,color='black') +
   
    # facet_wrap(~variable, scales = "free_y",
    #            axes = "all", axis.labels = "all") +

     facet_grid(variable~Scenario,
               scales = "free_y", switch = "y",
               axes = "all_y", axis.labels = "all_y") +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(
      panel.grid = element_blank(),  
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.background = element_rect(fill = "transparent"),
      axis.text.x=element_text(angle=90,vjust = 0.5, hjust = 1,color="black",size = GenText_Sz-6),
      axis.text.y=element_text(color="black",size = GenText_Sz-6),
      plot.title = element_blank(),
      axis.ticks = element_line(size = 0.5),
      
      legend.justification = c(0.5,0.5),
      legend.key.size = unit(0.3, "cm"),
      legend.position = ("right"),
      legend.text = element_text(size = GenText_Sz-6),
      legend.title=element_blank(), 
      legend.spacing.y = unit(0.1, 'cm'),
      
      strip.placement = "outside",
      strip.text = element_text(size = GenText_Sz-3, color = "black",family=Plot_Text_bf),
      strip.background = element_rect(colour=NA, fill=NA),
      panel.spacing = unit(1,'lines'),
      
      text = element_text(size = GenText_Sz-2)) +
    
    scale_fill_manual(name="Plant Type",values=col_scale,drop = TRUE,limits = force) +
    
    scale_y_continuous(expand = expansion(c(0,0.05)), breaks=pretty_breaks(8),
                       labels = function(x) sprintf("%.0f", x)) +
    
    scale_x_continuous(expand = c(0,0), limits=c(2023,2045),breaks=seq(2023,2045,Yr_gap))
    
    #guides(fill = guide_legend(nrow=2))
  
}

################################################################################
## EPC PLOTS
################################################################################
################################################################################
## FUNCTION: Annual_Cap_group_dots
## Plots annual capacity for selected resource group in years
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
Annual_Cap_group_dots <- function(list_groups,EPC_rename) {
  
  years_cap = c(2025,2030,2035,2040,2045) 
  
  # Filter data & aggregate years
  Capdata <- ResGrYr %>%
    compare_rename(.,"l")%>%
    mutate(Scenario=as.factor(Scenario),
           Ptype = as.character(Plant_Type),
           Ptype=if_else(Ptype %in% c("Storage - Compressed Air","Storage - Pumped Hydro","Storage - Battery"),"Storage",Ptype),
           Ptype=as.factor(Ptype)) %>%
    filter(Year %in% years_cap,
           Ptype %in% list_groups) %>%
    group_by(Year,Scenario,Ptype) %>%
    summarise(Cap_MW = sum(Capacity_MW),
              Cap_GW = Cap_MW/10^3) %>%
    ungroup()
  
  if (EPC_rename==TRUE){
    Capdata <- Capdata %>%
      mutate(Scenario = if_else(Scenario=="Current Policy","90%",
                                if_else(grepl('No Emissions Credits',Scenario),"0%",
                                        if_else(grepl('30%',Scenario),"30%",
                                                if_else(grepl('50%',Scenario),"50%",
                                                        if_else(grepl('70%',Scenario),"70%","unknown"))))))
    Capdata$Scenario <- factor(Capdata$Scenario, levels=c("0%","30%","50%","70%","90%"))
  }
  
  # Order resources
  # Set levels to each category in order specified
  Capdata$Ptype <- factor(Capdata$Ptype, levels=c("Storage","Solar","Wind", "Other", "Hydro", 
                                                  "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                                  "Blended  Simple Cycle","Blended  Combined Cycle",
                                                  "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                                                  "Coal-to-Gas", 
                                                  "Coal", "Cogeneration"))
  
  # Map short names
  new_names <- c("NGConv"="Coal-to-Gas",
                 "Cogen"="Cogeneration",
                 "NGCC"="Natural Gas Combined Cycle",
                 "NGCC + CCUS"="Natural Gas Combined Cycle + CCS",
                 "NGSC"= "Natural Gas Simple Cycle",
                 "H2SC"="Hydrogen Simple Cycle")
  Capdata$Ptype <- fct_recode(Capdata$Ptype, !!!new_names)
  
  
  GenText_Sz = GenText_Sz-8
  #Plot data
  ggplot(Capdata,aes(x=Year, y=Cap_GW)) +
    geom_point(aes(shape=Scenario),color='black',size=2) +
    facet_wrap(~Ptype, strip.position = "top",scales = "free_y",
               axes = "all", axis.labels = "all") +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(
      panel.grid = element_blank(),  
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = GenText_Sz+20, vjust=1,family=Plot_Text_bf),
      panel.background = element_rect(fill = "transparent"),
      axis.text.x=element_text(angle=0,vjust = 0.5, hjust = 0.5,color="black",size = GenText_Sz),
      axis.text.y=element_text(color="black",size = GenText_Sz),
      plot.title = element_blank(),
      
      legend.justification = c(0.5,0.5),
      legend.key.size = unit(0.3, "cm"),
      legend.position = ("right"),
      legend.text = element_text(size = GenText_Sz+6),
      legend.title=element_text(size = GenText_Sz+6),
      legend.spacing.y = unit(0.1, 'cm'),
      
      strip.placement = "outside",
      strip.text = element_text(size = GenText_Sz, color = "black"),
      strip.background = element_rect(colour="black", fill=NA),
      #panel.spacing = unit(0,'lines'),
      
      text = element_text(size = GenText_Sz)) +
    
    scale_shape_discrete(name="EPC Value") +
    
    scale_y_continuous(expand = expansion(c(0,0.1)),
                       limits = c(0, NA),
                       breaks = pretty_breaks(8),labels = formatter(nsmall = 1)) +
    
    labs(y = "Capacity (GW)") 
  
}

################################################################################
## FUNCTION: Cum_Gen_group_dots
## Plots gen for grouped resource years
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
Cum_Gen_group_dots <- function(EPC_rename,all_groups) {
  
  # Filter data
  Gendata <- ResGrYr %>%
    compare_rename(.,"l")%>%
    mutate(Ptype = as.character(Plant_Type),
           Ptype=if_else(Ptype %in% c("Storage - Compressed Air","Storage - Pumped Hydro","Storage - Battery"),"Storage",Ptype),
           Scenario=as.factor(Scenario))%>%
    group_by(Year,Scenario,Ptype) %>%
    filter(Ptype != "Coal",
           Ptype %in% all_groups) %>%
    summarise(Output_MWH = sum(Output_MWH))
  
  
  # Imports
  export_import <- Zone %>%
    compare_rename(.,"l")%>%
    select(.,c(Scenario,Year,Imports_Total,Exports_Total))
  
  trade <- melt(export_import,id=c("Scenario","Year")) %>%
    rename(Output_MWH=value,
           Ptype=variable) %>%
    mutate(Ptype = if_else(grepl('Exports',Ptype),"Exports","Imports"))
  
  DataGen1 <- rbind(Gendata,trade)
  
  # Aggregate
  DataGen <-DataGen1 %>%
    mutate(report_group = if_else(between(Year,2025,2029),"2025-2029",
                                          if_else(between(Year,2030,2034),"2030-2034",
                                                  if_else(between(Year,2035,2039),"2035-2039",
                                                          if_else(between(Year,2040,2044),"2040-2044","na")))))%>%
    filter(report_group != "na") %>%
    group_by(report_group,Scenario,Ptype) %>%
    summarise(Output_TWh = sum(Output_MWH)/10^6) %>%
    ungroup() 
  
  # Order resources
  # Set levels to each category in order specified
  DataGen$Ptype <- factor(DataGen$Ptype, levels=c("Storage","Solar","Wind", "Other", "Hydro", 
                                                  "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                                  "Blended  Simple Cycle","Blended  Combined Cycle",
                                                  "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                                                  "Coal-to-Gas", 
                                                  "Coal", "Cogeneration",
                                                  "Imports","Exports"))
  
  if (EPC_rename==TRUE){
    DataGen <- DataGen %>%
      mutate(Scenario = if_else(Scenario=="Current Policy","90%",
                                if_else(grepl('No Emissions Credits',Scenario),"0%",
                                        if_else(grepl('30%',Scenario),"30%",
                                                if_else(grepl('50%',Scenario),"50%",
                                                        if_else(grepl('70%',Scenario),"70%","unknown"))))))
    DataGen$Scenario <- factor(DataGen$Scenario, levels=c("0%","30%","50%","70%","90%"))
  }
  
  # Map short names
  new_names <- c("NGConv"="Coal-to-Gas",
                 "Cogen"="Cogeneration",
                 "NGCC"="Natural Gas Combined Cycle",
                 "NGCC + CCUS"="Natural Gas Combined Cycle + CCS",
                 "NGSC"= "Natural Gas Simple Cycle",
                 "H2SC"="Hydrogen Simple Cycle")
  DataGen$Ptype <- fct_recode(DataGen$Ptype, !!!new_names)
  
  GenText_Sz = GenText_Sz-8
  #Plot data
  ggplot(DataGen,aes(x=report_group, y=Output_TWh)) +
    geom_point(aes(shape=Scenario),color='black',size=1.25) +
    facet_wrap(~Ptype, strip.position = "top",scales = "free_y",
               axes = "all", axis.labels = "all") +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
     
    theme(
      panel.grid = element_blank(),  
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = GenText_Sz+20, vjust=1,family=Plot_Text_bf),
      panel.background = element_rect(fill = "transparent"),
      axis.text.x=element_text(angle=0,vjust = 0.5, hjust = 0.5,color="black",size = GenText_Sz-6),
      axis.text.y=element_text(color="black",size = GenText_Sz),
      plot.title = element_blank(),
      
      legend.justification = c(0.5,0.5),
      legend.key.size = unit(0.3, "cm"),
      legend.position = ("right"),
      legend.text = element_text(size = GenText_Sz+6),
      legend.title=element_text(size = GenText_Sz+6),
      legend.spacing.y = unit(0.1, 'cm'),
      
      strip.placement = "outside",
      strip.text = element_text(size = GenText_Sz-6, color = "black"),
      strip.background = element_rect(colour="black", fill=NA),
      #panel.spacing = unit(0,'lines'),
      
      text = element_text(size = GenText_Sz)) +
    
    scale_shape_discrete(name="EPC Value") +
    
    scale_x_discrete(expand = expansion(c(0.1,0.1))) +
  
    scale_y_continuous(expand = expansion(c(0,0.1)),
                       limits = c(0, NA),
                       breaks = pretty_breaks(8),labels = formatter(nsmall = 0)) +
    
    labs(y = "Generation (TWh)") 
  
}

################################################################################
## FUNCTION: CF_group_dots
## Plots CF for grouped resource years
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
CF_group_dots <- function(EPC_rename) {
  
  years_cap = c(2025,2030,2035,2040,2045) 
  
  # Filter data
  CF_data <- ResGrYr %>%
    compare_rename(.,"l")%>%
    mutate(Ptype = as.character(Plant_Type),
           Ptype=if_else(Ptype %in% c("Storage - Compressed Air","Storage - Pumped Hydro","Storage - Battery"),"Storage",Ptype),
           Scenario=as.factor(Scenario))%>%
    filter(Year %in% years_cap,
           Capacity_Factor>0) %>%
    select(Year,Scenario,Ptype,Capacity_Factor) 

  # Order resources
  # Set levels to each category in order specified
  CF_data$Ptype <- factor(CF_data$Ptype, levels=c("Storage","Solar","Wind", "Other", "Hydro", 
                                                  "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                                  "Blended  Simple Cycle","Blended  Combined Cycle",
                                                  "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                                                  "Coal-to-Gas", 
                                                  "Coal", "Cogeneration",
                                                  "Imports","Exports"))
  
  if (EPC_rename==TRUE){
    CF_data <- CF_data %>%
      mutate(Scenario = if_else(Scenario=="Current Policy","90%",
                                if_else(grepl('No Emissions Credits',Scenario),"0%",
                                        if_else(grepl('30%',Scenario),"30%",
                                                if_else(grepl('50%',Scenario),"50%",
                                                        if_else(grepl('70%',Scenario),"70%","unknown"))))))
    CF_data$Scenario <- factor(CF_data$Scenario, levels=c("0%","30%","50%","70%","90%"))
  }
  
  
  # Map short names
  new_names <- c("Cogen"="Cogeneration",
                 "NGCC"="Natural Gas Combined Cycle",
                 "NGCC + CCUS"="Natural Gas Combined Cycle + CCS",
                 "NGSC"= "Natural Gas Simple Cycle",
                 "H2SC"="Hydrogen Simple Cycle")
  CF_data$Ptype <- fct_recode(CF_data$Ptype, !!!new_names)
  
  GenText_Sz = GenText_Sz-8
  #Plot data
  ggplot(CF_data,aes(x=Year, y=Capacity_Factor)) +
    geom_point(aes(shape=Scenario),color='black',size=1.25) +
    facet_wrap(~Ptype, strip.position = "top",scales = "free_y",
               axes = "all", axis.labels = "all") +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(
      panel.grid = element_blank(),  
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = GenText_Sz+20, vjust=1,family=Plot_Text_bf),
      panel.background = element_rect(fill = "transparent"),
      axis.text.x=element_text(angle=0,vjust = 0.5, hjust = 0.5,color="black",size = GenText_Sz),
      axis.text.y=element_text(color="black",size = GenText_Sz),
      plot.title = element_blank(),
      
      legend.justification = c(0.5,0.5),
      legend.key.size = unit(0.3, "cm"),
      legend.position = ("right"),
      legend.text = element_text(size = GenText_Sz+6),
      legend.title=element_text(size = GenText_Sz+6),
      legend.spacing.y = unit(0.1, 'cm'),
      
      strip.placement = "outside",
      strip.text = element_text(size = GenText_Sz, color = "black"),
      strip.background = element_rect(colour="black", fill=NA),
      #panel.spacing = unit(0,'lines'),
      
      text = element_text(size = GenText_Sz)) +
    
    scale_shape_discrete(name="EPC Value") +
    
    scale_y_continuous(expand = expansion(c(0,0.1)),
                       limits = c(0, NA),
                       #breaks = seq(0,1,by=0.1),
                       labels = percent) +
    
    labs(y = "Capacity Factor (%)") 
  
}

################################################################################
## FUNCTION: value_group_dots
## Value of entire study
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
value_group_dots <- function(EPC_rename) {
  # 
  # years_cap = c(2025,2030,2035,2040,2045) 
  # 
  # Price_data <- Zone %>%
  #   compare_rename(.,"l")%>%
  #   select(Year,Scenario,Avg_Price) %>%
  #   mutate(yr_hrs = if_else(Year %in% c(2024,2028,2032,2036,2040,2044),8784,8760))
  
  # Filter data & aggregate years
  Capdata <- ResGrYr %>%
    compare_rename(.,"l")%>%
    mutate(Scenario=as.factor(Scenario),
           Ptype = as.character(Plant_Type),
           Ptype=if_else(Ptype %in% c("Storage - Compressed Air","Storage - Pumped Hydro","Storage - Battery"),"Storage",Ptype),
           Ptype=as.factor(Ptype)) %>%
    group_by(Scenario,Ptype) %>%
    reframe(Gen_MWh = sum(Output_MWH),
            Rev = sum(Revenue)*10^3/Gen_MWh,
            Cost_M = sum(Total_Cost)/10^3,
            Value_MWh = sum(Value)*10^3/Gen_MWh,
            Value_M = sum(Value)/10^3,
              )  %>%
    filter(Ptype != "Storage")
  
  if (EPC_rename==TRUE){
    Capdata <- Capdata %>%
      mutate(Scenario = if_else(Scenario=="Current Policy","90%",
                                if_else(grepl('No Emissions Credits',Scenario),"0%",
                                        if_else(grepl('30%',Scenario),"30%",
                                                if_else(grepl('50%',Scenario),"50%",
                                                        if_else(grepl('70%',Scenario),"70%","unknown"))))))
    Capdata$Scenario <- factor(Capdata$Scenario, levels=c("0%","30%","50%","70%","90%"))
  }
  
  # Order resources
  # Set levels to each category in order specified
  Capdata$Ptype <- factor(Capdata$Ptype, levels=c("Storage","Solar","Wind", "Other", "Hydro", 
                                                  "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                                  "Blended  Simple Cycle","Blended  Combined Cycle",
                                                  "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                                                  "Coal-to-Gas", 
                                                  "Coal", "Cogeneration"))
  
  # Map short names
  new_names <- c("Cogen"="Cogeneration",
                 "NGCC"="Natural Gas Combined Cycle",
                 "NGCC+CCUS"="Natural Gas Combined Cycle + CCS",
                 "NGSC"= "Natural Gas Simple Cycle",
                 "H2SC"="Hydrogen Simple Cycle")
  Capdata$Ptype <- fct_recode(Capdata$Ptype, !!!new_names)
  
  #Plot data
  ggplot(Capdata,aes(x=Ptype, y=Value_MWh)) +
    geom_point(aes(shape=Scenario),color='black',size=2) +
    # facet_wrap(~Ptype, strip.position = "top",
    #            axes = "all", axis.labels = "all") +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    geom_hline(yintercept =0,linewidth=0.5) +
    
    theme(
      panel.grid = element_blank(),  
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = GenText_Sz+6, vjust=1,family=Plot_Text_bf),
      panel.background = element_rect(fill = "transparent"),
      axis.text.x=element_text(angle=90,vjust = 0.5, hjust = 1,color="black",size = GenText_Sz),
      axis.text.y=element_text(color="black",size = GenText_Sz),
      plot.title = element_blank(),
      
      legend.justification = c(0.5,0.5),
      legend.key.size = unit(0.3, "cm"),
      legend.position = ("right"),
      legend.text = element_text(size = GenText_Sz),
      legend.title=element_text(size = GenText_Sz),
      legend.spacing.y = unit(0.1, 'cm'),
      
      strip.placement = "outside",
      strip.text = element_text(size = GenText_Sz-6, color = "black"),
      strip.background = element_rect(colour="black", fill=NA),
      #panel.spacing = unit(0,'lines'),
      
      text = element_text(size = GenText_Sz)) +
    
    scale_shape_discrete(name="Scenario") +
    
    scale_y_continuous(expand = expansion(c(0.1,0.1)),
                       #limits = c(0, NA),
                       breaks = pretty_breaks(8),labels = function(x) sprintf("%.1f", x)) +
    
    labs(y = "Cumulative 2023-2045 Value (nominal $/MWh)") 
  
}

################################################################################
## FUNCTION: Avg_Bid_Cost_dots
## Plots annual average bid by group.
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
Avg_Bid_Cost_dots <- function(EPC_rename) {
  
  years_cap = c(2025,2030,2035,2040,2045) 
  
  # Filter data & aggregate years
  Biddata <- ResGrYr %>%
    compare_rename(.,"l")%>%
    mutate(Scenario=as.factor(Scenario),
           Ptype = as.character(Plant_Type),
           Ptype=if_else(Ptype %in% c("Storage - Compressed Air","Storage - Pumped Hydro","Storage - Battery"),"Storage",Ptype),
           Ptype=as.factor(Ptype)) %>%
    filter(Year %in% years_cap) %>%
    group_by(Year,Scenario,Ptype) %>%
    reframe(Capacity_MW=sum(Capacity_MW),
            Output_MWH=sum(Output_MWH),
            Avg_Dispatch_Cost=mean(Avg_Dispatch_Cost)) %>%
    mutate(Avg_Dispatch_Cost = if_else(Avg_Dispatch_Cost<0,0,
                                       if_else(Avg_Dispatch_Cost>1000,999.99,Avg_Dispatch_Cost)))
  
  if (EPC_rename==TRUE){
    Biddata <- Biddata %>%
      mutate(Scenario = if_else(Scenario=="Current Policy","90%",
                                if_else(grepl('No Emissions Credits',Scenario),"0%",
                                        if_else(grepl('30%',Scenario),"30%",
                                                if_else(grepl('50%',Scenario),"50%",
                                                        if_else(grepl('70%',Scenario),"70%","unknown"))))))
    Biddata$Scenario <- factor(Biddata$Scenario, levels=c("0%","30%","50%","70%","90%"))
  }
  
  # Order resources
  # Set levels to each category in order specified
  Biddata$Ptype <- factor(Biddata$Ptype, levels=c("Storage","Solar","Wind", "Other", "Hydro", 
                                                  "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                                  "Blended  Simple Cycle","Blended  Combined Cycle",
                                                  "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                                                  "Coal-to-Gas", 
                                                  "Coal", "Cogeneration"))
  GenText_Sz = GenText_Sz-10
  #Plot data
  ggplot(Biddata,aes(x=Year, y=Avg_Dispatch_Cost)) +
    geom_point(aes(shape=Scenario),color='black',size=2) +
    facet_wrap(~Ptype, strip.position = "top",
               axes = "all", axis.labels = "all") +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(
      panel.grid = element_blank(),  
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = GenText_Sz+6, vjust=1,family=Plot_Text_bf),
      panel.background = element_rect(fill = "transparent"),
      axis.text.x=element_text(angle=0,vjust = 0.5, hjust = 0.5,color="black",size = GenText_Sz-12),
      axis.text.y=element_text(color="black",size = GenText_Sz-12),
      plot.title = element_blank(),
      
      legend.justification = c(0.5,0.5),
      legend.key.size = unit(0.3, "cm"),
      legend.position = ("right"),
      legend.text = element_text(size = GenText_Sz-12),
      legend.title=element_text(size = GenText_Sz-12),
      legend.spacing.y = unit(0.1, 'cm'),
      
      strip.placement = "outside",
      strip.text = element_text(size = GenText_Sz-6, color = "black"),
      strip.background = element_rect(colour="black", fill=NA),
      #panel.spacing = unit(0,'lines'),
      
      text = element_text(size = GenText_Sz)) +
    
    scale_shape_discrete(name="EPC Value") +
    
    scale_y_continuous(expand = expansion(c(0.1,0.1)),
                       #limits = c(0, NA),
                       breaks = pretty_breaks(8),labels = function(x) sprintf("%.1f", x)) +
    
    labs(y = "Average Bid ($/MWh)") 
  
}

################################################################################
## FUNCTION: Marginal_Resource_Compare
## Plots annual average bid by group.
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################

Marginal_Resource_Compare <- function() {
  
  years_cap = c(2025,2030,2035,2040,2045) 

# Filter data & aggregate years
Marginal_data <- ResGrYr %>%
  compare_rename(.,"l")%>%
  mutate(Scenario=as.factor(Scenario),
         Ptype = as.character(Plant_Type),
         Ptype=if_else(Ptype %in% c("Storage - Compressed Air","Storage - Pumped Hydro","Storage - Battery"),"Storage",Ptype),
         Ptype=as.factor(Ptype)) %>%
  filter(Year %in% years_cap) %>%
  group_by(Year,Scenario,Ptype) %>%
  reframe(Capacity_MW=sum(Capacity_MW),
          Output_MWH=sum(Output_MWH),
          Percent_Marginal=sum(Percent_Marginal)/100)

# Map short names
new_names <- c("NGConv"="Coal-to-Gas",
               "Cogen"="Cogeneration",
               "NGCC"="Natural Gas Combined Cycle",
               "NGCC + CCUS"="Natural Gas Combined Cycle + CCS",
               "NGSC"= "Natural Gas Simple Cycle",
               "H2SC"="Hydrogen Simple Cycle")
Marginal_data$Ptype <- fct_recode(Marginal_data$Ptype, !!!new_names)

Marginal_data$Scenario<-fct_relevel(Marginal_data$Scenario, "No Emissions Credits")

# Plot
ggplot(Marginal_data) +
  geom_bar(aes(x = Year, y = Percent_Marginal, fill = Scenario), 
           size = 0.25,stat="identity",position = "dodge",color="black") +
  theme_bw() +
  facet_wrap(~Ptype, strip.position = "top",
             axes = "all", axis.labels = "all") +
  theme(text=element_text(family=Plot_Text)) +
  theme(axis.text = element_text(color="black"),
        axis.title = element_text(size = GenText_Sz+6,family=Plot_Text_bf),
        axis.text.x = element_text(angle = 0, hjust=0.5,color="black"),
        plot.title = element_blank(),
        text = element_text(size=GenText_Sz),
        axis.title.x=element_blank(),
        legend.text = element_text(size = GenText_Sz-6),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        #panel.grid.major.y = element_line(size=0.25,linetype=2,color = 'gray70'),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.spacing = unit(1.5, "lines"),
        
        strip.placement = "outside",
        strip.text = element_text(size = GenText_Sz-6, color = "black"),
        strip.background = element_rect(colour="black", fill=NA),
        #panel.spacing = unit(0,'lines'),
        
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
        legend.background = element_rect(fill='transparent'),
        legend.key.size = unit(0.3, "cm"),
        legend.box.background = element_rect(fill='transparent', colour = "transparent"),
  ) +
  labs(y = expression("Percent Marginal")) +
  
  #scale_colour_grey() +
  scale_fill_manual(name="Guide1",values = sn_colors2_l,drop = TRUE,limits = force) +
  
  scale_x_continuous(expand = c(0.01,0.01),breaks=seq(2025,2045,by=5)) +
  
  scale_y_continuous(expand=c(0,0.01),breaks=pretty_breaks(8),labels = formatter(nsmall = 0))

}

################################################################################
## FUNCTION: AnnualEm_vs_group
## Plots annual GHG emissions compared to chosen resource group.
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
AnnualEm_vs_group <- function(name_type,resfilter_type) {
  
  # Plot color
  if (name_type == "l"){
    scenario_colors<-sn_colors2_l
    
    order_legend <-c("Current Policy","Current Policy No ITCs","No Emissions Credits",
                     "30% Credit Value","50% Credit Value","70% Credit Value",
                     "Draft CER","Draft CER No ITCs","Emissions Limit",
                     "TIER 2050","TIER 2035","No CCUS")
    ccs_name <- "No CCUS"
    
  }else{
    scenario_colors<-sn_colors2_s
    
    order_legend <- c("CP","noITCs","30EPC","50EPC","70EPC","noEPCs",
                      "CER","CERnoITCs","EL","TIER2050","TIER2035","no_CCS")
    ccs_name<-"no_CCS"
    
  }
  
    Sim <- Zone %>%
      mutate(Year=as.numeric(Year))%>%
      select(.,c(Year,NonCogen_Emissions,Scenario))%>%
      compare_rename(.,name_type)%>%
      rename(Emissions=NonCogen_Emissions)%>%
      mutate(Scenario=as.factor(Scenario))%>%
      group_by(Scenario)%>%
      summarise(Year = Year,
                "Annual_Emissions"=Emissions) 
    
  # Get CCS capacity
  Res_cap <- ResGrYr %>%
    compare_rename(.,name_type)%>%
    filter(Plant_Type == resfilter_type)%>%
    select(Scenario,Year,Capacity_MW) %>%
    mutate(Capacity_MW=Capacity_MW/10^3)
  
  # Add arbitrary values for no CCS case as needed
  if (resfilter_type == "Natural Gas Combined Cycle + CCS"){
    noCCS <- data.frame(Scenario=ccs_name,
                        Year = 2023:2045,
                        Capacity_MW=0)
    x_name =  "Natural Gas Combined Cycle + CCUS"
    Res_cap <- rbind(Res_cap,noCCS)
    Years_res_filt = c(2025,2030,2035,2040,2045)
    
  } else{
    x_name = resfilter_type
    Years_res_filt = c(2025,2030,2035,2040,2045)
  }
  
  # Add CCS capacity
  Res_cap_comb <- merge(Sim,Res_cap,by=c("Scenario","Year")) %>%
    filter(Year %in% Years_res_filt) %>%
    mutate(Year_fct =as.factor(Year))

  
  Res_cap_comb$Scenario <- factor(Res_cap_comb$Scenario, levels=order_legend)
  # Plot
  ggplot(Res_cap_comb) +
    geom_point(aes(x = Capacity_MW, y = Annual_Emissions, shape=Scenario,color=Year_fct), 
               size = 2.5) +
    theme_bw() +

    scale_shape_manual(name="Scenario",values = sn_shape_l) +
    
    scale_color_manual(name="Year",values=c("2025"='#353313',
                                            "2030"="#515151",
                                            "2035"="#969191",
                                            "2040"="#c6c6c6",
                                            "2045"="black")) +
    
    guides(
      color = guide_legend(order = 1),
      shape = guide_legend(order = 2)
    ) +
    
    theme(text=element_text(family=Plot_Text)) +
    theme(axis.text = element_text(color="black",size = GenText_Sz),
          axis.title = element_text(size = GenText_Sz+6,family=Plot_Text_bf),
          plot.title = element_blank(),
          text = element_text(size=GenText_Sz),
          legend.text = element_text(size = GenText_Sz-6),
          panel.grid = element_blank(),
          legend.position = "right",
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),

          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.key.size = unit(0.3, "cm"),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(y = expression("Annual GHG Emissions (Mt"[CO2e]*")"),x=paste(x_name," Capacity (GW)")) +
    
    scale_x_continuous(expand=expansion(c(0.01,0.01)),
                       breaks=pretty_breaks(10),labels = function(x) sprintf("%.1f", x)) +
  
    scale_y_continuous(expand=c(0,0),limits=c(0,16),
                       breaks=seq(0,16,by=2),labels = function(x) sprintf("%.1f", x))
  
}

################################################################################  
## FUNCTION: TotalCapChange_Compare
## Gives capacity added and retired each year in the same plot
##
## INPUTS: 
##    input - ResGroupYear
## TABLES REQUIRED: 
##    ResGroupYear -Yearly resoruce group emissions
################################################################################
TotalCapChange_Compare <- function() {

  # Bring in Resource Year Table and filter for relevant data. Format date columns
  Add_Ret_data <- Ann_Cap%>%
    compare_rename(.,"l")%>%
    mutate(Scenario=as.factor(Scenario),
           Ptype = as.character(Plant_Type),
           Ptype=if_else(Ptype %in% c("Storage - Compressed Air","Storage - Pumped Hydro","Storage - Battery"),"Storage",Ptype)) %>%
    group_by(Scenario,Year,Ptype) %>%
    reframe(Cap_Add = sum(Capacity_Added)/1000,
            Cap_Ret = sum(Capacity_Retired)/-1000,
            Cap_net = sum(Difference_MW)/1000) %>%
    filter(!is.na(Year) & !is.na(Cap_Add) & !is.na(Cap_Ret))
  
  # Sum all up
  Tot <- Add_Ret_data %>%
    group_by(Year,Scenario) %>%
    summarise(maxy = sum(Cap_Add), miny = sum(Cap_Ret))
  
  # Capacity limits for plot
  mny <- plyr::round_any(min(Tot$miny),2, f=floor)
  mxy <- plyr::round_any(max(Tot$maxy),2, f=ceiling)
  
  # Set levels to each category in order specified
  Add_Ret_data$Ptype <- factor(Add_Ret_data$Ptype, 
                                      levels=c("Coal","Coal-to-Gas", "Hydrogen Simple Cycle",
                                               "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle CCS Retrofit",
                                               "Natural Gas Combined Cycle", 
                                               "Hydro", "Other",
                                               "Wind", "Solar", 
                                               "Storage", 
                                               "Cogeneration"))
  new_names <- c("Cogen"="Cogeneration",
                 "H2SC"="Hydrogen Simple Cycle",
                 "NGCC"="Natural Gas Combined Cycle",
                 "NGCC+CCUS"="Natural Gas Combined Cycle + CCS",
                 "NGCC+CCUS Retrofit"="Natural Gas Combined Cycle CCS Retrofit",
                 "NGSC"= "Natural Gas Simple Cycle")
  
  Add_Ret_data$Ptype <- fct_recode(Add_Ret_data$Ptype, !!!new_names)
  
  #Plot data
  Add_Ret_data %>%
  ggplot() +
    # Plot added
    # geom_col(aes(Year, Cap_Add, fill = Ptype),color="black") +
    # 
    # # Plot retired
    # geom_col(aes(Year, Cap_Ret, fill = Ptype),color="black") + 
    
    geom_col_pattern(aes(Year, Cap_Add, fill = Ptype,pattern=Ptype),
                     size=.25, colour="black",
                     pattern_density = 0.35,
                     pattern_fill = "black",
                     pattern_colour  = NA,
                     pattern_spacing=0.01
                     ) +

    # Plot retired
    geom_col_pattern(aes(Year, Cap_Ret, fill = Ptype,pattern=Ptype),
                     size=.25, colour="black",
                     pattern_density = 0.3,
                     pattern_fill = "black",
                     pattern_colour  = NA,
                     pattern_spacing=0.01
                     ) +
    geom_hline(yintercept = 0, size=0.25,color="black") +
    
    theme_bw() +
    facet_wrap(~Scenario, strip.position = "top",
               axes = "all", axis.labels = "all") +
    
    scale_x_continuous(expand = c(0.01, 0.01),limits=c(2022.5,2045.5),breaks=seq(2023,2045,by=1)) +
    scale_y_continuous(expand = c(0, 0),limits = c(-2, 6),breaks=seq(-2, 6,by=1)) +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),  
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = GenText_Sz+6, vjust=0,family = Plot_Text_bf),
          panel.background = element_rect(fill = "transparent"),
          axis.text.x=element_text(angle=90,vjust=0.5,color="black",size = GenText_Sz-6),
          axis.text.y=element_text(color="black",size = GenText_Sz-6),
          plot.title = element_blank(),
          legend.justification = c(0.5,0.5),
          legend.position = ("right"),
          legend.text = element_text(size = GenText_Sz-12),
          legend.title=element_blank(), 
          legend.key.size = unit(1,"lines"),
          strip.placement = "outside",
          strip.text = element_text(size = GenText_Sz, color = "black"),
          strip.background = element_rect(colour=NA, fill=NA),
          
    ) +
    
    guides(fill = guide_legend(ncol=1)) +
    
    scale_fill_manual(name="Plant Type",values=colours5d,drop = TRUE,limits = force) +
    scale_pattern_manual(name="Plant Type",values=Patterns5d,drop = TRUE,limits = force) +
    
    labs(y = "Annual Capacity Changes (GW)") 
}

################################################################################  
## FUNCTION: NetCapChange_Compare
## Gives net capacity added and retired each year in the same plot
##
## INPUTS: 
##    input - ResGroupYear
## TABLES REQUIRED: 
##    ResGroupYear -Yearly resoruce group emissions
################################################################################
NetCapChange_Compare <- function() {
  
  # Bring in Resource Year Table and filter for relevant data. Format date columns
  Add_Ret_data <- Ann_Cap%>%
    compare_rename(.,"l")%>%
    mutate(Scenario=as.factor(Scenario),
           Ptype = as.character(Plant_Type),
           Ptype=if_else(Ptype %in% c("Storage - Compressed Air","Storage - Pumped Hydro","Storage - Battery"),"Storage",Ptype)) %>%
    group_by(Scenario,Year,Ptype) %>%
    reframe(Cap_Add = sum(Capacity_Added)/1000,
            Cap_Ret = sum(Capacity_Retired)/-1000,
            Cap_net = sum(Difference_MW)/1000) %>%
    filter(!is.na(Year) & !is.na(Cap_Add) & !is.na(Cap_Ret)) %>%
    mutate(Year_fact=as.factor(Year))
  
  # Capacity limits for plot
  mny <- plyr::round_any(min(Add_Ret_data$Cap_net),2, f=floor)
  mxy <- plyr::round_any(max(Add_Ret_data$Cap_net),2, f=ceiling)

  
  # Set levels to each category in order specified
  Add_Ret_data$Ptype <- factor(Add_Ret_data$Ptype, 
                               levels=c("Coal","Coal-to-Gas", "Hydrogen Simple Cycle",
                                        "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle CCS Retrofit",
                                        "Natural Gas Combined Cycle", 
                                        "Hydro", "Other",
                                        "Wind", "Solar", 
                                        "Storage", 
                                        "Cogeneration"))
  
  
  new_names <- c("Cogen"="Cogeneration",
                 "H2SC"="Hydrogen Simple Cycle",
                 "NGCC"="Natural Gas Combined Cycle",
                 "NGCC+CCUS"="Natural Gas Combined Cycle + CCS",
                 "NGCC+CCUS Retrofit"="Natural Gas Combined Cycle CCS Retrofit",
                 "NGSC"= "Natural Gas Simple Cycle")
  
  Add_Ret_data$Ptype <- fct_recode(Add_Ret_data$Ptype, !!!new_names)
  
  #Plot data
  Add_Ret_data %>%
    ggplot() +
    # Plot added
    # geom_col(aes(Year, Cap_net, fill = Ptype),color="black") +
 
    geom_col_pattern(aes(Year, Cap_net, fill = Ptype,pattern=Ptype),
                     size=.25, colour="black",
                     pattern_density = 0.35,
                     pattern_fill = "black",
                     pattern_colour  = NA,
                     pattern_spacing=0.01
    ) +

    geom_hline(yintercept = 0, size=0.25,color="black") +
    
    theme_bw() +
    facet_wrap(~Scenario, strip.position = "top",
               axes = "all", axis.labels = "all") +
    
    scale_x_continuous(expand = c(0.01, 0.01),limits=c(2022.5,2045.5),breaks=seq(2023,2045,by=1)) +
    scale_y_continuous(expand = c(0, 0),limits = c(-2, 5),breaks=seq(-2,5,by=1)) +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),  
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = GenText_Sz+6, vjust=0,family = Plot_Text_bf),
          panel.background = element_rect(fill = "transparent"),
          axis.text.x=element_text(angle=90,vjust=0.5,color="black",size = GenText_Sz-6),
          axis.text.y=element_text(color="black",size = GenText_Sz-6),
          plot.title = element_blank(),
          legend.justification = c(0.5,0.5),
          legend.position = ("right"),
          legend.text = element_text(size = GenText_Sz-12),
          legend.title=element_blank(), 
          legend.key.size = unit(1,"lines"),
          strip.placement = "outside",
          strip.text = element_text(size = GenText_Sz, color = "black"),
          strip.background = element_rect(colour=NA, fill=NA),
          
          ) +
    
    guides(fill = guide_legend(ncol=1)) +
    
    scale_fill_manual(name="Plant Type",values=colours5d,drop = TRUE,limits = force) +
    scale_pattern_manual(name="Plant Type",values=Patterns5d,drop = TRUE,limits = force) +
    
    labs(y = "Net Annual Capacity Changes (GW)") 
}
