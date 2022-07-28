################################################################################
# TITLE: intertie_info
# DESCRIPTION: Functions To use for plotting and evaluating intertie activities

# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: July 21, 2022; LAST EDIT: July 21, 2022
#
################################################################################
###############################################################################  
## FUNCTION: Imp_Exp 
## AB imports and exports plotted as yearly totals 
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    Import_Yr - All imports from AB
##    Export_Yr - All exports from AB
################################################################################

Imp_Exp <- function(case) {
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
  
  # Plot
  data %>%
    ggplot() +
    aes(Time_Period, (Output_MWH/1000), fill = ID) +
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
    
    labs(x = "Year", y = "Total Annual Imports and Exports (GWh)", fill = "Resource") +
    
    guides(fill = guide_legend(nrow = 1)) 
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

BC_SK_IE <- function(case) {
  Imp_BC <- Import_Yr %>%
    filter(Name == "WECC_BritishColumbia") %>%
    filter(Run_ID == case) %>%
    mutate(Time_Period = format(.$Time_Period, format="%Y")) %>%
    mutate(ID = "AB Exports to BC") 
  
  Imp_SK <- Import_Yr %>%
    filter(Name == "MRO_Saskatchewan") %>%
    filter(Run_ID == case) %>%
    mutate(Time_Period = format(.$Time_Period, format="%Y")) %>%
    mutate(ID = "AB Exports to SK") 
  
  Exp_BC <- Export_Yr %>%
    filter(Name == "WECC_BritishColumbia") %>%
    filter(Run_ID == case) %>%
    mutate(Time_Period = format(.$Time_Period, format="%Y")) %>%
    mutate(ID = "AB Imports from BC") 
  
  Exp_SK <- Export_Yr %>%
    filter(Name == "MRO_Saskatchewan") %>%
    filter(Run_ID == case) %>%
    mutate(Time_Period = format(.$Time_Period, format="%Y")) %>%
    
    mutate(ID = "AB Imports from SK") 

  data <-rbind(Imp_BC,Imp_SK,Exp_BC,Exp_SK)
  
  # Re-arrange
  data$ID <- factor(data$ID,levels=c("AB Exports to BC","AB Exports to SK","AB Imports from BC","AB Imports from SK"),ordered=TRUE)
  
  
  # Set the max for the plot
  MX <- plyr::round_any(max(abs(data$Output_MWH)/1000), 1000, f = ceiling)
  
  # Plot
  data %>%
    ggplot() +
    aes(Time_Period, (Output_MWH/1000), fill = ID) +
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
    
    labs(x = "Year", y = "Annual Total Imports/Exports from AB (GWh)", fill = "Resource",caption = SourceDB) +
    
    guides(fill = guide_legend(nrow = 1)) 
  
  
}

################################################################################  
## FUNCTION: Evalyr_Int *Not Done 
## Plotting year profiles of BC/SK resource output
##
## INPUTS: 
##    input - ResgroupMnor ResGroupYr
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    Import - Import table derived of zone average table
################################################################################

Evalyr_Int <- function(prov,case) {
 
  # Plot
  data %>%
    ggplot() +
    aes(Time_Period, (Output_MWH/1000000), fill = ID, colour=ID) +
    geom_area(alpha=1, size=.5) +
    
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
    
    scale_x_date(expand=c(0,0),breaks = "year",date_labels = "%Y") +
    scale_y_continuous(expand=c(0,0),limits = c(0,MX),breaks=pretty_breaks(6)) +
    
    labs(x = "Date", y = "Annual Generation (TWh)", fill = "Resource",colour="Resource",caption = SourceDB) +
    
    guides(fill = guide_legend(nrow = 1))
  
  
}