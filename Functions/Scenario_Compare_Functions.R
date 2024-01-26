################################################################################
# TITLE: Scenario_Compare_Functions
# DESCRIPTION: Functions for plots with data from multiple scenarios
#
# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: January 22, 2024; 
#
################################################################################

################################################################################
## FUNCTION: AvgYr_price_COMPARE
## Plots monthly average pool price with average internal load
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
AvgYr_price_COMPARE <- function(txt_sz_choose) {
  
  # Filter and prepare Simulation data
  Sim <- Zone %>%
    mutate(Year=as.numeric(Year))
  
  # Get plot max/mins
  YearMX<-max(Sim$Year)
  YearMN<-min(Sim$Year)
  Upplim <- round_any(max(Sim$Avg_Price)+51,50)
  
  # Reset text size if wanted
  GenText_Sz=txt_sz_choose
  
  ggplot(Sim) +
    geom_line(aes(x = Year, y = Avg_Price, colour = Scenario,linetype= Scenario), 
              size = 1.5) +
    theme_bw() +
    theme(text=element_text(family=Plot_Text)) +
    theme(axis.text = element_text(color="black"),
          axis.title = element_text(size = GenText_Sz+6),
          axis.text.x = element_text(angle = 45, hjust=1,color="black"),
          plot.title = element_blank(),
          text = element_text(size=GenText_Sz),
          axis.title.x=element_blank(),
          legend.text = element_text(size = GenText_Sz-6),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(y = "Annaul Average Pool Price ($/MWh)", x="Year",colour="Condition",linetype="Condition") +
    
    scale_colour_grey() +
    
    scale_x_continuous(expand=c(0,0),limits = c(YearMN,YearMX),breaks=seq(YearMN, YearMX, 1)) +
    
    scale_y_continuous(expand=c(0,0),limits=c(0,Upplim),n.breaks = 5, 
    )
}