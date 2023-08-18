################################################################################
# TITLE: Emission_Functions
# DESCRIPTION: Functions to evaluate and show emissions.

# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: January 6, 2023; LAST EDIT: January 6, 2023

################################################################################

################################################################################  
## FUNCTION: AnnualEmStackCol
## Plot annual emissions by resource group as as stacked chart
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupEmYr -Yearly resoruce group emissions
################################################################################
AnnualEmStackCol <- function(case) {
  
  # Filters for the desired case study
  data <- ResGroupEmYr %>%
    filter(Run_ID == case & Condition == "Average") %>%
    filter(Type== "CO2") %>%
    select(ID, Report_Year, Amount, Cost) %>%
    sim_filtEm(.)  %>%
    mutate(Amount=Amount*0.90718474)
      # %>% # Convert Ton to Tonne
      # filter(!ID=="Cogeneration") # Temp remove cogen
  
  # Set the max for the plot
  YearMX <- aggregate(data["Amount"], by=data["Report_Year"], sum)
  YearMX$Amount <-YearMX$Amount/1000000
  MX <- plyr::round_any(max(abs(YearMX$Amount+1)), 5, f = ceiling)
  
  # Format years to assist plot
  data$Report_Year  <- as.numeric(data$Report_Year)
  
  # Get Year max for run
  MaxYr <- as.numeric(max(data$Report_Year))-5
  MinYr <- (min(data$Report_Year))
  
  # Filter to remove the final 5 years (as per AURORA, want to run 5 years past year of interest)
  data <- data%>%
    filter(Report_Year<=MaxYr)
  
  # Print the result out for 2035 
  message("Total Annaul Emissions (Mt)")
  print(YearMX)
  
  # Plot 
  data %>%
    ggplot() +
    aes(Report_Year, (Amount/1000000), fill = ID,) +
    geom_bar(position="stack", stat="identity",alpha=Plot_Trans, size=.5, colour="black") +
    
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(vjust = 1,color="black"),
          axis.text.y = element_text(color="black"),          
          #axis.title.x = element_text(size = XTit_Sz),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = YTit_Sz),
          plot.title = element_text(size = Tit_Sz),
          plot.subtitle = element_text(hjust = 0.5), 
          panel.background = element_rect(fill = NA),
          #panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          legend.key.size = unit(1,"lines"), #Shrink legend
          legend.position = "bottom",
          legend.justification = c(0.5,0.5),
          legend.title=element_blank(),
          text = element_text(size = 15)) +
    
    scale_x_continuous(expand = c(0, 0),limits = NULL,breaks=seq(MinYr, MaxYr, by=1)) +
    
    scale_y_continuous(expand=c(0,0),limits = c(0,MX),breaks=pretty_breaks(6)) +
    
    labs(x = "Year", y = "Annual Emissions (Mt Co2e)", fill = "Resource",colour="Resource",caption =paste(SourceDB,',','Cogen has been removed for this figure')) +
    
    guides(fill = guide_legend(nrow = 2)) +
    
    scale_fill_manual(values = colours7,drop = FALSE) 
  
}

################################################################################  
## FUNCTION: AnnualEmLine
## Plot annual emissions by resource group as as stacked chart
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupEmYr -Yearly resoruce group emissions
################################################################################
AnnualEmLine <- function(case) {
  
  # Filters for the desired case study
  data <- ResGroupEmYr %>%
    filter(Run_ID == case & Condition == "Average") %>%
    filter(Type== "CO2") %>%
    select(ID, Report_Year, Amount, Cost) %>%
    sim_filtEm(.)  %>%
    mutate(Amount=Amount*0.90718474)%>% # Convert Ton to Tonne
    filter(!ID=="Cogeneration") # Temp remove cogen for now
  
  data$Report_Year  <- as.numeric(data$Report_Year)
  
  # Get Year max for run
  MaxYr <- as.numeric(max(data$Report_Year))
  MinYr <- (min(data$Report_Year))
  
  # Get total emissions per year
  AllEm <- data %>%
    group_by(Report_Year) %>%
    summarise(.,Amount=sum(Amount),Cost=sum(Cost))%>%
    ungroup() %>%
    relocate(Report_Year, .before = Amount)  %>%
    add_column(ID="Total Emissions",.before="Report_Year")
  
  CombData <-rbind(data,AllEm)
  
  
  # Set the max for the plot
  MX <- plyr::round_any(max(abs(CombData$Amount/1000000)+1), 1, f = ceiling)
  
  # Plot
  CombData %>%
    ggplot() +
    geom_line(size=1.5,alpha = 1) +
    aes(Report_Year, (Amount/1000000),colour = ID,linetype = ID) +
    
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(vjust = 1,color="black"),
          axis.text.y = element_text(color="black"),
          #axis.title.x = element_text(size = XTit_Sz),
          axis.title.x = element_blank(),          axis.title.y = element_text(size = YTit_Sz),
          plot.title = element_text(size = Tit_Sz),
          plot.subtitle = element_text(hjust = 0.5), 
          panel.background = element_rect(fill = NA),
          # panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          #legend.key.size = unit(1,"lines"), #Shrink legend
          legend.position = "bottom",
          legend.justification = c(0.5,0.5),
          legend.title=element_blank(),
          text = element_text(size = 15)) +
    
    labs(x = "Year", y = "Annual Emissions (Mt Co2e)",colour="ID",linetype="ID",caption = paste(SourceDB,',','Cogen has been removed for this figure')) +
    
    scale_x_continuous(expand = c(0, 0),limits = NULL,breaks=seq(MinYr, MaxYr, by=1)) +
    
    scale_y_continuous(expand=c(0,0),limits = c(0,MX),breaks=pretty_breaks(6)) +
    
    scale_colour_manual(name="Guide1",values = colours7,drop = FALSE) +
    scale_linetype_manual(name="Guide1",values = Lines7,drop = FALSE)
  
  
  
}
