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
AnnualEmStackCol <- function(case,Ptype) {
  
  if (Ptype == "NAICS"){
    # Filters for the desired case study
    data <- ResGroupEmYr %>%
      filter(Run_ID == case & Condition == "Average") %>%
      filter(Type== "CO2") %>%
      select(ID, Report_Year, Amount, Cost) %>%
      sim_filtEm(.)  %>%
      mutate(Amount=Amount*0.90718474) %>% # Convert Ton to Tonne
      filter(!ID=="Cogeneration") # Remove non-NAICS cogen
    
    CaptionE = paste(SourceDB,',','Includes cogeneration emissions attributed to electricity, reported under NAICS 2211122 only')
    
  }
  else if (Ptype == "ALL"){
    # Filters for the desired case study
    data <- ResGroupEmYr %>%
      filter(Run_ID == case & Condition == "Average") %>%
      filter(Type== "CO2") %>%
      select(ID, Report_Year, Amount, Cost) %>%
      sim_filtEm(.)  %>%
      mutate(Amount=Amount*0.90718474) %>% # Convert Ton to Tonne
      filter(!ID=="NAICS 221112 Cogeneration") # Remove NAICS 2211122 Cogen
    
    CaptionE = paste(SourceDB,',','Includes cogeneration emissions attributed to electricity only')
    
  }
  else {
    # Filters for the desired case study
    data <- ResGroupEmYr %>%
      filter(Run_ID == case & Condition == "Average") %>%
      filter(Type== "CO2") %>%
      select(ID, Report_Year, Amount, Cost) %>%
      sim_filtEm(.)  %>%
      mutate(Amount=Amount*0.90718474) %>% # Convert Ton to Tonne
      filter(!ID %in% c("Cogeneration","NAICS 221112 Cogeneration")) # Remove All cogen
    
    CaptionE = paste(SourceDB,',','Cogen has been removed from this figure')
  }

  # Set the max for the plot
  YearMX <- aggregate(data["Amount"], by=data["Report_Year"], sum)
  YearMX$Amount <-YearMX$Amount/1000000
  MX <- plyr::round_any(max(abs(YearMX$Amount+1)), 5, f = ceiling)
  
  # Format years to assist plot
  data$Report_Year  <- as.numeric(data$Report_Year)
  
  # Get Year max for run
  MaxYr <- MaxYrStudy
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
          axis.title.y = element_text(size = GenText_Sz+6),
          plot.title = element_text(size = GenText_Sz),
          plot.subtitle = element_text(hjust = 0.5), 
          panel.background = element_rect(fill = NA),
          #panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          legend.key.size = unit(1,"lines"), #Shrink legend
          legend.position = "bottom",
          legend.justification = c(0.5,0.5),
          legend.title=element_blank(),
          legend.text = element_text(size = GenText_Sz-6),
          text = element_text(size = GenText_Sz)) +
    
    scale_x_continuous(expand = c(0, 0),limits = NULL,breaks=seq(MinYr, MaxYr, by=1)) +
    
    scale_y_continuous(expand=c(0,0),limits = c(0,MX),breaks=pretty_breaks(6)) +
    
    labs(x = "Year", y = "Annual Emissions (Mt Co2e)", fill = "Resource",colour="Resource",caption = CaptionE) +
    
    guides(fill = guide_legend(nrow = 2)) +
    
    scale_fill_manual(values = colours7,drop = TRUE) 
  
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
AnnualEmLine <- function(case,Ptype) {
  
  if (Ptype == "NAICS"){
    # Filters for the desired case study
    data <- ResGroupEmYr %>%
      filter(Run_ID == case & Condition == "Average") %>%
      filter(Type== "CO2") %>%
      select(ID, Report_Year, Amount, Cost) %>%
      sim_filtEm(.)  %>%
      mutate(Amount=Amount*0.90718474) %>% # Convert Ton to Tonne
      filter(!ID=="Cogeneration") # Remove non-NAICS cogen
    
    CaptionE = paste(SourceDB,',','Includes cogeneration emissions attributed to electricity, reported under NAICS 2211122 only')
    
  }
  else if (Ptype == "ALL"){
    # Filters for the desired case study
    data <- ResGroupEmYr %>%
      filter(Run_ID == case & Condition == "Average") %>%
      filter(Type== "CO2") %>%
      select(ID, Report_Year, Amount, Cost) %>%
      sim_filtEm(.)  %>%
      mutate(Amount=Amount*0.90718474) %>% # Convert Ton to Tonne
      filter(!ID=="NAICS 221112 Cogeneration") # Remove NAICS 2211122 Cogen
    
    CaptionE = paste(SourceDB,',','Includes cogeneration emissions attributed to electricity only')
    
  }
  else {
    # Filters for the desired case study
    data <- ResGroupEmYr %>%
      filter(Run_ID == case & Condition == "Average") %>%
      filter(Type== "CO2") %>%
      select(ID, Report_Year, Amount, Cost) %>%
      sim_filtEm(.)  %>%
      mutate(Amount=Amount*0.90718474) %>% # Convert Ton to Tonne
      filter(!ID %in% c("Cogeneration","NAICS 221112 Cogeneration")) # Remove All cogen
    
    CaptionE = paste(SourceDB,',','Cogen has been removed from this figure')
  }
  
  data$Report_Year  <- as.numeric(data$Report_Year)
  
  # Get Year max for run
  MaxYr <- MaxYrStudy
  MinYr <- (min(data$Report_Year))
  
  # Get total emissions per year
  AllEm <- data %>%
    group_by(Report_Year) %>%
    summarise(.,Amount=sum(Amount),Cost=sum(Cost))%>%
    ungroup() %>%
    relocate(Report_Year, .before = Amount)  %>%
    add_column(ID="Total Emissions",.before="Report_Year")
  
  CombData <-rbind(data,AllEm) %>%
    filter(Report_Year<=MaxYr)
  
  
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
          plot.title = element_text(size = GenText_Sz),
          plot.subtitle = element_text(hjust = 0.5), 
          panel.background = element_rect(fill = NA),
          # panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          #legend.key.size = unit(1,"lines"), #Shrink legend
          legend.position = "right",
          legend.text = element_text(size = GenText_Sz-6),
          legend.justification = c(0.5,0.5),
          legend.title=element_blank(),
          text = element_text(size = GenText_Sz)) +
    
    labs(x = "Year", y = "Annual Emissions (Mt Co2e)",colour="ID",linetype="ID",caption = CaptionE) +
    
    scale_x_continuous(expand = c(0, 0),limits = NULL,breaks=seq(MinYr, MaxYr, by=1)) +
    
    scale_y_continuous(expand=c(0,0),limits = c(0,MX),breaks=pretty_breaks(6)) +
    
    scale_colour_manual(name="Guide1",values = colours7,drop = TRUE) +
    scale_linetype_manual(name="Guide1",values = Lines7,drop = TRUE)
  
  
  
}

################################################################################
## FUNCTION: Emissions_CER_Res
## Show hours of opperation for each resource in each year.  
##
## INPUTS: 
##
##    case - case to see 
## TABLES REQUIRED: 
##    ResGroupHr_sub - Hourly resource group tables
################################################################################
  Emissions_CER_Res <- function(case) {
    
    # Plots the capacity factor by technology for AESO and Sim
    # Like AESO Market Report 2021 Figure 15
    
    CER_Names <- ResYr%>%
      sim_filt3(.) %>% #Filter to rename fuels
      subset(., select=c(YEAR,Time_Period,End_Date,Beg_Date,Name,Condition,Capacity,Run_ID,Primary_Fuel,Capacity_Factor,Total_Hours_Run,Active_Constraints)) %>%
      filter(Run_ID == case,
             Condition == "Average",
             Primary_Fuel %in% c("Coal-to-Gas","Natural Gas Simple Cycle","Natural Gas Combined Cycle")) %>%
      mutate(CER_year=if_else(grepl('2035',Active_Constraints)==TRUE,"CER 2035",
                              if_else(grepl('2036',Active_Constraints)==TRUE,"CER 2036",
                                      if_else(grepl('2040',Active_Constraints)==TRUE,"CER 2040",
                                              if_else(grepl('2044',Active_Constraints)==TRUE,"CER 2044",
                                                      if_else(grepl('2045',Active_Constraints)==TRUE,"CER 2045","CER NA")))))) %>%
      filter(!CER_year == "CER NA")
    
    # Get names of CER plants
    CER_Names_list<-as.list(unique(CER_Names$Name))
    
    # Get types for each plant
    CER_Types<-CER_Names %>%
      filter(YEAR==2024) %>%
      rename(Resource_Name=Name)%>%
      select(Resource_Name, Primary_Fuel,CER_year)
    
    # Emissions date for plants
    Emdata <- ResEmYr %>%
      filter(Run_ID == case,
             Condition == "Average",
             Type== "CO2",
             Resource_Name %in% CER_Names_list) %>%
      select(Resource_Name, Time_Period,Report_Year, Amount, Cost) %>%
      mutate(Amount=Amount*0.90718474/1000,
             Time_Period=as.numeric(Time_Period)) # Convert Ton to Tonne

    # Add types back in 
    Emdata<-merge(Emdata,CER_Types,by=c("Resource_Name"), all.x = TRUE)
    
    # Plot max
    Em_max<-max(Emdata$Amount)+10
    
    # Generate plot
    ggplot(Emdata)+
      geom_line(aes(x = Time_Period, y = Amount, colour = CER_year,group=Resource_Name), 
                size = 1.5) +
      
      theme_bw() +
      
      # Changes the font type
      theme(text=element_text(family=Plot_Text)) +             
      
      geom_hline(yintercept=150, color = "darkred",size=0.25,linetype=2) +
      geom_text(data = data.frame(x=1,y=1),aes(2023.5,150,label = "Maximum Emissions Constraint = 150"), vjust = -1,hjust=0, colour="darkred",size=5)  +
      
      theme(
        # General Plot Settings
        panel.grid = element_blank(),
        # (t,r,b,l) margins, adjust to show full x-axis, default: (5.5,5.5,5.5,5.5)
        plot.margin = unit(c(6, 12, 5.5, 5.5), "points"),      # Plot margins
        panel.background = element_rect(fill = "transparent"), # Transparent background
        text = element_text(size = GenText_Sz),                # Text size
        plot.title = element_text(size = GenText_Sz),              # Plot title size (if present)
        plot.subtitle = element_text(hjust = 0.5),             # Plot subtitle size (if present)
        #panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray90'), # Adds horizontal lines
        plot.caption = element_text(size = GenText_Sz-10),
        # X-axis
        axis.text.x = element_text(vjust = 1,color="black"),                 # Horizontal text
        axis.title.x = element_blank(),                         # y-axis title text size
        #axis.title.x = element_text(size = XTit_Sz),           # x-axis title text size
        # Y-axis
        axis.title.y = element_text(size = GenText_Sz+6),           # y-axis title text size
        axis.text.y=element_text(color="black"),
        # Legend
        legend.key.size = unit(1,"lines"),                     # Shrink legend boxes
        legend.position = "bottom",                             # Move legend to the bottom
        legend.justification = c(0.5,0.5),                     # Center the legend
        legend.text = element_text(size =GenText_Sz-6),              # Size of legend text
        legend.title=element_blank()) +                        # Remove legend title
      
      # Set axis scales
      scale_x_continuous(expand=c(0,0),limits=c(2023,MaxYrStudy)) +
      scale_y_continuous(expand=c(0,0),limits=c(0,Em_max),breaks=pretty_breaks(5)) +
      
      # Plot labels
      labs(x = "Year", y = "Emissions (kt CO2e)", 
           colour="Plant_Type",caption = SourceDB) +
      
      # Legend color scheme
      scale_colour_manual(values = c("CER 2035"='black',"CER 2036"='grey30',"CER 2040"='gray60',"CER 2044"='grey75','CER 2045'='grey90'),drop = FALSE)  
  }  
  
################################################################################
## FUNCTION: Emissions_CER_group
## Show emissions for CER groups each year
##
## INPUTS: 
##
##    case - case to see 
## TABLES REQUIRED: 
##    ResGroupHr_sub - Hourly resource group tables
################################################################################
  Emissions_CER_group <- function(case) {
    
    # Emissions date for plants
    Emdata <- ResGroupEmYr %>%
      filter(Run_ID == case,
             Condition == "Average",
             Type== "CO2",
             ID %in% c("CER_2035","CER_2036","CER_2044","CER_2045")) %>%
      select(ID,Report_Year, Amount, Cost) %>%
      mutate(Amount=Amount*0.90718474/1000000,
             Report_Year=as.numeric(Report_Year)) # Convert Ton to Tonne

    # Plot max
    Em_max<-max(Emdata$Amount)+1
    
    # Generate plot
    ggplot(Emdata)+
      geom_line(aes(x = Report_Year, y = Amount, colour = ID), 
                size = 1.5) +
      
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
        plot.title = element_text(size = GenText_Sz),              # Plot title size (if present)
        plot.subtitle = element_text(hjust = 0.5),             # Plot subtitle size (if present)
        #panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray90'), # Adds horizontal lines
        plot.caption = element_text(size = GenText_Sz-10),
        # X-axis
        axis.text.x = element_text(vjust = 1,color="black"),                 # Horizontal text
        axis.title.x = element_blank(),                         # y-axis title text size
        #axis.title.x = element_text(size = XTit_Sz),           # x-axis title text size
        # Y-axis
        axis.title.y = element_text(size = GenText_Sz+6),           # y-axis title text size
        axis.text.y=element_text(color="black"),
        # Legend
        legend.key.size = unit(1,"lines"),                     # Shrink legend boxes
        legend.position = "bottom",                             # Move legend to the bottom
        legend.justification = c(0.5,0.5),                     # Center the legend
        legend.text = element_text(size =GenText_Sz-6),              # Size of legend text
        legend.title=element_blank()) +                        # Remove legend title
      
      # Set axis scales
      scale_x_continuous(expand=c(0,0),limits=c(2023,MaxYrStudy)) +
      scale_y_continuous(expand=c(0,0),limits=c(0,Em_max),breaks=pretty_breaks(5)) +
      
      # Plot labels
      labs(x = "Year", y = "Emissions (Mt CO2e)", 
           colour="Plant_Type",caption = SourceDB) +
      
      # Legend color scheme
      scale_colour_manual(values = c("CER_2035"='grey20',"CER_2036"='grey40',"CER_2044"='grey70',CER_2045='grey90'),drop = FALSE) 
  }  
