################################################################################
# TITLE: sim_eval_1
# DESCRIPTION: Functions To use for plotting and evaluating simulation data.

# ORIGINAL AUTHOR: Taylor Pawlenchuk (Retrieved June 3, 2022)
# EDITS & ADDITIONAL CONTENT: Jessica Van Os
# LAST EDIT: June 17, 2022
#
################################################################################
#
#
# TO USE IN PLOTING FUNCTIONS SECTION
#
#
################################################################################

round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

## FUNCTION: sim_filt
## This function filters for the data that will be evaluated.
################################################################################

{ sim_filt <- function(inputdata) {
  
    # Filter the data by resource, creates a table for each resource
    {Coal <- inputdata %>%
      filter(ID=="LTO_Coal")
    Coal2Gas  <- inputdata %>%
      filter(ID=="LTO_Coal2Gas")
        #Force zero output if negative
        #Coal2Gas$Output_MWH[Coal2Gas$Output_MWH <= 0] <- 0
    Cogen  <- inputdata %>%
      filter(ID=="LTO_Cogen")
    NatGas <- inputdata %>%
      filter(ID=="LTO_NatGas")
    Other <- inputdata %>%
      filter(ID=="LTO_Other")
    Hydro <- inputdata %>%
      filter(ID=="LTO_Hydro")
    Solar <- inputdata %>%
      filter(ID=="LTO_Solar")
    Storage <- inputdata %>%    
      filter(ID=="LTO_Storage")
    Wind <- inputdata %>%
      filter(ID=="LTO_Wind")   }
    
  # Combine the grouped data tables into one
  { case <- rbind(Coal, Coal2Gas, Cogen, NatGas, Hydro, Solar, Wind, Storage, Other)
    
    # Sort the table by case ID
    #A factor is a categorical variable 
    case$ID <- factor(case$ID, levels=c("LTO_Coal", "LTO_Coal2Gas", "LTO_Cogen", 
                                        "LTO_NatGas", "LTO_Hydro","LTO_Other",  
                                        "LTO_Wind", "LTO_Solar", "LTO_Storage"))
    # Replace ID value with name 
    levels(case$ID) <- c("Coal", "Coal-to-Gas", "Cogen", "Natural Gas", "Hydro", "Other",
                         "Wind", "Solar", "Storage")   }
    return(case)  }
}
  
################################################################################
## FUNCTION: sim_filt1
## This function filters for the data that will be evaluated.  
################################################################################
  
  { sim_filt1 <- function(inputdata) {
      # Filter the data by resource
      {Coal <- inputdata %>%
        filter(ID=="LTO_Coal")
      NGConv <- inputdata %>%
        filter(ID=="AB_NGCONV")
      SCCT  <- inputdata %>%
        filter(ID=="AB_SCCT_noncogen")
      Cogen  <- inputdata %>%
        filter(ID=="LTO_Cogen")
      CCCT <- inputdata %>%
        filter(ID=="AB_CCCT_noncogen")
      Other <- inputdata %>%
        filter(ID=="LTO_Other")
      Hydro <- inputdata %>%
        filter(ID=="LTO_Hydro")
      Solar <- inputdata %>%
        filter(ID=="LTO_Solar")
      Storage <- inputdata %>%    
        filter(ID=="LTO_Storage")
      Wind <- inputdata %>%
        filter(ID=="LTO_Wind")  }
      
    # Combine the grouped data
    { case <- rbind(Coal, Cogen, NGConv, SCCT, CCCT, Hydro, Solar, Wind, Storage, Other)
      case$ID <- factor(case$ID, levels=c("LTO_Coal", "LTO_Cogen","AB_NGCONV", "AB_SCCT_noncogen", "AB_CCCT_noncogen",
                                           "LTO_Hydro", "LTO_Other", 
                                          "LTO_Wind", "LTO_Solar", "LTO_Storage"))
      
      levels(case$ID) <- c("Coal","Cogen", "Coal-to-Gas", "SCGT", "NGCC", "Hydro", "Other",
                           "Wind", "Solar", "Storage")  }
      return(case)  }
  }

################################################################################
## FUNCTION: sim_filt2
## This function filters for the data that will be evaluated.  
################################################################################

{ sim_filt2 <- function(inputdata) {
  # Filter the data by resource
  {Coal <- inputdata %>%
    filter(Primary_Fuel=="Coal Canada West") 
  Cogen  <- inputdata %>%
    filter(Primary_Fuel=="WECC-AECO Hub NaturalGas-COGEN_oilsands_Alberta")
  Gas  <- inputdata %>%
    filter(Primary_Fuel=="WECC-Alberta NaturalGas-NonCycling")
  Gas1 <- inputdata %>%
    filter(Primary_Fuel=="WECC-Alberta NaturalGas")
  Gas2 <- inputdata %>%
    filter(Primary_Fuel=="WECC-Alberta NaturalGas-Peaking")
  Other <- inputdata %>%
    filter(Primary_Fuel=="Other, Bio, ZZ, WC, WH")
  Hydro <- inputdata %>%
    filter(Primary_Fuel=="Water")
  Solar <- inputdata %>%
    filter(Primary_Fuel=="Solar")
  Storage <- inputdata %>%    
    filter(Primary_Fuel=="Storage")
  Wind <- inputdata %>%
    filter(Primary_Fuel=="Wind")  }
  
  # Combine the grouped data
  { case <- rbind(Coal, Cogen, Gas, Gas1, Gas2, Hydro, Solar, Wind, Storage, Other)
    
    case$Primary_Fuel <- factor(case$Primary_Fuel, levels=c("Coal Canada West", "WECC-AECO Hub NaturalGas-COGEN_oilsands_Alberta", 
                                        "WECC-Alberta NaturalGas-NonCycling","WECC-Alberta NaturalGas",
                                        "WECC-Alberta NaturalGas-Peaking","Water", "Solar", 
                                        "Wind", "Storage", "Other, Bio, ZZ, WC, WH"))
    
    levels(case$Primary_Fuel) <- c("Coal", "Cogen", "NG - NonCycling", "NG", "NG - Peaking", "Hydro","Solar",
                         "Wind", "Storage", "Other")  }
  return(case)  }
}

################################################################################
## FUNCTION: sim_filt3
## This function filters for the data that will be evaluated.  
################################################################################

{ sim_filt3 <- function(inputdata) {
  # Filter the data by resource
  {Coal <- inputdata %>%
    filter(Primary_Fuel=="Coal Canada West") 
  Cogen  <- inputdata %>%
    filter(Primary_Fuel=="WECC-AECO Hub NaturalGas-COGEN_oilsands_Alberta")
  CCCT  <- inputdata %>%
    filter(Primary_Fuel=="WECC-Alberta NaturalGas-NonCycling")
  Gas2 <- inputdata %>%
    filter(Primary_Fuel=="WECC-Alberta NaturalGas-Peaking")
  
  NGConv <- Gas2[Gas2$Name %like% "%Retrofit%",] # Seperate retrofits
  NGConv$Primary_Fuel<- "NGConv"
  
  SCCT1 <- inputdata %>%
    filter(Primary_Fuel=="WECC-Alberta NaturalGas")
  SCCT2 <- Gas2[Gas2$ID %like% "%Simple%",]  # Seperate Simple Cycle
  SCCT <- rbind(SCCT1,SCCT2)
  SCCT$Primary_Fuel<- "NG-SCCT"
  
  Other <- inputdata %>%
    filter(Primary_Fuel=="Other, Bio, ZZ, WC, WH")
  Hydro <- inputdata %>%
    filter(Primary_Fuel=="Water")
  Solar <- inputdata %>%
    filter(Primary_Fuel=="Solar")
  Storage <- inputdata %>%    
    filter(Primary_Fuel=="Storage")
  Wind <- inputdata %>%
    filter(Primary_Fuel=="Wind")  }
  
  # Combine the grouped data
  { case <- rbind(Coal, Cogen, NGConv, SCCT, CCCT, Hydro, Solar, Wind, Storage, Other)
    
    case$Primary_Fuel <- factor(case$Primary_Fuel, levels=c("Coal Canada West", "WECC-AECO Hub NaturalGas-COGEN_oilsands_Alberta", 
                                                            "NGConv","NG-SCCT",
                                                            "WECC-Alberta NaturalGas-NonCycling","Water", "Solar", 
                                                            "Wind", "Storage", "Other, Bio, ZZ, WC, WH"))
    
    levels(case$Primary_Fuel) <- c("Coal", "Cogen", "Coal-to-Gas", "SCCT", "NGCC", "Hydro","Solar",
                                   "Wind", "Storage", "Other")  }
  return(case)  }
}
################################################################################
## FUNCTION: HrTime
## Convert the date and select a subset for one day from the data pulled in
################################################################################
  
{ HrTime <- function(data, year, month, day) {
    subset(data,
          (date >= paste(year,"-", month, "-", day," 00:00:00", sep = "") & 
              date <= 
              paste(year,"-", month, "-", (day)," 24:00:00", sep = "")))  }
}

################################################################################
## FUNCTION: WkTime
## Convert the date and select a subset for one week from the data pulled in
################################################################################

{ WkTime <- function(data, year, month, day) {
  
  #Set start and end dates of week  
  wk_st <- as.POSIXct(paste(day,month,year, sep = "/"), format="%d/%m/%Y")
  wk_end <- as.POSIXct(paste(day+7,month,year, sep = "/"), format="%d/%m/%Y")
  
  #Create subset for specified week
  subset(data,
         (date >= wk_st & date <= wk_end)) 
  
  }
}

################################################################################
## FUNCTION: YrTime
## Convert the date and select a subset for one week from the data pulled in
################################################################################

{ YrTime <- function(data, year) {
  
  #Set start and end dates of week  
  yr_st <- as.POSIXct(paste(01,01,year, sep = "/"), format="%d/%m/%Y")
  yr_end <- as.POSIXct(paste(31,12,year, sep = "/"), format="%d/%m/%Y")
  
  #Create subset for specified week
  subset(data,
         (date >= yr_st & date <= yr_end)) 
  
}
}

################################################################################
#
#
# PLOTING FUNCTIONS SECTION
#
#
################################################################################
################################################################################
## FUNCTION: week1
## Plots output for a single week given the case study
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupHr_sub - Filtered version of Resource Group Hour Table
##    ZoneHr_Avg - Average hourly info in zone
##    Export - Exports selected from Zone Hourly Table
################################################################################

  Week1 <- function(year, month, day, case) {
    # Filters for the desired case study from the resource groups
    data <- ResGroupHr_sub%>%
      sim_filt1(.) %>%
      subset(., select=-c(Report_Year,Capacity_Factor)) %>%
      rbind(.,Import) %>%
      filter(Run_ID == case)
    
    # Set levels to each category in order specified
    data$ID <- factor(data$ID, levels=c("Import","Coal","Cogen", "Coal-to-Gas", "SCGT", "NGCC", "Hydro", "Other",
                                        "Wind", "Solar", "Storage"))
    ## SELECT A SINGLE WEEK

    # Select only a single week from the zone Hourly, and Export data
    WK <- WkTime(data,year,month,day)
    ZPrice <- WkTime(ZoneHr_Avg,year,month,day) %>%
      filter(Run_ID == case)
    Expo <- WkTime(Export,year,month,day) %>%
      filter(Run_ID == case)
    
    # Get y-max, demand to meet + exports
    WK$MX <- ZPrice$Demand + Expo$Output_MWH
    
    # Set the max and min for the plot Output axis (y), Set slightly above max (200 above)
    MX <- plyr::round_any(max(abs(WK$MX))+500, 100, f = ceiling)
    MN <- plyr::round_any(min(WK$Output_MWH), 100, f = floor)
    
    # Title Formating
    wk_st <- as.Date(paste(year,month,day, sep = "-"),tz="MST")
    wk_end <- as.Date(paste(year,month,day+7, sep = "-"),tz="MST")
    
    
    ## PLOT WITH AREA PLOT
    
    ggplot() +
      geom_area(data = WK, aes(x = date, y = Output_MWH, fill = ID, colour = ID), 
                alpha=0.7, size=0.5) +

      # Add hourly load line (black line on the top)
      geom_line(data = ZPrice, 
                aes(x = date, y = Demand), size=1.5, colour = "black") +
      scale_x_datetime(expand=c(0,0),date_labels = "%b-%e", breaks = "day") +
      
      # Set the theme for the plot
      theme_bw() +
      theme(panel.grid = element_blank()) +
      
      theme(text=element_text(family=Plot_Text)) +
      
      theme(plot.title = element_text(size= Tit_Sz)) +
      
      theme(axis.text.x = element_text(vjust = 1),
            axis.title.x = element_text(size= XTit_Sz),
            axis.title.y = element_text(size= YTit_Sz),
            panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent", color = NA),
            legend.title=element_blank(),
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.background = element_rect(fill='transparent',colour ='transparent'),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
            legend.key.size = unit(1,"lines"), #Shrink legend
            legend.position = "bottom",
            text = element_text(size= 15)
      ) +
      scale_y_continuous(expand=c(0,0), limits = c(0,MX), 
                         breaks = seq(0, MX, by = MX/4)) +
      guides(fill = guide_legend(nrow = 1)) +
      
      labs(x = "Date", y = "Output (MWh)", fill = "Resource", colour = "Resource",caption=SourceDB) +
      
      #Add colour
      scale_fill_manual(values = colours1) +
      
      # Make outline the same as fill colors
      scale_colour_manual(values = Outline1)
  }

################################################################################
## FUNCTION: Week14
## Same as week1 except adjusts limits for comparison
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupHr_sub - Filtered version of Resource Group Hour Table
##    ZoneHr_Avg - Average hourly info in zone
##    Export - Exports selected from Zone Hourly Table
################################################################################

Week14 <- function(year, month, day, case) {
  # Filters for the desired case study from the resource groups
  data <- ResGroupHr_sub%>%
    sim_filt1(.) %>%
    subset(., select=-c(Report_Year,Capacity_Factor)) %>%
    rbind(.,Import) %>%
    filter(Run_ID == case)
  
  # Set levels to each category in order specified
  data$ID <- factor(data$ID, levels=c("Import","Coal","Cogen","Coal-to-Gas",  "SCGT", "NGCC", "Hydro", "Other",
                                      "Wind", "Solar", "Storage"))
  ## SELECT A SINGLE WEEK
  
  # Select only a single week from the zone Hourly, and Export data
  WK <- WkTime(data,year,month,day)
  ZPrice <- WkTime(ZoneHr_Avg,year,month,day) %>%
    filter(Run_ID == case)
  Expo <- WkTime(Export,year,month,day) %>%
    filter(Run_ID == case)
  
  # Get y-max, demand to meet + exports
  WK$MX <- ZPrice$Demand + Expo$Output_MWH
  
  # Set the max and min for the plot
  MX1 <- WkTime(WK,Yr4Sp[[1]],month,day)
  MX2 <- WkTime(WK,Yr4Sp[[2]],month,day)
  MX3 <- WkTime(WK,Yr4Sp[[3]],month,day)
  MX4 <- WkTime(WK,Yr4Sp[[4]],month,day)
  MXtime <- rbind(MX1, MX2, MX3, MX4)
  
  # Set the max and min for the plot Output axis (y), Set slightly above max (500 above)
  MX <- plyr::round_any(max(abs(MXtime$MX))+500, 100, f = ceiling)
  MN <- plyr::round_any(min(MXtime$Output_MWH), 100, f = floor)
  
  
  # Title Formating
  wk_st <- as.POSIXct(paste(year,month,day, sep = "-"),tz="MST")
  wk_end <- as.POSIXct(paste(year,month,day+7, sep = "-"),tz="MST")
  
  ## PLOT WITH AREA PLOT
  
  ggplot() +
    geom_area(data = WK, aes(x = date, y = Output_MWH, fill = ID, colour = ID), 
              alpha=0.7, size=0.5) +
    
    # Add hourly load line (black line on the top)
    geom_line(data = ZPrice, 
              aes(x = date, y = Demand), size=1.5, colour = "black") +
    scale_x_datetime(expand=c(0,0),date_labels = "%b-%e" ,breaks = "day") +
    
    # Set the theme for the plot
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = "right",) +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(plot.title = element_text(size= Tit_Sz)) +
    
    theme(axis.text.x = element_text(vjust = 1),
          axis.title.x = element_text(size= XTit_Sz,face = 'bold'),
          axis.title.y = element_text(size= YTit_Sz,face = 'bold'),
          panel.background = element_rect(fill = "transparent"),
         # panel.grid.major.y = element_line(size=0.25,linetype=5,color = 'gray36'),
          panel.ontop = TRUE,
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.title=element_blank(),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          legend.key.size = unit(1,"lines"), #Shrink legend
          legend.position = "bottom",
          text = element_text(size= 15)
    ) +
    scale_y_continuous(expand=c(0,0), limits = c(0,MX), 
                       breaks = seq(MN, MX, by = MX/4)) +
    guides(fill = guide_legend(nrow = 1)) +
    
    labs(x = "Date", y = "Output (MWh)", fill = "Resource", colour = "Resource") +
    
    #Add colour
    scale_fill_manual(values = colours1) +
    
    # Make outline the same as fill colors
    scale_colour_manual(values = colours1)
}

################################################################################
## FUNCTION: week12
## Plots output for a single week given the case study
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupHr_sub - Filtered version of Resource Group Hour Table
##    ZoneHr_Avg - Average hourly info in zone
##    Export - Exports selected from Zone Hourly Table
################################################################################

Week12 <- function(year, month, day, case) {
  # Filters for the desired case study from the resource groups
  data <- ResGroupHr_sub%>%
    sim_filt1(.) %>%
    subset(., select=-c(Report_Year,Capacity_Factor)) %>%
    rbind(.,Import) %>%
    filter(Run_ID == case)
  
  # Set levels to each category in order specified
  data$ID <- factor(data$ID, levels=c("Import","Coal","Cogen", "Coal-to-Gas", "SCGT", "NGCC", "Hydro", "Other",
                                      "Wind", "Solar", "Storage"))
  ## SELECT A SINGLE WEEK
  
  # Select only a single week from the zone Hourly, and Export data
  WK <- WkTime(data,year,month,day)
  ZPrice <- WkTime(ZoneHr_Avg,year,month,day) %>%
    filter(Run_ID == case)
  Expo <- WkTime(Export,year,month,day) %>%
    filter(Run_ID == case)
  
  # Get y-max, demand to meet + exports
  WK$MX <- ZPrice$Demand + Expo$Output_MWH
  
  # # Set the max and min for the plot
  # MX1 <- WkTime(WK,year,01,day)
  # MX2 <- WkTime(WK,year,02,day)
  # MX3 <- WkTime(WK,year,03,day)
  # MX4 <- WkTime(WK,year,04,day)
  # MX5 <- WkTime(WK,year,05,day)
  # MX6 <- WkTime(WK,year,06,day)
  # MX7 <- WkTime(WK,year,07,day)
  # MX8 <- WkTime(WK,year,08,day)
  # MX9 <- WkTime(WK,year,09,day)
  # MX10 <- WkTime(WK,year,10,day)
  # MX11<- WkTime(WK,year,11,day)
  # MX12 <- WkTime(WK,year,12,day)
  # 
  # MXtime <- rbind(MX1, MX2, MX3, MX4, MX5, MX6, MX7, MX8, MX9, MX10, MX11, MX12)
  
  # # Set the max and min for the plot Output axis (y), Set slightly above max (200 above)
  # MX <- plyr::round_any(max(abs(MXtime$MX))+500, 100, f = ceiling)

  MX <- 13000
  # Title Formating
  wk_st <- as.Date(paste(year,month,day, sep = "-"),tz="MST")
  wk_end <- as.Date(paste(year,month,day+7, sep = "-"),tz="MST")
  
  Mtitle=month.abb[month]
  
  ## PLOT WITH AREA PLOT
  
  ggplot() +
    geom_area(data = WK, aes(x = date, y = Output_MWH, fill = ID, colour = ID), 
              alpha=0.7, size=0.5) +
    
    # Add hourly load line (black line on the top)
    geom_line(data = ZPrice, 
              aes(x = date, y = Demand), size=1.25, colour = "black") +
    scale_x_datetime(expand=c(0,0),date_labels = "%e", breaks = "day") +
    
    # Set the theme for the plot
    theme_bw() +
    theme(panel.grid = element_blank()) +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(plot.title = element_text(size= 10)) +
    
    theme(axis.text.x = element_text(vjust = 1),
          axis.title.x = element_text(size= 10),
          axis.title.y = element_text(size= 10),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.title=element_blank(),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent',colour ='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          legend.key.size = unit(1,"lines"), #Shrink legend
          legend.position = "bottom",
          legend.text = element_text(size= 12),
          text = element_text(size= 8)
    ) +
    scale_y_continuous(expand=c(0,0), limits = c(0,MX), 
                       breaks = seq(0, MX, by = MX/8)) +
    
    labs(x = "Date", y = "Output (MWh)", fill = "Resource", colour = "Resource",title=Mtitle) +
    
    guides(fill = guide_legend(nrow = 1)) +
    
    #Add colour
    scale_fill_manual(values = colours1) +
    
    # Make outline the same as fill colors
    scale_colour_manual(values = Outline1)
}
################################################################################
## FUNCTION: day1
## Plots output for a single day given the case study
##
## INPUTS: 
##    year, month, day - Date to plot
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupHr_sub - Filtered version of Resource Group Hour Table
##    ZoneHr_Avg - Average hourly info in zone
##    Export - Exports selected from Zone Hourly Table
################################################################################
  
  day1 <- function(year, month, day, case) {
    # Filters for the desired case study from the resource groups
    data <- ResGroupHr_sub%>%
      sim_filt1(.) %>%
      subset(., select=-c(Report_Year,Capacity_Factor)) %>%
      rbind(.,Import) %>%
      filter(Run_ID == case)
    
    # Set levels to each category in order specified
    data$ID <- factor(data$ID, levels=c("Import","Coal","Cogen","Coal-to-Gas", "SCGT", "NGCC", "Hydro", "Other",
                                        "Wind", "Solar", "Storage"))
    # Get full date
    day_report <- as.POSIXct(paste(day,month,year, sep = "/"), format="%d/%m/%Y")
    
    ## SELECT A SINGLE DAY
    # Select only a single day from the zone Hourly, and Export data
    DY <- HrTime(data,year,month,day)
    ZPrice <- HrTime(ZoneHr_Avg,year,month,day) %>%
      filter(Run_ID == case)
    Expo <- HrTime(Export,year,month,day) %>%
      filter(Run_ID == case)
    
    # Get y-max, demand to meet + exports
    DY$MX <- ZPrice$Demand + Expo$Output_MWH
    
    # Set the max and min for the plot Output axis (y), Set slightly above max ( round up to nearest 500)
    MX <- plyr::round_any(max(abs(DY$MX)), 500, f = ceiling)
    
    ## PLOT WITH AREA PLOT
    
    ggplot() +
      geom_area(data = DY, aes(x = date, y = Output_MWH, fill = ID, colour = ID), 
                alpha=0.7, size=0.5) +
      
      # Add hourly load line (black line on the top)
      geom_line(data = ZPrice, 
                aes(x = date, y = Demand,colour = "Demand"), size=1.5, colour = "black") +
      scale_x_datetime(expand=c(0,0)) +
      
      # Set the theme for the plot
      theme_bw() +
      theme(legend.position = "bottom") +
      
      theme(text=element_text(family=Plot_Text)) +
      
      theme(plot.title = element_text(size= Tit_Sz)) +
      
      theme(axis.text.x = element_text(vjust = 1),
            panel.background = element_rect(fill = "transparent", colour = "transparent"),
            panel.grid = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            plot.background = element_blank(),
            legend.title=element_blank(),
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.key.size = unit(1,"lines"), #Shrink legend
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
            axis.title.x = element_text(size=XTit_Sz,face='bold'),
            axis.title.y = element_text(size=YTit_Sz,face='bold'),
            text = element_text(size= Overall_Sz)
      ) +
      guides(fill = guide_legend(nrow = 1)) +
      
      scale_y_continuous(expand=c(0,0), limits = c(0,MX), 
                         breaks = seq(0, MX, by = MX/4)) +
      
      labs(title=paste("Resource Output, ",day_report),x = "Date", y = "Output (MWh)", fill = "Resource",colour = "Resource",caption=SourceDB ) +
      
      #Add colour
      scale_fill_manual(values = colours1) +
      
      # Make outline the same as fill colors
      scale_colour_manual(values = colours1)
  }
  
################################################################################  
## FUNCTION: Stor1
## Weekly storage output
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupHr_sub - Filtered version of Resource Group Hour Table
################################################################################
  
  Stor1 <- function(year, month, day, case) {

    # Filters for the desired case study
    data <- ResGroupHr_sub%>%
      filter(ID=="LTO_Storage") %>%
      filter(Run_ID == case)
    
    # Select only a single week
    WK <- WkTime(data,year,month,day)
    
    # Set the max and min for the plot
    MX <- plyr::round_any(max(abs(WK$Output_MWH)), 10, f = ceiling)
    
    # Plot the data    
    ggplot() +
      geom_area(data = WK, aes(x = date, y = Output_MWH,fill="Storage"), 
                alpha=0.7, size=.5, colour="black") +
      ggtitle(year)+
      
      # Set the theme for the plot
      theme_bw() +
      
      theme(text=element_text(family=Plot_Text)) +
      
      theme(panel.grid = element_blank(),
            axis.text.x=element_text(vjust = 0),
            plot.title = element_text(size=Tit_Sz),
            legend.title = element_blank(),
            legend.position = "none",
            axis.title.x = element_text(size=XTit_Sz,face='bold',vjust = -1),
            axis.title.y = element_text(size=YTit_Sz,face='bold'),
            text = element_text(size= Overall_Sz)
      ) +
      scale_x_datetime(expand=c(0,0),date_labels = "%b-%e",breaks = "day") +
      scale_y_continuous(breaks = seq(-MX, MX, by = MX), 
                         limits = c(-MX-1,MX+1),
                         labels = label_number(accuracy = 1)) +
      labs(x = "Date", y = "Storage (MWh)") +
      scale_fill_manual(values = cOL_STORAGE)
  }
  
################################################################################  
## FUNCTION: week_price 
## Electricity price for one week
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Zone Hour average condition only table
################################################################################
  
  week_price <- function(year, month, day,case) {
    # Filters for the desired case study
    data <- ZoneHr_Avg%>%
      filter(Run_ID == case)
    
    # Select only a single week using function WkTime
    ZPrice <- WkTime(data,year,month,day)
    
    # Set the max and min for the plot
    MX <- plyr::round_any(max(abs(ZPrice$Price)+10), 10, f = ceiling)
    MN <- plyr::round_any(min(abs(ZPrice$Price)), 10, f = floor) #Could put in scale y limits
    
    #Max min for date (x-axis)
    day_MN <- as.POSIXct(paste(day,month,year, sep = "/"), format="%d/%m/%Y")
    day_MX <- as.POSIXct(paste(day+7,month,year, sep = "/"), format="%d/%m/%Y")
    
    # Plot the data    
    ggplot() +
      geom_line(data = ZPrice, 
                aes(x = date, y = Price), 
                size = 1.5, colour = "darkred") +
      theme_bw() +
      theme(text=element_text(family=Plot_Text)) +
      theme(panel.background = element_rect(fill = "transparent"),
            axis.text.x=element_text(vjust=-1),
            axis.title.x = element_text(vjust=-1,size= XTit_Sz,face="bold"),
            axis.text.y=element_text(hjust=-0.5),
            axis.title.y = element_text(vjust=2,size= YTit_Sz,face="bold"),
            panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'grey'),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid = element_blank(),
            plot.background = element_rect(fill = "transparent", color = NA),
            plot.title = element_text(size = Tit_Sz),
            text = element_text(size = 15) 
            
      ) +
      labs(y = "Pool Price ($/MWh)", x="Date",fill = "Resource",caption=SourceDB) +
      scale_x_datetime(expand=c(0,0),limits=c(day_MN,day_MX),breaks = "day",date_labels = "%b-%e") +
      scale_y_continuous(expand=c(0,0), 
                         limits= c(0,MX),
                         #                       labels = label_number(accuracy = 1),
                         breaks = seq(0, MX, by = MX/4)
      )
  }
  
################################################################################  
## FUNCTION: Eval 
## Plotting month/year profiles of resource output
##
## INPUTS: 
##    input - ResgroupMnor ResGroupYr
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    Import - Import table derived of zone average table
################################################################################
  
  Eval <- function(input,case) {
    Imp <- Import_Yr %>%
      filter(Name == "WECC_Alberta") %>%
      mutate(Time_Period = format(.$Time_Period, format="%Y")) %>%
      select(ID, Time_Period, Output_MWH) %>%
      mutate(ID = "Import") 
    
   # Imp$Time_Period  <- as.Date(as.character(Imp$Time_Period), 
   #                             format = "%Y")
    
    # Filters for the desired case study
    data <- input %>%
      filter(Run_ID == case & Condition == "Average") %>%
      select(ID, Time_Period, Output_MWH) %>%
      sim_filt(.) %>%
      rbind(.,Imp) 
    
    data$ID<-fct_relevel(data$ID, "Import")
    data$Time_Period <- as.Date(data$Time_Period)
    
    # Set the max for the plot
    dyMX <- aggregate(data["Output_MWH"], by=data["Time_Period"], sum)
    MX <- plyr::round_any(max(abs(dyMX$Output_MWH)/1000000), 20, f = ceiling)
    
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
            panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
            panel.background = element_rect(fill = NA),
            legend.key.size = unit(1,"lines"), #Shrink legend
            legend.position = "bottom",
            legend.justification = c(0.5,0.5),
            legend.title=element_blank(),
            text = element_text(size = 20)) +
      
      scale_x_date(expand=c(0,0),breaks = "year",date_labels = "%Y") +
      scale_y_continuous(expand=c(0,0),limits = c(0,MX),breaks=pretty_breaks(6)) +
      
      labs(x = "Date", y = "Generation (TWh)", fill = "Resource",colour="Resource",caption = SourceDB) +
      
      guides(fill = guide_legend(nrow = 1)) +
      
      scale_fill_manual(values = colours4) +
      scale_colour_manual(values = Outline4)
    

  }
  
  ################################################################################  
  ## FUNCTION: Evalyr 
  ## Plotting year profiles of resource output
  ##
  ## INPUTS: 
  ##    input - ResgroupMnor ResGroupYr
  ##    case - Run_ID which you want to plot
  ## TABLES REQUIRED: 
  ##    Import - Import table derived of zone average table
  ################################################################################
  
  Evalyr <- function(input,case) {
    Imp <- Import_Yr %>%
      filter(Name == "WECC_Alberta") %>%
      filter(Run_ID == case) %>%
      select(ID, Time_Period, Output_MWH) %>%
      mutate(ID = "Import") 

    # Imp$Time_Period  <- as.Date(as.character(Imp$Time_Period), 
    #                              format = "%Y")
    
    # Filters for the desired case study
    data <- input %>%
      filter(Run_ID == case & Condition == "Average") %>%
      select(ID, Time_Period, Output_MWH) %>%
      sim_filt(.) %>%
      rbind(.,Imp) 
    
    data$ID<-fct_relevel(data$ID, "Import")
    data$Time_Period <- as.Date(data$Time_Period,format="%Y")

    # Set the max for the plot
    dyMX <- aggregate(data["Output_MWH"], by=data["Time_Period"], sum)
    MX <- plyr::round_any(max(abs(dyMX$Output_MWH)/1000000), 20, f = ceiling)
    
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
      
      guides(fill = guide_legend(nrow = 1)) +
      
      scale_fill_manual(values = colours4) +
      scale_colour_manual(values = Outline4)
    
    
  }
################################################################################  
## FUNCTION: Evalcap 
## Plotting month/year profiles of resource capacity
##
## INPUTS: 
##    input - ResgroupMn or ResGroupYr
##    case - Run_ID which you want to plot
################################################################################
  
  Evalcap <- function(input,case) {
    
    # Filters for the desired case study
    data <- input %>%
      filter(Run_ID == case & Condition == "Average") %>%
      select(ID, Time_Period, Capacity) %>%
      sim_filt(.)
    
    # Set the max and min for the plot Output axis (y)
    # Set the max for the plot
    ppMX <-  aggregate(data["Capacity"], by=data["Time_Period"], sum)
    MX <- plyr::round_any(max(abs(ppMX$Capacity)), 5000, f = ceiling)
    
    
    data %>%
      ggplot() +
      geom_area(aes(Time_Period, (Capacity), fill = ID, colour=ID),alpha=1, size=.5) +
      theme_bw() +
      
      theme(text=element_text(family=Plot_Text)) +
      
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(),
            axis.title.x = element_text(size = XTit_Sz,face="bold"),
            axis.title.y = element_text(size = YTit_Sz,face="bold"),
            plot.title = element_text(size = Tit_Sz),
            panel.background = element_rect(fill = "transparent"),
            panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
            plot.subtitle = element_text(hjust = 0.5), 
            legend.position = "bottom",
            legend.key.size = unit(1,"lines"),
            legend.title = element_blank(),
            legend.text = element_text(size =15),
            legend.justification = c(0.5,0.5),
            text = element_text(size = 15)) +
      
      scale_x_date(expand=c(0,0),
                   breaks = "year",date_labels = "%Y") +
      
      scale_y_continuous(expand=c(0,0), limits=c(0,30000),breaks = pretty_breaks(6)) +
      
      labs(x = "Date", y = "Capacity (MW)", fill = "Resource",colour="Resource") +
    
      guides(fill = guide_legend(nrow = 1)) +
      
      scale_fill_manual(values = colours2) +
      scale_colour_manual(values = Outline2) 
    
  }
 
################################################################################  
## FUNCTION: EvalPerc 
## Year/month profiles as a percentage of the total
##
## INPUTS: 
##    input - 
##    case - Run_ID which you want to plot
################################################################################ 
  
  EvalPerc <- function(input,case) {
    # Filters for the desired case study
    data <- input %>%
      filter(Run_ID == case & Condition == "Average")# %>%

    # Filter the data by resource
    case_Time <- sim_filt(data)
    
    # Remove negative generation (Storage)
    case_Time$Output_MWH[case_Time$Output_MWH < 0] <- NA
    
    case_Time %>%
      ggplot() +
      geom_area(aes(Time_Period, Output_MWH, fill = ID,colour= ID),
                position = "fill", alpha = 0.7, size=.5) +
      geom_vline(xintercept = as.Date(ISOdate(2035, 1,1)),
                 linetype = "dashed", color = "black", size = 1) +
      #    facet_wrap(~ Condition, nrow = 1) +
      theme_bw() +
      
      theme(text=element_text(family=Plot_Text)) +
      
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5), 
            legend.title = element_blank(),
            panel.background = element_rect(fill = "transparent"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            plot.background = element_rect(fill = "transparent", color = NA),
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.background = element_rect(fill='transparent',colour='transparent'),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
            # panel.grid.major.y = element_line(size=0.25,linetype=5,color = 'gray70'),
            # panel.ontop = TRUE,
            legend.key.size = unit(1,"lines"),
            legend.position = "bottom",
            legend.justification = c(0,0.5)) +
      
      scale_x_date(expand=c(0,0),breaks = "year",date_labels = "%Y") +
      scale_y_continuous(expand=c(0,0),
                         labels = scales::percent, 
                         breaks = sort(c(seq(0,1,length.out=5)))) +
      
      guides(fill = guide_legend(nrow = 1)) +
      labs(x = "Date", y = "Percentage of Generation", fill = "Resource",colour="Resource") +
      scale_fill_manual(values = colours2) +
      scale_colour_manual(values = Outline2) 
      
  }
  
################################################################################  
## FUNCTION: Units
## Unit specific bar chart showing builds
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
## Unit specific bar chart showing units not built
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
  
  
###############################################################################  
## FUNCTION: Sim_dur 
## Simulation duration curve ploted each year
## The price duration curve represents the percentage of hours in which pool price 
## equaled or exceeded a specified level.
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_All - Zone hourly table for all conditions
################################################################################  
  
  Sim_dur <- function(case) {
    
    tot <- ZoneHr_All%>%
      group_by(Condition, Report_Year)%>%
      mutate(perc = 1-ecdf(Price)(Price))
    
    tot$Report_Year <- as.factor(tot$Report_Year)
    
    tot <- tot %>%
      filter(Report_Year %in% Years2Disp)
    
    ggplot() +
      geom_line(data = tot, 
                aes(x = perc, y = Price, colour = Report_Year), size = 1.25) +
      facet_grid(cols = vars(Condition)) +
      
      theme_bw() +
      
      theme(text=element_text(family=Plot_Text)) +
      
      theme(panel.grid = element_blank(),
            panel.spacing = unit(2, "lines"),
            axis.title.x = element_text(size = XTit_Sz,face="bold"),
            axis.title.y = element_text(size = YTit_Sz,face="bold"),
            text = element_text(size = 15),
            legend.title = element_blank(),
            panel.grid.major.y = element_line(size=0.25,linetype=5,color = "gray70")) +
            
      labs(y = "Pool Price ($/MWh)", x = "Percentage of Time",caption = SourceDB) +
      
      #scale_color_brewer(palette= "Dark2") +
  
      scale_x_continuous(expand=c(0,0), 
                         limits = c(0,1),
                         labels = percent) +
      
      scale_y_continuous(expand=c(0,0),limits = c(0,1000),breaks = pretty_breaks(5)) 
  }
  
################################################################################
#
#
# COMBINED PLOTS SECTION
#
#
################################################################################ 
  
################################################################################
## FUNCTION: year_weeks
## Plots output for a single week given the case study
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupHr_sub - Filtered version of Resource Group Hour Table
##    ZoneHr_Avg - Average hourly info in zone
##    Export - Exports selected from Zone Hourly Table
################################################################################
year_weeks <- function(year,case) {
  
  
  # Create a graph for each month of the year
  p1 <- Week12(year,01,08,case) +
    theme(axis.title.y=element_blank(),
          axis.title.x=element_blank())
          

  p2 <- Week12(year,02,08,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p3 <- Week12(year,03,08,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p4 <- Week12(year,04,08,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p5 <- Week12(year,05,08,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p6 <- Week12(year,06,08,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p7 <- Week12(year,07,08,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p8 <- Week12(year,08,08,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p9 <- Week12(year,09,08,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p10 <- Week12(year,10,08,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p11 <- Week12(year,11,08,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p12 <- Week12(year,12,08,case) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  # Get a common legend
  legend <- get_legend(p1)
  p1 <- p1 + theme(legend.position ="none")
  
  # Plot Labels
  yleft <- textGrob("Output (MWh)", rot = 90, gp = gpar(fontsize = 15))
  bottom <- textGrob("Date", gp = gpar(fontsize = 15))
  
  # Cheat way to put an x title in
  xtitle <- ggplot() +
    annotate("text", x = 10,  y = 10,
             size = 6,
             label = "Date") + 
               theme_void()
  
  # Label the source and year
  xsubtitle <- ggplot() +
    annotate("text", x = 10,  y = 10,
             size = 4,
             label = paste("year:",year,", Database:",SourceDB)) + 
    theme_void()
  
  #Create a big window
  windows(18,12)
  
  # Arrange all the plots
  grid.arrange(plot_grid(p1, p2, p3, p4, ncol=4, align="v", axis = "l", rel_widths = c(1,1,1,1)),
                    plot_grid(p5,p6, p7, p8, ncol=4, align="v", axis = "l", rel_widths = c(1,1,1,1)),
                            plot_grid(p9, p10, p11, p12, ncol=4, align="v", axis = "l", rel_widths = c(1,1,1,1)),
               plot_grid(xtitle),
               plot_grid(legend),
               plot_grid(xsubtitle),
               ncol=1,nrow=6, 
               heights=c(1, 1,1,0.1,0.2,0.1),
               left=yleft)
  

  }
    
################################################################################
## FUNCTION: Week4
## Function to plot four years for a specific case study
################################################################################
  
  Week4 <- function(month,day,case) {
    ggdraw(add_sub(ggarrange(Week14(Yr4Sp[[1]],month,day,case) +
                             theme(legend.position ="none"),
                             Week14(Yr4Sp[[2]],month,day,case) +
                               theme(legend.position ="none"),
                             Week14(Yr4Sp[[3]],month,day,case) +
                               theme(legend.position ="none"),
                             Week14(Yr4Sp[[4]],month,day,case) +
                               theme(legend.position ="none"),
                             labels = c(Yr4Sp[[1]],Yr4Sp[[2]],Yr4Sp[[3]],Yr4Sp[[4]]),
                             common.legend = TRUE, 
                             legend = "bottom",ncol = 2, nrow = 2),paste("Simulation Name: SourceDB")))
  }
  
################################################################################
## FUNCTIONS: PrOt, PrOut, PrOut4 ** Not Done
## Plot Price and Output together
################################################################################
  
  PrOt <- function(year,month,day,case) {
    plot_grid(week_price(year,month,day,case) + 
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.title=element_blank()),
                Week1(year,month,day,case)+
                theme(legend.position ="bottom"), 
                ncol = 1, align="v", axis = "l",rel_heights = c(1,2.5)) +
      
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) 
  }
  
#Includes storage
  PrOut <- function(year,month,day,case) {
    plot_grid(Stor1(year,month,day,case),
              week_price(year,month,day,case) + theme(axis.title.x=element_blank(),
                                                      axis.text.x=element_blank()),
              Week1(year,month,day,case)+theme(legend.position ="none"), 
              ncol = 1, align="v", axis = "l",rel_heights = c(1,1,2.5))
  }

################################################################################
## FUNCTION: EvalOut
## Plotting the month/year profile with the build
################################################################################
  
  EvalOut <- function(input,case) {
    p1 <- Eval(input,case) +
                      theme(legend.position="bottom",
                            legend.spacing.x = unit(1,"lines")) +
                      guides(fill = guide_legend(nrow = 1, byrow = TRUE)) 
    p2 <- Builtcol(case) +
            theme(legend.position ="none",
               axis.title.x = element_blank(),
               axis.text.x = element_blank())
    p3 <- BuiltMW(case) +
            theme(legend.position ="none",
                  axis.title.x = element_blank()) 
                    
    p4 <- plot_grid(p3, p2, p1, ncol = 1, nrow=3, align="v", axis = "l",rel_heights = c(1,1,2.5))
    
    ggdraw(add_sub(p4,paste("Simulation: ",SourceDB, sep = "")))
  }
################################################################################
## FUNCTIONS: BuildUnits, BuildUnits2
## Plot four years for a specific case study of the combined plots
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
  
  BuildUnits2 <- function(case, Fuel) {
    p1 <- plot_grid(Units2(case,Fuel)+theme(axis.title.x = element_blank(),
                                            axis.text.x = element_blank(),
                                            text = element_text(size= 15)),
                    Slack2(case,Fuel)+theme(text = element_text(size= 15)),
                    ncol = 1, align="v", axis = "l",rel_heights = c(1,1.5))
    
    ggdraw(add_sub(p1,paste("Simulation: ",SourceDB, sep = "")))
  }
  
