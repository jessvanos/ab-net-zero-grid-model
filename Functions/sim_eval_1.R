################################################################################
# TITLE: sim_eval_1
# DESCRIPTION: Functions To use for plotting and evaluating AURORA simulation data

# ORIGINAL AUTHOR: Taylor Pawlenchuk (Retrieved June 3, 2022)
# EDITS & ADDITIONAL CONTENT: Jessica Van Os
# LAST EDIT: June 14, 2022

################################################################################
## FUNCTION: sim_filt
## This function filters for the data that will be evaluated.
################################################################################

{ sim_filt <- function(inputdata) {
    # Filter the data by resource
    {Coal <- inputdata %>%
      filter(ID=="LTO_Coal")
    Coal2Gas  <- inputdata %>%
      filter(ID=="LTO_Coal2Gas")
    Coal2Gas$Output_MWH[Coal2Gas$Output_MWH < 0] <- 0
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
    
  # Combine the grouped data
  { case <- rbind(Coal, Coal2Gas, Cogen, NatGas, Hydro, Solar, Wind, Storage, Other)
    case$ID <- factor(case$ID, levels=c("LTO_Coal", "LTO_Coal2Gas", "LTO_Cogen", 
                                        "LTO_NatGas", "LTO_Other", "LTO_Hydro", 
                                        "LTO_Wind", "LTO_Solar", "LTO_Storage"))
    levels(case$ID) <- c("Coal", "Coal2Gas", "Cogen", "NatGas", "Other", "Hydro",
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
    { case <- rbind(Coal, Cogen, SCCT, CCCT, Hydro, Solar, Wind, Storage, Other)
      case$ID <- factor(case$ID, levels=c("LTO_Coal", "AB_CCCT_noncogen", "LTO_Cogen",
                                          "AB_SCCT_noncogen", "LTO_Hydro", "LTO_Other", 
                                          "LTO_Wind", "LTO_Solar", "LTO_Storage"))
      levels(case$ID) <- c("COAL", "NGCC", "COGEN", "SCGT", "HYDRO", "OTHER",
                           "WIND", "SOLAR", "STORAGE")  }
      return(case)  }
  }
  
################################################################################
## FUNCTION: HrTime
## Function to convert the date time for plotting
################################################################################
  
{ HrTime <- function(data, year, month, day) {
    subset(data,
          (date >= paste(year,"-", month, "-", day," 00:00:00", sep = "") & 
              date <= 
              paste(year,"-", month, "-", (day+7)," 00:00:00", sep = "")))  }
}

################################################################################
## FUNCTION: week1
## Plots output for a single week given the case study
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResGroupHr_sub - Filtered version of Resource Group Hour Table
################################################################################

  Week1 <- function(year, month, day, case) {
    # Filters for the desired case study
    data <- ResGroupHr_sub%>%
      sim_filt1(.) %>%
      subset(., select=-c(Report_Year,Capacity_Factor)) %>%
      rbind(.,Import) %>%
      filter(Run_ID == case)
    
    data$ID <- factor(data$ID, levels=c("Import", "COAL", "COGEN", "SCGT", "NGCC", 
                                        "HYDRO", "OTHER",
                                        "WIND", "SOLAR", "STORAGE"))
    
    #    data$date <- as.POSIXct(data$date, tz = "MST")
    
    wk_st <- as.POSIXct(paste(day,month,year, sep = "/"), format="%d/%m/%Y")
    wk_end <- as.POSIXct(paste(day+7,month,year, sep = "/"), format="%d/%m/%Y")
    
    # Select only a single week
    ##############################################################################
    WK <- data %>%
      filter(date >= wk_st, date <= wk_end)
    
    # Select only a single week
    #    WK <- HrTime(data,year,month,day)
    ZPrice <- HrTime(ZoneHr_Avg,year,month,day)
    Expo <- HrTime(Export,year,month,day)
    WK$MX <- ZPrice$Demand + Expo$Output_MWH
    
    # Set the max and min for the plot
    MX <- plyr::round_any(max(abs(WK$MX)), 100, f = ceiling)
    MN <- plyr::round_any(min(WK$Output_MWH), 100, f = floor)
    
    # Plot the data    
    ggplot() +
      geom_area(data = WK, aes(x = date, y = Output_MWH, fill = ID), 
                alpha=0.6, size=.5, colour="black") +
      
      # Add hourly load line
      geom_line(data = ZPrice, 
                aes(x = date, y = Demand), size=2, colour = "black") +
      scale_x_datetime(expand=c(0,0)) +
      
      # Set the theme for the plot
      theme_bw() +
      theme(panel.grid = element_blank(),
            legend.position = "right",
      ) +
      theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust = 1),
            panel.background = element_rect(fill = "transparent"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            plot.background = element_rect(fill = "transparent", color = NA),
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
            text = element_text(size= 15)
      ) +
      scale_y_continuous(expand=c(0,0), limits = c(MN,MX), 
                         breaks = seq(MN, MX, by = MX/4)) +
      labs(x = "Date", y = "Output (MWh)", fill = "Resource") +
      scale_fill_manual(values = colours1)
  }
  
  ################################################################################
  # Functions for weekly evaluation over four years
  ################################################################################  
  
  Week14 <- function(year, month, day, case) {
    # Add imports and exports to data
    
    
    # Filters for the desired case study
    data <- ResGroupHr_sub%>%
      sim_filt1(.) %>%
      select(-Report_Year) %>%
      rbind(.,Import) %>%
      filter(Run_ID == case)
    
    data$ID <- factor(data$ID, levels=c("Import", "COAL", "COGEN", "SCGT", "NGCC", 
                                        "HYDRO", "OTHER",
                                        "WIND", "SOLAR", "STORAGE"))
    
    # Select only a single week
    WK <- HrTime(data,year,month,day)
    ZPrice <- HrTime(ZoneHr_Avg,year,month,day)
    Expo <- HrTime(Export,year,month,day)
    data$MX <- ZoneHr_Avg$Demand + Export$Output_MWH
    
    # Set the max and min for the plot
    MX1 <- HrTime(data,Yr4Sp[[1]],month,day)
    MX2 <- HrTime(data,Yr4Sp[[2]],month,day)
    MX3 <- HrTime(data,Yr4Sp[[3]],month,day)
    MX4 <- HrTime(data,Yr4Sp[[4]],month,day)
    MXtime <- rbind(MX1, MX2, MX3, MX4)
    
    MX <- plyr::round_any(max(abs(MXtime$MX)), 100, f = ceiling)
    MN <- plyr::round_any(min(MXtime$Output_MWH), 100, f = floor)
    
    # Plot the data    
    ggplot() +
      geom_area(data = WK, aes(x = date, y = Output_MWH, fill = ID), 
                alpha=0.6, size=.5, colour="black") +
      
      # Add hourly load line
      geom_line(data = ZPrice, 
                aes(x = date, y = Demand), size=2, colour = "black") +
      scale_x_datetime(expand=c(0,0)) +
      
      # Set the theme for the plot
      theme_bw() +
      theme(panel.grid = element_blank(),
            legend.position = "right",
      ) +
      theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust = 1),
            panel.background = element_rect(fill = "transparent"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            plot.background = element_rect(fill = "transparent", color = NA),
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
            text = element_text(size= 10)
      ) +
      scale_y_continuous(expand=c(0,0), limits = c(MN,MX), 
                         breaks = seq(MN, MX, by = MX/4)) +
      labs(x = "Date", y = "Output (MWh)", fill = "Simulated Data: \nResource") +
      scale_fill_manual(values = colours1)
  }
  
  ################################################################################
  # Generate weekly storage output plot function
  ################################################################################
  ################################################################################
  Stor1 <- function(year, month, day, case) {
    # Add imports and exports to data
    
    
    # Filters for the desired case study
    data <- ResGroupHr_sub%>%
      filter(ID=="LTO_Storage") %>%
      filter(Run_ID == case)
    
    
    # Select only a single week
    WK <- HrTime(data,year,month,day)
    
    # Set the max and min for the plot
    MX <- plyr::round_any(max(abs(WK$Output_MWH)), 10, f = ceiling)
    
    # Plot the data    
    ggplot() +
      geom_area(data = WK, aes(x = date, y = Output_MWH), 
                alpha=0.6, size=.5, colour="black") +
      ggtitle(year)+
      
      # Set the theme for the plot
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            plot.title = element_text(hjust = 0.5)
      ) +
      scale_x_datetime(expand=c(0,0)) +
      scale_y_continuous(breaks = seq(-MX, MX, by = MX), 
                         limits = c(-MX-1,MX+1),
                         labels = label_number(accuracy = 1)) +
      labs(x = "Date", y = "Storage\n(MWh)", fill = "Resource") +
      scale_fill_manual(values = "cyan")
  }
  
  ################################################################################
  # Generate weekly storage output plot function with axis limits for 4 years
  ################################################################################
  
  Stor14 <- function(year, month, day, case) {
    # Add imports and exports to data
    
    
    # Filters for the desired case study
    data <- ResGroupHr_sub%>%
      filter(ID=="LTO_Storage") %>%
      filter(Run_ID == case)
    
    
    # Select only a single week
    WK <- HrTime(data,year,month,day)
    
    # Set the max and min for the plot
    MX1 <- HrTime(data,Yr4Sp[[1]],month,day)
    MX2 <- HrTime(data,Yr4Sp[[2]],month,day)
    MX3 <- HrTime(data,Yr4Sp[[3]],month,day)
    MX4 <- HrTime(data,Yr4Sp[[4]],month,day)
    MXtime <- rbind(MX1, MX2, MX3, MX4)
    
    MX <- plyr::round_any(max(abs(MXtime$Output_MWH)), 10, f = ceiling)
    
    # Plot the data    
    ggplot() +
      geom_area(data = WK, aes(x = date, y = Output_MWH), 
                alpha=0.6, size=.5, colour="black") +
      ggtitle(year)+
      
      # Set the theme for the plot
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            plot.title = element_text(hjust = 0.5)
      ) +
      scale_x_datetime(expand=c(0,0)) +
      scale_y_continuous(breaks = seq(-MX, MX, by = MX), 
                         limits = c(-MX-1,MX+1),
                         labels = label_number(accuracy = 1)) +
      labs(x = "Date", y = "Storage\n(MWh)", fill = "Resource") +
      scale_fill_manual(values = "cyan")
  }
  
  ################################################################################
  # Function for plotting prices
  ################################################################################
  ################################################################################
  
  week_price <- function(year, month, day,case) {
    # Filters for the desired case study
    data <- ZoneHr_Avg%>%
      filter(Run_ID == case)
    
    # Select only a single week using function HrTime
    ZPrice <- HrTime(data,year,month,day)
    
    # Set the max and min for the plot
    MX <- plyr::round_any(max(abs(ZPrice$Price)), 10, f = ceiling)
    MN <- plyr::round_any(min(abs(ZPrice$Price)), 10, f = floor)
    
    # Plot the data    
    ggplot() +
      geom_line(data = ZPrice, 
                aes(x = date, y = Price), 
                size = 1.5, colour = "red") +
      theme_bw() +
      theme(panel.background = element_rect(fill = "transparent"),
            panel.grid = element_blank(),
            plot.background = element_rect(fill = "transparent", color = NA),
            text = element_text(size= 15)
      ) +
      labs(y = "Pool Price \n$/MWh", fill = "Resource") +
      scale_x_datetime(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0), 
                         limits= c(MN,MX),
                         #                       labels = label_number(accuracy = 1),
                         breaks = seq(MN, MX, by = MX/4)
      )
  }
  
  ################################################################################
  # Function for plotting prices with axis limits for 4 years
  ################################################################################
  
  week_price4 <- function(year, month, day,case) {
    # Filters for the desired case study
    data <- ZoneHr_Avg%>%
      filter(Run_ID == case)
    
    # Select only a single week using function HrTime
    ZPrice <- HrTime(data,year,month,day)
    
    # Set the max and min for the plot
    MX1 <- HrTime(data,Yr4Sp[[1]],month,day)
    MX2 <- HrTime(data,Yr4Sp[[2]],month,day)
    MX3 <- HrTime(data,Yr4Sp[[3]],month,day)
    MX4 <- HrTime(data,Yr4Sp[[4]],month,day)
    MXtime <- rbind(MX1, MX2, MX3, MX4)
    
    MX <- plyr::round_any(max(abs(MXtime$Price)), 10, f = ceiling)
    MN <- plyr::round_any(min(abs(MXtime$Price)), 10, f = floor)
    
    # Plot the data    
    ggplot() +
      geom_line(data = ZPrice, 
                aes(x = date, y = Price), 
                size = 1.5, colour = "red") +
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank()
      ) +
      labs(y = "$/MWh", fill = "Resource") +
      scale_x_datetime(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0), 
                         limits= c(MN,MX),
                         #                       labels = label_number(accuracy = 1),
                         breaks = seq(MN, MX, by = MX/4)
      )
  }
  
  ################################################################################
  ################################################################################
  # Function for plotting month/year profiles
  ################################################################################
  ################################################################################
  
  Eval <- function(input,case) {
    Imp <- Import %>%
      filter(Run_ID == case) %>%
      mutate(Time_Period = format(.$date, format="%Y")) %>%
      group_by(Time_Period) %>%
      summarise(Output_MWH = mean(Output_MWH)) %>%
      mutate(ID = "Import")
    
    #  Imp$Time_Period  <- as.Date(as.character(Imp$Time_Period), 
    #                               format = "%Y")
    
    #  Imp <- subset(Imp, Time_Period <= '2040-04-05')
    
    # Filters for the desired case study
    data <- input %>%
      filter(Run_ID == case & Condition == "Average") %>%
      select(ID, Time_Period, Output_MWH) %>%
      sim_filt(.) %>%
      rbind(.,Imp) 
    
    data$ID<-fct_relevel(data$ID, "Import")
    data$Time_Period <- as.Date(data$Time_Period)
    
    data %>%
      ggplot() +
      aes(Time_Period, (Output_MWH/1000), fill = ID) +
      geom_area(alpha=0.6, size=.5, colour="black") +
      #    facet_wrap(~ Condition, nrow = 1) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5), 
            legend.justification = c(0,0.5)) +
      #    scale_x_date(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      scale_fill_manual(values = colours1) +
      labs(x = "Date", y = "Output (GWh)", fill = "Resource") 
  }
  
  Evalcap <- function(input,case) {
    
    # Filters for the desired case study
    data <- input %>%
      filter(Run_ID == case & Condition == "Average") %>%
      select(ID, Time_Period, Capacity) %>%
      sim_filt(.)
    
    data %>%
      ggplot() +
      aes(Time_Period, (Capacity/1000), fill = ID) +
      geom_area(alpha=0.6, size=.5, colour="black") +
      #    facet_wrap(~ Condition, nrow = 1) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5), 
            legend.justification = c(0,0.5)) +
      scale_x_date(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      scale_fill_manual(values = colours2) +
      labs(x = "Date", y = "Capacity (GWh)", fill = "Resource") 
  }
  
  ################################################################################
  # Function for plotting month/year profiles as a percentage of total
  ################################################################################
  ################################################################################
  
  EvalPerc <- function(input,case) {
    # Filters for the desired case study
    data <- input %>%
      filter(Run_ID == case & Condition == "Average")# %>%
    #    group_by(Time_Period, ID) %>%
    #    summarise(n = sum(Output_MWH)) %>%
    #    mutate(Percentage = n / sum(n))
    
    # Filter the data by resource
    case_Time <- sim_filt(data)
    
    # Remove negative generation (Storage)
    case_Time$Output_MWH[case_Time$Output_MWH < 0] <- NA
    
    case_Time %>%
      ggplot() +
      aes(Time_Period, Output_MWH, fill = ID) +
      geom_area(position = "fill", alpha=0.6, size=.5, colour="black") +
      geom_hline(yintercept = 0.3, linetype = "dashed", color = "forestgreen", size = 1.5) +
      geom_vline(xintercept = as.Date(ISOdate(2035, 1,1)),
                 linetype = "dashed", color = "dodgerblue", size = 1.5) +
      #    facet_wrap(~ Condition, nrow = 1) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5), 
            legend.title = element_blank(),
            panel.background = element_rect(fill = "transparent"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            plot.background = element_rect(fill = "transparent", color = NA),
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
            legend.justification = c(0,0.5)) +
      scale_x_date(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0),
                         labels = scales::percent, 
                         breaks = sort(c(seq(0,1,length.out=5),0.3))) +
      scale_fill_manual(values = colours2) +
      labs(x = "Date", y = "Percentage of Generation", fill = "Resource") 
  }
  
  ################################################################################
  # Function for plotting the resources built
  ################################################################################
  ################################################################################
  
  # Stacked Area showing totals for Fuel Types
  Built <- function(case) {
    data <- Build %>%
      filter(Run_ID == case & LT_Iteration == max(LT_Iteration) & 
               Time_Period != "Study")%>%
      group_by(Fuel_Type, Time_Period) %>%
      summarise(Units = sum(Units_Built)) 
    
    data$Fuel_Type <- factor(data$Fuel_Type, 
                             levels = c("WND","SUN","Gas0","Gas1", "PS", "OT"))
    
    data %>%
      ggplot() +
      aes(Time_Period, Units, fill = Fuel_Type, group = Fuel_Type) +
      geom_area(alpha=0.6, size=.5, colour="black") +
      theme_bw() +
      theme(panel.grid = element_blank(), 
            legend.justification = c(0,0.5),
      ) +
      labs(x = "Date", y = "# of Units Built", fill = "Fuel Type") +
      scale_y_continuous(expand=c(0,0),
                         limits = c(0,(max(data$Units)+1))) +
      #    scale_x_discrete(expand=c(0,0)) +
      scale_fill_manual(values = colours3)
  }
  
  ################################################################################
  # Function for plotting the resources built as bar chart
  ################################################################################
  
  # Stacked Area showing totals for Fuel Types
  Builtcol <- function(case) {
    data <- Build %>%
      filter(Run_ID == case & LT_Iteration == max(LT_Iteration) & 
               Time_Period != "Study")%>%
      group_by(Fuel_Type, Time_Period) %>%
      summarise(Units = sum(Units_Built), Capacity = sum(Capacity_Built)) 
    
    data$Fuel_Type <- factor(data$Fuel_Type, 
                             levels = c("WND","SUN","Gas0","Gas1", "PS", "OT"))
    
    Tot <- data %>%
      group_by(Time_Period) %>%
      summarise(totu = sum(Units), totc = sum(Capacity))
    
    mxu <- max(Tot$totu)
    mxc <- max(Tot$totc)
    
    ggplot(data) +
      aes(Time_Period, Units, fill = Fuel_Type, group = Fuel_Type) +
      geom_bar(position="stack", stat="identity", alpha=0.6, colour = "black") +
      theme_bw() +
      theme(panel.grid = element_blank(),  
            legend.position ="none"
            #          legend.justification = c(0,0.5),
            #          legend.position = "top"
      ) +
      labs(x = "Date", y = "# of Units Built", fill = "Fuel Type") +
      scale_y_continuous(expand=c(0,0),
                         limits = c(0,(mxu+1))) +
      #    scale_x_discrete(expand=c(0,0)) +
      scale_fill_manual(values = colours3)
  }
  
  ################################################################################
  # Function for plotting the capacity of resources built
  ################################################################################
  
  # Stacked Area showing totals for Fuel Types
  BuiltMW <- function(case) {
    data <- Build %>%
      filter(Run_ID == case & LT_Iteration == max(LT_Iteration) & 
               Time_Period != "Study")%>%
      group_by(Fuel_Type, Time_Period) %>%
      summarise(Units = sum(Units_Built), Capacity = sum(Capacity_Built)) 
    
    data$Fuel_Type <- factor(data$Fuel_Type, 
                             levels = c("WND","SUN","Gas0","Gas1", "PS", "OT"))
    
    Tot <- data %>%
      group_by(Time_Period) %>%
      summarise(totu = sum(Units), totc = sum(Capacity))
    
    mxu <- max(Tot$totu)
    mxc <- max(Tot$totc)
    
    ggplot(data) +
      aes(Time_Period, Capacity, fill = Fuel_Type, group = Fuel_Type) +
      geom_bar(position="stack", stat="identity", alpha=0.6, colour = "black") +
      theme_bw() +
      theme(panel.grid = element_blank(), 
            legend.position ="none"
      ) +
      labs(x = "Date", y = "Capacity Built \n(MW)", fill = "Fuel Type") +
      scale_y_continuous(expand=c(0,0),
                         limits = c(0,plyr::round_any(mxc, 100, f = ceiling))) +
      #    scale_x_discrete(expand=c(0,0)) +
      scale_fill_manual(values = colours3)
  }
  
  ################################################################################
  # Unit specific bar chart showing builds
  ################################################################################
  ################################################################################
  
  Units <- function(case, Fuel) {
    data <- Build %>%
      filter(Run_ID == case & LT_Iteration == max(LT_Iteration) & 
               Time_Period == "Study" & Fuel_Type == Fuel) 
    
    data %>%
      ggplot() +
      aes(Name, Units_Built) + 
      geom_col() +
      labs(x = "Plant Name", y = "Units Built") +
      scale_y_continuous(expand = c(0,0),
                         limits = c(0,(max(data$Units_Built)+1))) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            #          axis.title.x = element_text(vjust=0.5),
            panel.background = element_rect(fill = "transparent"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            plot.background = element_rect(fill = "transparent", color = NA),
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
            panel.border = element_rect(colour = "black", fill = "transparent")) 
  }
  
  ################################################################################
  # Unit specific bar chart showing availability not built 
  ################################################################################
  ################################################################################
  
  Slack <- function(case, Fuel) {
    data <- Build %>%
      filter(Run_ID == case & LT_Iteration == max(LT_Iteration) & 
               Time_Period == "Study" & Fuel_Type == Fuel) 
    
    data %>%
      ggplot() +
      aes(Name, Max_Limit_Slack) + 
      geom_col() +
      labs(x = "Plant Name", y = "Units Available") +
      scale_y_continuous(expand = c(0,0),
                         limits = c(0,(max(data$Max_Limit_Slack)+1))) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            panel.background = element_rect(fill = "transparent"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            plot.background = element_rect(fill = "transparent", color = NA),
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
            panel.border = element_rect(colour = "black", fill = "transparent"))
  }
  
  ################################################################################
  # Unit specific bar chart showing builds with potential builds highlighted
  ################################################################################
  
  Units2 <- function(case, Fuel) {
    data <- Build %>%
      filter(Run_ID == case & LT_Iteration == max(LT_Iteration) & 
               Time_Period == "Study" & Fuel_Type == Fuel) %>%
      mutate(Potential = ifelse(grepl("Potential", Name, fixed = TRUE), 
                                "Hypothetical", "AESO Queue"))
    
    data %>%
      ggplot() +
      aes(Name, Units_Built, fill = Potential) + 
      geom_col() +
      labs(x = "Plant Name", y = "Units Built") +
      scale_fill_manual(
        values = c("Hypothetical"="forestgreen", "AESO Queue"="gray"),
        guide = "none") +
      scale_y_continuous(expand = c(0,0),
                         limits = c(0,(max(data$Units_Built)+1))) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            panel.background = element_rect(fill = "transparent"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            plot.background = element_rect(fill = "transparent", color = NA),
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
            panel.border = element_rect(colour = "black", fill = "transparent")) 
  }
  
  ################################################################################
  # Unit specific bar chart showing availability not built with potential sites 
  # highlighted
  ################################################################################
  
  Slack2 <- function(case, Fuel) {
    data <- Build %>%
      filter(Run_ID == case & LT_Iteration == max(LT_Iteration) & 
               Time_Period == "Study" & Fuel_Type == Fuel) %>%
      mutate(Potential = ifelse(grepl("Potential", Name, fixed = TRUE), 
                                "Hypothetical", "AESO Queue")) 
    
    data %>%
      ggplot() +
      aes(Name, Max_Limit_Slack, fill = Potential) + 
      geom_col() +
      labs(x = "Plant Name", y = "Units Available") +
      scale_fill_manual(
        values = c("Hypothetical"="forestgreen", "AESO Queue"="gray"),
        guide = "none") +
      scale_y_continuous(expand = c(0,0),
                         limits = c(0,(max(data$Max_Limit_Slack)+1)),
                         breaks = c((max(data$Max_Limit_Slack)+1)/2,(max(data$Max_Limit_Slack)+1))) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            panel.background = element_rect(fill = "transparent"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            plot.background = element_rect(fill = "transparent", color = NA),
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent', colour = "transparent"),
            panel.border = element_rect(colour = "black", fill = "transparent"))
  }
  
  ################################################################################
  # Simulation duration curve. 
  # The price duration curve represents the percentage of hours in which pool price 
  # equaled or exceeded a specified level.
  ################################################################################
  
  Sim_dur <- function(case) {
    
    tot <- ZoneHr_All%>%
      group_by(Condition, Report_Year)%>%
      mutate(perc = 1-ecdf(Price)(Price))
    
    tot$Report_Year <- as.factor(tot$Report_Year)
    
    ggplot() +
      geom_line(data = tot, 
                aes(x = perc, y = Price, colour = Report_Year), size = 1) +
      facet_grid(cols = vars(Condition)) +
      theme_bw() +
      theme(panel.grid = element_blank(),
      ) +
      labs(y = "Pool Price$/MWh", x = "Percentage of Time") +
      scale_color_manual(values = c("goldenrod1", "forestgreen", "cornflowerblue")) +
      scale_x_continuous(expand=c(0,0), 
                         limits = c(0,1.1),
                         labels = percent) +
      scale_y_continuous(expand=c(0,0)
      )
  }
  
  ################################################################################
  ################################################################################
  # Combination plotting functions defined
  ################################################################################
  ################################################################################
  
  ################################################################################
  # Function to plot four years for a specific case study
  ################################################################################
  
  Week4 <- function(month,day,case) {
    ggdraw(add_sub(ggarrange(Week14(Yr4Sp[[1]],month,day,case),
                             Week14(Yr4Sp[[2]],month,day,case),
                             Week14(Yr4Sp[[3]],month,day,case),
                             Week14(Yr4Sp[[4]],month,day,case),
                             labels = c(Yr4Sp[[1]],Yr4Sp[[2]]),#,Yr4Sp[[3]],Yr4Sp[[4]]),
                             common.legend = TRUE, legend = "right",
                             ncol = 2, nrow = 2), SourceDB))
  }
  
  ################################################################################
  # Function to plot Price and Output together
  ################################################################################
  
  PrOt <- function(year,month,day,case) {
    plot_grid(week_price(year,month,day,case) + 
                theme(axis.title.x=element_blank(),axis.text.x=element_blank()),
              Week1(year,month,day,case)+theme(legend.position ="none"), 
              ncol = 1, align="v", axis = "l",rel_heights = c(1,2.5))
  }
  
  PrOut <- function(year,month,day,case) {
    plot_grid(Stor1(year,month,day,case),
              week_price(year,month,day,case) + theme(axis.title.x=element_blank(),
                                                      axis.text.x=element_blank()),
              Week1(year,month,day,case)+theme(legend.position ="none"), 
              ncol = 1, align="v", axis = "l",rel_heights = c(1,1,2.5))
  }
  
  PrOut4 <- function(year,month,day,case) {
    plot_grid(Stor14(year,month,day,case),week_price4(year,month,day,case),
              Week14(year,month,day,case)+theme(legend.position ="none"), 
              ncol = 1, align="v", axis = "l",rel_heights = c(1,1,2.5))
  }
  
  ################################################################################
  # Function for plotting the month/year profile with the build
  ################################################################################
  
  EvalOut <- function(input,case) {
    p1 <- plot_grid(Eval(input,case) + theme(legend.position="top"), 
                    Builtcol(case)+theme(legend.position ="none",
                                         axis.title.x = element_blank(),
                                         axis.text.x = element_blank()), 
                    BuiltMW(case)+theme(legend.position ="none"), 
                    ncol = 1, align="v", axis = "l",rel_heights = c(2.5,0.8,1))
    
    ggdraw(add_sub(p1,paste("Simulation: ",SourceDB, sep = "")))
  }
  
  ################################################################################
  # Function to plot four years for a specific case study of the combined plots
  ################################################################################
  
  BuildUnits <- function(case, Fuel) {
    p1 <- plot_grid(Units(case,Fuel)+theme(axis.title.x = element_blank(),
                                           axis.text.x = element_blank()),
                    Slack(case,Fuel), 
                    ncol = 1, align="v", axis = "l",rel_heights = c(1,1))
    
    ggdraw(add_sub(p1,paste("Simulation: ",SourceDB, sep = "")))
  }
  
  BuildUnits2 <- function(case, Fuel) {
    p1 <- plot_grid(Units2(case,Fuel)+theme(axis.title.x = element_blank(),
                                            axis.text.x = element_blank(),
                                            text = element_text(size= 15)),
                    Slack2(case,Fuel)+theme(text = element_text(size= 15)),
                    ncol = 1, align="v", axis = "l",rel_heights = c(1,1.5))
    
    ggdraw(add_sub(p1,paste("Simulation: ",SourceDB, sep = "")))
  }
  
  ################################################################################
  # Function to plot four years for a specific case study of the combined plots
  ################################################################################
  
  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  Eval2 <- function(month,day,case) {
    ggarrange(arrangeGrob(PrOut(Yr2Sp[[1]],month,day,case)+theme(legend.position ="none"),
                          PrOut(Yr2Sp[[2]],month,day,case)+theme(legend.position ="none"),
                          ncol=2),
              g_legend(Week1(Yr2Sp[[1]],month,day,case)),
              ncol = 2, widths=c(12,1))
  }
  
  Eval4 <- function(month,day,case) {
    ggarrange(arrangeGrob(PrOut4(Yr4Sp[[1]],month,day,case)+theme(legend.position ="none"),
                          PrOut4(Yr4Sp[[2]],month,day,case)+theme(legend.position ="none"),
                          PrOut4(Yr4Sp[[3]],month,day,case)+theme(legend.position ="none"),
                          PrOut4(Yr4Sp[[4]],month,day,case)+theme(legend.position ="none"),
                          ncol=4),
              ggdraw(
                add_sub(g_legend(Week1(Yr4Sp[[1]],month,day,case)), 
                        paste("Simulation: \n",SourceDB, sep = ""))),
              ncol = 2, widths=c(7,1))
  }


subtit <- function(plot) {
  ggdraw(add_sub(plot,paste("Simulation: ",SourceDB, sep = "")))
}

imsave <- function(name) {
  setwd("D:/Documents/GitHub/AuroraEval")
  ggsave(path = "images", filename = paste(name,".png", sep = ""), bg = "transparent")   }
