################################################################################
# TITLE: aeso_sim_comp_1
# DESCRIPTION: Functions To used to compare simulation data with other data.

# ORIGINAL AUTHOR: Taylor Pawlenchuk (Retrieved June 7, 2022)
# EDITS & ADDITIONAL CONTENT: Jessica Van Os
# LAST EDIT: June 14, 2022

################################################################################
################################################################################
## FUNCTIONS: AESO_SimOP
## Plot comparison between actual and simulated data
##
## INPUTS:
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
################################################################################

AESO_SimOP <- function(year,month,day,case) {
  SimP <- week_price(year,month,day,case)
  ActP <- wkPrice(year,month,day)
  SimO <- Week1(year,month,day,case)
  ActO <- Week_act(year,month,day)
  
  #Price Limits
  MXP <- plyr::round_any(
    max(layer_scales(SimP)$y$range$range,layer_scales(ActP)$y$range$range),
    50, f = ceiling)
  MNP <- plyr::round_any(
    min(layer_scales(SimP)$y$range$range,layer_scales(ActP)$y$range$range),
    50, f = floor)  
  
  # output Lmiits
  MXO <- plyr::round_any(
    max(layer_scales(SimO)$y$range$range,layer_scales(ActO)$y$range$range),
    100, f = ceiling)
  MNO <- plyr::round_any(
    min(layer_scales(SimO)$y$range$range,layer_scales(ActO)$y$range$range),
    100, f = floor)
  
  # Create each plot
  sz <- 15
  
  p1 <- week_price(year,month,day,case) + 
          labs(title = paste0("Simulated Data for ",year),
               subtitle = paste0(SourceDB,", ",case)) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.title.y=element_text(size=sz, vjust=4),
                legend.position ="none",
                plot.title = element_text(hjust = 0.5, size = sz),
                plot.subtitle = element_text(hjust = 0.5, size = sz-2)) + 
          scale_y_continuous(expand=c(0,0), limits = c(MNP,MXP), 
                             breaks = pretty_breaks(4))
  
  p2 <- Week1(year,month,day,case)+
          theme(legend.position ="none",
                axis.title.y=element_text(size=sz)) + 
          scale_y_continuous(expand=c(0,0), limits = c(MNO,MXO), 
                             breaks = seq(0, MXO, by = MXO/4)) 

  p3 <- wkPrice(year,month,day) + 
          labs(title = paste0("AESO Data for ",year),
               subtitle = "NRGStream Data") +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.title.y=element_blank(),
                legend.position ="none",
                plot.title = element_text(hjust = 0.5, size = sz),
                plot.subtitle = element_text(hjust = 0.5, size = sz-2, face="italic")) + 
          scale_y_continuous(expand=c(0,0), limits = c(MNP,MXP), 
                             breaks = pretty_breaks(4))
  
  p4 <- Week_act(year,month,day)+
          theme(axis.title.y=element_blank(),
                plot.title=element_blank())+
          scale_y_continuous(expand=c(0,0), limits = c(MNO,MXO), 
                             breaks = seq(0, MXO, by = MXO/4))
  
  legend <- get_legend(p4)
        p4 <- p4 + theme(legend.position ="none")
  
  dev.new(width=3.75, height=2, unit="in")      
  
  grid.arrange(plot_grid(p1, p2, ncol=1, align="v", axis = "l", rel_heights = c(1,2.5)),
               plot_grid(p3, p4, ncol=1,  align="v", axis = "l", rel_heights = c(1,2.5)),
               plot_grid(legend),
                 ncol=3, nrow=2,
                 layout_matrix = rbind(c(1,2), c(3,3)),
                 heights=c(1, 0.1),widths=c(1, 1,0.05))
}

################################################################################
## FUNCTIONS: AESO_SimP
## Plot comparison between actual and simulated data price for 1 week
##
## INPUTS:
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
################################################################################
AESO_SimP <- function(year,month,day,case) {
  
  #Firt get Sim Data
    # Filters for the desired case study
    data <- ZoneHr_Avg%>%
      filter(Run_ID == case)
    
    # Select only a single week using function WkTime
    ZPrice <- WkTime(data,year,month,day)
    
    # Set the max and min for the plot
    MXa <- plyr::round_any(max(abs(ZPrice$Price)+10), 10, f = ceiling)
    MN <- plyr::round_any(min(abs(ZPrice$Price)), 10, f = floor) #Could put in scale y limits
    
    #Max min for date (x-axis)
    day_MN <- as.POSIXct(paste(day,month,year, sep = "/"), format="%d/%m/%Y")
    day_MX <- as.POSIXct(paste(day+7,month,year, sep = "/"), format="%d/%m/%Y")
    
  #Get AESO Data
    
    price_WK <- demand %>%
      filter(time >= day_MN & time <= day_MX)
    
    # Set the max for the plot
    MXb <- plyr::round_any(max(abs(price_WK$Price)+100), 10, f = ceiling)
    MX <-max(MXa,MXb)
    
  # Plot the data    
  ggplot() +
    geom_line(data = ZPrice, 
              aes(x = date, y = Price,colour = "Simulated (AURORA)"), 
              size = 1.5) +
    
    geom_line(data = price_WK, 
              aes(x=time, y=Price, color="Actual (AESO)"), 
              size = 1.5) +
    
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
          text = element_text(size = 15),
          legend.position = "bottom",
          legend.background = element_rect(fill='transparent',colour ='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent")
          
    ) +
    
    scale_colour_manual("", 
                        breaks = c("Simulated (AURORA)","Actual (AESO)"),
                        values = c("darkred","midnightblue")) +
    
    labs(title=year, y = "Pool Price ($/MWh)", x="Date",fill = "Resource") +
    scale_x_datetime(expand=c(0,0),limits=c(day_MN,day_MX),breaks = "day",date_labels = "%a, %b-%e") +
    
    scale_y_continuous(expand=c(0,0), 
                       limits= c(0,MX),
                       breaks = seq(0, MX, by = MX/8)

  )
}

################################################################################
## FUNCTIONS: AESO_SimP
## Plot comparison between actual and simulated data price for 1 week
##
## INPUTS:
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
################################################################################
AESO_SimP2 <- function(year,case) {
  
  #Firt get Sim Data
  # Filters for the desired case study
  data <- ZoneHr_Avg%>%
    filter(Run_ID == case)
  
  # Select only a single year using function WkTime
  ZPrice <- YrTime(data,year)
  
  # Set the max and min for the plot
  MXa <- plyr::round_any(max(abs(ZPrice$Price)+10), 10, f = ceiling)
  MN <- plyr::round_any(min(abs(ZPrice$Price)), 10, f = floor) #Could put in scale y limits
  
  #Max min for date (x-axis)
  day_MN <- as.POSIXct(paste(01,01,year, sep = "/"), format="%d/%m/%Y")
  day_MX <- as.POSIXct(paste(31,12,year, sep = "/"), format="%d/%m/%Y")
  
  #Get AESO Data
  
  price_WK <- demand %>%
    filter(time >= day_MN & time <= day_MX)
  
  # Set the max for the plot
  MXb <- plyr::round_any(max(abs(price_WK$Price)+100), 10, f = ceiling)
  MX <-max(MXa,MXb)
  
  # Plot the data    
  ggplot() +

    geom_line(data = price_WK, 
              aes(x=time, y=Price, color="Actual (AESO)"), 
              size = 1) +
    
    geom_line(data = ZPrice, 
              aes(x = date, y = Price,colour = "Simulated (AURORA)"), 
              size = 1) +
    
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.background = element_rect(fill = "transparent"),
          axis.text.x=element_text(vjust=-1),
          axis.title.x = element_text(vjust=-1,size= XTit_Sz,face="bold"),
          axis.text.y=element_text(hjust=-0.5),
          axis.title.y = element_text(vjust=2,size= YTit_Sz,face="bold"),
          panel.grid.major.y = element_line(size=0.25,linetype=5,color = 'grey'),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          plot.title = element_text(size = Tit_Sz),
          text = element_text(size = 15),
          legend.position = "bottom",
          legend.background = element_rect(fill='transparent',colour ='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent")
          
    ) +
    
    scale_colour_manual("", 
                        breaks = c("Simulated (AURORA)","Actual (AESO)"),
                        values = c("darkred","midnightblue")) +
    
    labs(title=year, y = "Pool Price ($/MWh)", x="Date",fill = "Resource") +
    scale_x_datetime(expand=c(0,0),limits=c(day_MN,day_MX),breaks = "month",date_labels = "%b") +
    
    scale_y_continuous(expand=c(0,0), 
                       limits= c(0,MX),
                       breaks = seq(0, 1000, by = 100)
                       
    )
}

################################################################################
## FUNCTIONS: AESO_SimO
## Plot comparison between actual and simulated data
##
## INPUTS:
##    year, month, day - Date to plot, the week will start on the day chosen
##    case - Run_ID which you want to plot
################################################################################

AESO_SimO <- function(year,month,day,case) {
  SimO <- Week1(year,month,day,case)
  ActO <- Week_act(year,month,day)

  # output Lmiits
  MXO <- plyr::round_any(
    max(layer_scales(SimO)$y$range$range,layer_scales(ActO)$y$range$range),
    100, f = ceiling)
  MNO <- plyr::round_any(
    min(layer_scales(SimO)$y$range$range,layer_scales(ActO)$y$range$range),
    100, f = floor)
  
  # Create each plot
  sz <- 15
  
  p1 <- Week1(year,month,day,case)+ 
    labs(title = paste0("Simulated Data for ",year),
         subtitle = paste0(SourceDB,", ",case)) +
    theme(legend.position ="none",
          axis.title.y=element_text(size=sz),
          plot.title = element_text(hjust = 0.5, size = sz),
          plot.subtitle = element_text(hjust = 0.5, size = sz-2))+ 
    scale_y_continuous(expand=c(0,0), limits = c(MNO,MXO), 
                       breaks = seq(0, MXO, by = MXO/4)) 
  
  p2 <- Week_act(year,month,day) + 
    labs(title = paste0("AESO Data for ",year),
         subtitle = "NRGStream Data") +
    theme(axis.title.y=element_blank(),
          plot.title = element_text(hjust = 0.5, size = sz),
          plot.subtitle = element_text(hjust = 0.5, size = sz-2))+ 
    scale_y_continuous(expand=c(0,0), limits = c(MNO,MXO), 
                       breaks = seq(0, MXO, by = MXO/4))
  
  legend <- get_legend(p2)
  p2 <- p2 + theme(legend.position ="none")
  
  #dev.new(width=3.75, height=2, unit="in")      
  
  # grid.arrange(plot_grid(p1,p2, nrow=1, align="hv", axis = "l",rel_widths=c(1,1)),
  #              legend,
  #              ncol=1, nrow=2,
  #              heights=c(1, 0.1))
  
  grid.arrange(p1,p2,legend,
               ncol=2, nrow=2,
               layout_matrix = rbind(c(1,2), c(3,3)),
               widths = c(2.7, 2.7), heights = c(2.5, 0.2))
}

################################################################################
## FUNCTIONS: rev_dur **NOT READ
## Plot difference between simulated and actual pool price
##
## INPUTS:
##    year1 and year2 - Years to compare
##    type - Plant type, ex: "WIND"
##    case - Run_ID which you want to plot
################################################################################

rev_dur <- function(year1, year2, type, case) {
  # Plots revenue duration plot by plant type, comparing simulated and AESO
  
  totZone <- ZoneHr_All%>%
    filter(Report_Year >= year1 & 
             Report_Year <= year2,
           Run_ID == case, 
           Condition != "Average") %>%
    subset(.,select=c(date,Condition,Price))
  
  typeH <- ResGroupHr_sub%>%
    sim_filt1(.) %>%
    filter(Report_Year >= year1 & 
             Report_Year <= year2,
           ID == type,
           Run_ID == case) 
  
  data1 <- left_join(totZone, typeH, by=c("date")) 
  
  totSim <- data1 %>%
    group_by(Condition, Report_Year) %>%
    mutate(Revenue = Price*Output_MWH/1000, perc = 1-ecdf(Revenue)(Revenue)) %>%
    select(Condition, Report_Year, Revenue, perc) %>%
    rename(Year = Report_Year) %>%
    ungroup() %>%
    mutate(sit = "Simulated")
  
  #  totSim$Report_Year <- as.factor(totSim$Report_Year)
  
  Actual <- na.omit(sub_samp)
  Actual$Year <- format(as.POSIXct(Actual$time, format = "%Y/%m/%d %H:%M:%S"), "%Y")
  Actual$Hour <- format(as.POSIXct(Actual$time, format = "%Y/%m/%d %H:%M:%S"), "%H")
  
  totAct <- Actual %>%
    filter(Year >= year1, 
           Year <= year2,
           Plant_Type == type) %>%
    mutate(Condition = if_else(between(Hour, 08, 23), 
                               "On-Peak WECC", "Off-Peak WECC"),
           Revenue = Revenue/1000) %>%
    group_by(Year, Condition) %>%
    mutate(perc = 1-ecdf(Revenue)(Revenue)) %>%
    select(Condition, Year, Revenue, perc) %>%
    ungroup() %>%
    mutate(sit = "Actual")
  
  total <- rbind(totSim, totAct)
  sz <- 15
  
  ggplot() +
    geom_line(data = total, 
              aes(x = perc, y = Revenue, colour = Year, linetype = sit), size = 1) +
    facet_grid(cols = vars(Condition)) +
    theme_bw() +
    theme(axis.text = element_text(size = sz),
          axis.title = element_text(size = sz),
          plot.title = element_text(size = sz+2),
          legend.text = element_text(size = sz),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(y = "Revenue ($ in thousands)", 
         x = "Percentage of Time", 
         title = paste("AESO ", type, "Data vs Simulation"),
         subtitle = SourceDB) +
    scale_color_manual(values = c("goldenrod1", "forestgreen", "cornflowerblue",
                                  "firebrick","gray60")) +
    scale_x_continuous(expand=c(0,0), 
                       limits = c(0,1.1),
                       labels = percent) +
    scale_y_continuous(expand=c(0,0)
    )
}

################################################################################
## FUNCTIONS: year_comp 
## Plots the difference in Pool Price between AESO and Sim
##
## INPUTS:
##    year- Year to compare
##    case - Run_ID which you want to plot
################################################################################

year_comp <- function(year,case) {
  
  wk_st <- as.POSIXct(paste(01,01,year, sep = "/"), format="%d/%m/%Y")
  wk_end <- as.POSIXct(paste(01,01,year+1, sep = "/"), format="%d/%m/%Y")
  
  sim <- ZoneHr_Avg
  #  sim$Date <- as.POSIXct(as.character(sim$date), tz = "MST")
  
  sim_wk <- sim %>%
    filter(date >= wk_st & date <= wk_end & Run_ID == case) %>%
    subset(., select = c(date, Price))
  
  
  act <- demand
  act$ActPrice <- act$Price
  
  act_wk <- act %>%
    filter(time >= wk_st & time <= wk_end) %>%
    subset(., select = c(time, ActPrice))
  colnames(act_wk) <- c("date","ActPrice")
  
  data <- merge(sim_wk, act_wk, by.x="date", by.y="date")
  
  data$diff <- (data$Price - data$ActPrice)
  
  # Plot the data    
  ggplot() +
    geom_line(data = data, 
              aes(x = date, y = diff), 
              size = 1.5, colour = "red") +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          text = element_text(size= 15)
    ) +
    ggtitle(paste("Simulated Data from "," (",SourceDB,")", sep = ""))+
    labs(x = year,
         y = "Difference in Simulated and \n Actual Pool Price ($/MWh)", 
         fill = "Resource") +
    scale_x_datetime(expand=c(0,0))
}

################################################################################
## FUNCTIONS: year_comp 
## Bar plot showing the difference between AESO and Sim
##
## INPUTS:
##    year- Year to compare
##    case - Run_ID which you want to plot
################################################################################

year_dif <- function(year,case) {
  
  wk_st <- as.POSIXct(paste(01,01,year, sep = "/"), format="%d/%m/%Y")
  wk_end <- as.POSIXct(paste(31,12,year, sep = "/"), format="%d/%m/%Y")
  
  sim <- ZoneHr_Avg
  sim_wk <- sim %>%
    filter(date >= wk_st & date <= wk_end & Run_ID == case) %>%
    subset(., select = c(date, Price))
  
  act <- demand
  act_wk <- act %>%
    filter(time >= wk_st & time <= wk_end) %>%
    subset(., select = c(time, Price))
  colnames(act_wk) <- c("date","actPrice")
  
  data <- merge(sim_wk, act_wk, by.x="date", by.y="date")
  
  results <- data %>%
    mutate(month = floor_date(date, "month")) %>%
    group_by(month) %>%
    summarize(Price = mean(Price), actPrice = mean(actPrice))
  results$month <- as.Date(results$month)
  
  results$diff <- (results$Price - results$actPrice)
  
  mx <- plyr::round_any(max(results$diff), 20, f = ceiling)
  mn <- plyr::round_any(min(results$diff), 20, f = floor)
  
  # Plot the data    
  ggplot(results, aes(x = month, y = diff)) +
    geom_bar(stat="identity", alpha = 0.7) +
    geom_text(aes(label = paste("$",round(diff, digits = 0),sep="")), 
              vjust = -0.3, size = 4)+#, angle = 90) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          text = element_text(size= 15),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5)
    ) +
    scale_y_continuous(expand=c(0,0),
                       limits = c(-60,140), 
                       breaks = pretty_breaks(8)) +
    scale_x_date(date_labels="%B", date_breaks = "months") +
    ggtitle(paste("Monthly Price Average Differences"," (",SourceDB,")", sep = ""))+
    labs(x = year,
         y = "Difference in Simulated and Actual \nAverage Pool Price ($/MWh)",
         colour = element_blank())
}

################################################################################
## FUNCTIONS: year_avg 
## Bar chart comparing monthly average pool prices
##
## INPUTS:
##    year- Year to compare
##    case - Run_ID which you want to plot
################################################################################

year_avg <- function(year,case) {
  
  wk_st <- as.POSIXct(paste(01,01,year, sep = "/"), format="%d/%m/%Y")
  wk_end <- as.POSIXct(paste(31,12,year, sep = "/"), format="%d/%m/%Y")
  
  sim <- ZoneHr_Avg
  sim_wk <- sim %>%
    filter(date >= wk_st & date <= wk_end & Run_ID == case) %>%
    subset(., select = c(date, Price))
  
  sim_wk$sit <- "Simulated"
  
  act <- demand
  act$sit <- "Actual"
  
  act_wk <- act %>%
    filter(time >= wk_st & time <= wk_end) %>%
    subset(., select = c(time, Price, sit))
  colnames(act_wk) <- c("date","Price","sit")
  
  data <- rbind(sim_wk, act_wk)
  
  results <- data %>%
    mutate(month = floor_date(date, "month")) %>%
    group_by(month,sit) %>%
    summarize(avPrice = mean(Price), sd = sd(Price))
  results$month <- as.Date(results$month)
  
  mx <- plyr::round_any(max(results$avPrice), 20, f = ceiling)
  
  # Plot the data    
  ggplot(results, aes(x = month, y = avPrice, fill=sit, colour = sit)) +
    geom_bar(stat="identity", position = "dodge", alpha = 0.7) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          text = element_text(size= 15),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5)
    ) +
    scale_fill_manual(values = c("forestgreen","dodgerblue")) +
    scale_color_manual(values = c("forestgreen","dodgerblue")) +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,160), 
                       breaks = pretty_breaks(6)) +
    scale_x_date(date_labels="%B", date_breaks = "months") +
    ggtitle(paste("Monthly Price Averages"," (",SourceDB,")", sep = ""))+
    labs(x = year,
         y = "Average Pool Price \n($/MWh)",
         fill = element_blank(),
         colour = element_blank())
}

################################################################################
## FUNCTIONS: year_pool 
## A function to plot the Monthly average pool price 
## (Like in the AESO Market Report 2021 Figure 1)
##
## INPUTS:
##    year1. year2 - Year to compare
##    case - Run_ID which you want to plot
################################################################################

year_pool <- function(year1, year2,case) {
  
  # Filter and prepare Simulation data
  Sim <- ZoneHr_All%>%
    filter(
      Run_ID == case,
      Condition != "Average") %>%
    group_by(Report_Year,Report_Month) %>%
    summarise(
      Price = mean(Price)
    ) %>%
    mutate(Date = as.Date(paste(Report_Year,Report_Month,"01"), "%Y %m %d"),
           type = "MonAve",
           case = "Simulation"
    ) 
  
  # Calculate rolling average
  SimMA <- Sim %>%
    mutate(Price = 
             rollapplyr(Price, width = 12, FUN = mean, partial = TRUE),
           type = "RollAve")
  
  # Combine sim data with rolling average
  Sim <- rbind(Sim,SimMA) %>%
    filter(Report_Year >= year1 &
             Report_Year <= year2) %>%
    subset(.,select = c(Date,
                        Price,
                        case,
                        type
    ))
  
  # Prepare AESO data by creating Year and Month columns
  Actual <- na.omit(demand)
  Actual$Year <- format(as.POSIXct(Actual$time, format = "%Y/%m/%d %H:%M:%S"), "%Y")
  Actual$Month <- format(as.POSIXct(Actual$time, format = "%Y/%m/%d %H:%M:%S"), "%m")
  
  # Filter and prepare AESO data
  AESO <- Actual %>%
    group_by(Year,Month) %>%
    summarise(
      Price = mean(Price),
    ) %>%
    ungroup() %>%
    mutate(Date = as.Date(paste(Year,Month,"01"), "%Y %m %d"), 
           type = "MonAve",
           case = "AESO",
    )
  
  # Calculate rolling average
  ActMA <- AESO %>%
    mutate(Price = 
             rollapplyr(Price, width = 12, FUN = mean, partial = TRUE),
           type = "RollAve")
  
  # Combine AESO data with rolling average
  AESO <- rbind(AESO,ActMA) %>%
    filter(Year >= year1 &
             Year <= year2) %>%
    subset(.,select = c(Date,
                        Price,
                        case,
                        type
    ))
  
  # Combine simulation and AESO data
  total <- rbind(Sim,AESO)
  
  # Set font size for plot
  sz <- 15
  
  ggplot() +
    geom_line(data = total,
              aes(x = Date, y = Price, linetype = case, colour = type), 
              size = 1) +
    theme_bw() +
    theme(axis.text = element_text(size = sz),
          axis.title = element_text(size = sz),
          axis.text.x = element_text(angle = 45, hjust=1, size = sz),
          plot.title = element_text(size = sz+2),
          legend.text = element_text(size = sz),
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
    labs(y = "Average Pool Price \n($/MWh)", 
         title = "AESO Data vs Simulation Monthly Average Pool Price",
         subtitle = SourceDB) +
    scale_linetype_discrete(labels = c("Actual","Simulated"))+
    scale_color_manual(values = c("midnightblue", "orange"),
                       labels = c("Monthly Ave","12-Month Rolling")) +
    scale_x_date(date_labels = "%b-%Y",
                 expand=c(0,0), 
                 date_breaks = "3 months"
    ) +
    scale_y_continuous(expand=c(0,0),
                       n.breaks = 3
    )
}

################################################################################
## FUNCTIONS: comp_dur
## Plots the Pool Price duration vs percentile for AESO and Sim
## Like AESO Market Report 2021 Figures 2 and 3
##
## INPUTS:
##    year1. year2 - Year to compare
##    case - Run_ID which you want to plot
################################################################################

comp_dur <- function(year1, year2, case) {

    # Load and filter Simulation data, 
  # Calculate the percentage of time
  # Create column 'sit' to indicate Simulation
  totSim <- ZoneHr_All%>%
    filter(Report_Year >= (year1) & 
             Report_Year <= year2,
           Run_ID == case, 
           Condition != "Average") %>%
    group_by(Condition, Report_Year) %>%
    mutate(perc = 1-ecdf(Price)(Price)) %>%
    subset(., select=c(Condition, Report_Year, Price, perc)) %>%
    rename(Year = Report_Year) %>%
    ungroup() %>%
    mutate(sit = "Simulated")
  
  # Load and filter AESO data, 
  # Calculate the percentage of time
  # Create column 'sit' to indicate Actual AESO data
  Actual <- na.omit(demand)
  Actual$Year <- format(as.POSIXct(Actual$time, format = "%Y/%m/%d %H:%M:%S"), "%Y")
  Actual$Hour <- format(as.POSIXct(Actual$time, format = "%Y/%m/%d %H:%M:%S"), "%H")
  
  totAct <- Actual %>%
    filter(Year >= (year1), 
           Year <= year2,) %>%
    mutate(Condition = if_else(between(Hour, 08, 23), 
                               "On-Peak WECC", "Off-Peak WECC")) %>%
    group_by(Year, Condition) %>%
    mutate(perc = 1-ecdf(Price)(Price)) %>%
    subset(., select=c(Condition, Year, Price, perc)) %>%
    ungroup() %>%
    mutate(sit = "Actual")
  
  # Combine Actual and Simulation data
  total <- rbind(totSim, totAct)
  
  # Set font size for plot
  sz <- 15
  
  ggplot() +
    geom_line(data = total, 
              aes(x = perc, y = Price, colour = Year, linetype = sit), size = 1) +
    facet_grid(cols = vars(Condition)) +
    theme_bw() +
    theme(axis.text = element_text(size = sz),
          axis.title = element_text(size = sz),
          plot.title = element_text(size = sz+2),
          legend.text = element_text(size = sz),
          
          # For transparent background
          panel.grid = element_blank(),
          legend.title = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(y = "Pool Price$/MWh", 
         x = "Percentage of Time", 
         title = "AESO Data vs Simulation",
         subtitle = SourceDB) +
    scale_color_manual(values = c("goldenrod1", "forestgreen", "cornflowerblue",
                                  "firebrick","gray60")) +
    scale_x_continuous(expand=c(0,0), 
                       limits = c(0,1.1),
                       labels = percent) +
    scale_y_continuous(expand=c(0,0)
    )
}

################################################################################
## FUNCTIONS: load_dur
## Plots the load duration vs percentile for AESO and Sim
## Like AESO Market Report 2021 Figures 7 and 8
##
## INPUTS:
##    year1. year2 - Year to compare
##    case - Run_ID which you want to plot
################################################################################

load_dur <- function(year1, year2, case) {

    # Load and filter Simulation data, 
  # Calculate the percentage of time
  # Create column 'sit' to indicate Simulation
  totSim <- ZoneHr_All%>%
    filter(Report_Year >= year1 & 
             Report_Year <= year2,
           Run_ID == case, 
           Condition != "Average") %>%
    group_by(Condition, Report_Year) %>%
    mutate(perc = 1-ecdf(Demand)(Demand)) %>%
    dplyr::select(Condition, Report_Year, Demand, perc) %>%
    rename(Year = Report_Year) %>%
    ungroup() %>%
    mutate(sit = "Simulated")
  
  # Load and filter AESO data, 
  # Calculate the percentage of time
  # Use AIL as demand
  # Create column 'sit' to indicate Actual AESO data
  Actual <- na.omit(demand)
  Actual$Year <- format(as.POSIXct(Actual$time, format = "%Y/%m/%d %H:%M:%S"), "%Y")
  Actual$Hour <- format(as.POSIXct(Actual$time, format = "%Y/%m/%d %H:%M:%S"), "%H")
  
  totAct <- Actual %>%
    filter(Year >= year1, 
           Year <= year2,) %>%
    mutate(Condition = if_else(between(Hour, 08, 23), 
                               "On-Peak WECC", "Off-Peak WECC")) %>%
    group_by(Year, Condition) %>%
    mutate(perc = 1-ecdf(AIL)(AIL)) %>%
    dplyr::select(Condition, Year, AIL, perc) %>%
    ungroup() %>%
    mutate(sit = "Actual", Demand = AIL) %>%
    dplyr::select(Condition, Year, Demand, perc,sit)
  
  # Combine Actual and Simulation data
  total <- rbind(totSim, totAct)
  
  # Set font size for plot
  sz <- 15
  
  ggplot() +
    geom_line(data = total, 
              aes(x = perc, y = Demand, colour = Year, linetype = sit), size = 1) +
    facet_grid(cols = vars(Condition)) +
    theme_bw() +
    theme(axis.text = element_text(size = sz),
          axis.title = element_text(size = sz),
          plot.title = element_text(size = sz+2),
          legend.text = element_text(size = sz),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          
          # For transparent background
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(y = "Hourly Alberta Internal Load (MW)", 
         x = "Percentage of Time", 
         title = "AESO Data vs Simulation",
         subtitle = SourceDB) +
    scale_color_manual(values = c("goldenrod1", "forestgreen", "cornflowerblue",
                                  "firebrick","gray60")) +
    scale_x_continuous(expand=c(0,0), 
                       limits = c(0,1.1),
                       labels = percent) +
    scale_y_continuous(expand=c(0,0)
    )
}

################################################################################
## FUNCTIONS: tech_cap 
## Plots the capacity factor by technology for AESO and Sim
## Like AESO Market Report 2021 Figure 15
##
## INPUTS:
##    year1. year2 - Year to compare
##    case - Run_ID which you want to plot
################################################################################

tech_cap <- function(yearstart, yearend, case) {
  # Plots the capacity factor by technology for AESO and Sim
  # Like AESO Market Report 2021 Figure 15
  
  Act <- df1 %>%
    filter(Plant_Type != "STORAGE") %>%
    group_by(Year, Plant_Type) %>%
    summarise(Cap = mean(meancap)) %>%
    mutate(sit = "Actual")
  
  Act$Year <- as.numeric(as.character(Act$Year))
  
  Act <- na.omit(Act)
  
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "HYDRO",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "WIND",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "SOLAR",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "OTHER",after=Inf)
  #  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "STORAGE",after=Inf)
  
  Sim <- ResGroupHr_sub%>%
    filter(Run_ID == case,
           ID == "LTO_Coal" | ID == "AB_CCCT_noncogen" | ID == "LTO_Cogen" | 
             ID == "AB_SCCT_noncogen" | ID == "LTO_Hydro" | ID == "LTO_Other" | 
             ID == "LTO_Wind" | ID == "LTO_Solar") %>%
    group_by(Report_Year, ID) %>%
    summarise(Cap = mean(Capacity_Factor)) %>%
    mutate(sit = "Simulation")
  
  colnames(Sim) <- c("Year", "Plant_Type", "Cap", "sit")
  
  Sim$Plant_Type <- factor(Sim$Plant_Type, levels=c("LTO_Coal", "AB_CCCT_noncogen", "LTO_Cogen",
                                                    "AB_SCCT_noncogen", "LTO_Hydro", "LTO_Other", 
                                                    "LTO_Wind", "LTO_Solar"))
  levels(Sim$Plant_Type) <- c("COAL", "NGCC", "COGEN", "SCGT", "HYDRO", "OTHER",
                              "WIND", "SOLAR")
  
  total <- rbind(Sim,Act)
  
  total <- total %>%
    filter(Year >= yearstart,
           Year <= yearend)
  
  sz <- 15
  
  ggplot() +
    geom_col(data = total, position = "dodge", alpha = 0.8, width = 0.7,
             aes(x = Plant_Type, y = Cap, fill = sit, linetype = sit)) +
    facet_grid(~Year) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title.x = element_blank(),
          axis.text = element_text(size = sz),
          axis.title = element_text(size = sz),
          plot.title = element_text(size = sz+2),
          legend.text = element_text(size = sz),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          
          # For transparent background
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(y = "Capacity Factor", 
         title = "AESO Data vs Simulation",
         subtitle = SourceDB) +
    scale_fill_manual(values = c("black", "gray60"
                                 #      "goldenrod1", "forestgreen"
                                 #                                 "darkseagreen", "cornflowerblue",
                                 #                                  "firebrick","gray60", "forestgreen"
    )) +
    #    scale_x_continuous(expand=c(0,0), 
    #                       limits = c(0,1.1),
    #                       labels = percent) +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,1),
                       breaks = seq(0,1, by = 0.2)
    )
}

################################################################################
## FUNCTIONS: margin **Not read
## Plots the marginal price-setting technology for AESO and Sim
## Like AESO Market Report 2021 Figure 19
##
## INPUTS:
##    year1. year2 - Year to compare
##    case - Run_ID which you want to plot
################################################################################

margin <- function(year1, year2, case) {

    totZone <- ZoneHr_All%>%
    filter(Report_Year >= year1 & 
             Report_Year <= year2,
           Run_ID == case, 
           Condition != "Average") %>%
    mutate(Name = Marginal_Resource) %>%
    subset(.,select=c(date,Name,Price))
  
  totHour <- ResHr%>%
    filter(Report_Year >= year1 & 
             Report_Year <= year2,
           Run_ID == case) %>%
    subset(.,select=c(date,Name,Dispatch_Cost,Incr_Cost,Primary_Fuel,Percent_Marginal,Zone))
  
  data1 <- left_join(totZone, totHour, by=c("date","Name")) 
  
  data1 <- data1 %>%
    group_by(Name, Report_Year) %>%
    mutate(perc = 1-ecdf(Price)(Price)) %>%
    
    Act <- merit_filt %>%
    filter(dispatched_mw != 0) %>%
    group_by(date, he) %>%
    slice_max(n=1,merit)
}


################################################################################
## FUNCTIONS: tot_cap **Not read
## Plots the year-end capacity by technology for AESO and Sim
## Like AESO Market Report 2021 Figure 11
##
## INPUTS:
##    year1. year2 - Year to compare
##    case - Run_ID which you want to plot
################################################################################

tot_cap <- function(year1, year2, case) {

  
  Act <- sub_samp %>%
    filter(! NRG_Stream %in% trade_excl,
           year(time) >= year1,
           year(time) <= year2,
           month(time) == 12,
           day(time) == 31,
           hour(time) == 23,
           #           Plant_Type != "STORAGE"
    ) %>%
    group_by(time, Plant_Type) %>%
    summarise(Cap = sum(Capacity)) %>%
    #    subset(., select=c(time, Cap, perc)) %>%
    mutate(sit = "Actual", Year = as.factor(year(time))) %>%
    subset(., select=c(Year, Plant_Type, Cap, sit))
  
  Act <- na.omit(Act)
  
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "HYDRO",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "WIND",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "SOLAR",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "OTHER",after=Inf)
  Act$Plant_Type<-fct_relevel(Act$Plant_Type, "STORAGE",after=Inf)
  
  Sim <- ResGroupHr%>%
    filter(Run_ID == case,
           Report_Month == 12,
           Report_Day == 31,
           Report_Hour == 24,
           ID == "LTO_Coal" | ID == "AB_CCCT_noncogen" | ID == "LTO_Cogen" | 
             ID == "AB_SCCT_noncogen" | ID == "LTO_Hydro" | ID == "LTO_Other" | 
             ID == "LTO_Wind" | ID == "LTO_Solar") %>%
    group_by(Report_Year, ID) %>%
    subset(., select=c(Report_Year, ID, Capacity)) %>%
    #    summarise(Cap = mean(Capacity_Factor)) %>%
    mutate(sit = "Simulation")
  
  colnames(Sim) <- c("Year", "Plant_Type", "Cap", "sit")
  
  Sim$Plant_Type <- factor(Sim$Plant_Type, levels=c("LTO_Coal", "AB_CCCT_noncogen", "LTO_Cogen",
                                                    "AB_SCCT_noncogen", "LTO_Hydro", "LTO_Other", 
                                                    "LTO_Wind", "LTO_Solar"))
  levels(Sim$Plant_Type) <- c("COAL", "NGCC", "COGEN", "SCGT", "HYDRO", "OTHER",
                              "WIND", "SOLAR")
  
  Sim$Year <- as.factor(Sim$Year)
  
  total <- rbind(Sim,Act)
  
  sz <- 15
  
  ggplot() +
    geom_col(data = total, position = "dodge", alpha = 0.8, width = 0.7,
             aes(x = Plant_Type, y = Cap, fill = sit, linetype = sit)) +
    facet_grid(~Year) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title.x = element_blank(),
          axis.text = element_text(size = sz),
          axis.title = element_text(size = sz),
          plot.title = element_text(size = sz+2),
          legend.text = element_text(size = sz),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          
          # For transparent background
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(y = "Installed Generation Capacity (MW)", 
         title = "Year-end generation capacity AESO Data vs Simulation",
         subtitle = SourceDB) +
    scale_fill_manual(values = AESO_colours) +
    #    scale_x_continuous(expand=c(0,0), 
    #                       limits = c(0,1.1),
    #                       labels = percent) +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,6000),
                       #                       breaks = seq(0,1, by = 0.2)
    )
}

################################################################################
## FUNCTIONS: AESOSim **Not read
##
## INPUTS:
################################################################################

AESOSim <- function(year1,year2,case) {
  
  sz <- 16
  
  p.c <- comp_dur(year1,year2,case) +
    theme(axis.text = element_text(size = sz),
          axis.title = element_text(size = sz),
          axis.text.x = element_text(angle = 45, hjust=1, size = sz),
          plot.title = element_text(size = sz+2),
          legend.text = element_text(size = sz-2),
          axis.title.x = element_blank())
  p.y <- year_pool(year1,year2,case) + 
    theme(axis.text = element_text(size = sz),
          axis.title = element_text(size = sz),
          axis.text.x = element_text(angle = 45, hjust=1, size = sz),
          
          legend.text = element_text(size = sz-2),
          plot.title = element_blank(),
          plot.subtitle = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "right")
  p.t <- tech_cap(year1,year2,case) + 
    theme(axis.text = element_text(size = sz-2),
          axis.title = element_text(size = sz),
          axis.text.x = element_text(angle = 45, hjust=1, size = sz-2),
          
          legend.text = element_text(size = sz-2),
          plot.title = element_blank(),
          plot.subtitle = element_blank(),
          legend.position = "right")
  
  p.c + p.y + p.t + plot_layout(design = "A
                                B
                                C") &
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          panel.border = element_rect(colour = "black", fill = "transparent"))
  

}