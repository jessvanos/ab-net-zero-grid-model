################################################################################
# TITLE: Net_Zero_eval
# DESCRIPTION: Functions to evaluate electricity grid as it approaches possible net zero states

# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: July 4, 2022; LAST EDIT: July 4, 2022

################################################################################
################################################################################  
## FUNCTION: Retirecol
## Plotting the resources retired as a bar chart
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResYr - Resource Year table describing all resources start and end dates
################################################################################

# Stacked Area showing totals for Fuel Types
Retirecol <- function(case) {
  
  # Bring in Resource Year Table and filter columns
  Retdata <- ResYr%>%
    sim_filt2(.) %>% #Filter to rename fuels
    subset(., select=c(Name,Condition,YEAR,Capacity,End_Date,Run_ID,Primary_Fuel,Time_Period)) %>%
    filter(Run_ID == case) %>%
    filter(Condition == "Average") %>%
    filter(Time_Period == "2035")

  
  # Set levels to each category in order specified
  Retdata$Primary_Fuel <- factor(Retdata$Primary_Fuel, levels=c("Coal", "Cogen", "Gas", "Gas1", "Gas2", "Hydro","Solar",
                                      "Wind", "Storage", "Other"))
  
  #Get Year max for run and filter for end dates BEFORE this date
  MaxYr <- max(Retdata$YEAR)
  Retdata$End_Date  <- as.Date(Retdata$End_Date, 
                              format = "%m/%d/%Y")
  Retdata$End_Date <- format(Retdata$End_Date,format="%Y")
  Retdata <- Retdata%>%
    filter(.,End_Date <= MaxYr) 
  
  # Add a column to describe the new resources 
  Retdata$RetUnits <- 1  
  
  #Now group everything together
  Retdata <- Retdata%>%
  group_by(Primary_Fuel, End_Date) %>%
    summarise(Units = sum(RetUnits), Capacity = sum(Capacity))

  #Max Units Built
  mxu <- round_any(max(Retdata$Units),10,f=ceiling)
  mxc <- max(Retdata$Capacity)
  
  #Plot data
  ggplot(Retdata) +
    aes(x=End_Date, y=Units, fill = Primary_Fuel, group = Primary_Fuel,label=Units) +
    geom_bar(position="stack", stat="identity", alpha=1) +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),  
          axis.title.x = element_text(size = XTit_Sz,hjust=0.5),
          axis.title.y = element_text(size = YTit_Sz, vjust=0),
          panel.background = element_rect(fill = "transparent"),
          plot.title = element_blank(),
          legend.justification = c(0.5,0.5),
          legend.position = ("bottom"),
          legend.title=element_blank(), 
          legend.key.size = unit(1,"lines"),
          plot.caption=element_text(size=10),
          panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          text = element_text(size = 20)) +
    
    geom_text(position = position_stack(vjust = 0.5),colour="black") +
    
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    
    labs(x = "Year", y = "Units Retired", fill = "Fuel Type")  +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,(mxu)),breaks=breaks_pretty(6)) +
    
    scale_fill_manual(values=c("Coal" =cOL_COAL, "Cogen"=cOL_COGEN, "Gas"=cOL_Gas, 
                               "Gas1"=COL_Gas1, "Gas2"=COL_Gas2,
                               "Hydro"=cOL_HYDRO, "Solar"=cOL_SOLAR, 
                               "Wind"=cOL_WIND, "Storage"=cOL_STORAGE,"Other"=cOL_OTHER))
    
}

################################################################################  
## FUNCTION: RetireMW
## Plotting the resource capacity retired as a bar chart
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResYr - Resource Year table describing all resources start and end dates
################################################################################

# Stacked Area showing totals for Fuel Types
RetirecMW <- function(case) {

  # Bring in Resource Year Table and filter columns
  Retdata <- ResSt%>%
    sim_filt2(.) %>% #Filter to rename fuels
    subset(., select=c(Name,Condition,Capacity,End_Date,Run_ID,Primary_Fuel,Time_Period)) %>%
    filter(Run_ID == case) %>%
    filter(Condition == "Average") 
  
  
  # Set levels to each category in order specified
  Retdata$Primary_Fuel <- factor(Retdata$Primary_Fuel, levels=c("Coal", "Cogen", "Gas", "Gas1", "Gas2", "Hydro","Solar",
                                                                "Wind", "Storage", "Other"))
  
  #Get Year max for run and filter for end dates BEFORE this date
  MaxYr <- max(ResYr$YEAR)
  Retdata$End_Date  <- as.Date(Retdata$End_Date, 
                               format = "%m/%d/%Y")
  Retdata$End_Date <- format(Retdata$End_Date,format="%Y")
  Retdata <- Retdata%>%
    filter(.,End_Date <= MaxYr) 
  
  #Now group everything together
  Retdata <- Retdata%>%
    group_by(Primary_Fuel, End_Date) %>%
    summarise(Capacity = sum(Capacity))
  
  #Max Units Built
  mxc <- round_any(max(Retdata$Capacity),10,f=ceiling)
  
  #Plot data
  ggplot(Retdata) +
    aes(x=End_Date, y=Capacity, fill = Primary_Fuel, group = Primary_Fuel) +
    geom_bar(position="stack", stat="identity", alpha=1) +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),  
          axis.title.x = element_text(size = XTit_Sz,hjust=0.5),
          axis.title.y = element_text(size = YTit_Sz, vjust=0),
          panel.background = element_rect(fill = "transparent"),
          plot.title = element_blank(),
          legend.justification = c(0.5,0.5),
          legend.position = ("bottom"),
          legend.title=element_blank(), 
          legend.key.size = unit(1,"lines"),
          plot.caption=element_text(size=10),
          panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          text = element_text(size = 20)) +
    
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    
    labs(x = "Year", y = "Capacity Retired", fill = "Fuel Type")  +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,(mxc)),breaks=breaks_pretty(6)) +
    
    scale_fill_manual(values=c("Coal" =cOL_COAL, "Cogen"=cOL_COGEN, "Gas"=cOL_Gas, 
                               "Gas1"=COL_Gas1, "Gas2"=COL_Gas2,
                               "Hydro"=cOL_HYDRO, "Solar"=cOL_SOLAR, 
                               "Wind"=cOL_WIND, "Storage"=cOL_STORAGE,"Other"=cOL_OTHER))
  
}

################################################################################  
## FUNCTION: Builtcol
## Plotting the resources built as a bar chart
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    Build - Build table describing all new resources
################################################################################

# Stacked Area showing totals for Fuel Types
Builtcol <- function(case) {
  MaxIt <- max(Build$LT_Iteration)
  
  data <- Build %>%
    filter(Run_ID == case & LT_Iteration == MaxIt 
           &  Time_Period != "Study"
    )%>%
    group_by(Fuel_Type, Time_Period) %>%
    summarise(Units = sum(Units_Built), Capacity = sum(Capacity_Built)) 
  
  data$Fuel_Type <- factor(data$Fuel_Type, 
                           levels = c("WND","SUN","Gas0","Gas1", "PS", "OT"))
  
  levels(data$Fuel_Type) <- c("Wind","Solar","Gas0","Gas1", "Storage", "Other")
  
  Tot <- data %>%
    group_by(Time_Period) %>%
    summarise(totu = sum(Units), totc = sum(Capacity))
  
  mxu <- round_any(max(Tot$totu),10,f=ceiling)
  mxc <- max(Tot$totc)
  
  ggplot(data) +
    aes(Time_Period, Units, fill = Fuel_Type, group = Fuel_Type) +
    geom_bar(position="stack", stat="identity", alpha=1) +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),  
          axis.title.x = element_text(size = XTit_Sz,hjust=0.5),
          axis.title.y = element_text(size = YTit_Sz, vjust=0),
          panel.background = element_rect(fill = "transparent"),
          plot.title = element_text(size = Tit_Sz),
          legend.justification = c(0.5,0.5),
          legend.position = ("bottom"),
          legend.title=element_blank(), 
          legend.key.size = unit(1,"lines"),
          plot.caption=element_text(size=10),
          panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          text = element_text(size = 20)) +
    
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    
    labs(x = "Date", y = "Units Built", fill = "Fuel Type",
         caption="Note: Units may be partially built to a certain capacity which is why numbers are not all even") +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,(mxu)),breaks=breaks_pretty(6)) +
    
    scale_fill_manual(values=c("Coal" =cOL_COAL, "Cogen"=cOL_COGEN, "Gas0"=cOL_Gas, 
                               "Gas1"=COL_Gas1, "Gas2"=COL_Gas2,
                               "Hydro"=cOL_HYDRO, "Solar"=cOL_SOLAR, 
                               "Wind"=cOL_WIND, "Storage"=cOL_STORAGE,"Other"=cOL_OTHER))
}
################################################################################  
## FUNCTION: Add_Ret
## Plotting the resources added and retired for study
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResYr - Resource Year table describing all resources start and end dates
################################################################################
Add_Ret <- function(case) {
  
  #Retire Stuff
      # Bring in Resource Year Table and filter columns
      Retdata <- ResSt%>%
        sim_filt2(.) %>% #Filter to rename fuels
        subset(., select=c(Name,Condition,Capacity,End_Date,Run_ID,Primary_Fuel,Time_Period)) %>%
        filter(Run_ID == case) %>%
        filter(Condition == "Average") 
      
      
      # Set levels to each category in order specified
      Retdata$Primary_Fuel <- factor(Retdata$Primary_Fuel, levels=c("Coal", "Cogen", "Gas", "Gas1", "Gas2", "Hydro","Solar",
                                                                    "Wind", "Storage", "Other"))
      
      #Get Year max for run and filter for end dates BEFORE this date
      MaxYr <- max(ResYr$YEAR)
      Retdata$End_Date  <- as.Date(Retdata$End_Date, 
                                   format = "%m/%d/%Y")
      Retdata$End_Date <- format(Retdata$End_Date,format="%Y")
      Retdata <- Retdata%>%
        filter(.,End_Date <= MaxYr) 
      
      #Now group everything together
      Retdata <- Retdata%>%
        group_by(Primary_Fuel, End_Date) %>%
        summarise(Capacity = sum(Capacity))
      
      Retdata$Capacity <- Retdata$Capacity*-1
      
  # Build Stuff
      Buldata <- Build %>%
        filter(Run_ID == case & LT_Iteration == max(LT_Iteration) & 
                 Time_Period != "Study")%>%
        group_by(Fuel_Type, Time_Period) %>%
        summarise(Units = sum(Units_Built), Capacity = sum(Capacity_Built)) 
      
      Buldata$Fuel_Type <- factor(Buldata$Fuel_Type, 
                               levels = c("WND","SUN","Gas0","Gas1", "PS", "OT"))
      
      levels(Buldata$Fuel_Type) <- c("Wind","Solar","Gas0","Gas1", "Storage", "Other")
      
      Tot <- Buldata %>%
        group_by(Time_Period) %>%
        summarise(totc = sum(Capacity))

  
  #Max/Min Units Built
  mnc <- round_any(min(Retdata$Capacity),10,f=floor)
  mxc <- max(Tot$totc) 
  
  #Plot data
  ggplot() +
    aes(data=Retdata, x=End_Date, y=Capacity, fill = Primary_Fuel, group = Primary_Fuel) +
    geom_bar(position="stack", stat="identity", alpha=1) +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),  
          axis.title.x = element_text(size = XTit_Sz,hjust=0.5),
          axis.title.y = element_text(size = YTit_Sz, vjust=0),
          panel.background = element_rect(fill = "transparent"),
          plot.title = element_blank(),
          legend.justification = c(0.5,0.5),
          legend.position = ("bottom"),
          legend.title=element_blank(), 
          legend.key.size = unit(1,"lines"),
          plot.caption=element_text(size=10),
          panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          text = element_text(size = 20)) +
    
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    
    labs(x = "Year", y = "Capacity", fill = "Fuel Type")  +
    scale_y_continuous(expand=c(0,0),
                       limits = c(mnc,0),breaks=breaks_pretty(6)) +
    
    scale_fill_manual(values=c("Coal" =cOL_COAL, "Cogen"=cOL_COGEN, "Gas"=cOL_Gas, 
                               "Gas1"=COL_Gas1, "Gas2"=COL_Gas2,
                               "Hydro"=cOL_HYDRO, "Solar"=cOL_SOLAR, 
                               "Wind"=cOL_WIND, "Storage"=cOL_STORAGE,"Other"=cOL_OTHER)) 
  
  
  # # Create each plot
  # sz <- 15
  # 
  # p1 <- Builtcol(case) +
  #   theme(legend.position ="none")
  # 
  # p2 <- Retirecol(case) + 
  #   theme(axis.title.x = element_blank(),
  #         axis.text.x = element_blank()) +
  #   
  #   # Det up for an upside down graph      
  #   scale_y_reverse() +
  #   scale_x_discrete(position = "top") 
  #   
  # 
  # legend <- get_legend(p2)
  # p2 <- p2 + theme(legend.position ="none")
  # 
  # grid.arrange(p1,p2,legend,
  #              ncol=1, nrow=3,
  #              layout_matrix = rbind(c(1), c(2),c(3)),
  #              widths = c(1), heights = c(1,1, 0.2))
  # 
}
  
  
 #################################################################################
# NEW TEST
BBcol <- function(case) {
  
  # Bring in Resource Year Table and filter columns
  Buldata <- ResSt%>%
    sim_filt2(.) %>% #Filter to rename fuels
    subset(., select=c(Name,Condition,Capacity,Beg_Date,Run_ID,Primary_Fuel)) %>%
    filter(Run_ID == case) %>%
    filter(Condition == "Average") 
  
  
  # Set levels to each category in order specified
  Buldata$Primary_Fuel <- factor(Buldata$Primary_Fuel, levels=c("Coal", "Cogen", "Gas", "Gas1", "Gas2", "Hydro","Solar",
                                                                "Wind", "Storage", "Other"))
  
  #Get Year max for run and filter for end dates BEFORE this date
  MaxYr <- max(ResYr$YEAR)
  MinYr <- min(ResYr$YEAR)
    
  Buldata$Beg_Date  <- as.Date(Buldata$Beg_Date, 
                               format = "%m/%d/%Y")
  Buldata$Beg_Date <- format(Buldata$Beg_Date,format="%Y")
  Buldata <- Buldata%>%
    filter(.,Beg_Date <= MaxYr) %>%
    filter(.,Beg_Date >= MinYr) 
  
  # Add a column to describe the new resources 
  Buldata$BulUnits <- 1  
  
  #Now group everything together
  Buldata <- Buldata%>%
    group_by(Primary_Fuel, Beg_Date) %>%
    summarise(Units = sum(BulUnits), Capacity = sum(Capacity))
  
  #Max Units Built
  mxu <- round_any(max(Buldata$Units),20,f=ceiling)
  
  Buldata$Beg_Date  <- as.Date(Buldata$Beg_Date, 
                               format = "%Y")
  
  #Plot data
  ggplot(Buldata) +
    aes(x=Beg_Date, y=Units, fill = Primary_Fuel, group = Primary_Fuel) +
    geom_bar(position="Stack", stat="identity", alpha=1) +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),  
          axis.title.x = element_text(size = XTit_Sz,hjust=0.5),
          axis.text.x = element_text(size = XTit_Sz),
          axis.title.y = element_text(size = YTit_Sz, vjust=0),
          panel.background = element_rect(fill = "transparent"),
          plot.title = element_blank(),
          legend.justification = c(0.5,0.5),
          legend.position = ("bottom"),
          legend.title=element_blank(), 
          legend.key.size = unit(1,"lines"),
          plot.caption=element_text(size=10),
          panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          text = element_text(size = 20)) +
    
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    
    labs(x = "Year", y = "Units Added", fill = "Fuel Type")  +
    
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,(mxu)),breaks=breaks_pretty(6)) +
    
    scale_x_date(expand=c(0,0),breaks = "year",date_labels = "%Y") +
    
    scale_fill_manual(values=c("Coal" =cOL_COAL, "Cogen"=cOL_COGEN, "Gas"=cOL_Gas, 
                               "Gas1"=COL_Gas1, "Gas2"=COL_Gas2,
                               "Hydro"=cOL_HYDRO, "Solar"=cOL_SOLAR, 
                               "Wind"=cOL_WIND, "Storage"=cOL_STORAGE,"Other"=cOL_OTHER))
}
  
  