################################################################################
# TITLE: Price_Functions
# DESCRIPTION: Functions related to technology capture prices, pool prices, and other cost related material. 
#
# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: January 6, 2023; LAST EDIT: May 31, 2023
#
################################################################################

################################################################################  
## FUNCTION: week_price 
## Electricity price for one week. 
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
              size = 1.25, colour = "darkred") +
    theme_bw() +
    theme(text=element_text(family=Plot_Text)) +
    theme(panel.background = element_rect(fill = "transparent"),
          axis.text.x=element_text(vjust=-1,color="black"),
          axis.title.x = element_text(vjust=-1,size= XTit_Sz,face="bold"),
          axis.text.y=element_text(color="black"),
          axis.title.y = element_text(vjust=2,size= YTit_Sz,face="bold"),
          #panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'grey'),
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
                       limits= c(0,1050),
                       #                       labels = label_number(accuracy = 1),
                       breaks = seq(0, 1000, by = 200)
    )
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
          axis.text=element_text(colour = "black"),
          axis.title.y = element_text(size = YTit_Sz,face="bold"),
          text = element_text(size = 15),
          legend.title = element_blank(),
          #panel.grid.major.y = element_line(size=0.25,linetype=5,color = "gray70")
          ) +
    
    labs(y = "Wholesale Pool Price ($/MWh)", x = "Percentage of Time",caption = SourceDB) +
    
    scale_color_brewer(palette= "RdYlBu") +
    
    scale_x_continuous(expand=c(0,0), 
                       limits = c(0,1),
                       labels = percent) +
    
    scale_y_continuous(expand=c(0,0),limits = c(0,1000),breaks = pretty_breaks(5)) 
}

################################################################################
## FUNCTION: AvgMn_price
## Plots monthly average pool price with average internal load
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
AvgMn_price <- function(case) {
  
  # Source function written by Dr. Leach
  source("Referenced_Code.R")
  
  # Filter the zone hourly table for specific case
  data <- ZoneHr %>%
    mutate(year = year(date),
           time = date) %>%
    filter(Run_ID == case,
           year >= 2022 & year <= 2035,
           Condition != "Average",
           Name == "WECC_Alberta") %>%
    subset(.,select=-c(Condition,Marginal_Resource,date,Run_ID,Name,Report_Year))
  
  # Call function to sort hourly data into groups
  peak_data_Sim<-data %>%
    #    filter(!is.na(actual_posted_pool_price),!is.na(actual_ail))%>%
    assign_date_time_days()%>%
    assign_peaks()%>%
    group_by(year,month) %>%
    summarize(ail=mean(Demand,na.rm = T),peak_ail=max(Demand),trough_ail=min(Demand),
              q75_price=quantile(Price, probs=c(.95)),
              q25_price=quantile(Price, probs=c(.05)),
              q75_ail=quantile(Demand, probs=c(.95)),
              q25_ail=quantile(Demand, probs=c(.05)),
              mean_peak_price=sum(Price*Demand*(on_peak==TRUE),
                                  na.rm = T)/sum(Demand*(on_peak==TRUE),
                                                 na.rm = T),
              mean_off_peak_price=sum(Price*Demand*(on_peak==FALSE),
                                      na.rm = T)/sum(Demand*(on_peak==FALSE),
                                                     na.rm = T),
              mean_peak_ail=sum(Demand*(on_peak==TRUE),
                                na.rm = T)/sum((on_peak==TRUE),
                                               na.rm = T),
              mean_off_peak_ail=sum(Demand*(on_peak==FALSE),
                                    na.rm = T)/sum((on_peak==FALSE),
                                                   na.rm = T),
              mean_price=sum(Price*Demand,
                             na.rm = T)/sum(Demand,na.rm = T),
              peak_price=max(Price),
              trough_price=min(Price)
    )%>%  
    mutate(date=ymd(paste(year,month,1,sep="-")),
           sit = paste0("Simulation ",SourceDB))
  
  # Plot the data
  top_panel<-ggplot(peak_data_Sim) +
    geom_line(aes(date,mean_price,linetype="A"),size=.85,color="black")+
    geom_line(aes(date,mean_off_peak_price,linetype="B"),size=.85,color="blue")+
    geom_ribbon(aes(date,ymax=q75_price,ymin=q25_price,fill=sit),alpha=.5)+
    geom_hline(yintercept=0,linetype=5) +
    scale_color_manual("",values = c("black","royalblue4"))+
    scale_fill_manual("",values = c("grey50","royalblue"),
                      labels="Two-tailed 90th percentile range")+
    scale_linetype_manual("",values = c("solid","11"),
                          labels=c("Peak period average","Off-peak period average"))+
    
    scale_x_date(expand=c(0,0),breaks="1 year",labels = date_format("%Y",tz="America/Denver"))+
    scale_y_continuous(expand=c(0,0),limits=c(-10,1050))+
    
    expand_limits(y=0)+ #make sure you get the zero line
    guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
    theme_bw() +
    theme(legend.position="bottom",
          legend.box.spacing = unit(0, "pt"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1,color="black"),
          axis.text.y = element_text(color="black"),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          text = element_text(size= 15),
          legend.text = element_text(colour="black", size = 12),
          plot.title = element_text(hjust = 0.5),
          #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")),
    )+
    labs(y="Pool Prices ($/MWh)",x="",
         title=paste("Alberta Hourly Wholesale Power Prices",sep=""),caption=SourceDB) + 
    
    guides(fill=guide_legend(nrow=2,byrow=TRUE))
  
  # Finally, show the plot
  top_panel
}

################################################################################
## FUNCTION: poolprice_2year
## A function to plot the Monthly average pool price 
## (Like in the AESO Market Report 2021 Figure 1)
##
## INPUTS: 
##    year1 - First year
##    Year2 - Seond year
##    case - case to see
## TABLES REQUIRED: 
##    ZoneHr_All - Hourly zone information for Alberta
################################################################################
poolprice_2year <- function(year1, year2,case) {

  
  # Filter and prepare Simulation data
  Sim <- ZoneHr_All %>%
    filter(
      Run_ID == case,
      Condition != "Average") %>%
    group_by(Report_Year,Report_Month) %>%
    summarise(Price = mean(Price)
    ) %>%
    mutate(Date = as.Date(paste(Report_Year,Report_Month,"01"), "%Y %m %d"),
           type = "MonAve"
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
                        type
    ))
  
  # Set font size and limits for plot
  sz <- 15
  Upplim <- round_any(max(Sim$Price)+11,50)
  
  ggplot() +
    geom_line(data = Sim,
              aes(x = Date, y = Price, colour = type), 
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
    labs(y = "Monthly Average Pool Price \n($/MWh)", 
         title = "Simulation Monthly Average Pool Price",
         caption = SourceDB) +
    scale_color_manual(values = c("black", "grey"),
                       labels = c("Monthly Ave","12-Month Rolling")) +
    scale_x_date(date_labels = "%b-%Y",
                 expand=c(0,0), 
                 date_breaks = "2 months"
    ) +
    scale_y_continuous(expand=c(0,0),limits=c(0,Upplim),n.breaks = 5, 
    )
}

################################################################################
## FUNCTION: AvgYr_poolprice
## Plots monthly average pool price with average internal load
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ZoneHr_Avg - Average hourly info in zone
################################################################################
AvgYr_poolprice <- function(case) {
  
# Filter and prepare Simulation data
Sim <- ZoneHr_All %>%
  filter(Run_ID == case) %>%
  group_by(Report_Year,Condition) %>%
  summarise(Price = mean(Price)) 

Sim$YEAR <- as.numeric(format(Sim$Report_Year))

# Get max and min year for plot
YearMX<-max(Sim$YEAR)-5
YearMN<-min(Sim$YEAR)

Upplim <- round_any(max(Sim$Price)+11,100)

ggplot() +
  geom_line(data = Sim,
            aes(x = YEAR, y = Price, colour = Condition,linetype= Condition), 
            size = 1.5) +
  theme_bw() +
  theme(text=element_text(family=Plot_Text)) +
  theme(axis.text = element_text(color="black"),
        axis.title = element_text(size = XTit_Sz ),
        axis.text.x = element_text(angle = 45, hjust=1,color="black"),
        plot.title = element_blank(),
        axis.title.x=element_blank(),
        legend.text = element_text(size = Leg_Sz),
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
  labs(y = "Annaul Average Pool Price ($/MWh)", x="Year",caption = SourceDB,colour="Condition",linetype="Condition") +
  
  scale_color_manual(values = c("Average"="black", "Off-Peak WECC"="gray40","On-Peak WECC"="gray80")) +
  
  scale_linetype_manual(values = c("Average"=1, "Off-Peak WECC"=4,"On-Peak WECC"=2))+

  scale_x_continuous(expand=c(0,0),limits = c(YearMN,YearMX),breaks=seq(YearMN, YearMX, 1)) +
  
  scale_y_continuous(expand=c(0,0),limits=c(0,Upplim),n.breaks = 5, 
  )
}

################################################################################
## FUNCTION: System_Cost
## Describes the average system costs incured by all resources in the system.
##
## INPUTS: 
##    case - Case to run
## TABLES REQUIRED: 
##    ZoneYr
################################################################################

System_Cost<- function(case) {
  
  # Take data from table and subset columns
  # DESCRIPTIONS
  # Demand - Average hourly demand
  # Demand Total - Total demand for year
  # Net_Load - Average net load served
  # Net_Load_Total - Total load served in year
  # Production_Cost_Total - In Can$000. Sum of Startup cost,Fuel cost,Emissions cost, Variable O&M
  #                         cost for all resources
  # Fixed_Cost_Total - Fixed costs for units (summed). 
  
  ZnData1 <- ZoneYr %>%
    mutate(year = year(Time_Period),
           time = Time_Period) %>%
    filter(Run_ID == case,
           Condition == "Average",
           Name == "WECC_Alberta") %>%
    mutate(Report_Year=as.numeric(Report_Year)) %>%
    # Get the costs in unit of $MM/MWh
    mutate(Scenario=SourceDB,
           Production_Cost_Total_MM=Production_Cost_Total/1000,
           Fixed_Cost_Total_MM=Fixed_Cost_Total/1000) %>%
    subset(.,select=c(Name,year,Price,Demand, Demand_Total,
                      Net_Load, Net_Load_Total,
                      Production_Cost_Total_MM,Fixed_Cost_Total_MM,
                      Production_Cost_Total,Fixed_Cost_Total,Scenario))
  
  # Now get output to append
  AnnualOut <-ResGroupYr%>%
    sim_filt5(.) %>% #Filter to rename fuels
    filter(Run_ID == case) %>%
    filter(Condition == "Average") %>%
    mutate(year=year(Time_Period)) %>%
    group_by(year)%>%
    summarise(Total_Output=sum(Output_MWH))
  
  # Combine the total output in MWh with zone info
  ZnData<-merge(ZnData1,AnnualOut,by=c("year"), all.x = TRUE)
  
  # Create unit costs
  ZnData <- ZnData %>%
      mutate(Production_Cost_Unit=1000*Production_Cost_Total/Total_Output,
      Fixed_Cost_Unit=1000*Fixed_Cost_Total/Total_Output)
  
  
  # Get max and min year for plot
  YearMX<-max(ZnData$year)-5
  YearMN<-min(ZnData$year)
  
  # Filter to remove the final 5 years (as per AURORA, want to run 5 years past year of interest)
  ZnData <- ZnData%>%
    filter(year<=YearMX)
  
  # Get upper and lower plot limit
  Upplim <- round_any(max(ZnData$Production_Cost_Unit,
                          ZnData$Fixed_Cost_Unit,
                          ZnData$Price)+11,10)
  Lowlim <- round_any(min(ZnData$Production_Cost_Unit,
                          ZnData$Fixed_Cost_Unit,
                          ZnData$Price,-10)-11,10)
  
  # Re-arrange the data
  PCost <-ZnData %>%
    mutate(Cost=Production_Cost_Unit,
           Type="Production Annual Cost")%>%
    subset(.,select=c(year,Cost,Type,Scenario))
  
  FCost <-ZnData %>%
    mutate(Cost=Fixed_Cost_Unit,
           Type="Fixed Annual Cost")%>%
    subset(.,select=c(year,Cost,Type,Scenario))
  
  TCost <-ZnData %>%
    mutate(Cost=Fixed_Cost_Unit+Production_Cost_Unit,
           Type="Total Annual Cost")%>%
    subset(.,select=c(year,Cost,Type,Scenario))
  
  Price <-ZnData %>%
    mutate(Cost=Price,
           Type="Average Annaul Pool Price")%>%
    subset(.,select=c(year,Cost,Type,Scenario))
  
  # Combine the data
  totalcosts<- rbind(PCost,FCost,TCost,Price)
  
  ggplot() +
    geom_line(data = totalcosts,
              aes(x = year, y = Cost, colour = Type),
              size = 1.5) +
    
    # Add line at y=0
    geom_hline(yintercept=0, color = "black",size=0.5,linetype=2)+
    
    theme_bw() +
    theme(text=element_text(family=Plot_Text)) +
    theme(text=element_text(size=GenText_Sz),
          axis.text = element_text(color="black"),
          axis.title.y = element_text(size = YTit_Sz),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust=1),
          plot.title = element_text(size = Tit_Sz),
          legend.text = element_text(size = Leg_Sz),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          #panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray90'),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.position = c(.87, .9),                            # Move legend to the bottom
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(y = "Amount ($/MWh)", x="Year",
         caption = paste("Production cost is a sum of startup costs, fuel costs, emission costs, and variable O&M costs for all plants opperating in a given year
                         Fixed cost is a sum of fixed costs for all plants opperating in a given year
                         Total cost is a sum of production and fixed costs
                         Production, fixed, and total costs are divided by total annual load served to get units of $/MWh\n"
                         #,SourceDB
                         )) +
    
    scale_color_manual(values = c("Production Annual Cost"="grey80",
                                  "Fixed Annual Cost"="gray50",
                                  "Total Annual Cost"="black",
                                  "Average Annaul Pool Price"="lightblue3")) +
    
    scale_x_continuous(expand=c(0,0),limits = c(YearMN,YearMX),breaks=seq(YearMN, YearMX, 1)) +
    
    scale_y_continuous(expand=c(0,0),labels = scales::comma,limits=c(Lowlim,Upplim),
                       breaks=breaks_pretty(10))
  
}
################################################################################
## FUNCTION: ResValue_Line
## Shows the annual value of new resources based on plant type for all years.
## Define the Resource type based on number 
## 1 wind
## 2- Solar
## 3 - Storage
## 4 - Unabated natural gas
## 5- Abated natural gas
## 6 - Hydrogen
## 7 - Hydro
## 8 - Other
## 9 - Cogen
## INPUTS: 
##    ResNum - The resource you want
##    case - The case (eg.BC)
## TABLES REQUIRED: 
##    ResYr - Annual resource info
################################################################################

ResValue_Line<-function(ResNum,BuildYr,case) {
  
  # Filter for resource type, use primary fuel
  if (ResNum==1) {
    FuelType<-c("Wind")
    FuelIndicator<-"Wind"
  } else if (ResNum==2) {
    FuelType<-c("Solar")
    FuelIndicator<-"Solar"
  } else if (ResNum==3) {
    FuelType<-c("Storage - Battery", "Storage - Compressed Air", "Storage - Pumped Hydro")
    FuelIndicator<-"Storage"
  } else if (ResNum==4) {
    FuelType<-c("WECC-Alberta NaturalGas-Peaking", "WECC-Alberta NaturalGas")
    FuelIndicator<-"Unabated Gas"
  } else if (ResNum==5) {
    FuelType<-c("Alberta Natural Gas with CCS")
    FuelIndicator<-"Abated Gas"
  } else if (ResNum==6) {
    FuelType<-c("Hydrogen")
    FuelIndicator<-"Hydrogen"
  } else if (ResNum==7) {
    FuelType<-c("Water")
    FuelIndicator<-"Hydro"
  } else if (ResNum==8) {
    FuelType<-c("Other, ZZ, WC, WH","Biomass")
    FuelIndicator<-"Other"
  } else if (ResNum==9) {
    FuelType<-c("WECC-AECO Hub NaturalGas-COGEN_oilsands_Alberta")
    FuelIndicator<-"Cogen"
  }
  
  # Filter the annual resource table for resource group and selected columns
  DataYr <- ResYr %>%
    filter(Run_ID == case,
           Condition == "Average",
           Zone == "WECC_Alberta",) %>%
    # Format
    mutate(Report_Year=as.numeric(YEAR), 
           Beg_Date=as.Date(Beg_Date,format = "%m/%d/%Y"),
           Beg_Year=year(Beg_Date)) %>%
    filter(Beg_Year>=BuildYr) %>%
    filter(Capacity>0) %>%
    # Get fuel type of interest
    filter(Primary_Fuel %in% FuelType) %>%
    subset(.,select=c(Name,Report_Year,Capability,Capacity,Dispatch_Cost,Output_MWH,Capacity_Factor,
                      Primary_Fuel,
                      Net_Cost,Total_Cost_MWh,Fixed_Cost,
                      Variable_OM_Cost,Total_Emission_Cost,Fuel_Cost,Startup_Cost,Build_Cost,
                      Revenue,Energy_Revenue_MWh,Value,Value_MWh,
                      Total_Hours_Run,Beg_Date,Beg_Year,End_Date)) %>%
    # Remove the first part of name to make it shorter
    mutate(NameAbb=word(Name,3),
           # Add capacity of resource to tag
           NameAbb=paste("NR#:",NameAbb," (",round(Capacity,digits=0),"MW)"))
  
  # Filter data further - chose 4 years between the start and end
  YearMin<-min(DataYr$Report_Year)
  YearMax<-max(DataYr$Report_Year)-5
  
  # Re-arrange the data to plot
  # VALUE is in Can$000 -> convert to $ M
  Data_Val <-DataYr %>%
    subset(.,select=c(Name,Report_Year,Beg_Year,
                      Value,Value_MWh)) %>%
    mutate(Value=Value/1000,
           Report_Year=as.numeric(Report_Year))
  
  Mean_Val <- Data_Val %>%
    group_by(Report_Year)%>%
    summarise(MeanV=mean(Value),
              MeanV_MWh=mean(Value_MWh))
  
  # Get limits on value
  MaxP<-plyr::round_any(max(Data_Val$Value), 10, f = ceiling)
  MinP<-plyr::round_any(min(Data_Val$Value), 10, f = floor)
  
  # Create a plot to show the value of plants 
  ggplot(Data_Val,aes(x=Report_Year,y=Value)) +

    geom_line(aes(x = Report_Year, y = Value, group= Name,alpha="Individual Plants"),color="gray",na.rm=TRUE) +
    
    geom_line(data=Mean_Val,aes(x = Report_Year, y = MeanV,alpha="Mean Nominal Value"),color="black",group=1,size=1.5,na.rm=TRUE) +
    
    # Set line at 0
    geom_hline(yintercept=0, color = "black",size=0.25,linetype=2) +
    
    # Custom legend
    scale_alpha_manual(name=NULL,
                       values=c(1,1),
                       breaks=c("Individual Plants","Mean Nominal Value"),
                       guide = guide_legend(override.aes = list(color = c("gray","black")))) +
    
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(
      # General Plot Settings
      panel.grid = element_blank(),                          # Remove pannel grid
      panel.spacing=unit(1,"pt"),                            # Control space between plots
      panel.background = element_rect(fill = "transparent"), # Transparent background
      text = element_text(size= GenText_Sz),                # Text size
      plot.title = element_text(size=Tit_Sz ),              # Plot title size (if present)
      plot.subtitle = element_text(hjust = 0.5),             # Plot subtitle size (if present)
      
      # X-axis
      axis.text.x = element_text(color="black"),           # Horizontal text
      axis.title.x = element_blank(),           # x-axis title text size
      
      # Y-axis
      axis.title.y = element_text(face="bold",XTit_Sz ),           # y-axis title text size
      axis.text.y = element_text(color="black"),
      # Legend
      legend.position = c(0.99, 0.99), 
      legend.text = element_text(size=Leg_Sz),
      legend.justification = c(0.99, 0.99),                      
      legend.title=element_text()) +
    
    # Y-Axis 
    scale_y_continuous(name="Net Annual Value ( nominal $M)",limits=c(MinP,MaxP),
                       labels = scales::dollar_format(prefix="$", suffix = "M"),breaks=pretty_breaks(8)) +
    
    # X-AXIS
    scale_x_continuous(expand = c(0.01,0.01),limits=c(YearMin,YearMax),breaks=seq(YearMin,YearMax,by=1)) +
    
    # Other Settings
    labs(caption = SourceDB,
         title=paste("Resource Type:",FuelIndicator),color="Year Built") 
}

################################################################################
## FUNCTION: ResValue_Line_MWh
## Shows the annual value of new resources based on plant type for all years.
## Define the Resource type based on number 
## 1 wind
## 2- Solar
## 3 - Storage
## 4 - Unabated natural gas
## 5- Abated natural gas
## 6 - Hydrogen
## 7 - Hydro
## 8 - Other
## 9 - Cogen
## INPUTS: 
##    ResNum - The resource you want
##    case - The case (eg.BC)
## TABLES REQUIRED: 
##    ResYr - Annual resource info
################################################################################

ResValue_Line_MWh<-function(ResNum,BuildYr,case) {
  
  # Filter for resource type, use primary fuel
  if (ResNum==1) {
    FuelType<-c("Wind")
    FuelIndicator<-"Wind"
  } else if (ResNum==2) {
    FuelType<-c("Solar")
    FuelIndicator<-"Solar"
  } else if (ResNum==3) {
    FuelType<-c("Storage - Battery", "Storage - Compressed Air", "Storage - Pumped Hydro")
    FuelIndicator<-"Storage"
  } else if (ResNum==4) {
    FuelType<-c("WECC-Alberta NaturalGas-Peaking", "WECC-Alberta NaturalGas")
    FuelIndicator<-"Unabated Gas"
  } else if (ResNum==5) {
    FuelType<-c("Alberta Natural Gas with CCS")
    FuelIndicator<-"Abated Gas"
  } else if (ResNum==6) {
    FuelType<-c("Hydrogen")
    FuelIndicator<-"Hydrogen"
  } else if (ResNum==7) {
    FuelType<-c("Water")
    FuelIndicator<-"Hydro"
  } else if (ResNum==8) {
    FuelType<-c("Other, ZZ, WC, WH","Biomass")
    FuelIndicator<-"Other"
  } else if (ResNum==9) {
    FuelType<-c("WECC-AECO Hub NaturalGas-COGEN_oilsands_Alberta")
    FuelIndicator<-"Cogen"
  }
  
  # Filter the annual resource table for resource group and selected columns
  DataYr <- ResYr %>%
    filter(Run_ID == case,
           Condition == "Average",
           Zone == "WECC_Alberta",) %>%
    # Format
    mutate(Report_Year=as.numeric(YEAR), 
           Beg_Date=as.Date(Beg_Date,format = "%m/%d/%Y"),
           Beg_Year=year(Beg_Date)) %>%
    filter(Beg_Year>=BuildYr) %>%
    filter(Capacity>0) %>%
    # Get fuel type of interest
    filter(Primary_Fuel %in% FuelType) %>%
    subset(.,select=c(Name,Report_Year,Capability,Capacity,Dispatch_Cost,Output_MWH,Capacity_Factor,
                      Primary_Fuel,
                      Net_Cost,Total_Cost_MWh,Fixed_Cost,
                      Variable_OM_Cost,Total_Emission_Cost,Fuel_Cost,Startup_Cost,Build_Cost,
                      Revenue,Energy_Revenue_MWh,Value,Value_MWh,
                      Total_Hours_Run,Beg_Date,Beg_Year,End_Date)) %>%
    # Remove the first part of name to make it shorter
    mutate(NameAbb=word(Name,3),
           # Add capacity of resource to tag
           NameAbb=paste("NR#:",NameAbb," (",round(Capacity,digits=0),"MW)"))
  
  # Filter data further - chose 4 years between the start and end
  YearMin<-min(DataYr$Report_Year)
  YearMax<-max(DataYr$Report_Year)-5
  
  # Re-arrange the data to plot
  # VALUE is in Can$000 -> convert to $ M
  Data_Val <-DataYr %>%
    subset(.,select=c(Name,Report_Year,Beg_Year,
                      Value,Value_MWh)) %>%
    mutate(Value=Value/1000,
           Report_Year=as.numeric(Report_Year))
  
  Mean_Val <- Data_Val %>%
    group_by(Report_Year)%>%
    summarise(MeanV=mean(Value),
              MeanV_MWh=mean(Value_MWh))
  
  # Get limits on value
  MaxP<-plyr::round_any(max(Data_Val$Value_MWh), 10, f = ceiling)
  MinP<-plyr::round_any(min(Data_Val$Value_MWh), 10, f = floor)
  
  # Create a plot to show the value of plants 
  ggplot(Data_Val,aes(x=Report_Year,y=Value_MWh)) +
    
    geom_line(aes(x = Report_Year, y = Value_MWh, group= Name,alpha="Individual Plants"),color="gray",na.rm=TRUE) +
    
    geom_line(data=Mean_Val,aes(x = Report_Year, y = MeanV_MWh,alpha="Mean Nominal Value"),color="black",group=1,size=1.5,na.rm=TRUE) +
    
    # Set line at 0
    geom_hline(yintercept=0, color = "black",size=0.25,linetype=2) +
    
    # Custom legend
    scale_alpha_manual(name=NULL,
                       values=c(1,1),
                       breaks=c("Individual Plants","Mean Nominal Value"),
                       guide = guide_legend(override.aes = list(color = c("gray","black")))) +
    
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(
      # General Plot Settings
      panel.grid = element_blank(),                          # Remove pannel grid
      panel.spacing=unit(1,"pt"),                            # Control space between plots
      panel.background = element_rect(fill = "transparent"), # Transparent background
      text = element_text(size= GenText_Sz),                # Text size
      plot.title = element_text(size=Tit_Sz ),              # Plot title size (if present)
      plot.subtitle = element_text(hjust = 0.5),             # Plot subtitle size (if present)
      
      # X-axis
      axis.text.x = element_text(color="black"),           # Horizontal text
      axis.title.x = element_blank(),           # x-axis title text size
      
      # Y-axis
      axis.title.y = element_text(face="bold",XTit_Sz ),           # y-axis title text size
      axis.text.y = element_text(color="black"),
      # Legend
      legend.position = c(0.99, 0.99), 
      legend.text = element_text(size=Leg_Sz),
      legend.justification = c(0.99, 0.99),                      
      legend.title=element_text()) +
    
    # Y-Axis 
    scale_y_continuous(name="Net Annual Value ( nominal $/MWh)",limits=c(MinP,MaxP),
                       labels = scales::dollar_format(prefix="$", suffix = "/MWh"),breaks=pretty_breaks(8)) +
    
    # X-AXIS
    scale_x_continuous(expand = c(0.01,0.01),limits=c(YearMin,YearMax),breaks=seq(YearMin,YearMax,by=1)) +
    
    # Other Settings
    labs(caption = SourceDB,
         title=paste("Resource Type:",FuelIndicator),color="Year Built") 
}

################################################################################
## FUNCTION: ResValue_Annual
## Shows the annual value of new resources based on plant type.
## Define the Resource type based on number 
## 1 wind
## 2- Solar
## 3 - Storage
## 4 - Unabated natural gas
## 5- Abated natural gas
## 6 - Hydrogen
## 7 - Hydro
## 8 - Other
## 9 - Cogen
## INPUTS: 
##    ResNum - The resource you want
##    case - The case (eg.BC)
## TABLES REQUIRED: 
##    ResYr - Annual resource info
################################################################################

ResValue_Annual<-function(ResNum,BuildYr,case) {
  
  # Filter for resource type, use primary fuel
  if (ResNum==1) {
    FuelType<-c("Wind")
    FuelIndicator<-"Wind"
  } else if (ResNum==2) {
    FuelType<-c("Solar")
    FuelIndicator<-"Solar"
  } else if (ResNum==3) {
    FuelType<-c("Storage - Battery", "Storage - Compressed Air", "Storage - Pumped Hydro")
    FuelIndicator<-"Storage"
  } else if (ResNum==4) {
    FuelType<-c("WECC-Alberta NaturalGas-Peaking", "WECC-Alberta NaturalGas")
    FuelIndicator<-"Unabated Gas"
  } else if (ResNum==5) {
    FuelType<-c("Alberta Natural Gas with CCS")
    FuelIndicator<-"Abated Gas"
  } else if (ResNum==6) {
    FuelType<-c("Hydrogen")
    FuelIndicator<-"Hydrogen"
  } else if (ResNum==7) {
    FuelType<-c("Water")
    FuelIndicator<-"Hydro"
  } else if (ResNum==8) {
    FuelType<-c("Other, ZZ, WC, WH","Biomass")
    FuelIndicator<-"Other"
  } else if (ResNum==9) {
    FuelType<-c("WECC-AECO Hub NaturalGas-COGEN_oilsands_Alberta")
    FuelIndicator<-"Cogen"
  }
  
  # Filter the annual resource table for resource group and selected columns
  DataYr <- ResYr %>%
    filter(Run_ID == case,
           Condition == "Average",
           Zone == "WECC_Alberta",) %>%
    # Format
    mutate(Report_Year=as.numeric(YEAR), 
           Beg_Date=as.Date(Beg_Date,format = "%m/%d/%Y"),
           Beg_Year=year(Beg_Date)) %>%
    filter(Beg_Year>=BuildYr) %>%
    filter(Capacity>0) %>%
    # Get fuel type of interest
    filter(Primary_Fuel %in% FuelType) %>%
    subset(.,select=c(Name,Report_Year,Capability,Capacity,Dispatch_Cost,Output_MWH,Capacity_Factor,
                      Primary_Fuel,
                      Net_Cost,Total_Cost_MWh,Fixed_Cost,
                      Variable_OM_Cost,Total_Emission_Cost,Fuel_Cost,Startup_Cost,Build_Cost,
                      Revenue,Energy_Revenue_MWh,Value,Value_MWh,
                      Total_Hours_Run,Beg_Date,Beg_Year,End_Date)) %>%
    # Remove the first part of name to make it shorter
    mutate(NameAbb=word(Name,3),
           # Add capacity of resource to tag
           NameAbb=paste("NR#:",NameAbb," (",round(Capacity,digits=0),"MW)"))
  
  # Filter data further - chose 4 years between the start and end
  YearMin<-min(DataYr$Report_Year)
  YearMax<-max(DataYr$Report_Year)-5
  
  # Re-arrange the data to plot
  # VALUE is in Can$000 -> convert to $ M
  Data_Val <-DataYr %>%
    filter(Report_Year %in% Years2Disp)%>%
    subset(.,select=c(Name,Report_Year,Beg_Year,
                      Value,Value_MWh)) %>%
    mutate(Value=Value/1000,
           Report_Year=as.factor(Report_Year))
  
  
  # Get limits on value
  MaxP<-plyr::round_any(max(Data_Val$Value)+100, 5, f = ceiling)
  
  # Create a plot to show the value of plants 
  ggplot(Data_Val, aes(x = Report_Year, y = Value,color=Beg_Year)) +
    
    # Set line at 0
    geom_hline(yintercept=0, color = "black",size=0.5,linetype=1) +
    
    ggbeeswarm::geom_quasirandom(size = 4, width = .33, alpha = .8) +
    ggbeeswarm::geom_quasirandom(size = 4, width = .33, shape = 1, color = "black", stroke = .8) +
    
    stat_summary(fun = median, geom = "point", shape = 95, size = 20) +
    
    theme_bw() +
    theme(text=element_text(family=Plot_Text)) +
    theme(
      # General Plot Settings
      panel.grid = element_blank(),                          # Remove pannel grid
      panel.spacing=unit(1,"pt"),                            # Control space between plots
      panel.background = element_rect(fill = "transparent"), # Transparent background
      text = element_text(size= GenText_Sz),                # Text size
      plot.title = element_text(size=Tit_Sz ),              # Plot title size (if present)
      plot.subtitle = element_text(hjust = 0.5),             # Plot subtitle size (if present)
      
      # X-axis
      axis.text.x = element_text(color="black"),           # Horizontal text
      axis.title.x = element_blank(),           # x-axis title text size
      axis.ticks.x= element_blank(),
      # Y-axis
      axis.title.y = element_text(XTit_Sz ),           # y-axis title text size
      axis.text.y = element_text(color="black"),
      # Legend
      legend.position = c(0.99, 0.99), 
      legend.text = element_text(size=Leg_Sz),
      legend.justification = c(0.99, 0.99),                      
      legend.title=element_text()) +
    
    # Y-Axis 
    scale_y_continuous(name="Plant Value (nominal $M)",labels = scales::dollar_format(prefix="$", suffix = "M"),breaks=pretty_breaks(6)) +
    
    # Other Settings
    labs(caption = SourceDB,
         title=paste("Resource Type:",FuelIndicator),color="Year Built") +
    
    scale_colour_gradient(low="black",high="skyblue1")
}

################################################################################
## FUNCTION: ResValue_Annual_MWh
## Shows the annual value of new resources based on plant type.
## Define the Resource type based on number 
## 1 wind
## 2- Solar
## 3 - Storage
## 4 - Unabated natural gas
## 5- Abated natural gas
## 6 - Hydrogen
## 7 - Hydro
## 8 - Other
## 9 - Cogen
## INPUTS: 
##    ResNum - The resource you want
##    case - The case (eg.BC)
## TABLES REQUIRED: 
##    ResYr - Annual resource info
################################################################################

ResValue_Annual_MWh<-function(ResNum,BuildYr,case) {
  
  # Filter for resource type, use primary fuel
  if (ResNum==1) {
    FuelType<-c("Wind")
    FuelIndicator<-"Wind"
  } else if (ResNum==2) {
    FuelType<-c("Solar")
    FuelIndicator<-"Solar"
  } else if (ResNum==3) {
    FuelType<-c("Storage - Battery", "Storage - Compressed Air", "Storage - Pumped Hydro")
    FuelIndicator<-"Storage"
  } else if (ResNum==4) {
    FuelType<-c("WECC-Alberta NaturalGas-Peaking", "WECC-Alberta NaturalGas")
    FuelIndicator<-"Unabated Gas"
  } else if (ResNum==5) {
    FuelType<-c("Alberta Natural Gas with CCS")
    FuelIndicator<-"Abated Gas"
  } else if (ResNum==6) {
    FuelType<-c("Hydrogen")
    FuelIndicator<-"Hydrogen"
  } else if (ResNum==7) {
    FuelType<-c("Water")
    FuelIndicator<-"Hydro"
  } else if (ResNum==8) {
    FuelType<-c("Other, ZZ, WC, WH","Biomass")
    FuelIndicator<-"Other"
  } else if (ResNum==9) {
    FuelType<-c("WECC-AECO Hub NaturalGas-COGEN_oilsands_Alberta")
    FuelIndicator<-"Cogen"
  }
  
  # Filter the annual resource table for resource group and selected columns
  DataYr <- ResYr %>%
    filter(Run_ID == case,
           Condition == "Average",
           Zone == "WECC_Alberta",) %>%
    # Format
    mutate(Report_Year=as.numeric(YEAR), 
           Beg_Date=as.Date(Beg_Date,format = "%m/%d/%Y"),
           Beg_Year=year(Beg_Date)) %>%
    filter(Beg_Year>=BuildYr) %>%
    filter(Capacity>0) %>%
    # Get fuel type of interest
    filter(Primary_Fuel %in% FuelType) %>%
    subset(.,select=c(Name,Report_Year,Capability,Capacity,Dispatch_Cost,Output_MWH,Capacity_Factor,
                      Primary_Fuel,
                      Net_Cost,Total_Cost_MWh,Fixed_Cost,
                      Variable_OM_Cost,Total_Emission_Cost,Fuel_Cost,Startup_Cost,Build_Cost,
                      Revenue,Energy_Revenue_MWh,Value,Value_MWh,
                      Total_Hours_Run,Beg_Date,Beg_Year,End_Date)) %>%
    # Remove the first part of name to make it shorter
    mutate(NameAbb=word(Name,3),
           # Add capacity of resource to tag
           NameAbb=paste("NR#:",NameAbb," (",round(Capacity,digits=0),"MW)"))
  
  # Filter data further - chose 4 years between the start and end
  YearMin<-min(DataYr$Report_Year)
  YearMax<-max(DataYr$Report_Year)-5
  
  # Re-arrange the data to plot
  # VALUE is in Can$000 -> convert to $ M
  Data_Val <-DataYr %>%
    filter(Report_Year %in% Years2Disp)%>%
    subset(.,select=c(Name,Report_Year,Beg_Year,
                      Value,Value_MWh)) %>%
    mutate(Value=Value/1000,
           Report_Year=as.factor(Report_Year))
  
  
  # Get limits on value
  MaxP<-plyr::round_any(max(Data_Val$Value_MWh)+3, 5, f = ceiling)
  
  # Create a plot to show the value of plants 
  ggplot(Data_Val, aes(x = Report_Year, y = Value_MWh,color=Beg_Year)) +
    
    # Set line at 0
    geom_hline(yintercept=0, color = "black",size=0.5,linetype=1) +
    
    ggbeeswarm::geom_quasirandom(size = 4, width = .33, alpha = .8) +
    ggbeeswarm::geom_quasirandom(size = 4, width = .33, shape = 1, color = "black", stroke = .8) +
    
    stat_summary(fun = median, geom = "point", shape = 95, size = 20) +
    
    theme_bw() +
    theme(text=element_text(family=Plot_Text)) +
    theme(
      # General Plot Settings
      panel.grid = element_blank(),                          # Remove pannel grid
      panel.spacing=unit(1,"pt"),                            # Control space between plots
      panel.background = element_rect(fill = "transparent"), # Transparent background
      text = element_text(size= GenText_Sz),                # Text size
      plot.title = element_text(size=Tit_Sz ),              # Plot title size (if present)
      plot.subtitle = element_text(hjust = 0.5),             # Plot subtitle size (if present)
      
      # X-axis
      axis.text.x = element_text(color="black"),           # Horizontal text
      axis.title.x = element_blank(),           # x-axis title text size
      axis.ticks.x= element_blank(),
      # Y-axis
      axis.title.y = element_text(face="bold",XTit_Sz ),           # y-axis title text size
      axis.text.y = element_text(color="black"),
      # Legend
      legend.position = c(0.99, 0.99), 
      legend.text = element_text(size=Leg_Sz),
      legend.justification = c(0.99, 0.99),                      
      legend.title=element_text()) +
    
    # Y-Axis 
    scale_y_continuous(name="Plant Value (nominal $/MWh)",labels = scales::dollar_format(prefix="$", suffix = "/MWh"),breaks=pretty_breaks(6)) +
    
    # Other Settings
    labs(caption = SourceDB,
         title=paste("Resource Type:",FuelIndicator),color="Year Built") +
    
    scale_colour_gradient(low="black",high="skyblue1")
}


################################################################################
## FUNCTION: ResValue_Total_MWh
## Shows the cumulative annaul value of new resources based on plant type.
## Define the Resource type based on number 
## 1 wind
## 2- Solar
## 3 - Storage
## 4 - Natural gas
## 5- Hydrogen and Natural gas blend
## 6 - Hydrogen
## 7 - All rest (other, hydro, cogen, cola-to-gas)
## INPUTS: 
##    ResNum - The resource you want
##    case - The case (eg.BC)
## TABLES REQUIRED: 
##    ResYr - Annual resource info
################################################################################

ResValue_NPV_MWh<-function(ResNum,case) {
    
    # Filter for resource type, use primary fuel
    if (ResNum==1) {
      FuelType<-c("Wind")
      FuelIndicator<-"Wind"
    } else if (ResNum==2) {
      FuelType<-c("Solar")
      FuelIndicator<-"Solar"
    } else if (ResNum==3) {
      FuelType<-c("Storage - Battery", "Storage - Compressed Air", "Storage - Pumped Hydro")
      FuelIndicator<-"Storage"
    } else if (ResNum==4) {
      FuelType<-c("WECC-Alberta NaturalGas-Peaking", "WECC-Alberta NaturalGas")
      FuelIndicator<-"Unabated Gas"
    } else if (ResNum==5) {
      FuelType<-c("Alberta Natural Gas with CCS")
      FuelIndicator<-"Abated Gas"
    } else if (ResNum==6) {
      FuelType<-c("Hydrogen")
      FuelIndicator<-"Hydrogen"
    } else if (ResNum==7) {
      FuelType<-c("Water")
      FuelIndicator<-"Hydro"
    } else if (ResNum==8) {
      FuelType<-c("Other, ZZ, WC, WH","Biomass")
      FuelIndicator<-"Other"
    } else if (ResNum==9) {
      FuelType<-c("WECC-AECO Hub NaturalGas-COGEN_oilsands_Alberta")
      FuelIndicator<-"Cogen"
    }
    
    # Filter the annual resource table for resource group and selected columns
    DataYr <- ResYr %>%
      filter(Run_ID == case,
             Condition == "Average",
             Zone == "WECC_Alberta",) %>%
      # Take out new resources only
      filter(grepl('New Resource',Name)) %>%
      mutate(Report_Year=as.numeric(YEAR),
             Beg_Date=as.Date(Beg_Date,format = "%m/%d/%Y"),
             Beg_Year=year(Beg_Date)) %>%
      filter(Capacity>1) %>%
      # Get fuel type of interest
      filter(Primary_Fuel %in% FuelType) %>%
      subset(.,select=c(Name,Report_Year,Capability,Capacity,Dispatch_Cost,Output_MWH,Capacity_Factor,
                        Primary_Fuel,
                        Net_Cost,Total_Cost_MWh,Fixed_Cost,
                        Variable_OM_Cost,Total_Emission_Cost,Fuel_Cost,Startup_Cost,Build_Cost,
                        Revenue,Energy_Revenue_MWh,Value,Value_MWh,
                        Total_Hours_Run,Beg_Date,Beg_Year,End_Date)) %>%
      # Remove the resource number only
      mutate(NameAbb=word(Name,3),
             # Add capacity of resource to tag
             NameAbb=paste("NR#:",NameAbb,", Built:",Beg_Year," (",round(Capacity,digits=0),"MW)"))
    
    # Reference Names
    Refnames <-DataYr %>%
      group_by(NameAbb,Name)%>%
      summarise(MaxCapacity=max(Capacity),
                AvgCF=mean(Capacity_Factor),
                Start=max(Beg_Date),
                End=max(End_Date))
    
    # Filter data further
    YearMin<-min(DataYr$Report_Year)
    YearMax<-2035
    
    Data_Val <-DataYr %>%
      filter(Report_Year<=YearMax,
             Report_Year>=YearMin)%>%   
      # Convert to millions
      mutate(Calc_Present=Value_MWh/(1.025)^(Report_Year-2023)) %>%
      group_by(NameAbb,Beg_Year) %>%
      summarise(Calc_NPV=sum(Calc_Present),
                TotSign=Calc_NPV>=0) %>%
      arrange(Beg_Year)
    
    # Get limits on value
    MaxP<-plyr::round_any(max(abs(Data_Val$Calc_NPV))+11, 5, f = ceiling)
    
    # Split the data to assign into two pannels
    SplitLen <-round(nrow(Data_Val)/2,digits=0)
    Data_Val1 <-Data_Val[1:SplitLen,] %>%
      mutate(pannelCheck=1)
    Data_Val2 <-Data_Val[(SplitLen+1):nrow(Data_Val),]%>%
      mutate(pannelCheck=2)
    
    DATA<- rbind(Data_Val1,Data_Val2)
    
    # Create a plot to show the value of plants 
    ggplot(DATA, aes(x = NameAbb, y = Calc_NPV, fill = TotSign)) +
      
      theme_bw() +
      
      geom_bar(stat = "identity",alpha=0.5) +
      
      # Add lines at 0
      geom_hline(yintercept=0, color = "black",size=0.5,linetype=1)+
      
      facet_wrap(~pannelCheck, scales = "free_y" ) +
      theme(
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
      
      theme(
        # General Plot Settings
        panel.grid = element_blank(),                          # Remove pannel grid
        panel.spacing=unit(5,"pt"),                            # Control space between plots
        panel.background = element_rect(fill = "transparent"), # Transparent background
        text = element_text(size = GenText_Sz),                # Text size
        plot.title = element_text(size = Tit_Sz),              # Plot title size (if present)
        plot.subtitle = element_text(hjust = 0.5),             # Plot subtitle size (if present)
        panel.grid.major.y = element_line(size=0.25,
                                          linetype=1,color = 'gray90'),                          # Adds horizontal lines
        # X-axis
        axis.text.x = element_text(face="bold",
                                   size=8, angle=45,vjust=0.5),# Horizontal text
        axis.title.x = element_text(size = XTit_Sz),           # x-axis title text size
        # Y-axis
        axis.title.y = element_text(size = YTit_Sz),           # y-axis title text size
        axis.text.y = element_text(face="bold",
                                   size=8, angle=0),
        # Legend
        legend.position ="none") +                               # Remove legend
      
      # Y-Axis 
      scale_y_continuous(name="Net Present Value (2023$/MWh)",labels = label_number(accuracy = 0.01),
                         expand=c(0,0), limits=c(-MaxP,MaxP),breaks = pretty_breaks(12)) +
      coord_flip() +
      # X-axis (flipped)  
      scale_x_discrete(expand=c(0,0),name= "Plant Inforomation",
                       #labels = scales::label_wrap(20)
      ) +
      
      # Other Settings
      labs(caption = SourceDB,
           title=paste("Resource Type(s):",toString(FuelType))) +
      scale_fill_manual(values = c("TRUE"="darkblue","FALSE"="darkgreen")) 
    
    
  }
################################################################################
## FUNCTION: ResValue_NPV
## Shows the net present value in 2023
## 1 wind
## 2- Solar
## 3 - Storage
## 4 - Natural gas
## 5- Hydrogen and Natural gas blend
## 6 - Hydrogen
## 7 - All rest (other, hydro, cogen, cola-to-gas)
## INPUTS: 
##    ResNum - The resource you want
##    case - The case (eg.BC)
## TABLES REQUIRED: 
##    ResYr - Annual resource info
################################################################################

ResValue_NPV<-function(ResNum,case) {
  
  # Filter for resource type, use primary fuel
  if (ResNum==1) {
    FuelType<-c("Wind")
    FuelIndicator<-"Wind"
  } else if (ResNum==2) {
    FuelType<-c("Solar")
    FuelIndicator<-"Solar"
  } else if (ResNum==3) {
    FuelType<-c("Storage - Battery", "Storage - Compressed Air", "Storage - Pumped Hydro")
    FuelIndicator<-"Storage"
  } else if (ResNum==4) {
    FuelType<-c("WECC-Alberta NaturalGas-Peaking", "WECC-Alberta NaturalGas")
    FuelIndicator<-"Unabated Gas"
  } else if (ResNum==5) {
    FuelType<-c("Alberta Natural Gas with CCS")
    FuelIndicator<-"Abated Gas"
  } else if (ResNum==6) {
    FuelType<-c("Hydrogen")
    FuelIndicator<-"Hydrogen"
  } else if (ResNum==7) {
    FuelType<-c("Water")
    FuelIndicator<-"Hydro"
  } else if (ResNum==8) {
    FuelType<-c("Other, ZZ, WC, WH","Biomass")
    FuelIndicator<-"Other"
  } else if (ResNum==9) {
    FuelType<-c("WECC-AECO Hub NaturalGas-COGEN_oilsands_Alberta")
    FuelIndicator<-"Cogen"
  }
  
  # Filter the annual resource table for resource group and selected columns
  DataYr <- ResYr %>%
    filter(Run_ID == case,
           Condition == "Average",
           Zone == "WECC_Alberta",) %>%
    # Take out new resources only
    filter(grepl('New Resource',Name)) %>%
    mutate(Report_Year=as.numeric(YEAR),
           Beg_Date=as.Date(Beg_Date,format = "%m/%d/%Y"),
           Beg_Year=year(Beg_Date)) %>%
    filter(Capacity>1) %>%
    # Get fuel type of interest
    filter(Primary_Fuel %in% FuelType) %>%
    subset(.,select=c(Name,Report_Year,Capability,Capacity,Dispatch_Cost,Output_MWH,Capacity_Factor,
                      Primary_Fuel,
                      Net_Cost,Total_Cost_MWh,Fixed_Cost,
                      Variable_OM_Cost,Total_Emission_Cost,Fuel_Cost,Startup_Cost,Build_Cost,
                      Revenue,Energy_Revenue_MWh,Value,Value_MWh,
                      Total_Hours_Run,Beg_Date,Beg_Year,End_Date)) %>%
    # Remove the resource number only
    mutate(NameAbb=word(Name,3),
           # Add capacity of resource to tag
           NameAbb=paste("NR#:",NameAbb,", Built:",Beg_Year," (",round(Capacity,digits=0),"MW)"))
  
  # Reference Names
  Refnames <-DataYr %>%
    group_by(NameAbb,Name)%>%
    summarise(MaxCapacity=max(Capacity),
              AvgCF=mean(Capacity_Factor),
              Start=max(Beg_Date),
              End=max(End_Date))
  
  # Filter data further
  YearMin<-min(DataYr$Report_Year)
  YearMax<-2035
  
  Data_Val <-DataYr %>%
    filter(Report_Year<=YearMax,
           Report_Year>=YearMin)%>%   
    # Convert to millions
    mutate(Value=Value/1000,
           Calc_Present=Value/(1.025)^(Report_Year-2023)) %>%
    group_by(NameAbb,Beg_Year) %>%
    summarise(Calc_NPV=sum(Calc_Present),
              TotSign=Calc_NPV>=0) %>%
    arrange(Beg_Year)
  
  # Get limits on value
  MaxP<-plyr::round_any(max(abs(Data_Val$Calc_NPV))+11, 5, f = ceiling)
  
  # Split the data to assign into two pannels
  SplitLen <-round(nrow(Data_Val)/2,digits=0)
  Data_Val1 <-Data_Val[1:SplitLen,] %>%
    mutate(pannelCheck=1)
  Data_Val2 <-Data_Val[(SplitLen+1):nrow(Data_Val),]%>%
    mutate(pannelCheck=2)
  
  DATA<- rbind(Data_Val1,Data_Val2)
  
  # Create a plot to show the value of plants 
  ggplot(DATA, aes(x = NameAbb, y = Calc_NPV, fill = TotSign)) +
    
    theme_bw() +
    
    geom_bar(stat = "identity",alpha=0.5) +
    
    # Add lines at 0
    geom_hline(yintercept=0, color = "black",size=0.5,linetype=1)+
    
    facet_wrap(~pannelCheck, scales = "free_y" ) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank()) +
      
    theme(
      # General Plot Settings
      panel.grid = element_blank(),                          # Remove pannel grid
      panel.spacing=unit(5,"pt"),                            # Control space between plots
      panel.background = element_rect(fill = "transparent"), # Transparent background
      text = element_text(size = GenText_Sz),                # Text size
      plot.title = element_text(size = Tit_Sz),              # Plot title size (if present)
      plot.subtitle = element_text(hjust = 0.5),             # Plot subtitle size (if present)
      panel.grid.major.y = element_line(size=0.25,
                                        linetype=1,color = 'gray90'),                          # Adds horizontal lines
      # X-axis
      axis.text.x = element_text(face="bold",
                                 size=8, angle=45,vjust=0.5),# Horizontal text
      axis.title.x = element_text(size = XTit_Sz),           # x-axis title text size
      # Y-axis
      axis.title.y = element_text(size = YTit_Sz),           # y-axis title text size
      axis.text.y = element_text(face="bold",
                                 size=8, angle=0),
      # Legend
      legend.position ="none") +                               # Remove legend
    
    # Y-Axis 
    scale_y_continuous(name="Net Present Value (2023$MM)",labels = label_number(accuracy = 0.01),
                       expand=c(0,0), limits=c(-MaxP,MaxP),breaks = pretty_breaks(12)) +
    coord_flip() +
    # X-axis (flipped)  
    scale_x_discrete(expand=c(0,0),name= "Plant Inforomation",
                     #labels = scales::label_wrap(20)
                     ) +
    
    # Other Settings
    labs(caption = SourceDB,
         title=paste("Resource Type(s):",toString(FuelType))) +
    scale_fill_manual(values = c("TRUE"="darkblue","FALSE"="darkgreen")) 
  

}

################################################################################
## FUNCTION: capture_p
## Average capture price achieved for each technology and mean zone price.
##
## INPUTS: year1 - Select dates greater than or equal to year1.
##         year2 - Select dates less than or equal to year2.
##         Case - Case to view
##    
## TABLES REQUIRED: ZoneHr_Avg, ResGroupHr
##    
################################################################################
# Capture Prices

capture_p <- function(year1, year2, case) {
  
  if (year2-year1<6){
    # Get zone price data
    Zone_Data <- ZoneHr_Avg  %>%
      filter(year(date) >= year1,
             year(date) <= year2,
             Run_ID == case,
      ) %>%
      subset(., select = c(date, Price, Imports, Exports))
    
    # Get resource group data
    Res_Data <- ResGroupHr %>%
      filter(year(date) >= year1,
             year(date) <= year2,
             Output_MWH >= 0,
             Run_ID == case) %>%
      sim_filt(.) %>%
      mutate(Year = as.factor(Report_Year)) %>%
      subset(., select = c(date, ID, Output_MWH, Energy_Revenue, Year)) 
  }else{
    # Get zone price data
    Zone_Data <- ZoneHr_Avg  %>%
      filter(year(date) %in% Years2Disp,
             Run_ID == case,
      ) %>%
      subset(., select = c(date, Price, Imports, Exports))
    
    # Get resource group data
    Res_Data <- ResGroupHr %>%
      filter(year(date) %in% Years2Disp,
             Output_MWH >= 0,
             Run_ID == case) %>%
      sim_filt(.) %>%
      mutate(Year = as.factor(Report_Year)) %>%
      subset(., select = c(date, ID, Output_MWH, Energy_Revenue, Year)) 
  }
  # Combine data
  AllData <- merge(Zone_Data, Res_Data, by = "date") %>%
    subset(., select = -c(Imports,Exports))
  
  # This section calculates the achieved prices for imports and exports to add to rest
  Imp <- Zone_Data %>%
    mutate(Energy_Revenue = Price*Imports/1000, 
           Year = as.factor(year(date)), 
           ID = "Imports",
           Output_MWH = Imports) %>%
    subset(., select = -c(Imports,Exports))
  
  Exp <- Zone_Data %>%
    mutate(Energy_Revenue = Price*Exports/1000, 
           Year = as.factor(year(date)), 
           ID = "Exports",
           Output_MWH = Exports) %>%
    subset(., select = -c(Imports,Exports))
  
  # Combine trade data with rest
  Sim <- rbind(AllData,Imp,Exp) %>%
    group_by(ID,Year,date) %>%
    summarise(total_rev = sum(Energy_Revenue*1000), 
              total_gen = sum(Output_MWH),
              price_mean=mean(Price),
              capture=total_rev/total_gen) %>%
    ungroup() %>%
    mutate(Plant_Type = ID) %>%
    group_by(Plant_Type,Year) %>%
    summarise(Yr_capture = sum(total_rev)/sum(total_gen),
              p_mean=mean(price_mean, na.rm = TRUE)) %>%
    mutate(sit = "Simulation",
           YrName=paste(Year,"Average Pool Price"))%>%
    arrange(Yr_capture)
  
  #Plot limits
  PMax<-round(max(Sim$Yr_capture)+10,0)

  # Plot the data
  ggplot(Sim)+
    
    geom_bar(aes(x=fct_rev(fct_reorder(Plant_Type,Yr_capture)),Yr_capture,fill=Year),
             stat="identity",size=0.5,position = position_dodge(),width = .8,color="black")+
    
    geom_point(aes(x=Plant_Type,y=p_mean,shape=YrName), size=1,fill="black",color="black")+
    
    scale_fill_brewer(palette="Blues")+

    #   facet_grid(~Year) +
    
    scale_y_continuous(expand=c(0,0),limits = c(0,PMax),breaks = pretty_breaks(6)) +
    
    labs(x="",y="Avereage Revenue Relative to Mean Price ($/MWh)",
         title=paste0("Energy Price Capture Differential ($/MWh, ",year1,"-",year2,")"),
         caption=SourceDB) +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid.major.y = element_line(color = "gray",linetype=2,size=0.25),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          axis.text = element_text(size = GenText_Sz,color="black"),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title = element_text(size = XTit_Sz,face="bold"),
          plot.subtitle = element_text(size = GenText_Sz-2,hjust=0.5),
          plot.caption = element_text(face="italic",size = GenText_Sz-4,hjust=0),
          plot.title = element_blank(),
          #plot.title = element_text(hjust=0.5,size = GenText_Sz+2),
          
          # For transparent background
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          panel.border = element_rect(colour = "black", fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          
          # Legend stuff
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.key.size = unit(1,"lines"),
          legend.background = element_rect(fill='transparent'),
          legend.position="bottom",
          legend.text = element_text(size=Leg_Sz),
          legend.title = element_blank(),
          legend.spacing.y = unit(-0.4,"lines"),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) 
}

################################################################################
## FUNCTION: Relcapture_p
## Plot difference between average capture price achieved for each technology and mean zone price.
## Based on plot by Dr. Andrew Leach
##
## INPUTS: year1 - Select dates greater than or equal to year1.
##         year2 - Select dates less than or equal to year2.
##         Case - Case to view
##    
## TABLES REQUIRED: ZoneHr_Avg, ResGroupHr
##    
################################################################################
# Capture Prices

Relcapture_p <- function(year1, year2, case) {
  
  if (year2-year1<6){
      # Get zone price data
    Zone_Data <- ZoneHr_Avg  %>%
      filter(year(date) >= year1,
             year(date) <= year2,
             Run_ID == case,
      ) %>%
      subset(., select = c(date, Price, Imports, Exports))
    
    # Get resource group data
    Res_Data <- ResGroupHr %>%
      filter(year(date) >= year1,
             year(date) <= year2,
             Output_MWH >= 0,
             Run_ID == case) %>%
      sim_filt(.) %>%
      mutate(Year = as.factor(Report_Year)) %>%
      subset(., select = c(date, ID, Output_MWH, Energy_Revenue, Year)) 
  }else{
      # Get zone price data
      Zone_Data <- ZoneHr_Avg  %>%
        filter(year(date) %in% Years2Disp,
               Run_ID == case,
        ) %>%
        subset(., select = c(date, Price, Imports, Exports))
      
      # Get resource group data
      Res_Data <- ResGroupHr %>%
        filter(year(date) %in% Years2Disp,
               Output_MWH >= 0,
               Run_ID == case) %>%
        sim_filt(.) %>%
        mutate(Year = as.factor(Report_Year)) %>%
        subset(., select = c(date, ID, Output_MWH, Energy_Revenue, Year)) 
  }
  # Combine data
  AllData <- merge(Zone_Data, Res_Data, by = "date") %>%
    subset(., select = -c(Imports,Exports))
  
  # This section calculates the achieved prices for imports and exports to add to rest
  Imp <- Zone_Data %>%
    mutate(Energy_Revenue = Price*Imports/1000, 
           Year = as.factor(year(date)), 
           ID = "Imports",
           Output_MWH = Imports) %>%
    subset(., select = -c(Imports,Exports))
  
  Exp <- Zone_Data %>%
    mutate(Energy_Revenue = Price*Exports/1000, 
           Year = as.factor(year(date)), 
           ID = "Exports",
           Output_MWH = Exports) %>%
    subset(., select = -c(Imports,Exports))
  
  # Combine trade data with rest
  Sim <- rbind(AllData,Imp,Exp) %>%
    group_by(ID,Year,date) %>%
    summarise(total_rev = sum(Energy_Revenue*1000), 
              total_gen = sum(Output_MWH),
              price_mean=mean(Price),
              capture=total_rev/total_gen) %>%
    ungroup() %>%
    mutate(Plant_Type = ID) %>%
    group_by(Plant_Type,Year) %>%
    summarise(Yr_capture = sum(total_rev)/sum(total_gen),
              p_mean=mean(price_mean, na.rm = TRUE)) %>%
    mutate(sit = "Simulation")%>%
    arrange(Yr_capture)
  
  #Plot limits
  PMax<-round(max(Sim$Yr_capture-Sim$p_mean)+10,0)
  PMin<-round(min(Sim$Yr_capture-Sim$p_mean)-10,0)
  
  # Plot the data
  ggplot(Sim)+
    
    geom_hline(yintercept=0, linetype="solid", color="black",size=0.5)+
    
    geom_bar(aes(x=fct_rev(fct_reorder(Plant_Type,Yr_capture)),Yr_capture-p_mean,fill=Year),
             stat="identity",size=0.5,position = position_dodge(),width = .8,color="black")+
    
    scale_fill_brewer(palette="Blues")+
    
    #   facet_grid(~Year) +
    
    scale_y_continuous(expand=c(0,0),limits = c(PMin,PMax),breaks = pretty_breaks(8)) +
    
    labs(x="",y="Avereage Revenue Relative to Mean Price ($/MWh)",
         title=paste0("Energy Price Capture Differential ($/MWh, ",year1,"-",year2,")"),
         caption=SourceDB) +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid.major.y = element_line(color = "gray",linetype=2,size=0.25),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          axis.text = element_text(size = GenText_Sz,color="black"),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title = element_text(size = XTit_Sz,face="bold"),
          plot.subtitle = element_text(size = GenText_Sz-2,hjust=0.5),
          plot.caption = element_text(face="italic",size = GenText_Sz-4,hjust=0),
          plot.title = element_blank(),
          #plot.title = element_text(hjust=0.5,size = GenText_Sz+2),
          
          # For transparent background
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          panel.border = element_rect(colour = "black", fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          
          # Legend stuff
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.key.size = unit(1,"lines"),
          legend.background = element_rect(fill='transparent'),
          legend.justification = "right",
          legend.position=c(0.99, 0.94),
          legend.text = element_text(size=Leg_Sz),
          legend.title = element_blank(),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) 
}

################################################################################
## FUNCTION: ach_poolprem
## Achieved premium to pool price in each year by generation tech. 
## Based on wholesale energy revenue, calculated as weighted average of the hourly pool price. 
## Houly pool price is weighted in each interval by net-to-grid generation.
## Plot midifed from Taylor P.
##
## INPUTS: 
##    
## TABLES REQUIRED: 
##    
################################################################################

ach_poolprem <- function(year1, year2, case) {
  
  # Gather zone and resource data for given years
  Zone_Data <- ZoneHr_Avg  %>%
    filter(year(date) >= year1,
           year(date) <= year2,
           Run_ID == case,
    ) %>%
    subset(., select = c(date, Price, Imports, Exports))
  
  Res_Data <- ResGroupHr %>%
    filter(year(date) >= year1,
           year(date) <= year2,
           Output_MWH >= 0,
           Run_ID == case) %>%
    sim_filt(.) %>%
    subset(., select = c(date, ID, Output))
  
  # Combine into one dataframe
  Res_Data2 <- merge(Zone_Data, Res_Data, by = "date")
  Res_Data2 <- Res_Data2 %>%
    mutate(Year = as.factor(year(date)))
  
  # Calculate the annual average pool prices for simulated data
  AvgPool <- Res_Data2 %>%
    group_by(Year) %>%
    summarise(Pool = mean(Price))
  
  # This section calculates the achieved prices for imports and exports
  Imp <- Zone_Data %>%
    mutate(WeighPrice = Price*Imports, Year = as.factor(year(date))) %>%
    group_by(Year) %>%
    summarise(WeighPrice = sum(WeighPrice), gen = sum(Imports)) %>%
    mutate(AchPrice = WeighPrice/gen, Plant_Type = "IMPORT")
  
  Exp <- Zone_Data %>%
    mutate(WeighPrice = Price*Exports, Year = as.factor(year(date))) %>%
    group_by(Year) %>%
    summarise(WeighPrice = sum(WeighPrice), gen = sum(Exports)) %>%
    mutate(AchPrice = WeighPrice/gen, Plant_Type = "EXPORT")
  
  # This section calculates the achieved prices
  AchSim <- Res_Data2 %>%
    mutate(WeighPrice = Price*Output, Plant_Type = ID) %>%
    group_by(Year, Plant_Type) %>%
    summarise(WeighPrice = sum(WeighPrice), gen = sum(Output)) %>%
    mutate(AchPrice = WeighPrice/gen)
  
  AchSim <- rbind(AchSim, Imp, Exp)
  
  # Combine the two
  Sim <- merge(AvgPool, AchSim, by = "Year")
  
  # Calculate the achieved margin and the achieved premium-to-pool price
  Sim <- Sim %>%
    mutate(Margin = AchPrice-Pool, Ratio = Margin/Pool, sit = "Simulation")
  
  MaxP<-round(max(Sim$Ratio)+0.1,1)
  MinP<-round(min(Sim$Ratio,na.rm=TRUE)-0.1,1)
  
  Sim$Plant_Type <- factor(Sim$Plant_Type, levels=c("Coal-to-Gas", "Natural Gas","Natural Gas + CCS","Natual Gas and Hydrogen Blend","Hydrogen" , 
                                                    "Hydro","Other","Wind", "Solar", "Storage","Coal","Cogen", "EXPORT", "IMPORT"))
  
  levels(Sim$Plant_Type) <- c("Coal-to-Gas", "Natural Gas","Natural Gas + CCS","Natual Gas and Hydrogen Blend","Hydrogen" , 
                              "Hydro","Other","Wind", "Solar", "Storage","Coal","Cogen", "Export", "Import")
  
  
  # Plot the data
  ggplot() +
    
    geom_hline(yintercept=0, linetype="solid", color="black",size=0.5)+
    
    geom_bar(data = Sim,aes(x=fct_rev(fct_reorder(Plant_Type,Ratio)),Ratio,fill=Year),
             stat="identity",size=0.5,position = position_dodge(),width = .8,color="black")+
    
    scale_fill_brewer(palette="Blues")+
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme_bw() +
    theme(text=element_text(family=Plot_Text)) +
    
    theme(axis.line = element_line(color = "black"),
          axis.text = element_text(size = GenText_Sz,color="black"),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title.x=element_blank(),
          axis.title = element_text(size = XTit_Sz,face="bold"),
          plot.caption = element_text(face="italic",size = GenText_Sz-4,hjust=0),
          plot.title = element_blank(),
          
          # For transparent background
          panel.grid.major.y = element_line(color = "gray",linetype=2,size=0.25),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.x = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "black", fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          
          # Legend stuff
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.key.size = unit(1,"lines"),
          legend.background = element_rect(fill='transparent'),
          legend.position="bottom",
          legend.text = element_text(size=Leg_Sz),
          legend.title = element_blank(),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(y = "Achieved Premium to Pool Price", 
         title = "Annual achieved premium to pool price",
         caption  = SourceDB,) +
    
    scale_y_continuous(expand=c(0,0),
                       limits = c(MinP,MaxP),
                       breaks = seq(MinP,MaxP,by=0.2),
                       labels = percent
    )
}