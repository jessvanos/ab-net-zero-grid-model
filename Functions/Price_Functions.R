################################################################################
# TITLE: Price_Functions
# DESCRIPTION: Functions related to technology capture prices, pool prices, and other cost related material. 
#
# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: January 6, 2023; LAST EDIT: January 6, 2023
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
    
    labs(y = "Pool Price ($/MWh)", x = "Percentage of Time for Hourly Pool Price",caption = SourceDB) +
    
    #scale_color_brewer(palette= "Dark2") +
    
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
  source("DrLeach_Code.R")
  
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
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
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

# Set font size and limits for plot
sz <- 15
Upplim <- round_any(max(Sim$Price)+11,100)

ggplot() +
  geom_line(data = Sim,
            aes(x = YEAR, y = Price, colour = Condition,linetype= Condition), 
            size = 2) +
  theme_bw() +
  theme(axis.text = element_text(size = sz),
        axis.title = element_text(size = sz+5),
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
  labs(y = "Annaul Average Pool Price ($/MWh)", x="Year",caption = SourceDB,colour="Condition",linetype="Condition") +
  
  scale_color_manual(values = c("Average"="black", "Off-Peak WECC"="darkblue","On-Peak WECC"="darkgreen")) +
  
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
  
  Upplim <- round_any(max(ZnData$Production_Cost_Unit,
                          ZnData$Fixed_Cost_Unit,
                          ZnData$Price)+11,10)
  Lowlim <- round_any(min(ZnData$Production_Cost_Unit,
                          ZnData$Fixed_Cost_Unit,
                          ZnData$Price,-10)-11,10)
  
  # Filter to remove the final 5 years (as per AURORA, want to run 5 years past year of interest)
  ZnData <- ZnData%>%
    filter(year<=YearMX)
  
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
    theme(text=element_text(size=GenText_Sz),
          axis.text = element_text(),
          axis.title.y = element_text(size = YTit_Sz),
          axis.text.x = element_text(angle = 45, hjust=1, size = XTit_Sz),
          plot.title = element_text(size = Tit_Sz),
          legend.text = element_text(size = Leg_Sz),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = "right",
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray90'),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
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
    
    scale_color_manual(values = c("Production Annual Cost"="grey70",
                                  "Fixed Annual Cost"="black",
                                  "Total Annual Cost"="red",
                                  "Average Annaul Pool Price"="lightblue")) +
    
    scale_x_continuous(expand=c(0,0),limits = c(YearMN,YearMX),breaks=seq(YearMN, YearMX, 1)) +
    
    scale_y_continuous(expand=c(0,0),labels = scales::comma,limits=c(Lowlim,Upplim),
                       breaks=breaks_pretty(10))
  
}

################################################################################
## FUNCTION: ResValue_Annual
## Shows the annual value of new resoruces based on plant type.
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

ResValue_Annual<-function(ResNum,case) {
  
  # Filter for resource type
  if (ResNum==1) {
    FuelType<-c("Wind")
  } else if (ResNum==2) {
    FuelType<-c("Solar")
  } else if (ResNum==3) {
    FuelType<-c("Storage - Battery", "Storage - Compressed Air", "Storage - Pumped Hydro")
  } else if (ResNum==4) {
    FuelType<-c("Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle")
  } else if (ResNum==5) {
    FuelType<-c("Blended  Simple Cycle","Blended  Combined Cycle")
  } else if (ResNum==6) {
    FuelType<-c("Hydrogen Simple Cycle","Hydrogen Combined Cycle")
  } else if (ResNum==7) {
    FuelType<-c("Coal-to-Gas","Hydro","Other","Cogeneration")
  }

# Filter the annual resource table for resource group and selected columns
DataYr <- ResYr %>%
  filter(Run_ID == case,
         Condition == "Average",
         Zone == "WECC_Alberta",) %>%
  # Take out new resources only
  filter(grepl('New Resource',Name)) %>%
  mutate(Report_Year=as.numeric(YEAR)) %>%
  filter(Capacity>0) %>%
  sim_filt3(.) %>%
  # Get fuel type of interest
  filter(Primary_Fuel %in% FuelType) %>%
  subset(.,select=c(Name,Report_Year,Capability,Capacity,Dispatch_Cost,Output_MWH,Capacity_Factor,
                    Primary_Fuel,
                    Net_Cost,Total_Cost_MWh,Fixed_Cost,
                    Variable_OM_Cost,Total_Emission_Cost,Fuel_Cost,Startup_Cost,Build_Cost,
                    Revenue,Energy_Revenue_MWh,Value,Value_MWh,
                    Total_Hours_Run,Beg_Date,End_Date)) %>%
  # Remove the first part of name to make it shorter
  mutate(Name=str_remove(Name,"New Resource"))

# Filter data further
YearMin<-min(DataYr$Report_Year)
YearMax<-2035

DataYr <-DataYr %>%
  filter(Report_Year<=YearMax,
         Report_Year>=YearMin)
#filter(Report_Year %in% Years2Disp)

# Re-arrange the data to plot
# VALUE is in Can$000
Data_Val <-DataYr %>%
  subset(.,select=c(Name,Report_Year,
                    Value,Value_MWh)) %>%
  mutate(Type="Value",
         TotSign=Value_MWh>=0)%>%          # TotSign tells if pos or neg for plot
  rename(Total=Value,
         Total_Per_MWh=Value_MWh)

# Get limits on value
MaxP<-plyr::round_any(max(Data_Val$Total_Per_MWh)+10, 5, f = ceiling)

# Create a plot to show the value of plants 
ggplot(Data_Val, aes(x = Name, y = Total_Per_MWh, fill = TotSign)) +
  facet_grid(cols = vars(Report_Year)) +                     # Add new plot for each year
  
  theme_bw() +
  
  theme(text=element_text(family=Plot_Text)) +
  
  geom_bar(stat = "identity",alpha=0.5) +
  
  geom_hline(yintercept=0, color = "black",size=0.5,linetype=1)+
  
  theme(
    # General Plot Settings
    panel.grid = element_blank(),                          # Remove pannel grid
    panel.spacing=unit(1,"lines"),                         # Control space between plots
    panel.background = element_rect(fill = "transparent"), # Transparent background
    text = element_text(size = GenText_Sz),                # Text size
    plot.title = element_text(size = Tit_Sz),              # Plot title size (if present)
    plot.subtitle = element_text(hjust = 0.5),             # Plot subtitle size (if present)
    panel.grid.major.y = element_line(size=0.25,
                                      linetype=1,color = 'gray90'),                          # Adds horizontal lines
    # X-axis
    axis.text.x = element_text(face="bold",
                               size=8, angle=0),           # Horizontal text
    axis.title.x = element_text(size = XTit_Sz),           # x-axis title text size
    # Y-axis
    axis.title.y = element_text(size = YTit_Sz),           # y-axis title text size
    axis.text.y = element_text(face="bold",
                               size=8, angle=0),
    # Legend
    legend.position ="none") +                             # Remove legend
  
  # X-Axis (flipped)
  coord_flip() + scale_y_continuous(name="Plant Value ($/MWh)",
                                    expand=c(0,0), limits=c(-MaxP,MaxP),breaks = pretty_breaks(6)) +
  # Y-axis (flipped)  
  scale_x_discrete(name="New Plant Name") +
  
  # Other Settings
  labs(caption = SourceDB) +
  scale_fill_manual(values = c("TRUE"="darkblue","FALSE"="darkgreen"))          
}