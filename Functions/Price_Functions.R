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
    
    labs(y = "Pool Price ($/MWh)", x = "Percentage of Time",caption = SourceDB) +
    
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
           year >= 2022 & year <= 2040,
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
    geom_line(aes(date,mean_price,linetype="A"),size=.85)+#,color="black")+
    geom_line(aes(date,mean_off_peak_price,linetype="B"),size=.85)+#,color="blue")+
    geom_ribbon(aes(date,ymax=q75_price,ymin=q25_price,fill=sit),alpha=.5)+
    geom_hline(yintercept=0) +
    scale_color_manual("",values = c("black","royalblue4"))+
    scale_fill_manual("",values = c("grey50","royalblue"),
                      labels="Two-tailed 90th percentile range")+
    scale_linetype_manual("",values = c("solid","11"),
                          labels=c("Peak period average","Off-peak period average"))+
    scale_x_date(expand=c(0,0),breaks="1 year",labels = date_format("%Y",tz="America/Denver"))+
    scale_y_continuous(expand=c(0,0))+
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
YearMX<-max(Sim$YEAR)
YearMN<-min(Sim$YEAR)

# Set font size and limits for plot
sz <- 15
Upplim <- round_any(max(Sim$Price)+11,100)

ggplot() +
  geom_line(data = Sim,
            aes(x = YEAR, y = Price, colour = Condition,linetype= Condition), 
            size = 1) +
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