#Intertie capacities

###############################################################################################
# packages load
###############################################################################################
{ library(tidyverse)
  library(lubridate)
  library(httr)
  library(janitor)
  library(ggplot2)
  library(grid)
  library(gtable)
  library(gridExtra)
  library(odbc)
  library(ggpubr)
  library(cowplot)
  library(scales)
  library(dplyr)
  library(reshape2)
  library(zoo)
  library(ggpattern)
  library(showtext)
  library(pivottabler)
  library(openxlsx)
  library(Hmisc)
  library(DescTools)
  library(writexl)
  library(car)
}

yhour <- function(time) {
  (yday(time) - 1) * 24 + hour(time)
}  

###############################################################################################
## Leach Functions
###############################################################################################

get_intertie_capacity_report<-function(start_date,end_date){ 
  #testing  
  #start_date<-"2018-01-01"
  #end_date<-"2019-12-31"
  #max date is today
  if(ymd(end_date)>today())
    end_date<-today()
  #max number of days is 400 days
  if(as.numeric(ymd(end_date)-ymd(start_date))>=400)
    end_date<-as.character(ymd(start_date)+days(399))
  #build url
  url<-paste("http://itc.aeso.ca/itc/public/queryHistoricalIntertieReport.do?availableEffectiveDate=943279200000+1999-11-22+07%3A00%3A00+MST+%281999-11-22+14%3A00%3A00+GMT%29&availableExpiryDate=1582354800000+2020-02-22+00%3A00%3A00+MST+%282020-02-22+07%3A00%3A00+GMT%29&fileFormat=CSV&startDate=",start_date,"&endDate=",end_date,sep = "")  
  #download data
  download_indic<-download.file(url,"test.csv",mode="wb")
  stop_for_status(download_indic)
  #process data to build capability by hour and data
  itc_data<-read.csv("test.csv",skip = 2,stringsAsFactors = F) %>% clean_names() %>%
    mutate(date=ymd(date),hour_ending=as.character(hour_ending)) %>% 
    rename("he"="hour_ending") %>%
    select(date,he,sk_import_capability,sk_export_capability,bc_export_capability,bc_import_capability,matl_export_capability,matl_import_capability,bc_matl_export_capability,bc_matl_import_capability)
  itc_data
}


get_all_itc_data<-function(){
  itc_store <- list()
  index<-1
  for(year in seq(2000,year(today()))){
    start_date<-paste(year,"01","01",sep="-")
    end_date<-paste(year,"12","31",sep="-")
    itc_store[[index]]<-get_intertie_capacity_report(start_date,end_date)
    index<-index+1
  }  
  itc_data<-data.frame(do.call(rbind,itc_store))
  singles<-seq(1,9)
  for(hour in singles){
    itc_data$he[itc_data$he==hour]<-paste(0,hour,sep="")
  }  
  itc_data$he[itc_data$he=="2*"]<-"02*"
  save(itc_data, file= "aeso_itc_data.RData") 
}  

#get_all_itc_data()


update_itc_data<-function(){
  load(file= "aeso_itc_data.RData") 
  #find max date in file
  start_date<-max(itc_data$date)
  end_date<-today()
  itc_update<-get_intertie_capacity_report(start_date,end_date)
  #fix he characters
  singles<-seq(1,9)
  for(hour in singles){
    itc_update$he[itc_update$he==hour]<-paste(0,hour,sep="")
  }  
  itc_data$he[itc_update$he=="2*"]<-"02*"
  #take out today's last day obs from itc data, append updated data
  itc_data<-itc_data %>% filter(date<ymd(start_date)) %>% bind_rows(itc_update)
  save(itc_data, file= "aeso_itc_data.RData") 
}  

###############################################################################################
# Load the ITC data
###############################################################################################
setwd("C:/Users/jessv/Documents (computer)/R stuff")
# Load the ITC data
{  load("aeso_itc_data.RData")
  
  # Re-name the dataset
  ITC <- itc_data 
  # Min date based on first date where BC_matl is not an NA value
  MTin <- "2013-01-02"
  # Filter for dates greater than the one mentioned above
  ITC <- ITC %>%
    filter(date>MTin)
  
  # Re-format the date
  ITC$Date <- format(ITC$date,"%Y-%m-%d")
  # Get year
  ITC$Year <- (format(ITC$date,"%Y"))
  # Get day
  ITC$Day <- format(ITC$date,"%j")
  
  # Combine date and time, remove white space
  ITC$Hr <-paste((ITC$he),":00:00")
  ITC$Hr <- gsub(" ", "", ITC$Hr, fixed = TRUE)
  
  ITC$Fdate <- as.POSIXct(paste(ITC$Date,ITC$Hr), format="%Y-%m-%d %H:%M:%S",tz='MST')
  
  # Re-order the columns and rename them
  ITC2 <- ITC[, c(15, 1,12, 13, 2, 3:4,9:10)]
  names(ITC2) <- c('Fdate','date',"Year",'Day','he','SKImp_c','SKExp_c','BCMTExp_c','BCMTImp_c')
  
  # For easy later Referece
  { SKI <- 'SKImp_c'
    SKE <- 'SKExp_c'
    BCI <- 'BCMTImp_c'
    BCE <- 'BCMTExp_c'
  }
}  
###############################################################################################
## Check the capacities for full year (function)
###############################################################################################  

Capab_yr <- function(data,yearMin,yearMax) {
  
  # Set y scale based on sereis
  if (data %like% '%BC%') {
    ymax=1110 
  } else {
    ymax=160
  }
  
  # Create new subsereis 
  ITC3 <- ITC2 %>%
    select(.,c('Fdate',"Year",'Day','he',data)) %>%
    filter(Year>=yearMin) %>%
    filter(Year<=yearMax)
  
  # Rename so I can index
  names(ITC3) <- c('Fdate','Year','Day','he','data')
  
  ITC3$YrHour<-yhour(as.POSIXct(ITC3$Fdate))
  
  # Generate correct name
  if (data=='SKImp_c') {
    subt <- 'SK Import Capability'
  } else if (data=='SKExp_c') { 
    subt <- 'SK Export Capability'
  } else if (data=='BCMTExp_c') { 
    subt <- 'BC_MT Export Capability'
  } else { 
    subt <- 'BC_MT Import Capability'
  }
  
  # Plot for chosen series
  p1 <- ggplot(ITC3) +
    geom_point(aes(x=YrHour, y=data,color=Year),size=0.5,na.rm = TRUE)+
    
    theme_bw() +
    
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(vjust = 1),
          axis.title.x = element_text(),
          axis.title.y = element_text(),
          plot.title =element_text(size = 15),
          plot.subtitle = element_text(hjust = 0.5), 
          panel.background = element_rect(fill = NA),
          legend.key.size = unit(1,"lines"), #Shrink legend
          legend.position = "bottom",
          legend.justification = c(0.5,0.5),
          legend.title=element_blank(),
          text = element_text(size = 15)) +
    
    scale_y_continuous(expand=c(0,0),limits = c(-10,ymax),breaks=pretty_breaks(5)) +
    
    scale_x_continuous(expand=c(0,0),limits = c(0,8783),breaks=pretty_breaks(5)) +
    
    guides(color = guide_legend(nrow = 1)) +
    
    labs(x = "Hour of Year", y = "Link Capacity", title=subt)
  
  # Create a big pop out window to see it
  windows(20,10)
  plot(p1)
  
  
}

###############################################################################################
## Get % of zero capability hours
############################################################################################### 

data <- BCI
YEAR <- 2018

{
  ITC_yr <- ITC2 %>%
    filter(Year==YEAR) %>%
    select(.,c('Fdate',"Year",'Day','he',data)) 
  
  names(ITC_yr) <- c('Fdate','Year','Day','he','data')
  
  ZeroHours = sum(ITC_yr$data==0)
  AllHours = length(ITC_yr$data)
  
  (100*ZeroHours)/AllHours
}

#Get all the data for everything except BC Imports
AuroraData <- ITC2 %>%
  filter(Year==YEAR) %>%
  select(.,c('date','he','SKImp_c','SKExp_c','BCMTExp_c','BCMTImp_c')) 

names(AuroraData)<-c('Date','Hour','SKImp','SKExp','BCMTExp','BCMTImp')

AuroraData$Date <- format(AuroraData$Date,"%m/%d/%Y")
write_xlsx(AuroraData,"C:\\Users\\jessv\\Documents (computer)/R stuff\\Capabilitydata2018.xlsx")

###############################################################################################
## Monthly capability plot, years colored
###############################################################################################
Capab_mn <- function(data,month) {
  
  # Set y scale based on sereis
  if (data %like% '%BC%') {
    ymax=1110 
  } else {
    ymax=160
  }
  
  # Create new subsereis 
  ITC3 <- ITC2 %>%
    select(.,c('Date',"Year",'Day','he',data))
  # Rename so I can index
  names(ITC3) <- c('Date','Year','Day','he','data')
  
  # # Use for Daily Average
  # ITC3 <- ITC3 %>%
  #   group_by(Date,Year,Day) %>%
  #   summarise(Day_Avg = mean(data, na.rm = TRUE))
  # names(ITC3) <- c('Date','Year','Day','data')
  
  # # Use for Daily max
  # ITC3<- ITC3 %>%
  #   group_by(Date,Year,Day) %>%
  #   summarise(Day_Max = max(data, na.rm = TRUE))
  # names(ITC3) <- c('Date','Year','Day','data')
  
  # Use for Daily min
  ITC3<- ITC3 %>%
    group_by(Date,Year,Day) %>%
    summarise(Day_Min = min(data, na.rm = TRUE))
  names(ITC3) <- c('Date','Year','Day','data')
  
  
  # Min for date (x-axis)
  day_MN <- as.POSIXct(paste(01,month,2012, sep = "/"), format="%d/%m/%Y")
  day_lab <-as.POSIXct(paste(05,month,2012, sep = "/"), format="%d/%m/%Y")
  
  # December is special
  if (month <12) {
    # Max for date
    day_MX <- as.POSIXct(paste(01,month+1,2012, sep = "/"), format="%d/%m/%Y")
    day_MX <- ceiling_date(day_MX, "month") - 1 
    
  }else {
    day_MX <- as.POSIXct(paste(31,month,2012, sep = "/"), format="%d/%m/%Y") }
  
  # Format the dates as we have in set
  day_MN <- as.numeric(format(day_MN,"%j"))
  day_MX <- as.numeric(format(day_MX,"%j"))
  
  #Convert to numeric so that it scales well
  ITC3$Day <- as.numeric(ITC3$Day)
  
  # Plot based on data given
  ggplot(ITC3) +
    geom_point(aes(x=Day, y=data,color=Year),na.rm = TRUE,
               alpha = 1,size=1)+
    #position=position_jitter(h=0.1, w=0.1)
    scale_size(range = c(5,1)) +
    
    theme_bw() +
    
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(vjust = 1),
          axis.title.x = element_text(),
          axis.title.y = element_blank(),
          plot.title =element_text(size = 15),
          plot.subtitle = element_text(hjust = 0.5), 
          panel.background = element_rect(fill = NA),
          legend.key.size = unit(1,"lines"), #Shrink legend
          legend.position = 'bottom',
          legend.justification = c(0.5,0.5),
          legend.title=element_blank(),
          text = element_text(size = 15)) +
    
    scale_y_continuous(expand=c(0,0),limits = c(-5,ymax),breaks=pretty_breaks(5)) +
    
    scale_x_continuous(limits=c(day_MN,day_MX)) +
    
    labs(x = "Day of Year", y = "SK Import Capacity", title=month.abb[month]) +
    
    guides(color = guide_legend(nrow = 1) )      
  
  
}

###############################################################################################
## Plot all Months
###############################################################################################
Capab_Allmn <- function(data) {
  
  
  # Create a graph for each month of the year
  p1 <- Capab_mn(data,01) +
    theme(axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p2 <- Capab_mn(data,02) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p3 <- Capab_mn(data,03) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p4 <- Capab_mn(data,04) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p5 <- Capab_mn(data,05) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p6 <- Capab_mn(data,06) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p7 <- Capab_mn(data,07) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p8 <- Capab_mn(data,08) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p9 <- Capab_mn(data,09) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p10 <- Capab_mn(data,10) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p11 <- Capab_mn(data,11) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  p12 <- Capab_mn(data,12) +
    theme(legend.position ="none",
          axis.title.y=element_blank(),
          axis.title.x=element_blank())
  
  # Get a common legend
  legend <- get_legend(p1)
  p1 <- p1 + theme(legend.position ="none")
  
  # Plot Labels
  yleft <- textGrob("Capability", rot = 90, gp = gpar(fontsize = 15))
  
  # Cheat way to put an x title in
  xtitle <- ggplot() +
    annotate("text", x = 10,  y = 10,
             size = 6,
             label = "Day of Year") + 
    theme_void()
  
  # Label the source and year
  # First, generate correct name
  if (data=='SKImp_c') {
    subt <- 'SK Import Capability'
  } else if (data=='SKExp_c') { 
    subt <- 'SK Export Capability'
  } else if (data=='BCMTExp_c') { 
    subt <- 'BC_MT Export Capability'
  } else { 
    subt <- 'BC_MT Import Capability'
  }
  
  xsubtitle <- ggplot() +
    annotate("text", x = 10,  y = 10,
             size = 4,
             label = paste("Data from the AESO ATC and Intertie Capability Public Reports,","Series:",subt)) + 
    theme_void()
  
  #Create a big window
  windows(20,12)
  
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

###############################################################################################
## Get Some Stats!
###############################################################################################
Capab_Stats <- function(yearMN,yearMX) {
  
  # Create new sereis which only has the year data
  ITC4 <- ITC2 %>%
    select(.,c("Year",'SKImp_c','SKExp_c','BCMTExp_c','BCMTImp_c')) %>%
    filter(Year<=yearMX) %>%
    filter(Year>=yearMN) %>%
    select(.,-c("Year"))
  #Name them again 
  names(ITC4) <- c('SK_Import_Capability','SK_Export_Capability','BC_MT_Export_Capability','BC_MT_Import_Capability')
  
  # Bring them all into one big line and give keys
  data_long <- gather(ITC4, factor_key=TRUE)
  
  #Generate the result
  Result <- data_long%>% group_by(key)%>%
    summarise(mean= mean(value), sd= sd(value), max = max(value),min = min(value))
  
  #Print the result 
  print(Result, width = Inf)
  
  # QQ Plots
  # windows(10,8)
  # qqPlot(ITC4$SK_Import_Capability)
  # windows(10,8)
  # qqPlot(ITC4$SK_Export_Capability)
  # windows(10,8)
  # qqPlot(ITC4$BC_MT_Export_Capability)
  windows(10,8)
  qqPlot(ITC4$BC_MT_Import_Capability)
}  


###############################################################################################
## Call for all the options
###############################################################################################  
Capab_yr(SKE,2018,2018)
Capab_yr(SKI,2018,2018)
Capab_yr(BCE,2015,2021)
Capab_yr(BCI,2018,2018)

Capab_Allmn(SKE)
Capab_Allmn(SKI)
Capab_Allmn(BCE)
Capab_Allmn(BCI)

Capab_Stats(2018,2018)  
###############################################################################################
## Load the actual data
###############################################################################################
Act <- readRDS("aeso_act.RData")

#Select what I need, ignore other stuff
Act <- Act %>%
  select(.,Date_Begin_Local,ACTUAL_POOL_PRICE,ACTUAL_AIL,EXPORT_BC,EXPORT_MT,EXPORT_BC_MT,EXPORT_SK,IMPORT_BC,IMPORT_MT,IMPORT_BC_MT,IMPORT_SK)

# Format as date
Act$Date_Begin_Local <- as.POSIXct(Act$Date_Begin_Local,tz="",format="%Y-%m-%d %H:%M")
# Format as hour ending 
Act$he2 <- as.numeric(format(Act$Date_Begin_Local,"%H"))+1
# Date as value between 1-365
Act$date2 <- format(Act$Date_Begin_Local,"%Y-%m-%d")

