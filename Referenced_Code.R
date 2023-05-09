################################################################################
# TITLE: DrLeach_Code
# DESCRIPTION: Codes adapted from Dr. Andrew Leach and Taylor Pawlenchuk
#
# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: January 9, 2023; LAST EDIT: January 9, 2023
#
################################################################################
# 

library(dplyr)

assign_date_time_days<-function(data_sent,time_var=time){
  quo_time<- enquo(time_var)
  data_sent %>%
    mutate(year=year(!!quo_time),
           month=month(!!quo_time), #month dummies
           month_fac=factor(month.abb[month],levels = month.abb),
           day=day(!!quo_time),
           wday=wday(!!quo_time,label=T),
           hour=hour(!!quo_time),
           temp_time=NULL
    )
}

assign_peaks<-function(data_orig,time_var=time){
  #default is that we're receiving a data_frame with time as the time variable
  #create temp_time with whatever the time variable might be, then use that to create stats and peaks
  #modify data_sent so you have a data-frame with only the time varible
  #data_mod<-data_orig %>% mutate_(temp_time=`time_var`) %>% select(temp_time)
  temp_time<- enquo(time_var)
  Yr<- enquo(year)
  data_mod<-data_orig %>% dplyr::select(!!temp_time)
  #first, figure out the holidays
  #Holidays:
  #xNew Year's Day  January 1
  #xAlberta Family Day   Third Monday in February
  #xGood Friday   Friday before Easter
  #Victoria Day  Monday before May 25
  #xCanada Day July 1, except when it falls on a Sunday, then it is July 2
  #xLabour Day  First Monday in September
  #xThanksgiving Day  Second Monday in October
  #xRemembrance Day   November 11
  #xChristmas Day   December 25
  holiday_list<-c("Christmas","NYD","CDA_Day","Rem_Day","Labour_Day","Good_Friday","Family_Day",  
                  "Thanksgiving", "Victoria_Day")
  data_mod<-data_mod%>%mutate(
    Christmas=ifelse(month(!!temp_time)==12 & day(!!temp_time)==25,T,F),
    NYD=ifelse(month(!!temp_time)==1 & day(!!temp_time)==1,T,F),
    CDA_Day=ifelse(month(!!temp_time)==7 & day(!!temp_time)==1 & wday(!!temp_time,label = T)!="Sun" ,T,F), #Canada Day Holiday if it's not a Sunday
    CDA_Day=ifelse(month(!!temp_time)==7 & day(!!temp_time)==2 & wday(!!temp_time,label = T)=="Mon" ,T,F), #Canada Day Stat if the 2nd is a monday
    Rem_Day=ifelse(month(!!temp_time)==11 & day(!!temp_time)==11,T,F),
    Labour_Day=ifelse(month(!!temp_time)==9 & day(!!temp_time)<=7 & wday(!!temp_time,label = T)=="Mon",T,F), #first Monday in September
    Good_Friday=ifelse(date(!!temp_time)==as.Date(Easter(year(!!temp_time)))-days(2),T,F),
    #Family day - third monday in february so earliest it can be is day 15, latest is day 21
    Family_Day=ifelse(month(!!temp_time)==2 & day(!!temp_time)<=21 & day(!!temp_time)>=15 & wday(!!temp_time,label = T)=="Mon",T,F), #third Monday in Feb
    #Thanksgiving day - second monday in Oct so earliest it can be is day 8, latest is day 14
    Thanksgiving=ifelse(month(!!temp_time)==10 & day(!!temp_time)<=14 & day(!!temp_time)>=8 & wday(!!temp_time,label = T)=="Mon",T,F), #second Monday in Oct
    #Victoria day - monday before May 25, so earliest it can be is day 18, latest is day 24
    Victoria_Day=ifelse(month(!!temp_time)==5 & day(!!temp_time)<=24 & day(!!temp_time)>=18 & wday(!!temp_time,label = T)=="Mon",T,F) #Monday before May 25
  ) %>% 
    mutate(
      stat = dplyr::select(., all_of(holiday_list)) %>% rowSums()>0
    )
  #On-Peak: hour ending HE8 to HE23 Monday through Saturday, excluding Sundays and NERC holidays
  #Off-Peak: HE1 to HE7 and HE24 Monday through Saturday, and all hours on Sundays and NERC holidays
  #Extended Peak: HE8 to HE23 every day in the contract period
  #Extended Off-Peak: HE1 to HE7 and HE24 every day in the contract period
  #Super Peak: HE17 to HE22 each day in the contract period
  #for AS, AESO does AM super peak HE 6, 7, 8 and a winter PM Super Peak (HE 17-34, in Nov, Dec, Jan)
  data_mod<-data_mod%>%mutate(
    on_peak=ifelse(wday(!!temp_time,label = T)!="Sun" & stat==F & hour(!!temp_time)>=8 & hour(!!temp_time)<=23,T,F), #Peak hours, not stat or Sunday
    off_peak=ifelse(wday(!!temp_time,label = T)=="Sun" | stat==T | hour(!!temp_time)>=24 | hour(!!temp_time)<=7,T,F), #Off-Peak hours, stat or Sunday
    ext_peak=ifelse(hour(!!temp_time)>=8 & hour(!!temp_time)<=23,T,F), #Ext Peak hours
    ext_off_peak=ifelse(hour(!!temp_time)<8 & hour(!!temp_time)>23,T,F), #Ext Off Peak hours
    super_peak=ifelse(hour(!!temp_time)>=17 & hour(!!temp_time)<=22,T,F), #Super Peak hours
  )
  #return indicators for stats and peaks - same # of rows as data sent
  data_mod<-data_mod %>% dplyr::select(stat,on_peak,off_peak,ext_peak,ext_off_peak,super_peak)
  bind_cols(data_orig,data_mod)
}