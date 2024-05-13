################################################################################
# TITLE: Other_Functions
# DESCRIPTION: Additional functions to use, not related to plotting.
#
# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: June 14, 2022; LAST EDIT: May 11, 2023
################################################################################

################################################################################
## FUNCTION: packs_check
## Checks if packages are installed, installs them if not, and loads required functions
##
## INPUTS: 
##    packs_to_load - List of all packages needed
################################################################################

#Call function and pass package names through
packs_check <- function(packs_to_load) {

  #check for package and require it, 
  package.check <- lapply(
    packs_to_load,
    FUN = function(x) {
      if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)
      }
    }
  )
}

################################################################################
## FUNCTION: SaveRun_Loc
## Saves all plots to a new folder names after case
##
## INPUTS: 
##    CaseName - name for folder. Enter as "folder"
##    FileName - Name for image. Enter as "name"
################################################################################
#NEEDS WORK
 SaveRun_Loc <- function(CaseName,FileName) {

  # Set up folder if it does not exist
  fold_name<-paste(CaseName,SourceDB)

  # Check if folder exists, if not, make one
  if (file.exists(here("Figures (Local)",paste(fold_name)))) {

    cat("The folder exists\n")

  } else {

    # Create the folder
    FoldLocation <-
    dir.create(here("Figures (Local)",paste(fold_name)))

  }

  # Create file name
  FileName <-paste(FileName,SourceDB)
  
# Save to a local file as exactly what is shown on the windows()
  savePlot(
    filename = here(paste("Figures (Local)/",paste(fold_name),"/",FileName,".png", sep = "")),
    type = "png",
    device = dev.cur())
  
}
  
################################################################################
## FUNCTION: SaveRun_Loc_Ex
## Saves all plots to a new folder names after case
##
## INPUTS: 
##    CaseName - name for folder. Enter as "folder"
##    FileName - Name for image. Enter as "name"
################################################################################
#NEEDS WORK
SaveRun_Loc_Ex <- function(CaseName,FileName) {
  
  # Set up folder if it does not exist
  fold_name<-paste(CaseName,SourceDB)
  
  # Check if folder exists, if not, make one
  if (file.exists(here("Figures (Local)",paste(fold_name)))) {
    
    cat("The folder exists\n")
    
  } else {
    
    # Create the folder
    FoldLocation <-
      dir.create(here("Figures (Local)",paste(fold_name)))
    
  }
  
  # Check if folder exists, if not, make one
  if (file.exists(here("Figures (Local)",paste(fold_name),"/","Additional Analysis/"))) {
    
    cat("The folder exists\n")
    
  } else {
    
    # Create the folder
    FoldLocation <-
      dir.create(here("Figures (Local)",paste(fold_name),"/","Additional Analysis/"))
    
  }
  
  
  # Create file name
  FileName <-paste(FileName,SourceDB)
  
  
  
  # Save to a local file as exactly what is shown on the windows()
  savePlot(
    filename = here(paste("Figures (Local)/",paste(fold_name),"/","Additional Analysis/",FileName,".png", sep = "")),
    type = "png",
    device = dev.cur())
  
}

################################################################################
## FUNCTION: yhour
## Get the hour of year associated with a date in the form "%Y-%m-%d %H:%M:%S"
## EX: Jan 1 at 1:00 = 0001, Dec 31 at 23:00 = 8760
##
## INPUTS: 
##    d_time - Date and time to transform to hour of year 
################################################################################
yhour <- function(d_time) {
  (yday(d_time) - 1) * 24 + hour(d_time)
}


################################################################################
## FUNCTION: round_any
## Use to round value to a certain accuracy
##
## INPUTS: 
##    x - Value to round
##    accuracy - Accuracy to round to
################################################################################
round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}


################################################################################
#
# TO USE IN PLOTING FUNCTIONS SECTION
#
################################################################################

################################################################################
## FUNCTION: HrTime
## Convert the date and select a subset for one day from the data pulled in. From midnight to midnight.
################################################################################

HrTime <- function(data, year, month, day) {
  
 # Select single day  
 data1<- subset(data,
         (date >= as.POSIXct(paste(year,month,day," 00:00:00", sep = "-"),tz = "MST") &
          date <= as.POSIXct(paste(year,month,day+1," 00:00:00", sep = "-"),tz = "MST")))
  
 # Get up to 24 hours
 # data2<- data %>%
 #   filter(year(date)==year,
 #          month(date)==month,
 #          day(date)==day) %>%
 #     mutate(date2=date+60*60)
 # 
 # 
 #  data<-rbind(data1,data2)
  
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
## FUNCTION: YrDay_Time
## Convert the date and select a subset for specific year and day
################################################################################

{ YrDay_Time <- function(data, year,day) {
  
  # Create column for year 
  data$YEAR <- format(data$date,format="%Y") # Reformat for year only
  
  year <- format(as.character(year), 
                 format = "%Y")
  
  #Create subset for specified days in year
  data <- data %>%
    filter(.,YEAR==year) %>%    # Filter year out
    filter(.,wday(date) == day)  #Filter for day specified only
  
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
## FUNCTION: Legend_PlotAll
## Plot legend for all things referenced in other plots
################################################################################
Legend_PlotAll <- function(InputAplha){

plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1) +
legend("center", 
       legend =c("Nuclear","Cogeneration", "Coal",
                 "Coal-to-Gas",
                 "Natural Gas Simple Cycle", "Natural Gas Combined Cycle", 
                 #"Blended  Simple Cycle","Blended  Combined Cycle",
                 "Natural Gas Combined Cycle + CCS",
                 "Hydrogen Combined Cycle","Hydrogen Simple Cycle",
                 "Hydro", "Other", "Wind", 
                 "Solar",  "Storage - Battery", 
                 "Storage - Compressed Air", "Storage - Pumped Hydro",
                 "Import","Export"),
       
       pch=16,          # Type of point
       pt.cex=2,        # Expansion factor for points relative to text
       title.cex=1.5,     # Expansion factor for title relative to text
       bty='o',         # Box around legend
       ncol=2,          # Allow two columns for legend
       col = alpha(c(cOL_NUCLEAR,cOL_COGEN, cOL_COAL,
               cOL_NGConv,
               cOL_SCGT, cOL_NGCC, 
               #cOL_SCGT_Blend,cOL_NGCC_Blend,
               cOL_NGCC_CCS,
               cOL_NGCC_H2,cOL_SCGT_H2,
               cOL_HYDRO, cOL_OTHER, cOL_WIND, 
               cOL_SOLAR,  COL_Battery, 
               COL_CompAir,COL_Pumped,
               cOL_IMPORT,cOL_EXPORT),InputAplha),
       title=c("Legend"))
}
################################################################################
## FUNCTION: Legend_PlotMain
## Plot legend for main things referenced in other plots
################################################################################
Legend_PlotMain <- function(InputAplha){
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1) +
  legend("center", 
         legend =c("Cogeneration", "Coal","Coal-to-Gas",
                   #"Natural Gas and Hydrogen Blend",
                   "Natural Gas","Natural Gas Combined Cycle + CCS","Hydrogen",
                   "Hydro", "Other", "Wind", "Solar",  "Storage",
                   "Import","Export"),
         
         pch=16,          # Type of point
         pt.cex=2,        # Expansion factor for points relative to text
         title.cex=1.5,     # Expansion factor for title relative to text
         bty='o',         # Box around legend
         ncol=1,          # Allow two columns for legend
         col = alpha(c(cOL_COGEN, cOL_COAL,cOL_NGConv,
                 #COL_Blend,
                 COL_NatGas,cOL_NGCC_CCS,COL_H2,
                 cOL_HYDRO, cOL_OTHER, cOL_WIND, cOL_SOLAR,  cOL_STORAGE,
                 cOL_IMPORT,cOL_EXPORT),InputAplha),
         title=c("Legend"))
}

################################################################################
## FUNCTION: Legend_PlotGray
## Plot legend for all things referenced in other plots
################################################################################
Legend_PlotGray <- function(InputAplha){
  
  plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1) +
    legend("center", 
           legend =c("Import","Nuclear","Cogeneration", "Coal",
                     "Coal-to-Gas",
                     "Natural Gas Simple Cycle", "Natural Gas Combined Cycle", 
                     "Natural Gas Combined Cycle + CCS",
                     "Hydrogen",
                     "Hydro", "Other", "Wind", 
                     "Solar",  "Storage" 
                     ),
           
           pch=16,          # Type of point
           pt.cex=2,        # Expansion factor for points relative to text
           title.cex=1.5,     # Expansion factor for title relative to text
           bty='o',         # Box around legend
           ncol=2,          # Allow two columns for legend
           col = alpha(c(cOL_IMPORTg,cOL_NUCLEARg,cOL_COGENg, cOL_COALg,
                         cOL_NGConvg,
                         cOL_SCGTg, cOL_NGCCg, 
                         cOL_NGCC_CCSg,
                         COL_H2g,
                         cOL_HYDROg, cOL_OTHERg, cOL_WINDg, 
                         cOL_SOLARg,  cOL_STORAGEg 
                         ),InputAplha),
           title=c("Legend"))
}

################################################################################
## FUNCTION: SaveR_Loc
## Saves all plots to a new folder names after case
##
## INPUTS: 
##    CaseName - name for folder. Enter as "folder"
##    FileName - Name for image. Enter as "name"
################################################################################
#NEEDS WORK
SaveR_Loc <- function(Data,ScenarioName,ScenarioType) {
  
  # Set up folder if it does not exist
  fold_name<-paste(ScenarioName)
  
  # Check if folder exists, if not, make one
  if (file.exists(here("Data Files","Result Files",paste(fold_name)))) {
    
    cat("The folder exists\n")
    
  } else {
    
    # Create the folder
    FoldLocation <-
      dir.create(here("Data Files","Result Files",paste(fold_name)))
    
  }

  # Save to a local file 
  saveRDS(Data, here("Data Files","Result Files",paste(fold_name),paste(ScenarioType,ScenarioName, sep = "")))
  
}

################################################################################
## FUNCTION: ResGrouphr_Renew8760
## Saves all plots to a new folder names after case
##
## INPUTS: 
##    CaseName - name for folder. Enter as "folder"
##    FileName - Name for image. Enter as "name"
################################################################################
#NEEDS WORK
ResGrouphr_Renew8760 <- function() {
  
  # Years to get 
  years_in <-as.list(seq(2023, MaxYrStudy))
  
  # Bring in sim data & filter years
  Res_Data <- ResGroupHr %>%
    filter(ID %in% c("LTO_Wind","LTO_Solar"),
           Condition=="Average",
           Run_ID == case,
           Report_Year %in% years_in) %>%
    rename(CF=Capacity_Factor)%>%
    subset(.,select=c(date,Output,Capacity,CF,Forced_Outage)) %>%
    mutate(Year_f=as.numeric(year(date)),
           Month_f=as.numeric(month(date)),
           Day_f=as.numeric(day(date)),
           Hour_f=as.numeric(hour(date)),
           Wday_f=as.numeric(wday(date)),
           date=as.POSIXct(date,tz="MST"))
  Res_Data[is.na(Res_Data)] <- 0
  
  
  # Days already done (to save computational time)
  day_Aurora <- as.list(unique(Res_Data$Day_f))
  day_seq <- as.list(1:31)
  day_seq <- day_seq[!day_seq %in% day_Aurora]
  
  
  # Fill missing dates, set up all dates needed
  ts <- seq.POSIXt(as.POSIXct(paste(2023,"-01-01 0:00", sep = ""), tz = "MST"), as.POSIXct(paste(MaxYrStudy,"-12-31 23:00", sep = ""), tz = "MST"), by="hour")
  
  # Create df
  df_time <- data.frame(date=ts) 
  
  # Extend data, prepare to fill
  Res_Data_all <- full_join(Res_Data,df_time)  %>%
    mutate(Year_f=as.numeric(year(date)),
           Month_f=as.numeric(month(date)),
           Day_f=as.numeric(day(date)),
           Hour_f=as.numeric(hour(date)),
           Wday_f=as.numeric(wday(date)))
  
  # Loop over each hour to fill data. Assume days are representative other other days
  for (y in years_in){
    for (m in 1:12){
      for (d in day_seq){
        # Check to see if day exists. If TRUE, the day exists
        if (any(Res_Data_all$Day_f[Res_Data_all$Year_f == y & Res_Data_all$Month_f == m & Res_Data_all$Day_f == d] >0) == TRUE) {
          
          # Get day of week to find
          wday_match = as.numeric(wday(paste(y,m,d,sep="-")))
          
          for (h in 0:23){
            # Get actual data for weekday and hour
            act_CF <-Res_Data$CF[Res_Data$Year_f == y & Res_Data$Month_f == m & Res_Data$Hour_f == h & Res_Data$Wday_f == wday_match]
            act_Out <-Res_Data$Output[Res_Data$Year_f == y & Res_Data$Month_f == m & Res_Data$Hour_f == h & Res_Data$Wday_f == wday_match]
            act_Cap <-Res_Data$Capacity[Res_Data$Year_f == y & Res_Data$Month_f == m & Res_Data$Hour_f == h & Res_Data$Wday_f == wday_match]
            act_FOR <-Res_Data$Forced_Outage[Res_Data$Year_f == y & Res_Data$Month_f == m & Res_Data$Hour_f == h & Res_Data$Wday_f == wday_match]
            
            # Replace in the 8760 data
            Res_Data_all$CF[Res_Data_all$Year_f == y & Res_Data_all$Month_f == m & Res_Data_all$Day_f == d & Res_Data_all$Hour_f == h]<-act_CF
            Res_Data_all$Output[Res_Data_all$Year_f == y & Res_Data_all$Month_f == m & Res_Data_all$Day_f == d & Res_Data_all$Hour_f == h]<-act_Out
            Res_Data_all$Capacity[Res_Data_all$Year_f == y & Res_Data_all$Month_f == m & Res_Data_all$Day_f == d & Res_Data_all$Hour_f == h]<-act_Cap
            Res_Data_all$Forced_Outage[Res_Data_all$Year_f == y & Res_Data_all$Month_f == m & Res_Data_all$Day_f == d & Res_Data_all$Hour_f == h]<-act_FOR
          }
        }
      }
    }
  }
  
return(Res_Data_all)
  
}

################################################################################
## FUNCTION: NRG_student_generate_R
## Format input data and save as R files.
##
## INPUTS: 
##    student_data_name - raw student data name, in csv format
##    nrg_raw_name - nrg extract, r data name
##    merit_file_name - student data converted to R file name
##    nrg_file_name - output filtered nrg data file name, r data
##    demand_file_name - output demand file name, R data
##    
################################################################################

NRG_student_generate_R <- function(student_data_name,nrg_raw_name,merit_file_name,nrg_file_name,demand_file_name) {
  
  # Load merit data
  merit <- read_csv(here("Data Files","Alberta Data",student_data_name))
  # Save as R file
  saveRDS(merit, here("Data Files","Alberta Data",merit_file_name))
  
  # Load NRG data and rename time column
  load(here("Data Files","Alberta Data",nrg_raw_name))
  nrgstream_gen <- nrgstream_gen %>%
    rename(time=Time)
  
  # Remove NA values
  nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$gen),]
  nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$time),]
  
  # Apply data corrections
  corrected <- nrgstream_gen %>%
    filter(is.na(Latitude)) %>%
    mutate(Latitude=case_when(grepl("BRD1",ID) ~ 49.842735,
                              grepl("BUR1",ID) ~ 49.814877,
                              grepl("CLR",ID) ~ 50.032911,
                              grepl("CLY",ID) ~ 49.840967,
                              grepl("CHP1",ID) ~ 50.22189,
                              grepl("COL1",ID) ~ 49.833218,
                              grepl("CRD",ID) ~ 49.807,
                              grepl("CRR2",ID) ~ 49.55891,
                              grepl("FMG1",ID) ~ 49.66334,
                              grepl("KKP",ID) ~ 53.469986,
                              grepl("MON1",ID) ~ 49.833144,
                              grepl("NMK1",ID) ~ 51.026118,
                              grepl("RIV1",ID) ~ 49.53245,
                              grepl("STR",ID) ~ 51.033273,
                              grepl("TVS1",ID) ~ 50.27324,
                              grepl("VCN1",ID) ~ 50.0975,
                              grepl("VXH1",ID) ~ 50.095223,
                              grepl("WEF1",ID) ~ 49.65405,
                              grepl("WHT",ID) ~ 49.64029),
           Longitude=case_when(grepl("BRD1",ID) ~ -111.537891,
                               grepl("BUR1",ID) ~ -111.543323,
                               grepl("CHP1",ID) ~ -110.437106,
                               grepl("CLR",ID) ~ -113.484369,
                               grepl("CLY",ID) ~ -110.356864,
                               grepl("COL1",ID) ~ -112.97448,
                               grepl("CRD",ID) ~ -112.578,
                               grepl("CRR2",ID) ~ -113.983,
                               grepl("FMG1",ID) ~ -111.122,
                               grepl("KKP",ID) ~ -113.61337,
                               grepl("MON1",ID) ~ -112.974231,
                               grepl("NMK1",ID) ~ -113.163017,
                               grepl("RIV1",ID) ~ -113.977,
                               grepl("STR",ID) ~ -113.371296,
                               grepl("TVS1",ID) ~ -112.73059,
                               grepl("VCN1",ID) ~ -112.84841,
                               grepl("VXH1",ID) ~ -112.149936,
                               grepl("WEF1",ID) ~ -111.515812,
                               grepl("WHT",ID) ~ -111.291),
           Installation_Year=case_when(grepl("CRR2",ID)~2019,
                                       grepl("CYP",ID)~2022,
                                       #grepl("CYP2",ID)~"post2019",
                                       grepl("FMG1",ID)~2022,
                                       grepl("GDP1",ID)~2022,
                                       grepl("GRZ1",ID)~2022,
                                       grepl("HHW1",ID)~2022,
                                       grepl("HLD1",ID)~2022,
                                       grepl("JNR",ID)~2022,
                                       grepl("RIV1",ID)~2019,
                                       grepl("RTL1",ID)~2021,
                                       grepl("WHE1",ID)~2022,
                                       grepl("WHT1",ID)~2019,
                                       grepl("WHT2",ID)~2021,
                                       grepl("WRW1",ID)~2021),
           Installation_Year=case_when(is.na(Installation_Year)~"pre2019",
                                       TRUE~"post2019"))
  
  # Get non-corrected and remove Latitude
  nocorrection <- nrgstream_gen %>%
    filter(!is.na(Latitude))%>%
    mutate(Installation_Year="")
  
  # put back together and remove old files
  nrgstream_gen <- rbind(corrected,nocorrection)
  rm(corrected,nocorrection)
  
  # Save new file
  saveRDS(nrgstream_gen,here("Data Files","Alberta Data",nrg_file_name))
  
  # Make separate file for demand and save
  Actdemand <- nrgstream_gen %>%
    group_by(time) %>%
    summarise(Demand = median(Demand),
              Price = median(Price),
              AIL = median(AIL))
  
  # Save the demand
  saveRDS(Actdemand, here("Data Files","Alberta Data",demand_file_name))
  
}

################################################################################
## FUNCTION: Load_NRG_hourly
## Load nrgstream hourly data, can only be used once R file is generated
##
## INPUTS: 
##    date_filt - Date to filter "date"
##    nrg_file_name - Name for file version "name"
##    reformat_names = True/false to format names same as Aurora
################################################################################

Load_NRG_hourly <- function(date_filt,yr_max,nrg_file_name,reformat_names) {
    
  # Load nrgstream_gen - Load and demand info, plus a whole ton more
  nrgstream_gen <- readRDS(here("Data Files","Alberta Data",nrg_file_name)) 
  sub_samp<-filter(nrgstream_gen, time >= as.Date(date_filt))

  # Create a list to describe Import/Exports
  trade_excl<-c("AB - WECC Imp Hr Avg MW", 
                "AB - WECC Exp Hr Avg MW",
                "AB - WECC Imp/Exp Hr Avg MW")
  
  # Create Dataframe, only select rows where the Nat resource group is in the defined groups (ie trading)
  # then grouped by plant type 
  df1 <- sub_samp %>% 
    filter(! NRG_Stream %in% trade_excl)%>% 
    group_by(Plant_Type,time) %>% 
    summarise(meancap = mean(Cap_Fac),
              capacity =sum(Capacity),
              total_gen=sum(gen,na.rm = T),
              group_CF=total_gen/capacity,
              total_rev=sum(Revenue,na.rm = T),
              price_mean=mean(Price),
              heatrt_mean=mean(Heat.Rate)) %>% 
    ungroup()
  
  #Reformat the dates
  df1$Day <- date(df1$time)
  df1$Year <- as.factor(year(df1$time))
  df1$Hour <-hour(df1$time)
  
  {  # ORGANIZE RESOURCES
    #Make a resource type list
    plant_types<-c("COAL","NGCONV","COGEN","HYDRO","NGCC", "OTHER", "SCGT","SOLAR","IMPORT","EXPORT","WIND","STORAGE")
    
    # Create a new dataframe with plant types specified only, 
    # Then filter AESO data to exclude dates without information (till end of 2022)
    df1a <- df1 %>%
      filter(Plant_Type %in% plant_types,
             year(time)<yr_max)
    
    # Put in desired order: Coal, Cogen, NGCC, SCGT, Other, Hydro, Wind, Solar, Import, Export
    df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "OTHER",after=Inf)
    df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "HYDRO",after=Inf)
    df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "WIND",after=Inf)
    df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "SOLAR",after=Inf)
    df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "IMPORT",after=Inf)
    df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "EXPORT",after=Inf)
    
    nrg_names <- c("COAL","COGEN","NGCC","NGCONV","SCGT","STORAGE","OTHER",
                      "HYDRO","WIND","SOLAR","IMPORT","EXPORT")
    gc()   
  }
  
  if (reformat_names == TRUE){
    
    df1a$NRG_Plant_Type <- df1a$Plant_Type
    
    Aurora_names <- c("Coal","Cogeneration","Natural Gas Combined Cycle","Coal-to-Gas","Natural Gas Simple Cycle","Storage","Other",
                      "Hydro","Wind","Solar","Import","Export")
    
    levels(df1a$Plant_Type) <- Aurora_names
    
  }
  
  return(df1a)

}

################################################################################
## FUNCTION: Load_NRG_demand
## Load nrgstream data, can only be used once R fiel is generated
##
## INPUTS: 
##    CaseName - name for folder. Enter as "folder"
##    FileName - Name for image. Enter as "name"
################################################################################


Load_NRG_demand <- function(date_filt,demand_file_name) {
 
  #Reformat actual demand file
  Actdemand <- readRDS(here("Data Files","Alberta Data",demand_file_name))
  Actdemand$Day <- date(Actdemand$time)
  Actdemand<-filter(Actdemand, time >= as.Date(date_filt))
 
  return(Actdemand)
  
}

################################################################################
## FUNCTION: capcost_learning_compare
## Compare capital costs based on excel compare file.
##
## INPUTS: 
##    type_filt - Cost or Norm, type of cost compare 
##    Res_Filt - Resource types included
################################################################################

capcost_learning_compare <- function(type_filt,Res_Filt) {

  if (type_filt == "Norm"){
    title_lab = "Capital Cost Relative to 2022"
    cap_add=""
  } else{
    title_lab = "Capital Cost (2022$/kW)"
    cap_add="NREL costs converted to 2022 CAD based on 0.7688 conversion rate and 2.5% inflation rate"
  }
  
  # Colors and lines
  cost_COL = c("NREL"='#b6b1b1',"Model"="#4472C4","CEC"='#515151',"AESO"="black")
  cost_line = c("NREL"=1,"Model"=2,"CEC"=1,"AESO"=1)
  
  Cost_Compare_filt <- melt(Cost_Compare, id=c("Type","Source","Name","Cost_Type")) %>%
    rename(Year=variable,
           Amount=value)%>%
    filter(Cost_Type == type_filt,
           !is.na(Amount),
           Type %in% Res_Filt,
           ) %>%
    mutate(Year =as.numeric(as.character(Year)),
           Source = if_else(Source == "UofA","Model",Source))
  
  # Replace CEC data
  Cost_Compare_filt$Amount[(Cost_Compare_filt$Source == "CEC") & (Cost_Compare_filt$Year > 2035)] <- NA
  
  # Set order
  Cost_Compare_filt$Source <- factor(Cost_Compare_filt$Source, levels=c("AESO","CEC","NREL","Model"))
  
  
  # Plot
  ggplot(Cost_Compare_filt) +
    geom_line(aes(x = Year, y = Amount, colour = Source,linetype= Source), 
              size = 0.75) +
    facet_wrap(~Type, scales = "free_y",
               axes = "all", axis.labels = "all") +
    theme_bw() +
    theme(text=element_text(family=Plot_Text)) +
    theme(axis.text = element_text(color="black"),
          axis.title = element_text(size = GenText_Sz+6,family=Plot_Text_bf),
          axis.text.x = element_text(angle = 45, hjust=1,color="black"),
          plot.title = element_blank(),
          text = element_text(size=GenText_Sz),
          axis.title.x=element_blank(),
          legend.text = element_text(size = GenText_Sz-6),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.caption = element_text(size = GenText_Sz-12,family=Plot_Text,hjust = 1,face="italic"),
          panel.grid.major.y = element_line(size=0.25,linetype=2,color = 'gray85'),
          
          
          strip.placement = "outside",
          strip.text = element_text(size = GenText_Sz-2, color = "black"),
          strip.background = element_rect(colour=NA, fill=NA),
          panel.spacing = unit(1,'lines'),
          
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    labs(y = title_lab, x="Year",caption=cap_add) +
    
    scale_colour_manual(name="Guide1",values = cost_COL,drop = TRUE,limits = force) +
    scale_linetype_manual(name="Guide1",values = cost_line,drop=TRUE,limits = force)+
    
    scale_x_continuous(expand=c(0,0),breaks=seq(2022, 2045, 1)) +
    
    scale_y_continuous(expand=expansion(c(0,0.1)),
                       limits=c(0,NA),
                       breaks = pretty_breaks(12),
                       labels = comma)

}




