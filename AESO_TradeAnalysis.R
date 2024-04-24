################################################################################
# TITLE: AESO_Analysis
# DESCRIPTION:  Script imports data and analyses intertie behavior. includes heat rate analysis and link capabaility analysis.


# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: August 2022; LAST EDIT: August 4, 2022

# NOTES: Make sure the project file is open first or "here" commands wont work right.
#        Before running, create folder called "Data Files" withen project directory and populate it with AESO data. 
#        Once this file is run through completion, can call any functions with environment that is loaded.

################################################################################
## LOAD REQUIRED PACKAGES AND SOURCE FUNCTIONS

{# Package Info
  # tidyverse: Data science package
  # ggplot: Used for graphical packages and aestheticc
  # grid: Used for plotting, adds grid to the plot
  # gtable: Grob tables, more tools
  # gridExtra: User functions for grid graphics
  # odbc: Driver for Database Loading
  # ggpubr:
  # DBI: Package for interface between database and R
  # lubridate: Allow time and data manipulation
  # cowplot: Quality features for ggplots
  # scales: Graphical mapping stuff
  # dplyr: Data manipulation package
  # reshape2:
  # zoo: Used for time series indexing
  # ggpattern: Geoms for ggplot2
  # here: Package to set filepaths inside R project
  # beepr: Allows sound to paly when code is done
  # showtext: Allows fonts changes in ggplot
}

{ # Must load the here package in order to make sure internal project directories work
  library(here)
  
  # Import functions from files, take from the functions folder in R project
  source(here('Functions','other_functions.R'))
  source(here('Functions','Intertie_Functions.R'))    # Intertie Plots

  # Packages required
  packs_to_load = c("tidyverse","ggplot2","grid","gtable","gridExtra","odbc","ggpubr",
                    "DBI","lubridate","cowplot","scales","dplyr","reshape2","zoo",
                    "ggpattern","here","beepr","showtext","DescTools","pivottabler",
                    "openxlsx","Hmisc")
  # Function to check for packages, install if not present, and load
  packs_check(packs_to_load)
  
}
################################################################################
## GENERAL SET UP
# Available Fonts for plotting, can choose different one and change Plot_Text if needed
# Uses local computer font files (search font in search bar to confirm font names)
  
  font_add(family="Times",regular="times.ttf")
  Plot_Text <- 'Times'
  
  # Years to show in duration curve
  Years2See <- c(2010,2012,2014,2016,2018,2020,2022)

################################################################################
## PART 1: INTERTIE BEHAVIOR AND HEAT RATE
################################################################################  
  
################################################################################
## LOAD FROM >R FILE INTO WORKSPACE

  Imp_Exp <- readRDS(here("Data Files","AESO_IMP_EXP_edit.RData")) 
  
 { Imp_Exp$date <- as.POSIXct(Imp_Exp$Date_Begin_Local,tz="",format="%m/%d/%Y %H:%M")
  
  Imp_Exp<- Imp_Exp %>%
    select(.,-c("Date_Begin_GMT","DAY_AHEAD_POOL_PRICE"))
  
  #Reformat Day as day of year
  Imp_Exp$Day <- format(Imp_Exp$date,"%j")
  Imp_Exp$Week <- format(Imp_Exp$date,"%W") }
  
################################################################################
## LOAD FROM >R FILE INTO WORKSPACE
  
  HRcalc <- readRDS(here("Data Files","HRcalc.RData")) 
  
  { HRcalc$date <- as.POSIXct(HRcalc$Date_Begin_Local,tz="",format="%Y-%m-%d %H:%M")
    
    HRcalc<- HRcalc %>%
      select(.,-c("DAY_AHEAD_POOL_PRICE")) }
  
    #Replace all NA values with zero
  HRcalc[HRcalc==0] <- NA
    
    #Reformat Day as day of year
    HRcalc$Day <- format(HRcalc$date,"%j")
    HRcalc$Week <- format(HRcalc$date,"%W") 
    HRcalc$Month2 <- format(HRcalc$date,"%b")
  
################################################################################
## LOAD FROM EXCELL SHEET AND WRITE TO .R FILE
    # HRcalc <- read_csv("IMPEXP_HRcalcs.csv")
  # 
  # 
  # #Replease all NA values with zero
  # Imp_Exp[is.na(Imp_Exp)] <- 0
  # 
  # Imp_Exp <- Imp_Exp %>%
  #   mutate(EXPORT_BC_MT=EXPORT_BC+EXPORT_MT) %>%
  #   rename(IMPORT_BC_MT=BC_MT)
  # 
  # 
   # saveRDS(HRcalc, file = here("HRcalc.RData"))

################################################################################
## LOAD AESO TRADE INFO FROM R FILE INTO WORKSPACE (OPTIONAL)
################################################################################
  
# HRcalc <- readRDS(here("Data Files","HRcalc.RData")) 
# 
# { HRcalc$date <- as.POSIXct(HRcalc$Date_Begin_Local,tz="",format="%Y-%m-%d %H:%M")
#   
#   HRcalc<- HRcalc %>%
#     select(.,-c("DAY_AHEAD_POOL_PRICE")) 
# 
# #Replace all NA values with zero
# HRcalc[HRcalc==0] <- NA
# 
# #Reformat Day as day of year
# HRcalc$Day <- format(HRcalc$date,"%j")
# HRcalc$Week <- format(HRcalc$date,"%W") 
# HRcalc$Month2 <- format(HRcalc$date,"%b")
# }
    
################################################################################
## CORELATION
## Pearson corelation - looks at linear corelation between variables
## Check price and import/export correlation
# 
# # Peek at data
#   head(Imp_Exp)
#   
#   # Defualt Pearson Corelation test (2 sided)
#   cor.test(Imp_Exp$ACTUAL_POOL_PRICE,Imp_Exp$IMPORT_BC_MT)
#   
#   # Give Each Season a numerical Value 
#   Imp_Exp <-Imp_Exp %>%
#     mutate(Season2)
#     
#     Imp_Exp$Season2[Imp_Exp$Season=="Winter"] <-1
#     Imp_Exp$Season2[Imp_Exp$Season=="Spring"] <-2
#     Imp_Exp$Season2[Imp_Exp$Season=="Summer"] <-3
#     Imp_Exp$Season2[Imp_Exp$Season=="Fall"] <-4
#   
#   #Create set of varibales intrested in
#   set <-c("ACTUAL_POOL_PRICE","ACTUAL_AIL","Season2","Month","IMPORT_BC_MT","EXPORT_BC_MT","IMPORT_SK","EXPORT_SK")
#   
#   # New dataframe with just these filtered out
#   matrix <- Imp_Exp[set]
#   matrix2 <-rcorr(as.matrix(matrix),type="pearson")
#   print(matrix2)
#   
#   plot(Imp_Exp$IMPORT_BC_MT,Imp_Exp$Month)
#   abline(lm(Imp_Exp$ACTUAL_POOL_PRICE~Imp_Exp$IMPORT_BC_MT))
#   
#   chart.Correlation(matrix)


################################################################################
## FILTER DATA FOR TABLES OF AVERAGES
  ## DATA FILTER: BC_MT

  IE_BC <-Imp_Exp%>%
    select(.,c("date","Day","Week","Month","Season","Year","ACTUAL_AIL","ACTUAL_POOL_PRICE","IMPORT_BC_MT","EXPORT_BC_MT"))%>%
    mutate(Month=month.name[Month]) %>%
    filter(Year<2022)

  # Now, filter out the hours where import happened only 
  IE_BC2 <- IE_BC %>%
    filter(IMPORT_BC_MT>0) 
  
 ## DATA FILTER: SK

  IE_SK <-Imp_Exp%>%
    select(.,c("date","Day","Week","Month","Season","Year","ACTUAL_AIL","ACTUAL_POOL_PRICE","IMPORT_SK","EXPORT_SK"))%>%
    mutate(Month=month.name[Month]) %>%
    filter(Year<2022)
  
  # Now, filter out the hours where import happened only 
  IE_SK2 <- IE_SK %>%
    filter(IMPORT_SK>0)
  
################################################################################
## MONTHLY AVERAGES 
  # Create Table that gives monthly average pool price for imports
  pt1 <- PivotTable$new() 
  { pt1$addData(IE_BC2)
    pt1$addColumnDataGroups("Month", addTotal=FALSE)
    pt1$addRowDataGroups("Year", addTotal=FALSE) 
    pt1$defineCalculation(calculationName="MeanPrice", caption="Mean Price", 
                          summariseExpression="mean(ACTUAL_POOL_PRICE)", 
                          format="%.2f") 
    pt1$evaluatePivot()
    pt1$renderPivot() # Display in viewer
  }
  
  pt2 <- PivotTable$new() 
  { pt2$addData(IE_SK2)
    pt2$addColumnDataGroups("Month", addTotal=FALSE)
    pt2$addRowDataGroups("Year", addTotal=FALSE) 
    pt2$defineCalculation(calculationName="MeanPrice", caption="Mean Price", 
                         summariseExpression="mean(ACTUAL_POOL_PRICE)", 
                         format="%.2f")    
    pt2$evaluatePivot()
    pt2$renderPivot() # Display in viewer
  }
  
################################################################################
  ## SEASONAL AVERAGES

  ## SEASONAL DATA ANALYSIS: bc
  pt3 <- PivotTable$new() 
  { pt3$addData(IE_BC2)
    pt3$addColumnDataGroups("Season", addTotal=FALSE)
    pt3$addRowDataGroups("Year", addTotal=FALSE) 
    pt3$defineCalculation(calculationName="MeanPrice", caption="Mean Price", 
                         summariseExpression="mean(ACTUAL_POOL_PRICE)", 
                         format="%.2f")    
    pt3$evaluatePivot()
    pt3$renderPivot() # Display in viewer
  }
  
  ## SEASONAL DATA ANALYSIS: SK
    pt4 <- PivotTable$new() 
  { pt4$addData(IE_SK2)
    pt4$addColumnDataGroups("Season", addTotal=FALSE)
    pt4$addRowDataGroups("Year", addTotal=FALSE) 
    pt4$defineCalculation(calculationName="MeanPrice", caption="Mean Price", 
                         summariseExpression="mean(ACTUAL_POOL_PRICE)", 
                         format="%.2f")    
    pt4$evaluatePivot()
    pt4$renderPivot() # Display in viewer
    }
    
################################################################################
## DAILY AVERAGES
    
    ## SEASONAL DATA ANALYSIS: bc
    pt5 <- PivotTable$new() 
    { pt5$addData(IE_BC2)
      pt5$addColumnDataGroups("Day", addTotal=FALSE)
      pt5$addRowDataGroups("Year", addTotal=FALSE) 
      pt5$defineCalculation(calculationName="MeanPrice", caption="Mean Price", 
                            summariseExpression="mean(ACTUAL_POOL_PRICE)", 
                            format="%.2f")    
      pt5$evaluatePivot()
      pt5$renderPivot() # Display in viewer
    }
    
    ## SEASONAL DATA ANALYSIS: SK
    pt6 <- PivotTable$new() 
    { pt6$addData(IE_SK2)
      pt6$addColumnDataGroups("Day", addTotal=FALSE)
      pt6$addRowDataGroups("Year", addTotal=FALSE) 
      pt6$defineCalculation(calculationName="MeanPrice", caption="Mean Price", 
                            summariseExpression="mean(ACTUAL_POOL_PRICE)", 
                            format="%.2f")    
      pt6$evaluatePivot()
      pt6$renderPivot() # Display in viewer
    }
    
################################################################################
## WEEK AVERAGES
    
    ## WEEK DATA ANALYSIS: bc
    pt7 <- PivotTable$new() 
    { pt7$addData(IE_BC2)
      pt7$addColumnDataGroups("Week", addTotal=FALSE)
      pt7$addRowDataGroups("Year", addTotal=FALSE) 
      pt7$defineCalculation(calculationName="MeanPrice", caption="Mean Price", 
                            summariseExpression="mean(ACTUAL_POOL_PRICE)", 
                            format="%.2f")    
      pt7$evaluatePivot()
      pt7$renderPivot() # Display in viewer
    }
    
    ## WEEK DATA ANALYSIS: SK
    pt8 <- PivotTable$new() 
    { pt8$addData(IE_SK2)
      pt8$addColumnDataGroups("Week", addTotal=FALSE)
      pt8$addRowDataGroups("Year", addTotal=FALSE) 
      pt8$defineCalculation(calculationName="MeanPrice", caption="Mean Price", 
                            summariseExpression="mean(ACTUAL_POOL_PRICE)", 
                            format="%.2f")    
      pt8$evaluatePivot()
      pt8$renderPivot() # Display in viewer
    }
    
################################################################################
## HR SUMMARIES
  
# Want to find mean HR in each month for all data

    BC_I_HR <- HRcalc %>%
      filter(ACTUAL_POOL_PRICE<500) %>%
      group_by(Year,Month)%>%
      summarise(across(T_HR_IMPORT_BC, median, na.rm = TRUE)) %>%
      rename(Med_HR=T_HR_IMPORT_BC) %>%
      ungroup() %>%
      group_by(Month)%>%
      summarise(across(Med_HR, mean, na.rm = TRUE)) %>%
      mutate_if(is.numeric, round, 0) %>%
      ungroup() %>%
      mutate(ID="BC_AB")
    
    BC_E_HR <- HRcalc %>%
      filter(ACTUAL_POOL_PRICE<500) %>%
      group_by(Year,Month)%>%
      summarise(across(T_HR_EXPORT_BC, median, na.rm = TRUE)) %>%
      rename(Med_HR=T_HR_EXPORT_BC) %>%
      ungroup() %>%
      group_by(Month)%>%
      summarise(across(Med_HR, mean, na.rm = TRUE)) %>%
      mutate_if(is.numeric, round, 0) %>%
      ungroup() %>%
      mutate(ID="AB_BC")
    
    SK_I_HR <- HRcalc %>%
      filter(ACTUAL_POOL_PRICE<500) %>%
      group_by(Year,Month)%>%
      summarise(across(T_HR_IMPORT_SK, median, na.rm = TRUE)) %>%
      rename(Med_HR=T_HR_IMPORT_SK) %>%
      ungroup() %>%
      group_by(Month)%>%
      summarise(across(Med_HR, mean, na.rm = TRUE)) %>%
      mutate_if(is.numeric, round, 0) %>%
      ungroup() %>%
      mutate(ID="SK_AB")
    
    SK_E_HR <- HRcalc %>%
      filter(ACTUAL_POOL_PRICE<500) %>%
      group_by(Year,Month)%>%
      summarise(across(T_HR_EXPORT_SK, median, na.rm = TRUE)) %>%
      rename(Med_HR=T_HR_EXPORT_SK) %>%
      ungroup() %>%
      group_by(Month)%>%
      summarise(across(Med_HR, mean, na.rm = TRUE)) %>%
      mutate_if(is.numeric, round, 0) %>%
      ungroup() %>%
      mutate(ID="AB_SK")
    
   NewData <- rbind(BC_I_HR,BC_E_HR,SK_I_HR,SK_E_HR)
   
   NewData$Month <- month.name[as.numeric(NewData$Month)]
    
   ptHR <- PivotTable$new() 
   { ptHR$addData(NewData)
     ptHR$addColumnDataGroups("Month", addTotal=FALSE)
     ptHR$addRowDataGroups("ID", addTotal=FALSE) 
     ptHR$defineCalculation(calculationName="HR", caption="HR", 
                           summariseExpression="max(Med_HR)", 
                           format="%.0f")    
     ptHR$evaluatePivot()
     ptHR$renderPivot() # Display in viewer
   }
   
   # Want to find mean HR in each month for all data
   
  BC_I_HR <- HRcalc %>%
     filter(ACTUAL_POOL_PRICE<500) %>%
     group_by(Year,Month)%>%
     summarise(across(T_HR_IMPORT_BC, median, na.rm = TRUE)) %>%
     rename(Med_HR=T_HR_IMPORT_BC) %>%
     mutate_if(is.numeric, round, 0) %>%
     ungroup() %>%
     mutate(ID="BC_AB")
   
   BC_E_HR <- HRcalc %>%
     filter(ACTUAL_POOL_PRICE<500) %>%
     group_by(Year,Month)%>%
     summarise(across(T_HR_EXPORT_BC, median, na.rm = TRUE)) %>%
     rename(Med_HR=T_HR_EXPORT_BC) %>%
     mutate_if(is.numeric, round, 0) %>%
     ungroup() %>%
     mutate(ID="AB_BC")
   
   SK_I_HR <- HRcalc %>%
     filter(ACTUAL_POOL_PRICE<500) %>%
     group_by(Year,Month)%>%
     summarise(across(T_HR_IMPORT_SK, median, na.rm = TRUE)) %>%
     rename(Med_HR=T_HR_IMPORT_SK) %>%
     mutate_if(is.numeric, round, 0) %>%
     ungroup() %>%
     mutate(ID="SK_AB")
   
   SK_E_HR <- HRcalc %>%
     filter(ACTUAL_POOL_PRICE<500) %>%
     group_by(Year,Month)%>%
     summarise(across(T_HR_EXPORT_SK, median, na.rm = TRUE)) %>%
     rename(Med_HR=T_HR_EXPORT_SK) %>%
     mutate_if(is.numeric, round, 0) %>%
     ungroup() %>%
     mutate(ID="AB_SK")
   
   NewData <- rbind(BC_I_HR,BC_E_HR,SK_I_HR,SK_E_HR)
   
   NewData$Month <- month.name[as.numeric(NewData$Month)]
   
   ptHR <- PivotTable$new() 
   { ptHR$addData(NewData)
     ptHR$addColumnDataGroups("Month", addTotal=FALSE)
     ptHR$addRowDataGroups("ID", addTotal=FALSE) 
     ptHR$addRowDataGroups("Year", addTotal=FALSE) 
     ptHR$defineCalculation(calculationName="HR", caption="HR", 
                            summariseExpression="max(Med_HR)", 
                            format="%.0f")    
     ptHR$evaluatePivot()
     ptHR$renderPivot() # Display in viewer
   } 
   
   # Now, each year
   
   BC_I_HR2 <- HRcalc %>%
     filter(ACTUAL_POOL_PRICE<500) %>%
     group_by(Year)%>%
     summarise(across(T_HR_IMPORT_BC, mean, na.rm = TRUE)) %>%
     ungroup() %>%
     rename(Mean_HR=T_HR_IMPORT_BC) %>%
     mutate_if(is.numeric, round, 0) %>%
     mutate(ID="BC_AB")
   
   BC_E_HR2 <- HRcalc %>%
     filter(ACTUAL_POOL_PRICE<500) %>%
     group_by(Year)%>%
     summarise(across(T_HR_EXPORT_BC, mean, na.rm = TRUE)) %>%
     ungroup() %>%
     rename(Mean_HR=T_HR_EXPORT_BC) %>%
     mutate_if(is.numeric, round, 0) %>%
     mutate(ID="AB_BC")
   
   SK_I_HR2 <- HRcalc %>%
     filter(ACTUAL_POOL_PRICE<500) %>%
     group_by(Year)%>%
     summarise(across(T_HR_IMPORT_SK, mean, na.rm = TRUE)) %>%
     ungroup() %>%
     rename(Mean_HR=T_HR_IMPORT_SK) %>%
     mutate_if(is.numeric, round, 0) %>%
     mutate(ID="SK_AB")
   
   SK_E_HR2 <- HRcalc %>%
     filter(ACTUAL_POOL_PRICE<500) %>%
     group_by(Year)%>%
     summarise(across(T_HR_EXPORT_SK, mean, na.rm = TRUE)) %>%
     ungroup() %>%
     rename(Mean_HR=T_HR_EXPORT_SK) %>%
     mutate_if(is.numeric, round, 0) %>%
     mutate(ID="AB_SK")
   
   NewData <- rbind(BC_I_HR2,BC_E_HR2,SK_I_HR2,SK_E_HR2)
   
   ptHR <- PivotTable$new() 
   { ptHR$addData(NewData)
     ptHR$addColumnDataGroups("Year", addTotal=FALSE)
     ptHR$addRowDataGroups("ID", addTotal=FALSE) 
     ptHR$defineCalculation(calculationName="HR", caption="HR", 
                            summariseExpression="mean(Mean_HR)", 
                            format="%.0f")    
     ptHR$evaluatePivot()
     ptHR$renderPivot() # Display in viewer
   }
   
###############################################################################
   
   # Intertie cap 
   
   BC_I_HR <- HRcalc %>%
     filter(IMPORT_BC_MT>10) %>%
     mutate(Perc=IMPORT_BC_MT/1100) %>%
     group_by(Month)%>%
     summarise(across(Perc, max, na.rm = TRUE)) %>%
     mutate_if(is.numeric, round, 5) %>%
     ungroup() %>%
     mutate(ID="BC_AB")
   
   BC_E_HR <- HRcalc %>%
     filter(EXPORT_BC_MT>10) %>%
     mutate(Perc=EXPORT_BC_MT/1100) %>%
     group_by(Month)%>%
     summarise(across(Perc, max, na.rm = TRUE)) %>%
     mutate_if(is.numeric, round, 5) %>%
     ungroup() %>%
     mutate(ID="AB_BC")
   
   SK_I_HR <- HRcalc %>%
     filter(IMPORT_SK>10) %>%
     mutate(Perc=IMPORT_SK/153) %>%
     group_by(Month)%>%
     summarise(across(Perc, max, na.rm = TRUE)) %>%
     mutate_if(is.numeric, round, 5) %>%
     ungroup() %>%
     mutate(ID="SK_AB")
   
   SK_E_HR <- HRcalc %>%
     filter(EXPORT_SK>10) %>%
     mutate(Perc=EXPORT_SK/153) %>%
     group_by(Month)%>%
     summarise(across(Perc, max, na.rm = TRUE)) %>%
     mutate_if(is.numeric, round, 5) %>%
     ungroup() %>%
     mutate(ID="AB_SK")
   
   NewData <- rbind(BC_I_HR,BC_E_HR,SK_I_HR,SK_E_HR)
   
   NewData$Month <- month.name[as.numeric(NewData$Month)]
   
   ptCap <- PivotTable$new() 
   { ptCap$addData(NewData)
     ptCap$addColumnDataGroups("Month", addTotal=FALSE)
     ptCap$addRowDataGroups("ID", addTotal=FALSE) 
     ptCap$defineCalculation(calculationName="HR", caption="HR", 
                            summariseExpression="max(Perc)", 
                            format="%.5f")    
     ptCap$evaluatePivot()
     ptCap$renderPivot() # Display in viewer
   }
   

################################################################################
## FUNCTION CALL AESO
    #AESO Duration Curve
    Duration_AESO(Years2See)
    
    #Full year of trade
    Trade_Yr_AESO(2016)
    
    #Month of trade
    Trade_Mn_AESO(2021,01,Imp_Exp)
    TradeOnly_Mn_AESO(2021,12,Imp_Exp)
                  
    T_month_all(01)
    T_month_all(02)
    T_month_all(03)
    T_month_all(04)
    T_month_all(05)
    T_month_all(06)
    T_month_all(07)
    T_month_all(08)
    T_month_all(09)
    T_month_all(10)
    T_month_all(11)
    T_month_all(12)
    
################################################################################
## FUNCTION CALL HR    
    HR_month_SK_all(04)
    HR_month_BC_all(12)
    
    HR_year_SK_all()
    HR_year_BC_all()
    
    
################################################################################
## PART 2: LINK SHAPING (OPTIONAL)
## Functions found in "Intertie Functions" file
################################################################################
    
###############################################################################################
# 2.A Load the ITC data
###############################################################################################
    # Load the ITC data
    {  load(here('Data Files','Alberta Data',"aeso_itc_data.RData"))
      
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
      # Get day (1-365)
      ITC$Day <- format(ITC$date,"%j")
      
      # Combine date and time, remove white space
      ITC$Hr <-paste((ITC$he),":00:00")
      ITC$Hr <- gsub(" ", "", ITC$Hr, fixed = TRUE)
      
      # Full date
      ITC$Fdate <- as.POSIXct(paste(ITC$Date,ITC$Hr), format="%Y-%m-%d %H:%M:%S",tz='MST')
      
      # Re-order the columns and rename them
      ITC2 <- ITC[, c(15, 1,12, 13, 2, 3:4,9:10)]
      names(ITC2) <- c('Fdate','date',"Year",'Day','he','SKImp_c','SKExp_c','BCMTExp_c','BCMTImp_c')
      
      # For easy later Reference
      { SKI <- 'SKImp_c'
        SKE <- 'SKExp_c'
        BCI <- 'BCMTImp_c'
        BCE <- 'BCMTExp_c'
      }
    }  
    
###############################################################################################
## 2.B Print ITC data to excel
############################################################################################### 
    YEAR2PRINT <- 2018
    
    AuroraData <- ITC2 %>%
      filter(Year==YEAR2PRINT) %>%
      select(.,c('date','he','SKImp_c','SKExp_c','BCMTExp_c','BCMTImp_c')) 
    
    names(AuroraData)<-c('Date','Hour','SKImp','SKExp','BCMTExp','BCMTImp')
    
    AuroraData$Date <- format(AuroraData$Date,"%m/%d/%Y")
    write_xlsx(AuroraData,here("Data Files",'Alberta Data',"Capabilitydata2018.xlsx"))
    
###############################################################################################
## 2.C ITC Capability Plots and Stats
###############################################################################################  
    allyrs <- c(2017,2018,2019,2020,2021)
    
    # Percentage of zero trade hours
    ZeroTrade(SKE,ITC2,c(allyrs))
    ZeroTrade(SKI,ITC2,c(allyrs))
    ZeroTrade(BCE,ITC2,c(allyrs))
    ZeroTrade(BCI,ITC2,c(allyrs))
    
    Capab_yr(SKE,2018,2018)
    Capab_yr(SKI,2018,2018)
    Capab_yr(BCE,2016,2022)
    Capab_yr(BCI,2018,2018)
    
    # Choose between "Avg", "Max", "Min" and get monthly capability
    Capab_Allmn(SKE,"Avg")
    Capab_Allmn(SKI,"Avg")
    Capab_Allmn(BCE,"Avg")
    Capab_Allmn(BCI,"Avg")
    
    # Give stats for timeperiod selected and QQ plots
    Capab_Stats(ITC2,2017,2021)  
    