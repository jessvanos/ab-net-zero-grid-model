################################################################################
# TITLE: Group_PlotSave
# DESCRIPTION: Save groups of plots to folders.
#
#
# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: November 2023
#
################################################################################

################################################################################
## IMAGE SAVING FUNCTIONS
################################################################################

################################################################################
## FUNCTION: GGSave_Loc
## Saves all plots to a new folder names after case
##
## INPUTS: 
##    CaseName - name for folder. Enter as "folder"
##    FileName - Name for image. Enter as "name"
################################################################################
GGSave_Loc <- function(CaseName,FileName,plotinput,pDPI) {
  
  # Set up folder if it does not exist
  # Create file name
  if (exists('SourceDB')) {
    fold_name<-paste(CaseName,SourceDB)
  }else{
    fold_name<-paste(CaseName)
  }
  
  
  # Check if folder exists, if not, make one
  if (file.exists(here("Figures (Local)",paste(fold_name)))) {
    cat("The folder exists\n")
  } else {
    # Create the folder
    FoldLocation <-
      dir.create(here("Figures (Local)",paste(fold_name)))
  }
  
  # Create file name
  if (exists('SourceDB')) {
    FileName <-paste(FileName,SourceDB)
  }
  
  # Save to a local file 
  ggsave(
    filename = here(paste("Figures (Local)/",paste(fold_name),"/",FileName,".png", sep = "")),
    device = "png",
    plot = plotinput,
    width=14,
    height=10,
    dpi=pDPI)
  
}

################################################################################
## FUNCTION: GGSave_Loc_narrow
## Saves all plots to a new folder names after case
##
## INPUTS: 
##    CaseName - name for folder. Enter as "folder"
##    FileName - Name for image. Enter as "name"
################################################################################
GGSave_Loc_narrow <- function(CaseName,FileName,plotinput,pDPI) {
  
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
  
  # Save to a local file 
  ggsave(
    filename = here(paste("Figures (Local)/",paste(fold_name),"/",FileName,".png", sep = "")),
    device = "png",
    plot = plotinput,
    width=6,
    height=10,
    dpi=pDPI)
  
}

################################################################################
## FUNCTION: GGSave_Loc_wide
## Saves all plots to a new folder names after case
##
## INPUTS: 
##    CaseName - name for folder. Enter as "folder"
##    FileName - Name for image. Enter as "name"
################################################################################
GGSave_Loc_wide <- function(CaseName,FileName,plotinput,pDPI) {
  
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
  
  # Save to a local file 
  ggsave(
    filename = here(paste("Figures (Local)/",paste(fold_name),"/",FileName,".png", sep = "")),
    device = "png",
    plot = plotinput,
    width=14,
    height=6,
    dpi=pDPI)
  
}

################################################################################
## FUNCTION: GGSave_Loc_hourly
## Saves all plots to a new folder names after case, bigger and wider image
##
## INPUTS: 
##    CaseName - name for folder. Enter as "folder"
##    FileName - Name for image. Enter as "name"
################################################################################
GGSave_Loc_hourly <- function(CaseName,FileName,plotinput,pDPI) {
  
  # Set up folder if it does not exist
  fold_name<-paste(CaseName,SourceDB)
  
  # Check if folder exists, if not, make one
  if (file.exists(here("Figures (Local)",paste(fold_name)))) {
    cat("The base folder exists\n")
  } else {
    # Create the folder
    FoldLocation <-
      dir.create(here("Figures (Local)",paste(fold_name)))
  }
  
  # Check if folder exists, if not, make one
  if (file.exists(here("Figures (Local)",paste(fold_name),"/","Hourly Gen/"))) {
    cat("The hourly folder exists\n")
  } else {
    # Create the folder
    FoldLocation <-
      dir.create(here("Figures (Local)",paste(fold_name),"/","Hourly Gen/"))
  }
  
  # Create file name
  FileName <-paste(FileName,SourceDB)
  
  # Save to a local file 
  ggsave(
    filename = here(paste("Figures (Local)/",paste(fold_name),"/Hourly Gen/",FileName,".png", sep = "")),
    device = "png",
    plot = plotinput,
    width=22,
    height=12,
    dpi=pDPI)
  
}

################################################################################
## FUNCTION: GGSave_Loc_Ex
## Saves all plots to additional analysis folder inside image folder.
##
## INPUTS: 
##    CaseName - name for folder. Enter as "folder"
##    FileName - Name for image. Enter as "name"
################################################################################
GGSave_Loc_Ex <- function(CaseName,FileName,plotinput,pDPI) {
  
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
  
  # Save to a local file. Edit here to change image size or quality :)
  ggsave(
    filename = here(paste("Figures (Local)/",paste(fold_name),"/","Additional Analysis/",FileName,".png", sep = "")),
    device = png,
    plot = plotinput,
    width=6.82,
    height=4.9,
    dpi=pDPI)
  
}

################################################################################
## SAVE IMAGES TO FOLDERS IN GROUPS
################################################################################

################################################################################
# Value_saveall
# Save all value plots
# Date is the build date to filter by
# 1 wind, 2- Solar, 3 - Storage, (4,4.1,4.2) - Unabated natural gas, 5- Abated natural gas, 6 - Hydrogen
# 7 - Hydro, 8 - Other, 9 - Cogen
################################################################################

################################################################################
# Value_saveall
# Save all value plots including NPV, annual value, annual value per MWh generated.
# Date is the build date to filter by
# 1 wind, 2- Solar, 3 - Storage, (4,4.1,4.2) - Unabated natural gas, 5- Abated natural gas, 6 - Hydrogen
# 7 - Hydro, 8 - Other, 9 - Cogen
################################################################################
Value_saveall <- function(CaseName) {

# WIND
  
  GGSave_Loc_Ex(CaseName,"NPV Wind",ResValue_NPV(1,1899,BC),300)
  
  GGSave_Loc_Ex(CaseName,"NPV Wind MWh",ResValue_NPV_MWh(1,1899,BC),300)
  
  GGSave_Loc_Ex(CaseName,"Annual Value Line Wind",ResValue_Line(1,1800,BC),300)
  
  GGSave_Loc_Ex(CaseName,"Annual Value Dots Wind",ResValue_Annual(1,1899,BC),300)
  
  GGSave_Loc_Ex(CaseName,"Annual Value Dots Wind MWh",ResValue_Annual_MWh(1,1899,BC),300)
  
  
# SOLAR
  GGSave_Loc_Ex(CaseName,"NPV Solar",ResValue_NPV(2,1899,BC),300)
  
  GGSave_Loc_Ex(CaseName,"NPV Solar MWh",ResValue_NPV_MWh(2,1899,BC),300)
  
  GGSave_Loc_Ex(CaseName,"Annual Value Line Solar",ResValue_Line(2,1800,BC),300)
  
  GGSave_Loc_Ex(CaseName,"Annual Value Dots Solar",ResValue_Annual(2,1899,BC),300)
  
  GGSave_Loc_Ex(CaseName,"Annual Value Dots Solar MWh",ResValue_Annual_MWh(2,1899,BC),300)

# SIMPLE
  GGSave_Loc_Ex(CaseName,"NPV Simple Cycle Gas",ResValue_NPV(4.1,1899,BC),300)
  
  GGSave_Loc_Ex(CaseName,"NPV Simple Cycle Gas MWh",ResValue_NPV_MWh(4.1,1899,BC),300)
  
  GGSave_Loc_Ex(CaseName,"Annual Value Line Simple Cycle Gas",ResValue_Line(4.1,1800,BC),300)
  
  GGSave_Loc_Ex(CaseName,"Annual Value Dots Simple Cycle Gas",ResValue_Annual(4.1,1899,BC),300)
  
  GGSave_Loc_Ex(CaseName,"Annual Value Dots Simple Cycle Gas MWh",ResValue_Annual_MWh(4.1,1899,BC),300)

# COMBINED
  GGSave_Loc_Ex(CaseName,"NPV Combined Cycle Gas",ResValue_NPV(4.2,1899,BC),300)
  
  GGSave_Loc_Ex(CaseName,"NPV Combined Cycle Gas MWh",ResValue_NPV_MWh(4.2,1899,BC),300)
  
  GGSave_Loc_Ex(CaseName,"Annual Value Line Combined Cycle Gas",ResValue_Line(4.2,1800,BC),300)

# CCS
  GGSave_Loc_Ex(CaseName,"NPV CCS Gas",ResValue_NPV(5,1899,BC),300)
  
  GGSave_Loc_Ex(CaseName,"NPV CCS MWh",ResValue_NPV_MWh(5,1899,BC),300)
  
  GGSave_Loc_Ex(CaseName,"Annual Value Line CCS",ResValue_Line(5,1800,BC),300)

# H2
  GGSave_Loc_Ex(CaseName,"NPV Hydrogen",ResValue_NPV(6,1899,BC),300)
  
  GGSave_Loc_Ex(CaseName,"NPV Hydrogen MWh",ResValue_NPV_MWh(6,1899,BC),300)
  
  GGSave_Loc_Ex(CaseName,"Annual Value Line Hydrogen",ResValue_Line(6,1800,BC),300)

# HYDRO
  GGSave_Loc_Ex(CaseName,"NPV Hydro",ResValue_NPV(7,1899,BC),300)
  
  GGSave_Loc_Ex(CaseName,"NPV Hydro MWh",ResValue_NPV_MWh(7,1899,BC),300)
  
  GGSave_Loc_Ex(CaseName,"Annual Value Line Hydro",ResValue_Line(7,1800,BC),300)

# OTHER
  GGSave_Loc_Ex(CaseName,"NPV Other",ResValue_NPV(8,1899,BC),300)
  
  GGSave_Loc_Ex(CaseName,"NPV Other MWh",ResValue_NPV_MWh(8,1899,BC),300)
  
  GGSave_Loc_Ex(CaseName,"Annual Value Line Other",ResValue_Line(8,1800,BC),300)

# COGEN
  GGSave_Loc_Ex(CaseName,"NPV Cogen Gas",ResValue_NPV(9,1899,BC),300)
  
  GGSave_Loc_Ex(CaseName,"NPV Cogen MWh",ResValue_NPV_MWh(9,1899,BC),300)
  
  GGSave_Loc_Ex(CaseName,"Annual Value Line cogen",ResValue_Line(9,1800,BC),300)
}

################################################################################
# Slack_saveall
# Save plots to compare available units and built units in capacity expansion.
# ("WND", "SUN","GasCCS","BIO","Gas1","Gas2","H2","UR","PS")
################################################################################

Slack_saveall <- function(CaseName) {

# WIND
  GGSave_Loc_Ex(CaseName,"Res Slack Wind",BuildUnits(BC, "WND"),300)

# SUN
  GGSave_Loc_Ex(CaseName,"Res Slack Solar",BuildUnits(BC, "SUN"),300)


# CCS
  GGSave_Loc_Ex(CaseName,"Res Slack All CCS",BuildUnits(BC, "GasCCS"),300)

# CCS retrofits
  GGSave_Loc_Ex(CaseName,"Res Slack CCS Retrofit",Build_CCSRet(BC),300)
  GGSave_Loc_Ex(CaseName,"Units Retrofited",Build_CCSRet2(BC),300)

# BIOMASS
  GGSave_Loc_Ex(CaseName,"Res Slack Biomass",BuildUnits(BC, "OT"),300)

# SIMPLE
  GGSave_Loc_Ex(CaseName,"Res Slack Simple Cycle",BuildUnits(BC, "Gas2"),300)

# COMBINED
  GGSave_Loc_Ex(CaseName,"Res Slack Combined Cycle",BuildUnits(BC, "Gas1"),300)

# H2
  GGSave_Loc_Ex(CaseName,"Res Slack Hydrogen",BuildUnits(BC, "H2"),300)

# Nuclear
  GGSave_Loc_Ex(CaseName,"Res Slack Nuclear",BuildUnits(BC, "UR"),300)

# Storage
  GGSave_Loc_Ex(CaseName,"Res Slack Storage",BuildUnits(BC, "PS"),300)
}

################################################################################
# Analysis_saveall
# Save variety of plots related to new run
################################################################################

Analysis_saveall <- function(CaseName) {
  
  # Annual Generation
  GGSave_Loc(CaseName,"Annual Generation (Stacked Area)",Evalyr(BC,"n"),300)
  
  # Annual Capacity
  GGSave_Loc(CaseName,"Annual Capacity (Stacked Area)",Evalcap(BC,"n"),300)
  
  # Percent gen
  GGSave_Loc(CaseName,"Annual Generation (Perc)",EvalPerc(BC,"n"),300)
  
  # Output bar
  GGSave_Loc(CaseName,"Annual Generation (Bar Chart)",Output_Comp(BC),300)
  
  # Annual average capacity factors for all resource types
  GGSave_Loc(CaseName,"Annual Capacity Factors",CF_Annual(BC),300)
  
  # Annual average capacity factors for all resource types
  GGSave_Loc(CaseName,"Capacity Factors 2023 and 2043",CFcompare(2023,2043,BC),300)
  
  # Wind duration curves
  GGSave_Loc(CaseName,"Wind Load Duration Curve",Wind_Dur(BC),300)
  GGSave_Loc(CaseName,"Wind Capacity Factor Duration Curve",Wind_DurNorm(BC),300)

  # Ridgreline plots
  GGSave_Loc(CaseName,"Wind Ridgelines",Resource_Ridge_w("LTO_Wind",2023,2045,2,BC),300)
  GGSave_Loc(CaseName,"Solar Ridgelines",Resource_Ridge_w("LTO_Solar",2023,2045,5,BC),300)
  
  # Difference in capacity
  GGSave_Loc(CaseName,"Capacity Changes",TotalCapChange(BC),300)
  
  # Net difference in capacity
  GGSave_Loc(CaseName,"Net Capacity Changes",Eval_CapChange(BC),300)
  
  # Combined cycle study fate by resource
  GGSave_Loc(CaseName,"Combined Cycle Gas Fate",CC_Fate_study(BC),300)
  GGSave_Loc(CaseName,"Combined Cycle Gas Annual Cap",CC_Fate_year(BC),300)

  # Shows Prices for simulation duration
  GGSave_Loc(CaseName,"Price Duration Curve",Sim_dur(BC),300)
  
  
  # Retirements by capacity (grouped by fuel type)
  GGSave_Loc_wide(CaseName,"Retirements",RetireMW(BC),350)

  # All new capacity
  GGSave_Loc_wide(CaseName,"Additions",BuildMW(BC),350)
  
  # Shows Prices for simulation duration
  GGSave_Loc_narrow(CaseName,"Price Duration Curve Avg only",Sim_dur_avg(BC),300)
  
  # Shows production costs and fixed costs for full system
  GGSave_Loc(CaseName,"Total System Cost",System_Cost(BC),300)

  # Average monthly prices over full period
  GGSave_Loc_wide(CaseName,"Monthly Pool Prices",AvgMn_price(BC),300)
  
  # Average annual pool price
  GGSave_Loc(CaseName,"Average Annual Pool Prices",AvgYr_poolprice(BC),300)
  
  #Capture Prices
  GGSave_Loc(CaseName,"Capture Prices",capture_p(2023,2030,BC),300)
  
  # Relative capture prices
  GGSave_Loc(CaseName,"Relative Capture Prices",Relcapture_p(2023,2035,BC),300)

  # Premeium to pool price
  GGSave_Loc(CaseName,"Achived Premium to Pool Price",ach_poolprem(BC),300)
  
  # Annual emissions in stacked area chart
  GGSave_Loc(CaseName,"Annual Emissions (Bar) - NAICS",AnnualEmStackCol(BC,"NAICS"),300)
  GGSave_Loc(CaseName,"Annual Emissions (Bar) - ALL",AnnualEmStackCol(BC,"ALL"),300)
  GGSave_Loc(CaseName,"Annual Emissions (Bar) - no Cogen",AnnualEmStackCol(BC,"none"),300)
  
  # Annual emissions in individual lines
  GGSave_Loc(CaseName,"Annual Emissions (Line)",AnnualEmLine(BC,"ALL"),300)
  GGSave_Loc(CaseName,"Annual Emissions (Line)",AnnualEmLine(BC,"none"),300)
  
  # Import Export
  GGSave_Loc(CaseName,"Annual Imports and Exports",Imp_Exp1(BC),300)
  
  # Shows demand in AB 
  GGSave_Loc(CaseName,"Annual Demand",AnnualDemand(ZoneMn,BC),300)
  
  # Curtailment
  #GGSave_Loc(CaseName,"Max Curtail",MaxCurtail(BC),300)
  GGSave_Loc(CaseName,"Renewable Curtailment MWa",Renew_Curtail_MWa(BC),300)
  GGSave_Loc(CaseName,"Renewable Curtailment CF",Renew_Curtail_perc(BC),300)

  
  # Daily gen
  GGSave_Loc(CaseName,"Daily Output - Season 2040",CompDay_Season(2040,14,BC),300)
  GGSave_Loc(CaseName,"Daily Output - Max Wind 2040",CompDay_Wind(2040,BC),300)
  GGSave_Loc(CaseName,"Daily Output - Max Solar 2040",CompDay_Solar(2040,BC),300)
  GGSave_Loc(CaseName,"Daily Output Nov- Years",CompDay_Years(2023,2043,11,10,BC),300)
  
}

################################################################################
# Detail_Gen_save
# Save detailed year of weeks and daily generation plots.
################################################################################
Detail_Gen_save <- function(CaseName) {
  
  print('Starting plot generation, this may take a few minutes')
  
  # Year of weeks plots
  GGSave_Loc_hourly(CaseName,"2023 Hourly Generation for One Week (Stacked Area)",year_weeks(2023,BC),100)
  print('Done weekly 2023')
  GGSave_Loc_hourly(CaseName,"2025 Hourly Generation for One Week (Stacked Area)",year_weeks(2025,BC),100)
  print('Done weekly 2025')
  GGSave_Loc_hourly(CaseName,"2030 Hourly Generation for One Week (Stacked Area)",year_weeks(2030,BC),100)
  print('Done weekly 2030')
  GGSave_Loc_hourly(CaseName,"2035 Hourly Generation for One Week (Stacked Area)",year_weeks(2035,BC),100)
  print('Done weekly 2035')
  GGSave_Loc_hourly(CaseName,"2040 Hourly Generation for One Week (Stacked Area)",year_weeks(2040,BC),100)
  print('Done weekly 2040')
  GGSave_Loc_hourly(CaseName,"2043 Hourly Generation for One Week (Stacked Area)",year_weeks(2043,BC),100)
  print('Done weekly 2043')
  
  # Four month generation and pool price plots
  GGSave_Loc_hourly(CaseName,"2023 4 month sum (Stacked Area + price)",FourMonthSummary(2023,01,04,07,10,BC),100)
  print('Done 4 month 2023')
  GGSave_Loc_hourly(CaseName,"2030 4 month sum (Stacked Area + price)",FourMonthSummary(2030,01,04,07,10,BC),100)
  print('Done 4 month 2030')
  GGSave_Loc_hourly(CaseName,"2040 4 month sum (Stacked Area + price)",FourMonthSummary(2040,01,04,07,10,BC),100)
  print('Done 4 month 2040')
  
  # imports
  GGSave_Loc_hourly(CaseName,"Hourly import export 2023",Imp_Exp2(2023,BC),100)
  GGSave_Loc_hourly(CaseName,"Hourly import export 2030",Imp_Exp2(2030,BC),100)
  GGSave_Loc_hourly(CaseName,"Hourly import export 2040",Imp_Exp2(2040,BC),100)
  
}

################################################################################
# CER_saveall
# Save variety of plots related to CER contraints
################################################################################

CER_saveall <- function(CaseName) {

  # CER Resource capacity factors
  GGSave_Loc(CaseName,"CER Plant Capacity Factors",CF_CER_Res(BC),300)
  
  # CER group capacity factors
  GGSave_Loc(CaseName,"CER Capacity Factors by Year Applied",CF_CER_groups(BC),300)
  
  # Hours run and emissions for CER resources
  GGSave_Loc_wide(CaseName,"CER Plant Capacity Factors and Emissions",CER_EM_CF_Res(case),300)
  
  # Capacity factor and emissions for CER resources grouped by year applied
  GGSave_Loc_wide(CaseName,"CER Capacity Factors and Emissions by Year Applied",CER_EM_hour_group(case),300)
  
  
}


