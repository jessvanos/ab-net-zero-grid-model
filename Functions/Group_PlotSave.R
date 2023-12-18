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
    height=10,
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
# Gen_Analysis_saveall
# Save variety of plots related to new run
################################################################################

Gen_Analysis_saveall <- function(CaseName) {
  
  GGSave_Loc(CaseName,"Annual Generation (Stacked Area)",Evalyr(BC,"n"),300)
  
}

