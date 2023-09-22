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
## FUNCTION: imsave_git & imsave_loc
## Save most recent plot to git folder or local folder with transparent background.
##
## INPUTS: 
##    name - Date to plot, the week will start on the day chosen. Enter as "name"
################################################################################
# Save to a git folder
imsave_git <- function(name) {
  ggsave(plot=last_plot(),path = here("Figures"), 
         filename = paste(name,".png", sep = ""),
         width = 12, height=8, units=c("cm"),dpi=300, bg = "transparent")  }

# Save to a loacl folder that is ignored by git
imsave_loc <- function(name) {
  ggsave(path = here("Figures (Local)"), 
         filename = paste(name,".png", sep = ""),
         width = 14, height=10, units=c("cm"),dpi=300, bg = "transparent")}

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
## FUNCTION: SaveIm_Loc
## Saves all plots to a new folder names after case
##
## INPUTS: 
##    CaseName - name for folder. Enter as "folder"
##    FileName - Name for image. Enter as "name"
################################################################################
#NEEDS WORK
SaveIm_Loc <- function(CaseName,FileName) {
  
  # Set up folder if it does not exist
  fold_name<-paste(CaseName)
  
  # Check if folder exists, if not, make one
  if (file.exists(here("Figures (Local)",paste(fold_name)))) {
    
    cat("The folder exists\n")
    
  } else {
    
    # Create the folder
    FoldLocation <-
      dir.create(here("Figures (Local)",paste(fold_name)))
    
  }
  
  # Create file name
  FileName <-paste(FileName)
  
  # Save to a local file as exactly what is shown on the windows()
  savePlot(
    filename = here(paste("Figures (Local)/",paste(fold_name),"/",FileName,".png", sep = "")),
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
  if (file.exists(here("Data Files","Case Specific R Files",paste(fold_name)))) {
    
    cat("The folder exists\n")
    
  } else {
    
    # Create the folder
    FoldLocation <-
      dir.create(here("Data Files","Case Specific R Files",paste(fold_name)))
    
  }

  # Save to a local file 
  saveRDS(Data, here("Data Files","Case Specific R Files",paste(fold_name),paste(ScenarioType,ScenarioName, sep = "")))
  
}
