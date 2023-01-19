################################################################################
# TITLE: Other_Functions
# DESCRIPTION: Additional functions to use, not related to plotting.
#
# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: June 14, 2022; LAST EDIT: January 6, 2023
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
  ggsave(plot=last_plot(),path = here("Figures (Local)"), 
         filename = paste(name,".png", sep = ""),
         width = 12, height=8, units=c("cm"),dpi=300, bg = "transparent")  }

################################################################################
## FUNCTION: SaveRun_Loc
## Saves all common plots in a new folder
##
## INPUTS: 
##    name - Date to plot, the week will start on the day chosen. Enter as "name"
################################################################################
#NEEDS WORK
SaveRun_Loc <- function(CaseName)
  
  # SET UP FOLDER
  fold_name<-paste(CaseName," ",SourceDB)

  # Check if folder exists, if not, make one
  if (file.exists(here("Figures (Local)",paste(fold_name)))) {
    
    cat("The folder already exists")
    
  } else {
    
    # Create the folder
    dir.create(here("Figures (Local)",paste(fold_name)))
    
  }
  
  # ADD PLOTS
  # Create Retirement plot
  windows(16,6, pointsize = 12)
  RetireMW(BC)

  ggsave(plot=last_plot(),path = here("Figures (Local)",paste(fold_name)), 
         filename = paste("Retirements_",SourceDB,".png", sep = ""),
         width = 16, height=6, units=c("in"),
         dpi=300,
         bg = "transparent")
  

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
## Convert the date and select a subset for one day from the data pulled in
################################################################################

{ HrTime <- function(data, year, month, day) {
  subset(data,
         (date >= paste(year,"-", month, "-", day," 00:00:00", sep = "") & 
            date <= 
            paste(year,"-", month, "-", (day)," 24:00:00", sep = "")))  }
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