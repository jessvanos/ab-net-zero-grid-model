################################################################################
# TITLE: other_functions
# DESCRIPTION: Additional functions to use, not related the data itself.

# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: June 14, 2022; LAST EDIT: September 15, 2022

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
##
## INPUTS: 
##    
################################################################################
round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}
