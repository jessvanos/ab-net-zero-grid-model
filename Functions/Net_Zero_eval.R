################################################################################
# TITLE: Net_Zero_eval
# DESCRIPTION: Functions to evaluate electricity grid as it approaches possible net zero states

# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: July 4, 2022; LAST EDIT: July 4, 2022

################################################################################
################################################################################  
## FUNCTION: Retirecol
## Plotting the resources retired as a bar chart
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResYr - Resource Year table describing all resources start and end dates
################################################################################

# Stacked Area showing totals for Fuel Types
Retirecol <- function(case) {
  
  # Bring in Resource Year Table and filter columns
  Retdata <- ResYr%>%
    sim_filt2(.) %>% #Filter to rename fuels
    subset(., select=c(Name,Condition,YEAR,Capacity,End_Date,Run_ID,Primary_Fuel)) %>%
    filter(Run_ID == case) %>%
    filter(Condition == "Average")

  
  # Set levels to each category in order specified
  Retdata$Primary_Fuel <- factor(Retdata$Primary_Fuel, levels=c("Coal", "Cogen", "Gas", "Gas1", "Gas2", "Hydro","Solar",
                                      "Wind", "Storage", "Other"))
  
  #Get Year max for run and filter for end dates BEFORE this date
  MaxYr <- max(Retdata$YEAR)
  Retdata$End_Date  <- as.Date(Retdata$End_Date, 
                              format = "%m/%d/%Y")
  Retdata$End_Date <- format(Retdata$End_Date,format="%Y")
  Retdata <- Retdata%>%
    filter(.,End_Date <= MaxYr) 
  
  # Add a column to describe the new resources 
  Retdata$RetUnits <- 1  
  
  #Now group everything together
  Retdata <- Retdata%>%
  group_by(Primary_Fuel, End_Date) %>%
    summarise(Units = sum(RetUnits), Capacity = sum(Capacity))

  #Max Units Built
  mxu <- round_any(max(Retdata$Units),10,f=ceiling)
  mxc <- max(Retdata$Capacity)
  
  #Plot data
  ggplot(Retdata) +
    aes(x=End_Date, y=Units, fill = Primary_Fuel, group = Primary_Fuel,label=Units) +
    geom_bar(position="stack", stat="identity", alpha=1) +
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),  
          axis.title.x = element_text(size = XTit_Sz,hjust=0.5),
          axis.title.y = element_text(size = YTit_Sz, vjust=0),
          panel.background = element_rect(fill = "transparent"),
          plot.title = element_blank(),
          legend.justification = c(0.5,0.5),
          legend.position = ("bottom"),
          legend.title=element_blank(), 
          legend.key.size = unit(1,"lines"),
          plot.caption=element_text(size=10),
          panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray70'),
          text = element_text(size = 20)) +
    
    geom_text(position = position_stack(vjust = 0.5),colour="black") +
    
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    
    labs(x = "Year", y = "Units Retired", fill = "Fuel Type")  +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,(mxu)),breaks=seq(0,mxu,by=mxu/5)) +
    
    scale_fill_manual(values=c("Coal" =cOL_COAL, "Cogen"=cOL_COGEN, "Gas"=cOL_Gas, 
                               "Gas1"=COL_Gas1, "Gas2"=COL_Gas2,
                               "Hydro"=cOL_HYDRO, "Solar"=cOL_SOLAR, 
                               "Wind"=cOL_WIND, "Storage"=cOL_STORAGE,"Other"=cOL_OTHER))
    
}

################################################################################  
## FUNCTION: Add_Ret
## Plotting the resources added and retired for study
##
## INPUTS: 
##    case - Run_ID which you want to plot
## TABLES REQUIRED: 
##    ResYr - Resource Year table describing all resources start and end dates
################################################################################
Add_Ret <- function(case) {
  
  # Create each plot
  sz <- 15
  
  p1 <- Builtcol(case) +
    theme(legend.position ="none")
  
  p2 <- Retirecol(case) + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank()) +
    
    # Det up for an upside down graph      
    scale_y_reverse() +
    scale_x_discrete(position = "top") 
    
  
  legend <- get_legend(p2)
  p2 <- p2 + theme(legend.position ="none")
  
  grid.arrange(p1,p2,legend,
               ncol=1, nrow=3,
               layout_matrix = rbind(c(1), c(2),c(3)),
               widths = c(1), heights = c(1,1, 0.2))
  
}
  
  
  
  
  
  
  
  
