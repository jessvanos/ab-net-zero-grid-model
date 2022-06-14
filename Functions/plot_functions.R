################################################################################
# TITLE: plot_functions
# DESCRIPTION: Functions to generate plots from simulation and AESO input data


# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# OTHER CONTRIBUTORS:
# CREATED: June 2022; LAST EDIT: June 14, 2022

################################################################################
## SET UP FOR PLOTS
  
  # Set legend variables
  colours = c("darkslateblue", "grey", "darkslategrey", "coral4", "goldenrod4", 
              "dodgerblue", "forestgreen", "gold", "darkolivegreen1", "cyan")
  colours1 = c("darkslateblue", "grey", "darkslategrey", "coral4", "goldenrod4", 
               "darkcyan", "dodgerblue", "forestgreen", "gold", "cyan")
  colours2 = c("grey", "darkslategrey", "coral4", "goldenrod4", 
               "dodgerblue", "darkcyan", "forestgreen", "gold", "cyan")
  colours3 = c("forestgreen", "gold", "coral4", "goldenrod4", "cyan", "dodgerblue")
  
################################################################################
## FUNCTION: DATE CONVERSION GIVEN DAY
  
  day_Time <- function(data, year, week, hour)
    
    merit_filt <- filter(data, 
                       data$date = as.Date(year, month, day,sep="-"))
  
  ################################################################################
  ## FUNCTION: WEEKLY SIMULATION PRICE
  
  week_price <- function(year, month, day,case) 
{
    # Get zone hourly data and filter the case
    data <- ZoneHr_Avg %>%
      filter(Run_ID == case)
    
    # Select only a single week using function HrTime, uses date specified
    
    ZPrice <- HrTime(data,year,month,day)
    
    # Set the max and min for the plot
    MX <- plyr::round_any(max(abs(ZPrice$Price)), 10, f = ceiling)
    MN <- plyr::round_any(min(abs(ZPrice$Price)), 10, f = floor)
    
    # Plot the data    
    ggplot() +
      geom_line(data = ZPrice, 
                aes(x = date, y = Price), 
                size = 1.5, colour = "red") +
      theme_bw() +
      theme(panel.background = element_rect(fill = "transparent"),
            panel.grid = element_blank(),
            plot.background = element_rect(fill = "transparent", color = NA),
            text = element_text(size= 15)
      ) +
      labs(y = "Pool Price \n$/MWh", fill = "Resource") +
      scale_x_datetime(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0), 
                         limits= c(MN,MX),
                         #                       labels = label_number(accuracy = 1),
                         breaks = seq(MN, MX, by = MX/4)
      )
}