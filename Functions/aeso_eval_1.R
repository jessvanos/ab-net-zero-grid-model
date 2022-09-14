################################################################################
# TITLE: aeso_eval_1
# DESCRIPTION: Functions To used to plot and analyze AESO data

# ORIGINAL AUTHOR: Taylor Pawlenchuk (Retrieved June 3, 2022)
# EDITS & ADDITIONAL CONTENT: Jessica Van Os
# LAST EDIT: June 14, 2022

################################################################################
## FUNCTION: plnt_tr
## Identify Specific Plant traits
################################################################################
plnt_tr <- function(Asset_ID) {
  plnt <- sub_samp %>%
    filter(ID == Asset_ID) %>%
    subset(., select = c(time, Price, gen, Capacity, Plant_Type, AESO_Name, CO2, Heat.Rate, Revenue, Cap_Fac))
}

################################################################################
## FUNCTION: Week_act
## Plots actual AESO output for a single week given the case study
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
## TABLES REQUIRED: 
##    df1a - Filtered version of nrgstream
################################################################################
Week_act <- function(year,month,day) {
  
  wk_st <- as.POSIXct(paste(day,month,year, sep = "/"), format="%d/%m/%Y")
  wk_end <- as.POSIXct(paste(day+7,month,year, sep = "/"), format="%d/%m/%Y")
  
  df1a$time <- as.POSIXct(df1a$time, tz = "MST")
  
  # Select only a single week
  WKa <- df1a %>%
    filter(Plant_Type != "EXPORT" & Plant_Type != "IMPORT") %>%
    filter(time >= wk_st, time <= wk_end)
  
  WKIM <- df1a %>%
    filter(Plant_Type == "IMPORT") %>%
    filter(time >= wk_st & time <= wk_end)
  
  WKIM$total_gen <- WKIM$total_gen * -1
  
  WK <- rbind(WKIM, WKa)
  
  {
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "IMPORT", after = Inf)
    
    
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "SCGT", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "NGCC", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "HYDRO", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "OTHER", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "WIND", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "SOLAR", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "COAL", after = Inf)
    WK$Plant_Type<-fct_relevel(WK$Plant_Type, "COGEN", after = Inf)
  }
  
  WK$Plant_Type <- factor(WK$Plant_Type, levels=c("IMPORT", 
                                                  "SCGT", "NGCC", "HYDRO", 
                                                  "OTHER", "WIND", "SOLAR", 
                                                  "COAL", "COGEN"))
  
  levels(WK$Plant_Type) <- c("Import","Natural Gas Simple Cycle", "Natural Gas Combined Cycle", 
                            "Hydro", "Other",
                            "Wind", "Solar", "Coal", "Cogeneration")
  
  
  
  dmd <- demand %>%
    filter(time >= wk_st & time <= wk_end)
  
  # Plot the data    
  ggplot() +
    geom_area(data = WK, aes(x = time, y = total_gen, fill = Plant_Type, colour = Plant_Type), 
              alpha=0.7, size=0.5) +
    
    # Add hourly load line
    geom_line(data = dmd, 
              aes(x = time, y = Demand), size=2, colour = "black") +
    
    scale_x_datetime(expand=c(0,0),date_labels = "%b-%e" ,breaks = "day") +
    
    # Set the theme for the plot
    theme_bw() +
    
    theme(text=element_text(family=Plot_Text)) +
    
    theme(panel.grid = element_blank(),
          legend.position = "bottom",
    ) +
    theme(axis.text.x = element_text(vjust = 1),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.title.x = element_text(size= XTit_Sz),
          axis.title.y = element_text(size= YTit_Sz),
          title = element_text(size= Tit_Sz),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.key.size = unit(0.5,"lines"),
          legend.title=element_blank(),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          text = element_text(size= 15)
    ) +
    guides(fill = guide_legend(nrow = 1)) +
    scale_y_continuous(expand=c(0,0)) +
    labs(title = paste("AESO Data,", year), x = "Date", y = "Output (MWh)", fill = "Plant_Type", colour = "Plant_Type",
         caption="NRG Stream Data") +

    #Add colour
    scale_fill_manual(values = colours1) +
    
    # Make outline the same as fill colors
    scale_colour_manual(values = Outline1)
    
}

################################################################################
## FUNCTION: wkPrice
## Plots actual AESO pool price
##
## INPUTS: 
##    year, month, day - Date to plot, the week will start on the day chosen
## TABLES REQUIRED: 
##    demand - Filtered version of nrgstream
################################################################################
wkPrice <- function(year,month,day) {
  
  wk_st <- as.POSIXct(paste(day,month,year, sep = "/"), format="%d/%m/%Y")
  wk_end <- as.POSIXct(paste(day+7,month,year, sep = "/"), format="%d/%m/%Y")
  
  price_WK <- demand %>%
    filter(time >= wk_st & time <= wk_end)
  
  # Set the max for the plot
  MX <- plyr::round_any(max(abs(price_WK$Price)+100), 10, f = ceiling)
  
  ggplot() +
    geom_line(data = price_WK, 
              aes(x=time, y=Price), 
              size = 1.5, color="darkred") +
    scale_x_datetime(expand=c(0,0)) +
    
    theme_bw() +
    theme(text=element_text(family=Plot_Text)) +
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid = element_blank(),
          axis.title.x = element_text(size = XTit_Sz),
          axis.title.y = element_text(size = YTit_Sz),
          axis.text.y=element_text(hjust=-0.5),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'grey'),
          panel.grid.minor.y = element_blank(),
          text = element_text(size= 15)
    ) +
    scale_y_continuous(expand=c(0,0),limits=c(0,MX)) +
    labs(x = "Date", y = "Pool Price ($/MWh)")
}

################################################################################
## FUNCTION: cap_pf **NOT READ
## Plots actual AESO pool price
##
## INPUTS: 
##    Plant - plant to look at
## TABLES REQUIRED: 
##    meritfilt - Filtered version of leach merit data
################################################################################

cap_pf <- function(plant){#, year){
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  #  start <- as.Date(paste(year,"-01-01",sep=""))
  #  end <- as.Date(paste(year,"-12-31",sep=""))
  
  data <- merit_filt %>%
    filter(asset_id == plant, 
           #           date >= start, 
           #           date <= end
    ) %>%
    select(date, he, AESO_Name, Plant_Type, flexible, price, dispatched_mw)
  
  cap <- sub_samp %>%
    filter(ID == plant, 
           #           date >= start, 
           #           date <= end
    ) %>%
    select(date, he, Capacity, Cap_Fac)
  
  data1 <- left_join(data, cap, by=c("date","he")) %>%
    mutate(C_Fac = (dispatched_mw/Capacity)) %>%
    mutate(time = as.POSIXct(paste(date, he), format="%Y-%m-%d %H")) %>%
    filter(dispatched_mw != 0)
  
  must_run <- data1 %>%
    filter(flexible == "N")
  
  must_run <- Mode(must_run$C_Fac)
  
  data2 <- data1 %>%
    filter(flexible != "N") %>%
    mutate(Cap_F = (must_run + C_Fac))
  
  breaks= c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
  tags <- c("0-10%","10-20%","20-30%","30-40%","40-50%","50-60%","60-70%","70-80%","80-90%","90-100%")
  data2$bins <- cut(data2$Cap_F, 
                    breaks= breaks,
                    labels=tags)
  data2 <- na.omit(data2)
  
  data3 <- data2 %>%
    group_by(bins) %>%
    summarise(avPrice = mean(price))
  
  name <- data[1,3]
  type <- data[1,4]
  
  mx <- plyr::round_any(max(data3$avPrice), 50, f = ceiling)
  mn <- min(data3$avPrice)
  
  data3 <- data3 %>%
    mutate(bid_fact = round(avPrice/mn-1,3))
  
  #  ggplot(data1) +
  #    geom_line(aes(x = time, y = C_Fac, colour=flexible)) +
  #    geom_line(aes(x = time, y = Cap_Fac), alpha = 0.3) +
  #    geom_line(data = data2, aes(x = time, y = Cap_F), alpha = 0.9) +
  #    theme_bw() +
  #    theme(panel.background = element_rect(fill = "transparent"),
  #          panel.grid = element_blank(),
  #          plot.background = element_rect(fill = "transparent", color = NA),
  #          text = element_text(size= 15),
  #          plot.title = element_text(hjust = 0.5),
  #          axis.text.x = element_text(angle = 45, hjust=1)
  #    ) +
  #    scale_y_continuous(expand=c(0,0))#,
  #                       limits = c(0,750))# +
  #    ggtitle(paste(name, type, sep = ": "))+
  #    labs(x = "% of Capacity Dispatched",
  #         y = "Average Pool Price \n($/MWh)")
  
  ggplot(data3, aes(x = bins, y = avPrice, group=bins)) +
    geom_bar(stat="identity") +
    geom_text(aes(label = 
                    paste("$",round(avPrice, digits = 0),"\n",bid_fact,sep="")), 
              vjust = -0.3, size = 4)+#, angle = 90) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          text = element_text(size= 15),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust=1)
    ) +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,mx)) +
    #    ggtitle(paste(name, type, sep = ": "))+
    labs(x = "% of Capacity Dispatched",
         y = "Average Offer Price \n($/MWh)",
         title = paste(name, type, sep = ": "),
         subtitle = paste("Bids at $0 at ",round(must_run*100, digits=2),"% CF",sep=""))
}

################################################################################
## FUNCTION: hrc  **NOT READ
## 
##
## INPUTS: 
##    Plant - Plant to look at
## TABLES REQUIRED: 
##    sub_samb - Filtered version of ngrstream
################################################################################

hrc <- function(plant) {
  data <- sub_samp %>%
    filter(ID == plant) %>%
    select(time, gen, ID, AESO_Name, Plant_Type, GHG_ID, CO2, Heat.Rate, Cap_Fac)
  
  ggplot() +
    geom_line(data = data, 
              aes(x = Cap_Fac, y = Heat.Rate)) +#, colour = Year, linetype = sit), size = 1) +
    #    facet_grid(cols = vars(Condition)) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.title = element_blank()
    ) +
    labs(y = "Heat Rate", 
         x = "Capacity Factor", 
         #         title = "AESO Data vs Simulation",
         #         subtitle = DB
    ) +
    scale_color_manual(values = c("firebrick","goldenrod1", "forestgreen", "cornflowerblue","gray60")) +
    scale_x_continuous(expand=c(0,0), 
                       limits = c(0,1.1),
                       labels = percent) +
    scale_y_continuous(expand=c(0,0)
    )
}

################################################################################
## FUNCTION: cap_offer  **NOT READ
## 
##
## INPUTS: 
##    Plant - Plant to look at
## TABLES REQUIRED: 
##    
################################################################################

cap_offer <- function(plant){#, year){
  #  start <- as.Date(paste(year,"-01-01",sep=""))
  #  end <- as.Date(paste(year,"-12-31",sep=""))
  
  data <- merit_filt %>%
    filter(asset_id == plant, 
           #           date >= start, 
           #           date <= end
    ) %>%
    select(date, he, AESO_Name, Plant_Type,
           #co2_est, size, on_peak, flexible, hourly_dispatch, hourly_renewables, year, available_mw,
           price, dispatched_mw)
  
  name <- data[1,3]
  type <- data[1,4]
  
  data <- data %>%
    select(date, he, price, dispatched_mw)
  
  cap <- sub_samp %>%
    filter(ID == plant, 
           #           date >= start, 
           #           date <= end
    ) %>%
    select(date, he, Capacity)#, Cap_Fac)
  
  data1 <- left_join(data, cap, by=c("date","he")) %>%
    mutate(C_Fac = (dispatched_mw/Capacity)) %>%
    mutate(time = as.POSIXct(paste(date, he), format="%Y-%m-%d %H")) %>%
    select(-date,-he)
  
  if (sum(data1$price) == 0) {
    data3 <- data1 %>%
      filter(price == 0) %>%
      group_by(time) %>%
      mutate(tier = 1, Cap_F = sum(C_Fac))
  } else {
    
    data2 <- data1 %>%
      filter(price == 0) %>%
      group_by(time) %>%
      mutate(tier = 1, Cap_F = sum(C_Fac))
    
    
    data1 <- data1 %>%
      filter(price != 0) %>%
      group_by(time) %>%
      arrange(price) %>%
      mutate(tier = 2:(n()+1), Cap_F = sum(C_Fac))
    
    data3 <- rbind(data1,data2)
  }
  
  data1 <- na.omit(data3)
  
  data2 <- data1 %>%
    group_by(tier) %>%
    summarise(avPrice = mean(price), 
              avCap_Fac = mean(C_Fac),
    )
  
  
  #  mx <- plyr::round_any(max(data2$avPrice)+2, 100, f = ceiling)
  mn <- ifelse(length(data2$tier)==1,data2$avPrice,
               min(ifelse(data2$avPrice==0,
                          data2$avPrice[data2$avPrice!=min(data2$avPrice)],
                          data2$avPrice)))
  #               min(data2$avPrice[data2$avPrice!=min(data2$avPrice)]))
  
  ifelse(mn==0,data2$bid_fact<-0,
         data2 <- data2%>%
           mutate(bid_fact = round(avPrice/mn-1,2)) 
  )
  
  data2 <- data2 %>% 
    mutate(One = avCap_Fac + ifelse(is.na(lag(avCap_Fac))==TRUE,0,lag(avCap_Fac))) %>%
    mutate(Two = avCap_Fac + ifelse(is.na(lag(One))==TRUE,0,lag(One))) %>%
    mutate(Three = avCap_Fac + ifelse(is.na(lag(Two))==TRUE,0,lag(Two))) %>%
    mutate(Four = avCap_Fac + ifelse(is.na(lag(Three))==TRUE,0,lag(Three))) %>%
    mutate(Five = avCap_Fac + ifelse(is.na(lag(Four))==TRUE,0,lag(Four))) %>%
    mutate(Six = avCap_Fac + ifelse(is.na(lag(Five))==TRUE,0,lag(Five))) %>%
    mutate(Seven = avCap_Fac + ifelse(is.na(lag(Six))==TRUE,0,lag(Six)))
  
  data2 <- data2[, c(5,6,7,8,9,10,11,1,2,3,4)]
  
  #  mxtier <- ifelse(max(data2$tier)==1,2,max(data2$tier))
  mxtier <- max(data2$tier)
  #  names(data2)[mxtier-1] <- "xaxis"
  names(data2)[ifelse(mxtier==1,1,mxtier-1)] <- "xaxis"
  data2$xaxis <- as.character(paste(round(data2$xaxis*100,3),"%",sep=""))
  #  data2[ifelse(max(data2$tier)==1,1,mxtier),(mxtier-1)] <- 
  #    paste(data2[ifelse(max(data2$tier)==1,1,mxtier),
  #                mxtier-1],
  #          "-100%",sep="")
  
  
  if(data2[ifelse(mxtier==1,1,mxtier-1),10]==0) {
    data2[ifelse(mxtier==1,1,mxtier-1),ifelse(mxtier==1,1,mxtier-1)] <- 
      paste(data2[ifelse(mxtier==1,1,mxtier-1), ifelse(mxtier==1,1,mxtier-1)],
            "~100%",sep="")
  }
  
  data2[mxtier,ifelse(mxtier==1,1,mxtier-1)] <- 
    paste(data2[mxtier,
                ifelse(mxtier==1,1,mxtier-1)],
          "-100%",sep="")
  
  ggplot(data2, 
         aes(x = reorder(xaxis,tier), y = avPrice)) +
    #         aes(x = reorder(xaxis, +avPrice), y = avPrice)) +
    geom_bar(stat="identity") +
    geom_text(aes(label = 
                    paste("$",round(avPrice, digits = 0),
                          "\nBF: ",bid_fact,sep="")), 
              vjust = -0.3, size = 3.5)+#, angle = 90) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          text = element_text(size= 15),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust=1)
    ) +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,1300)) +
    labs(x = "% of Capacity Dispatched",
         y = "Average Offer Price \n($/MWh)",
         title = paste(name, type, sep = ": "),
    )
}

################################################################################
## FUNCTION: cap_offermn  **NOT READ
## 
##
## INPUTS: 
##    Plant - Plant to look at
## TABLES REQUIRED: 
##    
################################################################################

cap_offermn <- function(plant){#, year){
  
  # Set a start and end date if needed
  #  start <- as.Date(paste(year,"-01-01",sep=""))
  #  end <- as.Date(paste(year,"-12-31",sep=""))
  
  # Select just the plant in question from the spreadsheet 
  # "student_data_2021_Jul_23_14_09.csv.gz"
  data <- merit_filt %>%
    filter(asset_id == plant, 
           #           date >= start, 
           #           date <= end
    ) %>%
    select(date, month, he, AESO_Name, Plant_Type,
           price, dispatched_mw)
  
  # Save the plant name and type for the plot later
  name <- data[1,4]
  type <- data[1,5]
  
  # Retain only necessary columns
  data <- data %>%
    select(date, month, he, price, dispatched_mw)
  
  # Select just the plant in question from the nrgstream data.
  cap <- sub_samp %>%
    filter(ID == plant, 
           #           date >= start, 
           #           date <= end
    ) %>%
    select(date, he, Capacity)#, Cap_Fac)
  
  # Combine the two datasets by datetime and calculate the capacity factor for 
  # each row
  data1 <- left_join(data, cap, by=c("date","he")) %>%
    mutate(C_Fac = (dispatched_mw/Capacity)) %>%
    mutate(time = as.POSIXct(paste(date, he), format="%Y-%m-%d %H")) %>%
    select(-date,-he)
  
  # Create new column to define the pricing tier. $0/MWh is always tier 1, and 
  # the tiers are then sorted from low to high price.
  if (sum(data1$price) == 0) {
    data3 <- data1 %>%
      filter(price == 0) %>%
      group_by(time) %>%
      mutate(tier = 1, Cap_F = sum(C_Fac))
  } else {
    
    data2 <- data1 %>%
      filter(price == 0) %>%
      group_by(time) %>%
      mutate(tier = 1, Cap_F = sum(C_Fac))
    
    
    data1 <- data1 %>%
      filter(price != 0) %>%
      group_by(time) %>%
      arrange(price) %>%
      mutate(tier = 2:(n()+1), Cap_F = sum(C_Fac))
    
    data3 <- rbind(data1,data2)
  }
  
  # Omit NA data
  data1 <- na.omit(data3)
  
  # Summarize data by month and tier to find the average prices and capacity 
  # factors for each group
  data2 <- data1 %>%
    group_by(month,tier) %>%
    summarise(avPrice = mean(price), 
              avCap_Fac = mean(C_Fac),
    )
  
  
  #  Identify the lowest non-zero price to establish baseline
  mn <- ifelse(length(data2$tier)==1,data2$avPrice,
               min(ifelse(data2$avPrice==0,
                          data2$avPrice[data2$avPrice!=min(data2$avPrice)],
                          data2$avPrice)))
  
  # Calculate Aurora's bid factor for each row
  ifelse(mn==0,data2$bid_fact<-0,
         data2 <- data2%>%
           mutate(bid_fact = round(avPrice/mn-1,2)) 
  )
  
  # Long and cumbersome code to calculate the building capacity factors for each 
  # month.
  data2 <- data2 %>% 
    mutate(One = avCap_Fac + ifelse(is.na(lag(avCap_Fac))==TRUE,0,lag(avCap_Fac))) %>%
    mutate(Two = avCap_Fac + ifelse(is.na(lag(One))==TRUE,0,lag(One))) %>%
    mutate(Three = avCap_Fac + ifelse(is.na(lag(Two))==TRUE,0,lag(Two))) %>%
    mutate(Four = avCap_Fac + ifelse(is.na(lag(Three))==TRUE,0,lag(Three))) %>%
    mutate(Five = avCap_Fac + ifelse(is.na(lag(Four))==TRUE,0,lag(Four))) %>%
    mutate(Six = avCap_Fac + ifelse(is.na(lag(Five))==TRUE,0,lag(Five))) %>%
    mutate(Seven = avCap_Fac + ifelse(is.na(lag(Six))==TRUE,0,lag(Six)))
  
  # Rearrange the columns to put the previously added columns at the front
  data2 <- data2[, c(6,7,8,9,10,11,12,1,2,3,4,5)]
  
  # Identify the largest tier
  mxtier <- max(data2$tier)
  mx2tier <- ifelse(mxtier==1,1,mxtier-1)
  
  # Rename the column with the correct capacity factors to the 'xaxis' and change
  # column format to character for readability
  names(data2)[mx2tier] <- "xaxis"
  data2$xaxis <- as.character(paste(round(data2$xaxis*100,3),"%",sep=""))
  
  # Remove extra columns
  data2 <- data2[,c(mx2tier,8,9,10,11,12)]
  
  # Relabel the second top tier if it was never dispatched ie. it was another
  # maximum price
  data4 <- data2 %>%
    group_by(month) %>%
    filter(tier == max(tier[tier!=max(tier)])) %>%
    mutate(xaxis = ifelse(avCap_Fac == 0, paste(xaxis,"~100%",sep=""),xaxis))
  
  # Relabel the top tier to be a range to 100%
  data3 <- data2 %>%
    group_by(month) %>%
    filter(tier == max(tier)) %>%
    mutate(xaxis = paste(xaxis,"-100%",sep=""))
  
  data2 <- data2 %>%
    group_by(month) %>%
    filter(tier != max(tier),tier != max(tier[tier!=max(tier)]))
  
  # Recombine the data
  data1 <- rbind(data2,data3,data4)
  
  # Month list for plot labels
  mons <- list(
    "1" = "January",
    "2" = "February",
    "3" = "March",
    "4" = "April",
    "5" = "May",
    "6" = "June",
    "7" = "July",
    "8" = "August",
    "9" = "September",
    "10"= "October",
    "11"= "November",
    "12"= "December"
  )
  
  var_label <- function(variable,value){
    return(mons[value])
  }
  
  # Plot the data
  ggplot(data1, 
         aes(x = reorder(xaxis,tier), y = avPrice)) +
    geom_bar(stat="identity") +
    facet_wrap(~month, scales="free_x", labeller=var_label) +
    geom_text(aes(label = 
                    paste("$",round(avPrice, digits = 0),
                          #                          "\nBF: ",
                          "\n",bid_fact,sep="")), 
              vjust = -0.3, size = 3.5)+#, angle = 90) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          text = element_text(size= 15),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust=1, size = 8)
    ) +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,1300)) +
    labs(x = "% of Capacity Dispatched",
         y = "Average Offer Price \n($/MWh)",
         title = paste(name, type, sep = ": "),
    )
}

################################################################################
## FUNCTION: cdata  **NOT READ
## 
##
## INPUTS: 
##    Plant - Plant to look at
## TABLES REQUIRED: 
##    
################################################################################

cdata <- function(plant,parameter){
  
  # Set a start and end date if needed
  #  start <- as.Date(paste(year,"-01-01",sep=""))
  #  end <- as.Date(paste(year,"-12-31",sep=""))
  
  # Select just the plant in question from the spreadsheet 
  # "student_data_2021_Jul_23_14_09.csv.gz"
  data <- merit_filt %>%
    filter(asset_id == plant, 
           #           date >= start, 
           #           date <= end
    ) %>%
    select(date, month, he, AESO_Name, Plant_Type,
           price, dispatched_mw)
  
  # Save the plant name and type for the plot later
  name <- data[1,4]
  #  type <- data[1,5]
  
  # Retain only necessary columns
  data <- data %>%
    select(date, month, he, price, dispatched_mw)
  
  # Select just the plant in question from the nrgstream data.
  cap <- sub_samp %>%
    filter(ID == plant, 
           #           date >= start, 
           #           date <= end
    ) %>%
    select(date, he, Capacity)#, Cap_Fac)
  
  # Combine the two datasets by datetime and calculate the capacity factor for 
  # each row
  data1 <- left_join(data, cap, by=c("date","he")) %>%
    mutate(C_Fac = (dispatched_mw/Capacity)) %>%
    mutate(time = as.POSIXct(paste(date, he), format="%Y-%m-%d %H")) %>%
    select(-date,-he)
  
  # Create new column to define the pricing tier. $0/MWh is always tier 1, and 
  # the tiers are then sorted from low to high price.
  if (sum(data1$price) == 0) {
    data3 <- data1 %>%
      filter(price == 0) %>%
      group_by(time) %>%
      mutate(tier = 1, Cap_F = sum(C_Fac))
  } else {
    
    data2 <- data1 %>%
      filter(price == 0) %>%
      group_by(time) %>%
      mutate(tier = 1, Cap_F = sum(C_Fac))
    
    
    data1 <- data1 %>%
      filter(price != 0) %>%
      group_by(time) %>%
      arrange(price) %>%
      mutate(tier = 2:(n()+1), Cap_F = sum(C_Fac))
    
    data3 <- rbind(data1,data2)
  }
  
  # Omit NA data
  data1 <- na.omit(data3)
  
  # Summarize data by month and tier to find the average prices and capacity 
  # factors for each group
  data2 <- data1 %>%
    group_by(month,tier) %>%
    summarise(avPrice = mean(price), 
              avCap_Fac = mean(C_Fac),
    )
  
  
  #  Identify the lowest non-zero price to establish baseline
  mn <- ifelse(length(data2$tier)==1,data2$avPrice,
               min(ifelse(data2$avPrice==0,
                          data2$avPrice[data2$avPrice!=min(data2$avPrice)],
                          data2$avPrice)))
  
  # Calculate Aurora's bid factor for each row
  ifelse(mn==0,data2$bid_fact<-0,
         data2 <- data2%>%
           mutate(bid_fact = round(avPrice/mn-1,2)) 
  )
  
  # Long and cumbersome code to calculate the building capacity factors for each 
  # month.
  data2 <- data2 %>% 
    mutate(One = avCap_Fac + ifelse(is.na(lag(avCap_Fac))==TRUE,0,lag(avCap_Fac))) %>%
    mutate(Two = avCap_Fac + ifelse(is.na(lag(One))==TRUE,0,lag(One))) %>%
    mutate(Three = avCap_Fac + ifelse(is.na(lag(Two))==TRUE,0,lag(Two))) %>%
    mutate(Four = avCap_Fac + ifelse(is.na(lag(Three))==TRUE,0,lag(Three))) %>%
    mutate(Five = avCap_Fac + ifelse(is.na(lag(Four))==TRUE,0,lag(Four))) %>%
    mutate(Six = avCap_Fac + ifelse(is.na(lag(Five))==TRUE,0,lag(Five))) %>%
    mutate(Seven = avCap_Fac + ifelse(is.na(lag(Six))==TRUE,0,lag(Six)))
  
  # Rearrange the columns to put the previously added columns at the front
  data2 <- data2[, c(6,7,8,9,10,11,12,1,2,3,4,5)]
  
  # Identify the largest tier
  mxtier <- max(data2$tier)
  mx2tier <- ifelse(mxtier==1,1,mxtier-1)
  
  # Rename the column with the correct capacity factors to the 'xaxis' and change
  # column format to character for readability
  names(data2)[mx2tier] <- "xaxis"
  data2$xaxis <- round(data2$xaxis*100,3)
  
  # Remove extra columns
  data2 <- data2[,c(mx2tier,8,9,10,11,12)]
  
  colkeep <- ifelse(parameter == "xaxis", 1,6)
  
  data1 <- data2[,c(3,2,colkeep)]
  data3 <- reshape2::dcast(data1,tier~month)
  
  setwd("F:/My Drive/transfer")
  write.csv(data3, "Incremental_Bid_Factors.csv")
}

################################################################################
## FUNCTION: cap_type  **NOT READ
## 
##
## INPUTS: 
##    Plant - Plant to look at
## TABLES REQUIRED: 
##    
################################################################################

cap_type <- function(plant_type){#, year){
  
  # Set a start and end date if needed
  #  start <- as.Date(paste(year,"-01-01",sep=""))
  #  end <- as.Date(paste(year,"-12-31",sep=""))
  
  # Select just the plant in question from the spreadsheet 
  # "student_data_2021_Jul_23_14_09.csv.gz"
  data <- merit_filt %>%
    filter(Plant_Type == plant_type, 
           #           date >= start, 
           #           date <= end
    ) %>%
    group_by(date,he,month,price) %>%
    summarise(dispatched_mw = sum(dispatched_mw)) %>%
    select(date, month, he, price, dispatched_mw)
  
  # Save the plant name and type for the plot later
  type <- plant_type
  
  # Select just the plant in question from the nrgstream data.
  cap <- sub_samp %>%
    filter(Plant_Type == plant_type, 
           #           date >= start, 
           #           date <= end
    ) %>%
    group_by(date, he) %>%
    summarise(Capacity = sum(Capacity)) %>%
    select(date, he, Capacity)#, Cap_Fac)
  
  #  input <- ifelse(peak == "on", "On-Peak WECC", "Off-Peak WECC")
  
  # Combine the two datasets by datetime and calculate the capacity factor for 
  # each row
  data1 <- left_join(data, cap, by=c("date","he")) %>%
    mutate(Capacity_Factor = ifelse(dispatched_mw/Capacity > 1, 1, 
                                    dispatched_mw/Capacity),
    )
  
  data2 <- data1 %>%
    filter(price == 0) %>%
    group_by(date,he) %>%
    mutate(tier = 1, Cap_F = sum(Capacity_Factor))
  
  data1 <- data1 %>%
    filter(price != 0) %>%
    group_by(date,he) %>%
    arrange(price) %>%
    mutate(tier = 2:(n()+1), Cap_F = sum(Capacity_Factor))
  
  data3 <- rbind(data1,data2)
  
  # Omit NA data
  data <- na.omit(data3)
  
  data <- data %>%
    mutate(
      Condition = if_else(between(he, 08, 23), 
                          "On-Peak", "Off-Peak"),
      bidding_fact = round(price/mn-1,2)
    )
  
  data1 <- data %>%
    filter(Condition == "On-Peak")
  
  # Summarize data by month and tier to find the average prices and capacity 
  # factors for each group
  data3 <- data1 %>%
    #    mutate(fact = round(price/mn-1,2)) %>%
    group_by(month,tier) %>%
    summarise(avPrice = mean(price), 
              #              sdP = sd(price),
              avCap_Fac = mean(Capacity_Factor),
              #              sdC = sd(Capacity_Factor),
              #              avbid_fact = mean(bidding_fact),
              #              sdB = sd(bid_fact)
    ) %>%
    mutate(bid_fact = round(avPrice/mn-1,2))
  
  data2 <- data3 %>%
    group_by(month,avCap_Fac) %>%
    summarise(avPrice = max(avPrice),
              bid_fact = max(bid_fact),
              tier = min(tier)) %>%
    arrange(month,tier)
  
  #  Identify the lowest non-zero price to establish baseline
  mn <- ifelse(length(data2$tier)==1,data2$avPrice,
               min(ifelse(data2$avPrice==0,
                          data2$avPrice[data2$avPrice!=min(data2$avPrice)],
                          data2$avPrice)))
  
  # Calculate Aurora's bid factor for each row
  
  data3 <- data2 %>% 
    arrange(tier) %>%
    group_by(month) %>%
    mutate(overall_cap = cumsum(avCap_Fac))
  
  # Identify the largest tier
  mxtier <- max(data3$tier)
  mx2tier <- ifelse(mxtier==1,1,mxtier-1)
  
  #  par <- ifelse(parameter == "cap", "CapFac", "BidFac")
  
  #  colkeep1 <- ifelse(parameter == "cap", 6, 4)
  
  data1 <- data3[,c(1,5,6)]
  data1 <- reshape2::dcast(data1,tier~month)
  data2 <- data2[,c(1,5,4)]
  data2 <- reshape2::dcast(data2,tier~month)
  
  setwd("F:/My Drive/transfer")
  write.csv(data1, paste(type,"CapFac","OnPeak","Incremental_Bid_Factors.csv",sep="_"))
  write.csv(data2, paste(type,"BidFac","OnPeak","Incremental_Bid_Factors.csv",sep="_"))
  
  data1 <- data %>%
    filter(Condition == "Off-Peak")
  
  # Summarize data by month and tier to find the average prices and capacity 
  # factors for each group
  data3 <- data1 %>%
    #    mutate(fact = round(price/mn-1,2)) %>%
    group_by(month,tier) %>%
    summarise(avPrice = mean(price), 
              avCap_Fac = mean(Capacity_Factor),
    ) %>%
    mutate(bid_fact = round(avPrice/mn-1,2))
  
  data2 <- data3 %>%
    group_by(month,avCap_Fac) %>%
    summarise(avPrice = max(avPrice),
              bid_fact = max(bid_fact),
              tier = min(tier)) %>%
    arrange(month,tier)
  
  #  Identify the lowest non-zero price to establish baseline
  mn <- ifelse(length(data2$tier)==1,data2$avPrice,
               min(ifelse(data2$avPrice==0,
                          data2$avPrice[data2$avPrice!=min(data2$avPrice)],
                          data2$avPrice)))
  
  # Calculate Aurora's bid factor for each row
  
  data3 <- data2 %>% 
    arrange(tier) %>%
    group_by(month) %>%
    mutate(overall_cap = cumsum(avCap_Fac))
  
  # Identify the largest tier
  mxtier <- max(data3$tier)
  mx2tier <- ifelse(mxtier==1,1,mxtier-1)
  
  data1 <- data3[,c(1,5,6)]
  data1 <- reshape2::dcast(data1,tier~month)
  data2 <- data2[,c(1,5,4)]
  data2 <- reshape2::dcast(data2,tier~month)
  
  write.csv(data1, paste(type,"CapFac","OffPeak","Incremental_Bid_Factors.csv",sep="_"))
  write.csv(data2, paste(type,"BidFac","OffPeak","Incremental_Bid_Factors.csv",sep="_"))
}

################################################################################
## FUNCTION: table_type  **NOT READ
## 
##
## INPUTS: 
##    Plant - Plant to look at
## TABLES REQUIRED: 
##    
################################################################################

table_type <- function(plant_type){#, year){
  
  # Set a start and end date if needed
  #  start <- as.Date(paste(year,"-01-01",sep=""))
  #  end <- as.Date(paste(year,"-12-31",sep=""))
  
  # Select just the plant in question from the spreadsheet 
  # "student_data_2021_Jul_23_14_09.csv.gz"
  data <- merit_filt %>%
    filter(Plant_Type == plant_type, 
           #           date >= start, 
           #           date <= end
    ) %>%
    group_by(date,he,month,price) %>%
    summarise(dispatched_mw = sum(dispatched_mw)) %>%
    select(date, he, price, dispatched_mw)
  
  # Save the plant name and type for the plot later
  type <- plant_type
  
  # Select just the plant in question from the nrgstream data.
  cap <- sub_samp %>%
    filter(Plant_Type == plant_type, 
           #           date >= start, 
           #           date <= end
    ) %>%
    group_by(date, he) %>%
    summarise(Capacity = sum(Capacity)) %>%
    select(date, he, Capacity)#, Cap_Fac)
  
  # Combine the two datasets by datetime and calculate the capacity factor for 
  # each row
  data1 <- left_join(data, cap, by=c("date","he")) %>%
    mutate(Capacity_Factor = ifelse(dispatched_mw/Capacity > 1, 1, 
                                    dispatched_mw/Capacity),
    )
  
  data2 <- data1 %>%
    filter(price == 0) %>%
    group_by(date,he) %>%
    mutate(tier = 1)#, Cap_F = sum(Capacity_Factor))
  
  data1 <- data1 %>%
    filter(price != 0) %>%
    group_by(date,he) %>%
    arrange(price) %>%
    mutate(tier = 2:(n()+1))#, Cap_F = sum(Capacity_Factor))
  
  data3 <- rbind(data1,data2)
  
  # Omit NA data
  data <- na.omit(data3)
  
  data <- data %>%
    mutate(
      Condition = if_else(between(he, 08, 23), 
                          "On-Peak", "Off-Peak")
    )
  
  #  data1 <- data %>%
  #    filter(Condition == "On-Peak")
  
  # Summarize data by month and tier to find the average prices and capacity 
  # factors for each group
  data3 <- data %>%
    group_by(tier, Condition) %>%#,month) %>%
    summarise(avPrice = mean(price), 
              #              sdP = sd(price),
              avCap_Fac = mean(Capacity_Factor),
              #              sdC = sd(Capacity_Factor),
    )
  
  # Lump duplicate avCap_Fac together
  data2 <- data3 %>%
    group_by(avCap_Fac, Condition) %>%
    summarise(avPrice = max(avPrice),
              tier = min(tier)) %>%
    arrange(tier)
  
  #  Identify the lowest non-zero price to establish baseline
  mn <- ifelse(length(data2$tier)==1,data2$avPrice,
               min(ifelse(data2$avPrice==0,
                          data2$avPrice[data2$avPrice!=min(data2$avPrice)],
                          data2$avPrice)))
  
  # Calculate Aurora's bid factor for each row
  
  data3 <- data2 %>% 
    arrange(tier) %>%
    group_by(Condition) %>% #month) %>%
    mutate(overall_cap = round(cumsum(avCap_Fac),2),
           bid_fact = round(avPrice/mn-1,2)) %>%
    ungroup() %>%
    group_by(overall_cap, Condition) %>%
    summarise(tier = min(tier),
              bid_fact = mean(bid_fact))
  
  # Identify the largest tier
  mxtier <- max(data3$tier)
  mx2tier <- ifelse(mxtier==1,1,mxtier-1)
  
  #  par <- ifelse(parameter == "cap", "CapFac", "BidFac")
  
  #  colkeep1 <- ifelse(parameter == "cap", 6, 4)
  
  #  data1 <- data3[,c(2,3,4,5,6)]
  #  data1 <- reshape2::dcast(data1,tier~month)
  #  data2 <- data2[,c(1,5,4)]
  #  data2 <- reshape2::dcast(data2,tier~month)
  
  setwd("F:/My Drive/transfer")
  write.csv(data3, paste(type,"Incremental_Bid_Factors.csv",sep="_"))
  #  write.csv(data2, paste(type,"BidFac","OnPeak","Incremental_Bid_Factors.csv",sep="_"))
}

################################################################################
## FUNCTION: table_data  **NOT READ
## 
##
## INPUTS: 
##    Plant - Plant to look at
## TABLES REQUIRED: 
##    
################################################################################

table_data <- function(plant_type){#, year){
  
  # Set a start and end date if needed
  #  start <- as.Date(paste(year,"-01-01",sep=""))
  #  end <- as.Date(paste(year,"-12-31",sep=""))
  
  # Select just the plant in question from the spreadsheet 
  # "student_data_2021_Jul_23_14_09.csv.gz"
  data <- merit_filt %>%
    filter(Plant_Type == plant_type, 
           #           date >= start, 
           #           date <= end
    ) %>%
    group_by(date,he,month,price) %>%
    summarise(dispatched_mw = sum(dispatched_mw)) %>%
    select(date, he, price, dispatched_mw)
  
  # Save the plant name and type for the plot later
  type <- plant_type
  
  # Select just the plant in question from the nrgstream data.
  cap <- sub_samp %>%
    filter(Plant_Type == plant_type, 
           #           date >= start, 
           #           date <= end
    ) %>%
    group_by(date, he) %>%
    summarise(Capacity = sum(Capacity)) %>%
    select(date, he, Capacity)#, Cap_Fac)
  
  # Combine the two datasets by datetime and calculate the capacity factor for 
  # each row
  data1 <- left_join(data, cap, by=c("date","he")) %>%
    mutate(Capacity_Factor = ifelse(dispatched_mw/Capacity > 1, 1, 
                                    dispatched_mw/Capacity),
    )
  
  data2 <- data1 %>%
    filter(price == 0) %>%
    group_by(date,he) %>%
    mutate(tier = 1)#, Cap_F = sum(Capacity_Factor))
  
  data1 <- data1 %>%
    filter(price != 0) %>%
    group_by(date,he) %>%
    arrange(price) %>%
    mutate(tier = 2:(n()+1))#, Cap_F = sum(Capacity_Factor))
  
  data3 <- rbind(data1,data2)
  
  # Omit NA data
  data <- na.omit(data3)
  
  #data <- data %>%
  #  mutate(
  #    Condition = if_else(between(he, 08, 23), 
  #                        "On-Peak", "Off-Peak")
  #  )
  
  #  data1 <- data %>%
  #    filter(Condition == "On-Peak")
  
  # Summarize data by month and tier to find the average prices and capacity 
  # factors for each group
  data3 <- data %>%
    group_by(tier) %>%#,month) %>%
    summarise(avPrice = mean(price), 
              #              sdP = sd(price),
              avCap_Fac = mean(Capacity_Factor),
              #              sdC = sd(Capacity_Factor),
    )
  
  # Lump duplicate avCap_Fac together
  data2 <- data3 %>%
    group_by(avCap_Fac) %>%
    summarise(avPrice = max(avPrice),
              tier = min(tier)) %>%
    arrange(tier)
  
  #  Identify the lowest non-zero price to establish baseline
  mn <- ifelse(length(data2$tier)==1,data2$avPrice,
               min(ifelse(data2$avPrice==0,
                          data2$avPrice[data2$avPrice!=min(data2$avPrice)],
                          data2$avPrice)))
  
  # Calculate Aurora's bid factor for each row
  
  data3 <- data2 %>% 
    arrange(tier) %>%
    #    group_by(Condition) %>% #month) %>%
    mutate(overall_cap = round(cumsum(avCap_Fac),2),
           bid_fact = round(avPrice/mn-1,2)) %>%
    ungroup() %>%
    group_by(overall_cap) %>%
    summarise(tier = min(tier),
              bid_fact = mean(bid_fact))
  
  # Identify the largest tier
  mxtier <- max(data3$tier)
  mx2tier <- ifelse(mxtier==1,1,mxtier-1)
  
  #  par <- ifelse(parameter == "cap", "CapFac", "BidFac")
  
  #  colkeep1 <- ifelse(parameter == "cap", 6, 4)
  
  #  data1 <- data3[,c(2,3,4,5,6)]
  #  data1 <- reshape2::dcast(data1,tier~month)
  #  data2 <- data2[,c(1,5,4)]
  #  data2 <- reshape2::dcast(data2,tier~month)
  
  setwd("F:/My Drive/transfer")
  write.csv(data3, paste(type,"_Bid_Factors.csv",sep="_"))
  #  write.csv(data2, paste(type,"BidFac","OnPeak","Incremental_Bid_Factors.csv",sep="_"))
}

################################################################################
## FUNCTION: graph_type  **NOT READ
## 
##
## INPUTS: 
##    Plant - Plant to look at
## TABLES REQUIRED: 
##    
################################################################################

graph_type <- function(plant_type){
  # Plots each month capture prices, bidding factors at the various capacities
  
  # Set a start and end date if needed
  #  start <- as.Date(paste(year,"-01-01",sep=""))
  #  end <- as.Date(paste(year,"-12-31",sep=""))
  
  # Select just the plant in question from the spreadsheet 
  # "student_data_2021_Jul_23_14_09.csv.gz"
  data <- merit_filt %>%
    filter(Plant_Type == plant_type, 
           #           date >= start, 
           #           date <= end
    ) %>%
    group_by(date,he,month,price) %>%
    summarise(dispatched_mw = sum(dispatched_mw)) %>%
    select(date, month, he, price, dispatched_mw)
  
  # Save the plant name and type for the plot later
  type <- plant_type
  
  # Select just the plant in question from the nrgstream data.
  cap <- sub_samp %>%
    filter(Plant_Type == plant_type, 
           #           date >= start, 
           #           date <= end
    ) %>%
    group_by(date, he) %>%
    summarise(Capacity = sum(Capacity)) %>%
    select(date, he, Capacity)#, Cap_Fac)
  
  #  input <- ifelse(peak == "on", "On-Peak WECC", "Off-Peak WECC")
  
  # Combine the two datasets by datetime and calculate the capacity factor for 
  # each row
  data1 <- left_join(data, cap, by=c("date","he")) %>%
    mutate(Capacity_Factor = ifelse(dispatched_mw/Capacity > 1, 1, 
                                    dispatched_mw/Capacity),
    )
  
  data2 <- data1 %>%
    filter(price == 0) %>%
    group_by(date,he) %>%
    mutate(tier = 1, Cap_F = sum(Capacity_Factor))
  
  data1 <- data1 %>%
    filter(price != 0) %>%
    group_by(date,he) %>%
    arrange(price) %>%
    mutate(tier = 2:(n()+1), Cap_F = sum(Capacity_Factor))
  
  data3 <- rbind(data1,data2)
  
  # Omit NA data
  data <- na.omit(data3)
  
  data <- data %>%
    mutate(
      Condition = if_else(between(he, 08, 23), 
                          "On-Peak", "Off-Peak"),
      #     bidding_fact = round(price/mn-1,2)
    )
  
  # Summarize data by month and tier to find the average prices and capacity 
  # factors for each group
  data3 <- data %>%
    #    mutate(fact = round(price/mn-1,2)) %>%
    group_by(month,tier) %>%
    summarise(avPrice = mean(price), 
              #              sdP = sd(price),
              avCap_Fac = mean(Capacity_Factor),
              #              sdC = sd(Capacity_Factor),
              #              avbid_fact = mean(bidding_fact),
              #              sdB = sd(bid_fact)
    ) #%>%
  #    mutate(bid_fact = round(avPrice/mn-1,2))
  
  data2 <- data3 %>%
    group_by(month,avCap_Fac) %>%
    summarise(avPrice = max(avPrice),
              #              bid_fact = max(bid_fact),
              tier = min(tier)) %>%
    arrange(month,tier)
  
  #  Identify the lowest non-zero price to establish baseline
  mn <- ifelse(length(data2$tier)==1,data2$avPrice,
               min(ifelse(data2$avPrice==0,
                          data2$avPrice[data2$avPrice!=min(data2$avPrice)],
                          data2$avPrice)))
  
  # Calculate Aurora's bid factor for each row
  
  data3 <- data2 %>% 
    arrange(tier) %>%
    group_by(month) %>%
    mutate(overall_cap = cumsum(avCap_Fac))
  
  # Identify the largest tier
  mxtier <- max(data3$tier)
  mx2tier <- ifelse(mxtier==1,1,mxtier-1)
  
  # Summarize data by month and tier to find the average prices and capacity 
  # factors for each group
  data3 <- data1 %>%
    #    mutate(fact = round(price/mn-1,2)) %>%
    group_by(month,tier) %>%
    summarise(avPrice = mean(price), 
              avCap_Fac = mean(Capacity_Factor),
    ) %>%
    mutate(bid_fact = round(avPrice/mn-1,2))
  
  data2 <- data3 %>%
    group_by(month,avCap_Fac) %>%
    summarise(avPrice = max(avPrice),
              bid_fact = max(bid_fact),
              tier = min(tier)) %>%
    arrange(month,tier)
  
  #  Identify the lowest non-zero price to establish baseline
  mn <- ifelse(length(data2$tier)==1,data2$avPrice,
               min(ifelse(data2$avPrice==0,
                          data2$avPrice[data2$avPrice!=min(data2$avPrice)],
                          data2$avPrice)))
  
  # Calculate Aurora's bid factor for each row
  
  data3 <- data2 %>% 
    arrange(tier) %>%
    group_by(month) %>%
    mutate(overall_cap = cumsum(avCap_Fac))
  
  # Identify the largest tier
  mxtier <- max(data3$tier)
  mx2tier <- ifelse(mxtier==1,1,mxtier-1)
  
  # Rename the column with the correct capacity factors to the 'xaxis' and change
  # column format to character for readability
  #  names(data2)[mx2tier] <- "xaxis"
  data3$overall_cap <- as.character(paste(round(data3$overall_cap*100,3),"%",sep=""))
  
  # Remove extra columns
  #  data2 <- data2[,c(mx2tier,8,9,10,11,12)]
  
  # Relabel the second top tier if it was never dispatched ie. it was another
  # maximum price
  data4 <- data3 %>%
    group_by(month) %>%
    filter(tier == max(tier[tier!=max(tier)])) %>%
    mutate(overall_cap = ifelse(avCap_Fac == 0, paste(overall_cap,"~100%",sep=""),overall_cap))
  
  # Relabel the top tier to be a range to 100%
  data2 <- data3 %>%
    group_by(month) %>%
    filter(tier == max(tier)) %>%
    mutate(overall_cap = paste(overall_cap,"-100%",sep=""))
  
  data3 <- data3 %>%
    group_by(month) %>%
    filter(tier != max(tier),tier != max(tier[tier!=max(tier)]))
  
  # Recombine the data
  data1 <- rbind(data2,data3,data4)
  
  # Month list for plot labels
  mons <- list(
    "1" = "January",
    "2" = "February",
    "3" = "March",
    "4" = "April",
    "5" = "May",
    "6" = "June",
    "7" = "July",
    "8" = "August",
    "9" = "September",
    "10"= "October",
    "11"= "November",
    "12"= "December"
  )
  
  var_label <- function(variable,value){
    return(mons[value])
  }
  
  # Plot the data
  ggplot(data1, 
         aes(x = reorder(overall_cap,tier), y = avPrice)) +
    geom_bar(stat="identity") +
    facet_wrap(~month, scales="free_x", labeller=var_label) +
    geom_text(aes(label = 
                    paste("$",round(avPrice, digits = 0),
                          #                          "\nBF: ",
                          "\n",bid_fact,sep="")), 
              vjust = -0.3, size = 3.5)+#, angle = 90) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          text = element_text(size= 15),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust=1, size = 8)
    ) +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0,1300)) +
    labs(x = "% of Capacity Dispatched",
         y = "Average Offer Price \n($/MWh)",
         title = type
    )
}

################################################################################
## FUNCTION: Cap3  **NOT READ
## 
##
## INPUTS: 
##    Plant - Plant to look at
## TABLES REQUIRED: 
##    
################################################################################

Cap3 <- function(plant1,plant2,plant3) {
  #  MXa <- plyr::round_any(
  #    max(layer_scales(cap_offer(plant1))$y$range$range,
  #        layer_scales(cap_offer(plant2))$y$range$range,
  #        layer_scales(cap_offer(plant3))$y$range$range)+200,
  #    100, f = ceiling)
  #  MX <- 1300
  
  ggarrange(cap_offer(plant1), # + 
            #              scale_y_continuous(expand=c(0,0), limits = c(0,MX), 
            #                                 breaks = pretty_breaks(4)), 
            cap_offer(plant2), # + 
            #              scale_y_continuous(expand=c(0,0), limits = c(0,MX), 
            #                                 breaks = pretty_breaks(4)), 
            cap_offer(plant3), # + 
            #              scale_y_continuous(expand=c(0,0), limits = c(0,MX), 
            #                                 breaks = pretty_breaks(4)), 
            ncol = 2, nrow = 2)
}

Cap4 <- function(plant1,plant2,plant3,plant4) {
  #  MXa <- plyr::round_any(
  #    max(layer_scales(cap_offer(plant1))$y$range$range,
  #        layer_scales(cap_offer(plant2))$y$range$range,
  #        layer_scales(cap_offer(plant3))$y$range$range,
  #        layer_scales(cap_offer(plant4))$y$range$range)+200,
  #    100, f = ceiling)
  #
  #  MX <- 1300
  
  ggarrange(cap_offer(plant1), #+ 
            #              scale_y_continuous(expand=c(0,0), limits = c(0,MX), 
            #                                 breaks = pretty_breaks(4)), 
            cap_offer(plant2), #+ 
            #              scale_y_continuous(expand=c(0,0), limits = c(0,MX), 
            #                                 breaks = pretty_breaks(4)),
            cap_offer(plant3), #+ 
            #              scale_y_continuous(expand=c(0,0), limits = c(0,MX), 
            #                                 breaks = pretty_breaks(4)),
            cap_offer(plant4), #+ 
            #              scale_y_continuous(expand=c(0,0), limits = c(0,MX), 
            #                                 breaks = pretty_breaks(4)),
            ncol = 2, nrow = 2)
}

################################################################################
## FUNCTION: yearly_dmd  **NOT READ
## 
##
## INPUTS: 
##    Plant - Plant to look at
## TABLES REQUIRED: 
##    
################################################################################

yearly_dmd <- function(year) {
  data <- demand 
  
  data$Hour <- format(as.POSIXct(data$time, format = "%Y/%m/%d %H:%M:%S"), "%H")
  data$Year <- format(as.POSIXct(data$time, format = "%Y/%m/%d %H:%M:%S"), "%Y")
  data$Date <- format(as.POSIXct(data$time, format = "%Y/%m/%d %H:%M:%S"), "%Y-%m-%d")
  
  data <- data %>%
    filter(Year == year) %>%
    select(Date, Hour, Demand)
  
  setwd("D:/Documents/GitHub/AuroraEval")
  
  #SAVE FILE (To save the processed data) This is to be entered into Aurora.
  write.csv(data, file=paste("Alberta",year,".csv",sep=""))
}

################################################################################
## FUNCTION: monthly_dmd_ave  **NOT READ
## 
##
## INPUTS: 
##    Plant - Plant to look at
## TABLES REQUIRED: 
##    
################################################################################

monthly_dmd_ave <- function() {
  data <- demand 
  
  #  data$Hour <- format(as.POSIXct(data$time, format = "%Y/%m/%d %H:%M:%S"), "%H")
  #  data$Month <- format(as.POSIXct(data$time, format = "%Y/%m/%d %H:%M:%S"), "%m")
  data$Year <- format(as.POSIXct(data$time, format = "%Y/%m/%d %H:%M:%S"), "%Y")
  #  data$Date <- format(as.POSIXct(data$time, format = "%Y/%m/%d %H:%M:%S"), "%Y-%m-%d")
  
  data <- na.omit(data)
  
  data1 <- data %>%
    group_by(Year) %>%
    summarise(Load = mean(Demand)) %>%
    mutate(case = "Average")
  
  data2 <- data %>%
    group_by(Year) %>%
    summarise(Load = max(Demand)) %>%
    mutate(case = "Peak")
  
  data <- rbind(data1,data2)
  data$Year <- as.numeric(data$Year)
  #  data$Year <- ISOdate(data$Year, 1, 1)
  
  ggplot(data = data, 
         aes(x=Year, y=Load, group = case)) +
    #    geom_line(aes(linetype=case, color=case)) +
    geom_point(aes(shape = case, color=case)) +
    #    geom_smooth(method="lm", fullrange=TRUE) +
    geom_smooth(method=lm, fullrange = TRUE, se=FALSE, 
                aes(color=case)) +
    stat_regline_equation(
      #      label.y = 12000, 
      aes(label = ..eq.label..)
    ) +
    stat_regline_equation(
      label.x = 2010,
      aes(label = ..rr.label..)
    ) +
    geom_vline(xintercept = 2040,
               linetype = "solid", color = "dodgerblue", size = 0.5) +
    geom_hline(yintercept = 14860, linetype = "solid", color = "forestgreen", 
               size = 0.5) +
    geom_hline(yintercept = 12625, linetype = "solid", color = "forestgreen", 
               size = 0.5) +
    #    geom_text(x = 2005, y = 11500, label = lm_eqn(data), parse = TRUE) +
    scale_color_manual(values=c("black","grey40")) +
    #    scale_linetype_manual(values=c("solid","longdash")) +
    scale_x_continuous(breaks = seq(2004,2040,2),
                       limits = c(2004,2040)
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(vjust = 1, hjust = 0.5),
          panel.background = element_rect(fill = "transparent"),
          #          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          text = element_text(size= 15),
          legend.title = element_blank(),
    ) +
    scale_y_continuous(limits=c(7000,15000),
                       breaks=seq(7000,15000,500)) +
    labs(y = "Load (MWh)",
    )
}

################################################################################
## FUNCTION: AESO_Pr0t
## Price and output side by side 
##
## INPUTS: 
##    year, month, day - Date to look at
## FUNCTIONS REQUIRED: 
##    wkPrice - One week of AESO price data output
##    Week_act - One week of AESO data output
################################################################################

AESO_PrOt <- function(year,month,day) {
  plot_grid(wkPrice(year,month,day) + 
              theme(axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position ="bottom"),
            
            Week_act(year,month,day)+theme(legend.position ="none"), 
            ncol = 1, align="v", axis = "l",rel_heights = c(1,2.5)) 
  }