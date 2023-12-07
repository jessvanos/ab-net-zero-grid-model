################################################################################
# TITLE: AESO_ANALYSIS
# DESCRIPTION:  Code to analyze AESO data. Includes historical (NRGStream and merit data) data.


# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: December 2023

# NOTES: Make sure the project file is open first or "here" commands wont work right.
#        Before running, create folder called "Data Files" withen project directory and populate it with AESO data. 
#        Once this file is run through completion, can call any functions with environment that is loaded.

################################################################################

################################################################################
## LOAD REQUIRED PACKAGES AND SOURCE FUNCTIONS
################################################################################

{ # Must load the here package in order to make sure internal project directories work
  library(here)
  
  # Import functions from other R files, take from the functions folder in R project
  source(here('Functions','Other_Functions.R'))       # Other functions used in plotting functions
  #source(here('Functions','aeso_gen.R'))           #
  source(here('Functions','Group_PlotSave.R'))          #
  
  
  packs_to_load = c("tidyverse","ggridges","ggplot2","scales","grid","gtable","gridExtra","odbc","ggpubr","extrafont",
                   "DBI","lubridate","cowplot","scales","dplyr","reshape2","zoo",
                   "ggpattern","here","beepr","showtext","DescTools","pivottabler",
                   "openxlsx","sqldf","timeDate","writexl","viridis","ggnewscale","readxl")
  # Function to check for packages, install if not present, and load
  packs_check(packs_to_load)
  
}


################################################################################
## PART 1: LOAD DATE (REQUIRED)
################################################################################
################################################################################
## 1.A: Save new data as R file to modify (IF NEW DATA)
## Only need to run if new files are available
################################################################################
# 
# # Load merit data
# {
# merit <- read_csv(here("Data Files","Alberta Data","student_data_2023_Aug_15_16_56.csv.gz"))
#   # Save as R file
#   saveRDS(merit, here("Data Files","Alberta Data","Leach_MeritData15Aug2023.RData"))
#   # Remove from workspace
#    rm(merit)
#   }
# 
# # Load NRG data and rename time column
# {
#      load(here("Data Files","Alberta Data","nrgstream_gen03Mar2023.RData"))
#      nrgstream_gen <- nrgstream_gen %>%
#        rename(time=Time)
# 
#      # Remove NA values
#      nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$gen),]
#      nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$time),]
# 
#      # Apply data corrections
#      corrected <- nrgstream_gen %>%
#          filter(is.na(Latitude)) %>%
#          mutate(Latitude=case_when(grepl("BRD1",ID) ~ 49.842735,
#                                    grepl("BUR1",ID) ~ 49.814877,
#                                    grepl("CLR",ID) ~ 50.032911,
#                                    grepl("CLY",ID) ~ 49.840967,
#                                    grepl("CHP1",ID) ~ 50.22189,
#                                    grepl("COL1",ID) ~ 49.833218,
#                                    grepl("CRD",ID) ~ 49.807,
#                                    grepl("CRR2",ID) ~ 49.55891,
#                                    grepl("FMG1",ID) ~ 49.66334,
#                                    grepl("KKP",ID) ~ 53.469986,
#                                    grepl("MON1",ID) ~ 49.833144,
#                                    grepl("NMK1",ID) ~ 51.026118,
#                                    grepl("RIV1",ID) ~ 49.53245,
#                                    grepl("STR",ID) ~ 51.033273,
#                                    grepl("TVS1",ID) ~ 50.27324,
#                                    grepl("VCN1",ID) ~ 50.0975,
#                                    grepl("VXH1",ID) ~ 50.095223,
#                                    grepl("WEF1",ID) ~ 49.65405,
#                                    grepl("WHT",ID) ~ 49.64029),
#                 Longitude=case_when(grepl("BRD1",ID) ~ -111.537891,
#                                     grepl("BUR1",ID) ~ -111.543323,
#                                     grepl("CHP1",ID) ~ -110.437106,
#                                     grepl("CLR",ID) ~ -113.484369,
#                                     grepl("CLY",ID) ~ -110.356864,
#                                     grepl("COL1",ID) ~ -112.97448,
#                                     grepl("CRD",ID) ~ -112.578,
#                                     grepl("CRR2",ID) ~ -113.983,
#                                     grepl("FMG1",ID) ~ -111.122,
#                                     grepl("KKP",ID) ~ -113.61337,
#                                     grepl("MON1",ID) ~ -112.974231,
#                                     grepl("NMK1",ID) ~ -113.163017,
#                                     grepl("RIV1",ID) ~ -113.977,
#                                     grepl("STR",ID) ~ -113.371296,
#                                     grepl("TVS1",ID) ~ -112.73059,
#                                     grepl("VCN1",ID) ~ -112.84841,
#                                     grepl("VXH1",ID) ~ -112.149936,
#                                     grepl("WEF1",ID) ~ -111.515812,
#                                     grepl("WHT",ID) ~ -111.291),
#                 Installation_Year=case_when(grepl("CRR2",ID)~2019,
#                                             grepl("CYP",ID)~2022,
#                                             #grepl("CYP2",ID)~"post2019",
#                                             grepl("FMG1",ID)~2022,
#                                             grepl("GDP1",ID)~2022,
#                                             grepl("GRZ1",ID)~2022,
#                                             grepl("HHW1",ID)~2022,
#                                             grepl("HLD1",ID)~2022,
#                                             grepl("JNR",ID)~2022,
#                                             grepl("RIV1",ID)~2019,
#                                             grepl("RTL1",ID)~2021,
#                                             grepl("WHE1",ID)~2022,
#                                             grepl("WHT1",ID)~2019,
#                                             grepl("WHT2",ID)~2021,
#                                             grepl("WRW1",ID)~2021),
#                 Installation_Year=case_when(is.na(Installation_Year)~"pre2019",
#                                               TRUE~"post2019"))
# 
#      # Get non-corrected and remove Latitude
#      nocorrection <- nrgstream_gen %>%
#          filter(!is.na(Latitude))%>%
#        mutate(Installation_Year="")
# 
#      # put back together and remove old files
#      nrgstream_gen <- rbind(corrected,nocorrection)
#         rm(corrected,nocorrection)
# 
#      # Save new file
#      saveRDS(nrgstream_gen,here("Data Files","Alberta Data","nrgstream_gen_corrected03Mar2023.RData"))
# 
#      # Make separate file for demand and save
#      Actdemand <- nrgstream_gen %>%
#          group_by(time) %>%
#          summarise(Demand = median(Demand),
#                    Price = median(Price),
#                    AIL = median(AIL))
# 
#      # Save the demand
#      saveRDS(Actdemand, here("Data Files","Alberta Data","nrgstream_demand03Mar2023.RData"))
#         rm(Actdemand,nrgstream_gen)
# }

################################################################################
## 1.B: Read in data and format  (REQUIRED)
################################################################################
{ 
  # Load Leach Merit Data - Hourly resource info for Alberta (similar to ResHr and StackHr)
  merit <- readRDS(here("Data Files","Alberta Data","Leach_MeritData15Aug2023.RData"))
  
  #Filter Data to relevant dates & remove old data
  merit_filt <- filter(merit,date >= as.Date("2015-01-1"))
  rm(merit)
  
  # Load nrgstream_gen - Load and demand info, plus a whole ton more
  nrgstream_gen <- readRDS(here("Data Files","Alberta Data","nrgstream_gen_corrected03Mar2023.RData")) 
  Actdemand <- readRDS(here("Data Files","Alberta Data","nrgstream_demand03Mar2023.RData"))
  
  #Reformat the dates
  Actdemand$Day <- date(Actdemand$time)
  
  # Take out dates I don't care about and remove the old table
  sub_samp<-filter(nrgstream_gen, time >= as.Date("2015-01-1"))
  Actdemand<-filter(Actdemand, time >= as.Date("2015-01-1"))
  
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
              total_gen=sum(gen,na.rm = T),
              total_cap=sum(Capacity,rm = T),
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
             year(time)<2024)
    
    # Put in desired order: Coal, Cogen, NGCC, SCGT, Other, Hydro, Wind, Solar, Import, Export
    df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "OTHER",after=Inf)
    df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "HYDRO",after=Inf)
    df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "WIND",after=Inf)
    df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "SOLAR",after=Inf)
    df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "IMPORT",after=Inf)
    df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "EXPORT",after=Inf)
    gc()   
  }
}

################################################################################
## PART 2: COGEN ANALYSIS (OPTIONAL)
################################################################################

# Read data
Cogen_Hourly_Data <- readRDS(here("Data Files","Alberta Data","Cogen_Hourly_Data.RData"))





## PART 3
################################################################################
## FILTER DATA
################################################################################
# Filter data for wind and solar data in selected years
# Arrange by timepoint - important for calculation!
WSdata<-df1 %>%
  filter(Plant_Type %in% c("WIND","SOLAR"),
         Year == 2021) %>%
  mutate(Month=month(Day))%>%
  group_by(time,Day,Hour,Month,Year) %>%
  summarise(GEN=sum(total_gen),
            CAPACITY=sum(total_cap))%>%
  arrange(time)

# Fill missing timepoints
all_time_points <- seq(min(WSdata$time), max(WSdata$time), by = "1 hour")
all_times <- data.frame(time = all_time_points)
WSdata <- merge(all_times, WSdata, by = "time", all = TRUE) %>%
  mutate(GEN = ifelse(is.na(GEN), 0, GEN))

################################################################################
## CALCULATE ALL
################################################################################
# Group data by adding a descriptive column.
# Create labels for:  generation data < 500 MW
#                     500 MW <= generation data < 750 MW
#                     1000 MW <= generation data < 1500 MW
#                     1500 MW <= generation data < MAX
WSdata2<-WSdata %>%
  mutate(gen_range=ifelse(GEN<500,"< 500 MW",
                          ifelse((GEN>=500 & GEN<750),"500 - 750 MW",
                                 ifelse((GEN>=750 & GEN<1000),"750 - 1000 MW",
                                        ifelse((GEN>=1000 & GEN<1500),"1000 - 1500 MW",
                                               ifelse((GEN>=1500),"1500+ MW",0))))))%>%
  arrange(time) %>%
  # Steps to look up dates later -> shows the count for low gen days
  group_by(gen_range, grp = with(rle(gen_range), rep(seq_along(lengths), lengths))) %>%
  mutate(COUNTER = seq_along(grp)) %>%
  ungroup() %>%
  select(-grp)


# Get consecutive days for each value using the "rle" function, where "unclass" converts to dataframe
WSstats<-data.frame(unclass(rle(WSdata2$gen_range))) %>%
  rename(consec_hours=lengths,
         max_gen=values) %>%
  mutate(Hour_range=cut(consec_hours,breaks=c(0,2,5,10,20,30,40,50,100,150,200)))

# Take data and group by generation group (ie: <500 MW) and hour range. Count number of instances
WSstats2<-WSstats %>%
  group_by(max_gen,Hour_range)%>%
  count(Hour_range)%>%
  rename(count="n")

################################################################################
## PLOT ALL
################################################################################

# Plot the all data!
WSstats2 %>%
  ggplot(aes(x=Hour_range,y=count,fill=max_gen)) +
  geom_bar(position=position_dodge(preserve = 'single'),stat="identity",'color'="black") +
  
  theme_bw() +
  
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = NA),
        panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray90'),
        legend.position = "right") +
  
  scale_y_continuous(expand=c(0,0),limits=c(0,round(max(WSstats2$count),-1)),breaks=pretty_breaks(10)) +
  
  labs(x = "Consecutive Hours in Generation Range", y = "Number of Instances", fill = "Generation Range") +
  
  scale_fill_brewer(palette="Blues")       

################################################################################
## PLOT 2
################################################################################

# Same data, remove the lowest hour range and 1500+ category
WSstats3 <- WSstats2 %>%
  filter(
    #Hour_range != "(0,2]",
    max_gen != "1500+ MW")

# Plot more filtered data
WSstats3 %>%
  ggplot(aes(x=Hour_range,y=count,fill=max_gen)) +
  geom_bar(position=position_dodge(preserve = 'single'),stat="identity",'color'="black") +
  
  theme_bw() +
  
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = NA),
        panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray90'),
        legend.position = "right") +
  
  scale_y_continuous(expand=c(0,0),limits=c(0,round(max(WSstats3$count),-1)),breaks=pretty_breaks(10)) +
  
  labs(x = "Consecutive Hours in Generation Range", y = "Number of Instances", fill = "Generation Range") +
  
  scale_fill_brewer(palette="Blues")   

################################################################################
## CALCULATE SMALL GROUPS
################################################################################
# Group data by adding a descriptive column.
# Create labels for:  generation data < 500 MW
#                     500 MW <= generation data < 750 MW
#                     1000 MW <= generation data < 1500 MW
#                     1500 MW <= generation data < MAX

WSdataLOW<-WSdata %>%
  mutate(gen_range=ifelse(GEN<150,"< 150 MW",
                          ifelse((GEN>=150 & GEN<300),"150 - 300 MW",
                                 ifelse((GEN>=300 & GEN<450),"300 - 450 MW",
                                        ifelse((GEN>=450 & GEN<600),"450 - 600 MW",
                                               ifelse((GEN>=600),"600+ MW",0))))))%>%
  arrange(time)  %>%
  # Steps to look up dates later
  group_by(gen_range, grp = with(rle(gen_range), rep(seq_along(lengths), lengths))) %>%
  mutate(COUNTER = seq_along(grp)) %>%
  ungroup() %>%
  select(-grp)

# Get consecutive days for each value using the "rle" function, where "unclass" converts to dataframe
WSdataLOW2<-data.frame(unclass(rle(WSdataLOW$gen_range))) %>%
  rename(consec_hours=lengths,
         max_gen=values) %>%
  mutate(Hour_range=cut(consec_hours,breaks=c(0,2,5,10,20,30,40,50,100,150,200)))

# Take data and group by generation group (ie: <500 MW) and hour range. Count number of instances
WSdataLOW3<-WSdataLOW2 %>%
  group_by(max_gen,Hour_range)%>%
  count(Hour_range)%>%
  rename(count="n")%>%
  filter(max_gen != "600+ MW")

################################################################################
## PLOT 3
################################################################################

# Plot more filtered data
WSdataLOW3 %>%
  ggplot(aes(x=Hour_range,y=count,fill=max_gen)) +
  geom_bar(position=position_dodge(preserve = 'single'),stat="identity",'color'="black") +
  
  theme_bw() +
  
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = NA),
        panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'gray90'),
        legend.position = "right") +
  
  scale_y_continuous(expand=c(0,0),limits=c(0,round(max(WSdataLOW3$count),-1)),breaks=pretty_breaks(10)) +
  
  labs(x = "Consecutive Hours in Generation Range", y = "Number of Instances", fill = "Generation Range") +
  
  scale_fill_brewer(palette="Blues")   

################################################################################
## PART 4: OTHER AESO FUNCTIONS 
################################################################################

#AESO Output
Week_act(2020,04,08)

#AESO Week Price
wkPrice(2021,10,08)

# AESO Week Price and output in one
AESO_PrOt(2021,01,08)

# Wind duration curve with output as is
Wind_Dur_AESO(BC)

# Wind duration curve with Output normalized
Wind_DurNorm_AESO(BC)
