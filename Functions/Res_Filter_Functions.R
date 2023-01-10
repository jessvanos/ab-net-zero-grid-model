################################################################################
# TITLE: Res_Filter_Functions
# DESCRIPTION: Functions to select and sort certain resource groups for other functions to use. A collection of filters.

# ORIGINAL AUTHOR: Taylor Pawlenchuk (Retrieved June 3, 2022)
# EDITS & ADDITIONAL CONTENT: Jessica Van Os
# LAST EDIT: September 15, 2022
#
################################################################################
## FUNCTION: sim_filt
## This function filters for the data that will be evaluated.
################################################################################

{ sim_filt <- function(inputdata) {
  
  # Filter the data by resource, creates a table for each resource
  {Coal <- inputdata %>%
    filter(ID=="LTO_Coal")
  Coal2Gas  <- inputdata %>%
    filter(ID=="LTO_Coal2Gas")
  #Force zero output if negative
  #Coal2Gas$Output_MWH[Coal2Gas$Output_MWH <= 0] <- 0
  Cogen  <- inputdata %>%
    filter(ID=="LTO_Cogen")
  NatGas <- inputdata %>%
    filter(ID=="LTO_NatGas")
  NatGas_CCS <- inputdata %>%
    filter(ID=="AB_CC90CCS_noncogen")
  Other <- inputdata %>%
    filter(ID=="LTO_Other")
  Hydro <- inputdata %>%
    filter(ID=="LTO_Hydro")
  Solar <- inputdata %>%
    filter(ID=="LTO_Solar")
  Storage <- inputdata %>%    
    filter(ID=="LTO_Storage")
  Wind <- inputdata %>%
    filter(ID=="LTO_Wind")  
  H2 <-inputdata %>%
    filter(ID=="LTO_H2") 
  NGH2_Blend <-inputdata %>%
    filter(ID %in% c("AB_CCCT_Blended","AB_SCCT_Blended") )
  NGH2_Blend$ID<- "NGH2_Blend"
  
  # Not added yet, can add if building
  Nuclear <-inputdata %>%
    filter(ID=="LTO_Nuclear") 
  }
  
  # Combine the grouped data tables into one
  { case <- rbind( Coal2Gas, NatGas,NatGas_CCS, NGH2_Blend,H2, Hydro, Solar, Wind, Storage, Other,Coal,Cogen)
    
    # Sort the table by case ID
    #A factor is a categorical variable 
    case$ID <- factor(case$ID, levels=c( "LTO_Coal2Gas", "LTO_NatGas","AB_CC90CCS_noncogen",
                                         "NGH2_Blend", 
                                         "LTO_H2","LTO_Hydro","LTO_Other",  
                                         "LTO_Wind", "LTO_Solar", "LTO_Storage",
                                         "LTO_Coal", "LTO_Cogen"))
    # Replace ID value with name 
    levels(case$ID) <- c("Coal-to-Gas", "Natural Gas","Natural Gas + CCS","Natual Gas and Hydrogen Blend","Hydrogen" , 
                         "Hydro","Other","Wind", "Solar", "Storage","Coal","Cogen")   }
  return(case)  }
}

################################################################################
## FUNCTION: sim_filt1
## This function filters for the data that will be evaluated.  
################################################################################

{ sim_filt1 <- function(inputdata) {
  # Filter the data by resource
  {Coal <- inputdata %>%
    filter(ID=="LTO_Coal")
  Cogen  <- inputdata %>%
    filter(ID=="LTO_Cogen")
  Other <- inputdata %>%
    filter(ID=="LTO_Other")
  Hydro <- inputdata %>%
    filter(ID=="LTO_Hydro")
  Solar <- inputdata %>%
    filter(ID=="LTO_Solar")
  Storage <- inputdata %>%    
    filter(ID=="LTO_Storage")
  Wind <- inputdata %>%
    filter(ID=="LTO_Wind")  
  
  NGConv <- inputdata %>%
    filter(ID=="AB_NGCONV")
  SCCT  <- inputdata %>%
    filter(ID=="AB_SCCT_noncogen")
  CCCT <- inputdata %>%
    filter(ID=="AB_CCCT_noncogen")
  CCCT_CCS <- inputdata %>%
    filter(ID=="AB_CC90CCS_noncogen")
  
  SCCT_H2  <- inputdata %>%
    filter(ID=="AB_SCCT_0NG100H2")
  CCCT_H2 <- inputdata %>%
    filter(ID=="AB_CCCT_0NG100H2")
  SCCT_Blend  <- inputdata %>%
    filter(ID=="AB_SCCT_Blended")
  CCCT_Blend <- inputdata %>%
    filter(ID=="AB_CCCT_Blended")
  
  }
  
  # Not added yet, can add if building
  Nuclear <-inputdata %>%
    filter(ID=="LTO_Nuclear") 
  
  # Combine the grouped data
  { case <- rbind(NGConv, SCCT_H2,CCCT_H2,SCCT_Blend,CCCT_Blend,
                  SCCT, CCCT_CCS, CCCT, Hydro, Other,Wind, Solar, Storage, Coal, Cogen )
    
    case$ID <- factor(case$ID, levels=c("LTO_Solar","LTO_Wind","LTO_Other","LTO_Hydro", 
                                        "AB_SCCT_0NG100H2","AB_CCCT_0NG100H2",
                                        "AB_SCCT_Blended","AB_CCCT_Blended",
                                        "AB_SCCT_noncogen", "AB_CC90CCS_noncogen","AB_CCCT_noncogen",
                                         "AB_NGCONV", 
                                        "LTO_Coal","LTO_Cogen","LTO_Storage"))
    
    levels(case$ID) <- c("Solar","Wind", "Other", "Hydro", 
                          "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                         "Blended  Simple Cycle","Blended  Combined Cycle",
                         "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                         "Coal-to-Gas", 
                         "Coal", "Cogeneration","Storage")  }
  return(case)  }
}

################################################################################
## FUNCTION: sim_filt2
## This function filters for the data that will be evaluated.  
################################################################################

{ sim_filt2 <- function(inputdata) {
  {
    # Straight foreward part
    Coal <- inputdata %>%
      filter(Primary_Fuel=="Coal Canada West") 
    Cogen  <- inputdata %>%
      filter(Primary_Fuel=="WECC-AECO Hub NaturalGas-COGEN_oilsands_Alberta")
    Other <- inputdata %>%
      filter(Primary_Fuel=="Other, Bio, ZZ, WC, WH")
    Hydro <- inputdata %>%
      filter(Primary_Fuel=="Water")
    Solar <- inputdata %>%
      filter(Primary_Fuel=="Solar")
    Storage <- inputdata %>%    
      filter(grepl("Storage",Primary_Fuel))
    Wind <- inputdata %>%
      filter(Primary_Fuel=="Wind")  
    Nuclear <-inputdata %>%
      filter(Primary_Fuel=="Uranium") 
    
    # Get NG Units as defined in Resource Table
    CCCT1 <- inputdata %>%
      filter(Primary_Fuel=="WECC-Alberta NaturalGas")
    # More tricky to separate ones
    NG2AB <- inputdata %>%
      filter(Primary_Fuel=="WECC-Alberta NaturalGas-Peaking")
              
              # Separate retrofits
              NGConv1 <- NG2AB[NG2AB$Name %like% "%Retrofit%",] 
              NGConv<-NGConv1 # Swt this as the retrofits to output, will use the dataframe with non-edited values to filter the remaining
              NGConv$Primary_Fuel<- "NGConv"
              
              # Separate Simple Cycle
              SCCT1 <- sqldf('SELECT * FROM NG2AB EXCEPT SELECT * FROM NGConv1') #Select left over 
              SCCT1<-SCCT1 %>%
                mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"NG-SCCT")) 
              
              SCCT1$Primary_Fuel<- "NG-SCCT"
    
    # Units as defined by New Resources Table
    #First split up the fuel types
    NG100 <- inputdata %>%
      filter(Primary_Fuel=="100-NaturalGas-0-Hydrogen") 
    H2 <- inputdata %>%
      filter(Primary_Fuel=="0-NaturalGas-100-Hydrogen")

            # Combined units with CCS
            CCC_CCS <- NG100[NG100$Name %like% "%2022CC90CCS%",] %>%
              mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"CC-CCS"))
            
            # Combined NG units
            CCCT2 <-NG100[NG100$Name %like% "%2022CC_100NG0H2%",] %>%
              mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"WECC-Alberta NaturalGas"))  
            CCCT <- rbind(CCCT1,CCCT2)
            
            # Simple NG units
            SCCT2 <-NG100 %>%
              filter(grepl('2022Frame|2022Aeroderivative',Name)) %>%
              mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"NG-SCCT"))
    SCCT <- rbind(SCCT1,SCCT2)
    
    # Blended Combined cycle
    CC_Blend <-inputdata %>%
      filter(Primary_Fuel %in% c('20-NaturalGas-80-Hydrogen Combined Cycle','50-NaturalGas-50-Hydrogen Combined Cycle','70-NaturalGas-30-Hydrogen Combined Cycle')) %>%
      mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"Blend-CC")) 
    
    # Blended Simple Cycle
    SC_Blend <-inputdata %>%
      filter(Primary_Fuel %in% c('20-NaturalGas-80-Hydrogen Simple Cycle','50-NaturalGas-50-Hydrogen Simple Cycle','70-NaturalGas-30-Hydrogen Simple Cycle')) %>%
      mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"Blend-SC")) 
    
    # H2 Combined cycle
    CC_H2 <-H2[H2$Name %like% "%2022CC_0NG100H2%",] %>%
      mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"H2-CC"))  
    
    # H2 Simple cycle
    SC_H2 <-H2 %>%
      filter(grepl( '2022Frame|2022Aeroderivative',Name)) %>%
      mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"H2-SC")) 
    
  }
  
  # Combine the grouped data
  { case <- rbind(NGConv, SC_H2,CC_H2,SC_Blend,CC_Blend,
                  SCCT, CCC_CCS,CCCT, Hydro, Other, Solar, Wind, Storage, Coal, Cogen)
    
    case$Primary_Fuel <- factor(case$Primary_Fuel, levels=c(
      "NGConv","H2-SC","H2-CC","Blend-SC","Blend-CC",
      "NG-SCCT","CC-CCS","WECC-Alberta NaturalGas",
      "Water", "Other, Bio, ZZ, WC, WH",
      "Wind", "Solar", "Storage", 
      "Coal Canada West", "WECC-AECO Hub NaturalGas-COGEN_oilsands_Alberta"))
    
    levels(case$Primary_Fuel) <- c("Coal-to-Gas", "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                   "Blended  Simple Cycle","Blended  Combined Cycle",
                                   "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                                   "Hydro", "Other",
                                   "Wind", "Solar", "Storage","Coal", "Cogeneration") }
  return(case)  }
}

################################################################################
## FUNCTION: sim_filt3
## This function filters for the data that will be evaluated.  
## Same as 2, with coal removed and storage into 3 components
################################################################################

{ sim_filt3 <- function(inputdata) {
  {
    # Straight foreward part
    Cogen  <- inputdata %>%
      filter(Primary_Fuel=="WECC-AECO Hub NaturalGas-COGEN_oilsands_Alberta")
    Other <- inputdata %>%
      filter(Primary_Fuel=="Other, Bio, ZZ, WC, WH")
    Hydro <- inputdata %>%
      filter(Primary_Fuel=="Water")
    Solar <- inputdata %>%
      filter(Primary_Fuel=="Solar")
    Storage <- inputdata %>%    
      filter(grepl("Storage",Primary_Fuel))
    Wind <- inputdata %>%
      filter(Primary_Fuel=="Wind")  
    Nuclear <-inputdata %>%
      filter(Primary_Fuel=="Uranium") 
    
    # Blended Combined cycle
    CC_Blend <-inputdata %>%
      filter(Primary_Fuel %in% c('20-NaturalGas-80-Hydrogen Combined Cycle','50-NaturalGas-50-Hydrogen Combined Cycle','70-NaturalGas-30-Hydrogen Combined Cycle')) %>%
      mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"Blend-CC")) 
    
    # Blended Simple Cycle
    SC_Blend <-inputdata %>%
      filter(Primary_Fuel %in% c('20-NaturalGas-80-Hydrogen Simple Cycle','50-NaturalGas-50-Hydrogen Simple Cycle','70-NaturalGas-30-Hydrogen Simple Cycle')) %>%
      mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"Blend-SC")) 
    
    # Get NG Units as defined in Resource Table
    CCCT1 <- inputdata %>%
      filter(Primary_Fuel=="WECC-Alberta NaturalGas")
    
    # More tricky to separate ones
    NG2AB <- inputdata %>%
      filter(Primary_Fuel=="WECC-Alberta NaturalGas-Peaking")
            
            # Separate retrofits
            NGConv1 <- NG2AB[NG2AB$Name %like% "%Retrofit%",] 
            NGConv<-NGConv1 # Swt this as the retrofits to output, will use the dataframe with non-edited values to filter the remaining
              NGConv$Primary_Fuel<- "NGConv"
            
              # Separate Simple Cycle
            SCCT1 <- sqldf('SELECT * FROM NG2AB EXCEPT SELECT * FROM NGConv1') #Select left over 
            SCCT1<-SCCT1 %>%
              mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"NG-SCCT")) 
            
            SCCT1$Primary_Fuel<- "NG-SCCT"
    
    # Units as defined by New Resources Table
    #First split up the fuel types
    NG100 <- inputdata %>%
      filter(Primary_Fuel=="100-NaturalGas-0-Hydrogen") 

            # Combined units with CCS
            CCC_CCS <- NG100[NG100$Name %like% "%2022CC90CCS%",] %>%
              mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"CC-CCS"))
            
            # Combined NG units
            CCCT2 <-NG100[NG100$Name %like% "%2022CC_100NG0H2%",] %>%
              mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"WECC-Alberta NaturalGas"))    
              CCCT <- rbind(CCCT1,CCCT2)
              
              # Simple NG units
              SCCT2 <-NG100 %>%
                filter(grepl('2022Frame|2022Aeroderivative',Name)) %>%
                mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"NG-SCCT"))
              SCCT <- rbind(SCCT1,SCCT2)
    
    H2 <- inputdata %>%
      filter(Primary_Fuel=="0-NaturalGas-100-Hydrogen")         
    
              # H2 Combined cycle
              CC_H2 <-H2[H2$Name %like% "%2022CC_0NG100H2%",] %>%
                mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"H2-CC"))  
              
              # H2 Simple cycle
              SC_H2 <-H2 %>%
                filter(grepl( '2022Frame|2022Aeroderivative',Name)) %>%
                mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"H2-SC")) 
    
    # Storage Types
    Stor_B <-    Storage %>%    
      filter(Primary_Fuel=="Storage - Battery")
    
    Stor_HP <- Storage %>%    
      filter(Primary_Fuel=="Storage - HydroPumped")
    
    Stor_CA <- Storage  %>%    
      filter(Primary_Fuel=="Storage - CompressedAir") 
    
    
  }
  
  # Combine the grouped data
  { case <- rbind(NGConv, SC_H2,CC_H2,SC_Blend,CC_Blend,
                  SCCT, CCC_CCS,CCCT, Hydro, Other, Solar, Wind, Stor_B,Stor_CA, Stor_HP, Cogen)
    
    case$Primary_Fuel <- factor(case$Primary_Fuel, levels=c(
      "NGConv","H2-SC","H2-CC","Blend-SC","Blend-CC",
      "NG-SCCT","CC-CCS","WECC-Alberta NaturalGas",
      "Water", "Other, Bio, ZZ, WC, WH",
      "Wind", "Solar", 
      "Storage - Battery", "Storage - CompressedAir","Storage - HydroPumped",
      "WECC-AECO Hub NaturalGas-COGEN_oilsands_Alberta"))
    
    levels(case$Primary_Fuel) <- c("Coal-to-Gas", "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                   "Blended  Simple Cycle","Blended  Combined Cycle",
                                   "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                                   "Hydro", "Other",
                                   "Wind", "Solar", 
                                   "Storage - Battery", "Storage - Compressed Air", "Storage - Pumped Hydro", 
                                   "Cogeneration") }
  return(case)  }
}

################################################################################
## FUNCTION: sim_filt4
## This function filters for the data that will be evaluated.  
## Same as 3, however only includes Aurora build options and is based on Fuel Type
################################################################################

{ sim_filt4 <- function(inputdata) {
  {
    # Straight foreward part
    Other <- inputdata %>%
      filter(Fuel_Type=="OT")
    Hydro <- inputdata %>%
      filter(Fuel_Type=="WAT")
    Solar <- inputdata %>%
      filter(Fuel_Type=="SUN")
    Wind <- inputdata %>%
      filter(Fuel_Type=="WND")  
    Nuclear <-inputdata %>%
      filter(Fuel_Type=="UR") 
    
    # Storage Types
    Stor_B <-    inputdata %>%    
      filter(Fuel_Type=="PS")
    Stor_HP <- inputdata %>%    
      filter(Fuel_Type=="PS3")
    Stor_CA <- inputdata  %>%    
      filter(Fuel_Type=="PS2") 
    
    # Units as defined by New Resources Table
    #First split up the fuel types
    H2 <- inputdata %>%
      filter(Fuel_Type == "H2")
    Blend <- inputdata %>%
      filter(grepl( 'GasB_CC|GasB_SC',Fuel_Type)) %>%
      mutate(Fuel_Type=ifelse(is.na(Fuel_Type),NA,"GasB"))
    NG <-inputdata %>%
      filter(grepl( 'Gas|Gas1|Gas2',Fuel_Type)) %>%
      mutate(Fuel_Type=ifelse(is.na(Fuel_Type),NA,"NG")) 
    
  }
  
  # Combine the grouped data
  { case <- rbind(H2,Blend,NG,
                  Hydro, Other, Solar, Wind, Stor_B,Stor_CA, Stor_HP)
    
    case$Fuel_Type <- factor(case$Fuel_Type, levels=c(
      "H2","GasB","NG",
      "WAT", "OT",
      "WND", "SUN", 
      "PS", "PS2","PS3"))
    
    levels(case$Fuel_Type) <- c("Hydrogen","Natual Gas and Hydrogen Blend","Natural Gas", 
                                "Hydro", "Other",
                                "Wind", "Solar", 
                                "Storage - Battery", "Storage - Compressed Air", "Storage - Pumped Hydro") }
  return(case)  }
}

################################################################################
## FUNCTION: sim_filt5
## This function filters for the data that will be evaluated.  
################################################################################

sim_filt5 <- function(inputdata) {
  {
    # Filter the data by resource, creates a table for each resource
    {
    Coal <- inputdata %>%
      filter(ID=="LTO_Coal")
    Cogen  <- inputdata %>%
      filter(ID=="LTO_Cogen")
    Other <- inputdata %>%
      filter(ID=="LTO_Other")
    Hydro <- inputdata %>%
      filter(ID=="LTO_Hydro")
    Solar <- inputdata %>%
      filter(ID=="LTO_Solar")
    Wind <- inputdata %>%
      filter(ID=="LTO_Wind")  
    
    NGConv <- inputdata %>%
      filter(ID=="AB_NGCONV")
    SCCT  <- inputdata %>%
      filter(ID=="AB_SCCT_noncogen")
    CCCT <- inputdata %>%
      filter(ID=="AB_CCCT_noncogen")
    CCCT_CCS <- inputdata %>%
      filter(ID=="AB_CC90CCS_noncogen")
    
    SCCT_H2  <- inputdata %>%
      filter(ID=="AB_SCCT_0NG100H2")
    CCCT_H2 <- inputdata %>%
      filter(ID=="AB_CCCT_0NG100H2")
    SCCT_Blend  <- inputdata %>%
      filter(ID=="AB_SCCT_Blended")
    CCCT_Blend <- inputdata %>%
      filter(ID=="AB_CCCT_Blended")
    
    Stor_B <- inputdata %>%    
      filter(ID=="Battery")
    Stor_HP <- inputdata %>%    
      filter(ID=="HydroPumped")
    Stor_CA <- inputdata %>%    
      filter(ID=="CompressedAir")
    
    # Not added yet, can add if building
    Nuclear <-inputdata %>%
      filter(ID=="LTO_Nuclear") 
  }
  
  # Combine the grouped data
  { case <- rbind(NGConv, SCCT_H2,CCCT_H2,SCCT_Blend,CCCT_Blend,
                  SCCT, CCCT_CCS,CCCT, Hydro, Other, Wind, Solar, Stor_B,Stor_HP,Stor_CA,Nuclear,Coal,Cogen)
                  
    case$ID <- factor(case$ID, levels=c(
      "AB_NGCONV","AB_SCCT_0NG100H2","AB_CCCT_0NG100H2","AB_SCCT_Blended","AB_CCCT_Blended",
      "AB_SCCT_noncogen","AB_CC90CCS_noncogen","AB_CCCT_noncogen",
      "LTO_Hydro","LTO_Other","LTO_Wind","LTO_Solar","Battery","HydroPumped","CompressedAir","LTO_Nuclear","LTO_Coal","LTO_Cogen"))
    
    levels(case$ID) <- c("Coal-to-Gas", "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                   "Blended  Simple Cycle","Blended  Combined Cycle",
                                   "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                                   "Hydro", "Other","Wind", 
                                   "Solar","Storage - Battery", "Storage - Pumped Hydro", "Storage - Compressed Air",
                                   "Nuclear","Coal", "Cogeneration") }
  return(case)  
}
}
################################################################################
## FUNCTION: sim_filtEm
## This function filters for emission releasing resources 
################################################################################

{ sim_filtEm <- function(inputdata) {
  # Filter the data by resource
  {Coal <- inputdata %>%
    filter(ID=="LTO_Coal")
  Cogen  <- inputdata %>%
    filter(ID=="LTO_Cogen")
  Other <- inputdata %>%
    filter(ID=="LTO_Other")
 
  NGConv <- inputdata %>%
    filter(ID=="AB_NGCONV")
  SCCT  <- inputdata %>%
    filter(ID=="AB_SCCT_noncogen")
  CCCT <- inputdata %>%
    filter(ID=="AB_CCCT_noncogen")
  CCCT_CCS <- inputdata %>%
    filter(ID=="AB_CC90CCS_noncogen")
  
  SCCT_Blend  <- inputdata %>%
    filter(ID=="AB_SCCT_Blended")
  CCCT_Blend <- inputdata %>%
    filter(ID=="AB_CCCT_Blended")
  
  }
  

  
  # Combine the grouped data
  { case <- rbind(NGConv,SCCT_Blend,CCCT_Blend,
                  SCCT, CCCT_CCS, CCCT, Coal, Cogen, Other )
    
    case$ID <- factor(case$ID, levels=c("AB_NGCONV",
                                        "AB_SCCT_Blended","AB_CCCT_Blended",
                                        "AB_SCCT_noncogen", "AB_CC90CCS_noncogen","AB_CCCT_noncogen",
                                        "LTO_Coal","LTO_Cogen","LTO_Other"))
    
    levels(case$ID) <- c("Coal-to-Gas", 
                         "Blended  Simple Cycle","Blended  Combined Cycle",
                         "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                          "Coal", "Cogeneration","Other")  }
  return(case)  }
}
