################################################################################
# TITLE: Res_Filter_Functions
# DESCRIPTION: Functions to select and sort certain resource groups for other functions to use. A collection of filters.

# ORIGINAL AUTHOR: Taylor Pawlenchuk (Retrieved June 3, 2022)
# EDITS & ADDITIONAL CONTENT: Jessica Van Os
# LAST EDIT: January 20, 2023
################################################################################

################################################################################
## FUNCTION: sim_filt
## This function filters for the data that will be evaluated.
################################################################################

sim_filt <- function(inputdata) {
  
  # Filter the data by resource, creates a table for each resource
  Coal <- inputdata %>%
    filter(ID=="LTO_Coal")
  #Force zero output if negative
  #Coal2Gas$Output_MWH[Coal2Gas$Output_MWH <= 0] <- 0
  Cogen  <- inputdata %>%
    filter(ID=="LTO_Cogen")
  
  # Combine Gas Groups
  NatGas <- inputdata %>%
    filter(ID=="LTO_NatGas")
  Coal2Gas  <- inputdata %>%
    filter(ID=="AB_NGCONV")
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
    filter(ID %in% c("AB_CCCT_Blended","AB_SCCT_Blended") ) %>%
    mutate(ID=ifelse(is.na(ID),NA,"NGH2_Blend"))
  
  # Not added yet, can add if building
  Nuclear <-inputdata %>%
    filter(ID=="LTO_Nuclear") 
  
  
  # Combine the grouped data tables into one
  { case <- rbind(Coal2Gas, NatGas, NatGas_CCS,NGH2_Blend,H2,Other, Hydro, Storage, Solar, Wind, Coal,Nuclear,Cogen)
    
    # Sort the table by case ID
    #A factor is a categorical variable 
    case$ID <- factor(case$ID, levels=c("AB_NGCONV", "LTO_NatGas","AB_CC90CCS_noncogen",
                                         "NGH2_Blend","LTO_H2","LTO_Other", 
                                         "LTO_Hydro", "LTO_Storage", "LTO_Solar",  
                                         "LTO_Wind","LTO_Coal","LTO_Nuclear", "LTO_Cogen"))
    # Replace ID value with name 
    levels(case$ID) <- c("Coal-to-Gas", "Natural Gas","Natural Gas + CCS","Natual Gas and Hydrogen Blend","Hydrogen" , 
                         "Other","Hydro", "Storage", "Solar","Wind","Coal","Nuclear","Cogen")   }
  return(case)  
}

################################################################################
## FUNCTION: sim_filtg
## This function filters for the data that will be evaluated.For greyscale.
################################################################################

sim_filtg <- function(inputdata) {
  
  # Filter the data by resource, creates a table for each resource
  Coal <- inputdata %>%
    filter(ID=="LTO_Coal")
  #Force zero output if negative
  #Coal2Gas$Output_MWH[Coal2Gas$Output_MWH <= 0] <- 0
  Cogen  <- inputdata %>%
    filter(ID=="LTO_Cogen")
  
  # Combine Gas Groups
  NatGas <- inputdata %>%
    filter(ID=="LTO_NatGas")
  Coal2Gas  <- inputdata %>%
    filter(ID=="AB_NGCONV")
  
  NatGasTot<-rbind(NatGas,Coal2Gas)%>%
    mutate(ID="NG")%>%
    group_by(ID,Time_Period)%>%
    summarise(Output_MWH=sum(Output_MWH),
              Capacity=sum(Capacity))%>%
    ungroup()
  
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
    filter(ID %in% c("AB_CCCT_Blended","AB_SCCT_Blended") ) %>%
    mutate(ID=ifelse(is.na(ID),NA,"NGH2_Blend"))
  
  # Not added yet, can add if building
  Nuclear <-inputdata %>%
    filter(ID=="LTO_Nuclear") 
  
  
  # Combine the grouped data tables into one
  { case <- rbind(Storage, Solar, Wind,Hydro,Other,NGH2_Blend,H2,NatGas_CCS,NatGasTot, Coal,Cogen,Nuclear)
    
    # Sort the table by case ID
    #A factor is a categorical variable 
    case$ID <- factor(case$ID, levels=c(
                                         "LTO_Storage", "LTO_Solar","LTO_Wind","LTO_Hydro",   
                                         "LTO_Other","NGH2_Blend","LTO_H2", 
                                         "AB_CC90CCS_noncogen","NG","LTO_Coal","LTO_Cogen","LTO_Nuclear"))
    # Replace ID value with name 
    levels(case$ID) <- c( "Storage", "Solar","Wind","Hydro", 
                          "Other","Natual Gas and Hydrogen Blend","Hydrogen",
                          "Natural Gas + CCS","Natural Gas","Coal","Cogen","Nuclear")   }
  return(case)  
}

################################################################################
## FUNCTION: sim_filt1
## This function filters for the data that will be evaluated.  
################################################################################

sim_filt1 <- function(inputdata) {
  # Filter the data by resource
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
  
  
  
  # Not added yet, can add if building
  Nuclear <-inputdata %>%
    filter(ID=="LTO_Nuclear") 
  
  # Combine the grouped data
  { case <- rbind(Solar,Wind,Hydro,Other, 
                  SCCT_H2,CCCT_H2,SCCT_Blend,CCCT_Blend, 
                  CCCT_CCS,SCCT,CCCT, NGConv,
                  Coal, Cogen,Storage)
    
    case$ID <- factor(case$ID, levels=c("LTO_Solar","LTO_Wind","LTO_Hydro","LTO_Other", 
                                        "AB_SCCT_0NG100H2","AB_CCCT_0NG100H2",
                                        "AB_SCCT_Blended","AB_CCCT_Blended",
                                        "AB_CC90CCS_noncogen","AB_SCCT_noncogen","AB_CCCT_noncogen","AB_NGCONV", 
                                        "LTO_Coal","LTO_Cogen","LTO_Storage"))
    
    levels(case$ID) <- c("Solar","Wind","Hydro","Other", 
                         "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                         "Blended  Simple Cycle","Blended  Combined Cycle",
                         "Natural Gas Combined Cycle + CCS","Natural Gas Simple Cycle", "Natural Gas Combined Cycle","Coal-to-Gas", 
                         "Coal", "Cogeneration","Storage")  }
  return(case)  
}


################################################################################
## FUNCTION: sim_filt2
## This function filters for the data that will be evaluated.  
################################################################################

sim_filt2 <- function(inputdata) {
  
  # Straight forward part
  Coal <- inputdata %>%
    filter(Primary_Fuel=="Coal Canada West") 
  Cogen  <- inputdata %>%
    filter(Primary_Fuel=="WECC-AECO Hub NaturalGas-COGEN_oilsands_Alberta")
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
  
  # Other
  Other <- inputdata %>%
    filter(Primary_Fuel %in% c('Other, ZZ, WC, WH','Other, Bio, ZZ, WC, WH','Biomass'))%>%
    mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"Other"))
  
  # Get NG Units as defined in Resource Table
  SCCT <- inputdata %>%
    filter(Primary_Fuel=="WECC-Alberta NaturalGas-Peaking")%>%
    mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"NG-SCCT")) 
  
  # Get coal-to-gas
  NGConv <- inputdata %>%
    filter(Primary_Fuel=="WECC-Alberta NaturalGas-Boiler")%>%
    mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"NGConv"))
  
  CCCT <- inputdata %>%
    filter(Primary_Fuel=="WECC-Alberta NaturalGas")
  
  # Units as defined by New Resources Table
  #First split up the fuel types
  CCC_CCS <- inputdata %>%
    filter(Primary_Fuel %in% c("Alberta Natural Gas with CCS","Alberta Natural Gas CCS Retrofit")) %>%
    mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"Alberta Natural Gas with CCS")) 
  
  # Blended Combined cycle
  CC_Blend <-inputdata %>%
    filter(Primary_Fuel %in% c('20-NaturalGas-80-Hydrogen Combined Cycle','50-NaturalGas-50-Hydrogen Combined Cycle','70-NaturalGas-30-Hydrogen Combined Cycle')) %>%
    mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"Blend-CC")) 
  
  # Blended Simple Cycle
  SC_Blend <-inputdata %>%
    filter(Primary_Fuel %in% c('20-NaturalGas-80-Hydrogen Simple Cycle','50-NaturalGas-50-Hydrogen Simple Cycle','70-NaturalGas-30-Hydrogen Simple Cycle')) %>%
    mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"Blend-SC")) 
  
  # Hydrogen
  H2 <- inputdata %>%
    filter(Primary_Fuel=="Hydrogen")
  
  # H2 Combined cycle
  CC_H2 <-H2[H2$Name %like% "%CC_0NG100H2%",] %>%
    mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"H2-CC"))  
  
  # H2 Simple cycle
  SC_H2 <-H2 %>%
    filter(grepl( 'Frame_0NG100H2|Aeroderivative_0NG100H2',Name)) %>%
    mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"H2-SC"))  
  
  
  
  # Combine the grouped data
  { case <- rbind(Solar, Wind,Hydro, Other, 
                  SC_H2,CC_H2,SC_Blend,CC_Blend,
                  CCC_CCS,SCCT, CCCT,
                  NGConv,  Coal, Cogen,Storage)
    
    case$Primary_Fuel <- factor(case$Primary_Fuel, levels=c(
      "Solar","Wind","Water", "Other",
      "H2-SC","H2-CC","Blend-SC","Blend-CC",
      "Alberta Natural Gas with CCS","NG-SCCT","WECC-Alberta NaturalGas",
      "NGConv","Coal Canada West", "WECC-AECO Hub NaturalGas-COGEN_oilsands_Alberta","Storage"))
    
    levels(case$Primary_Fuel) <- c("Solar","Wind","Hydro", "Other",
                                   "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                   "Blended  Simple Cycle","Blended  Combined Cycle",
                                   "Natural Gas Combined Cycle + CCS","Natural Gas Simple Cycle", "Natural Gas Combined Cycle", 
                                    "Coal-to-Gas","Coal", "Cogeneration", "Storage") }
  return(case)  
}

################################################################################
## FUNCTION: sim_filt3
## This function filters for the data that will be evaluated.  
## Same as 2, with coal removed and storage into 3 components
################################################################################

sim_filt3 <- function(inputdata) {
  
  # Straight forward part
  Cogen  <- inputdata %>%
    filter(Primary_Fuel=="WECC-AECO Hub NaturalGas-COGEN_oilsands_Alberta")
  
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
  
  # Other
  Other <- inputdata %>%
    filter(Primary_Fuel %in% c('Other, ZZ, WC, WH','Other, Bio, ZZ, WC, WH','Biomass'))%>%
    mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"Other"))
  
  # Blended Combined cycle
  CC_Blend <-inputdata %>%
    filter(Primary_Fuel %in% c('20-NaturalGas-80-Hydrogen Combined Cycle','50-NaturalGas-50-Hydrogen Combined Cycle','70-NaturalGas-30-Hydrogen Combined Cycle')) %>%
    mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"Blend-CC")) 
  
  # Blended Simple Cycle
  SC_Blend <-inputdata %>%
    filter(Primary_Fuel %in% c('20-NaturalGas-80-Hydrogen Simple Cycle','50-NaturalGas-50-Hydrogen Simple Cycle','70-NaturalGas-30-Hydrogen Simple Cycle')) %>%
    mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"Blend-SC")) 
  
  # Get NG Units as defined in Resource Table
  SCCT <- inputdata %>%
    filter(Primary_Fuel=="WECC-Alberta NaturalGas-Peaking")%>%
    mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"NG-SCCT")) 
  
  # Get coal-to-gas
  NGConv <- inputdata %>%
    filter(Primary_Fuel=="WECC-Alberta NaturalGas-Boiler")%>%
    mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"NGConv"))
  
  CCCT <- inputdata %>%
    filter(Primary_Fuel=="WECC-Alberta NaturalGas")
  
  # Units as defined by New Resources Table
  #First split up the fuel types
  CCC_CCS <- inputdata %>%
    filter(Primary_Fuel=="Alberta Natural Gas with CCS") 
  
  CCS_R <- inputdata %>%
    filter(Primary_Fuel=="Alberta Natural Gas CCS Retrofit")
  
  H2 <- inputdata %>%
    filter(Primary_Fuel=="Hydrogen")         
  
  # H2 Combined cycle
  CC_H2 <-H2[H2$Name %like% "%CC_0NG100H2%",] %>%
    mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"H2-CC"))  
  
  # H2 Simple cycle
  SC_H2 <-H2 %>%
    filter(grepl( 'Frame_0NG100H2|Aeroderivative_0NG100H2',Name)) %>%
    mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"H2-SC")) 
  
  # Storage Types
  Stor_B <-    Storage %>%    
    filter(Primary_Fuel=="Storage - Battery")
  
  Stor_HP <- Storage %>%    
    filter(Primary_Fuel=="Storage - HydroPumped")
  
  Stor_CA <- Storage  %>%    
    filter(Primary_Fuel=="Storage - CompressedAir") 
  
  
  
  # Combine the grouped data, cogen is excluded - can add back in if made a resource option
  { case <- rbind(NGConv, SC_H2,CC_H2,SC_Blend,CC_Blend,
                  SCCT, CCC_CCS,CCS_R,CCCT, Hydro, Other, Solar, Wind, Stor_B,Stor_CA, Stor_HP,Nuclear)
    
    case$Primary_Fuel <- factor(case$Primary_Fuel, levels=c(
      "NGConv","H2-SC","H2-CC","Blend-SC","Blend-CC",
      "NG-SCCT","Alberta Natural Gas with CCS","Alberta Natural Gas CCS Retrofit","WECC-Alberta NaturalGas",
      "Water", "Other",
      "Wind", "Solar", 
      "Storage - Battery", "Storage - CompressedAir","Storage - HydroPumped","Uranium"))
    
    levels(case$Primary_Fuel) <- c("Coal-to-Gas", "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                   "Blended  Simple Cycle","Blended  Combined Cycle",
                                   "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle CCS Retrofit","Natural Gas Combined Cycle", 
                                   "Hydro", "Other",
                                   "Wind", "Solar", 
                                   "Storage - Battery", "Storage - Compressed Air", "Storage - Pumped Hydro","Nuclear") }
  return(case)  
}

################################################################################
## FUNCTION: sim_filt4
## This function filters for the data that will be evaluated.  
## Same as 3, however only includes Aurora build options and is based on Fuel Type
################################################################################

sim_filt4 <- function(inputdata) {
  
  # Straight foreward part
  Hydro <- inputdata %>%
    filter(Fuel_Type=="WAT")
  Solar <- inputdata %>%
    filter(Fuel_Type=="SUN")
  Wind <- inputdata %>%
    filter(Fuel_Type=="WND")  
  Nuclear <-inputdata %>%
    filter(Fuel_Type=="UR") 
  
  # Other
  Other <- inputdata %>%
    filter(Fuel_Type %in% c('OT','BIO'))%>%
    mutate(Fuel_Type=as.character(ifelse(is.na(Fuel_Type),NA,"Other")))
  
  # Storage Types
  Stor_B <-    inputdata %>%    
    filter(Fuel_Type=="PS")
  Stor_HP <- inputdata %>%    
    filter(Fuel_Type=="PS3")
  Stor_CA <- inputdata  %>%    
    filter(Fuel_Type=="PS2") 
  
  Stor<-rbind(Stor_B,Stor_CA,Stor_HP)%>%
    mutate(Fuel_Type=as.character(ifelse(is.na(Fuel_Type),NA,"PSAll")))
  
  # Units as defined by New Resources Table
  #First split up the fuel types
  H2 <- inputdata %>%
    filter(Fuel_Type == "H2")
  NG_CCS <- inputdata %>%
    filter(Fuel_Type == "GasCCS")
  Blend <- inputdata %>%
    filter(grepl( '^GasB_CC$|^GasB_SC$',Fuel_Type)) %>%
    mutate(Fuel_Type=as.character(ifelse(is.na(Fuel_Type),NA,"GasB")))
  NG <-inputdata %>%
    filter(grepl( '^Gas3$|^Gas1$|^Gas2$|^Gas$',Fuel_Type)) %>%
    mutate(Fuel_Type=as.character(ifelse(is.na(Fuel_Type),NA,"NG"))) 
  
  
  # Combine the grouped data
  { case <- rbind(H2,Blend,NG,NG_CCS,
                  Hydro, Other, Solar, Wind, Stor,Nuclear)
    
    case$Fuel_Type <- factor(case$Fuel_Type, levels=c(
      "H2","GasB","NG","GasCCS",
      "WAT", "OT",
      "WND", "SUN", 
      "PSAll","UR"))
    
    levels(case$Fuel_Type) <- c("Hydrogen","Natual Gas and Hydrogen Blend","Natural Gas", "Natural Gas + CCS",
                                "Hydro", "Other","Wind", "Solar","Storage","Nuclear") }
  return(case)  
}

################################################################################
## FUNCTION: sim_filt5
## This function filters for the data that will be evaluated.  
################################################################################

sim_filt5 <- function(inputdata) {
  
  # Filter the data by resource, creates a table for each resource
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
  
  
  # Combine the grouped data
  { case <- rbind(NGConv, SCCT_H2,CCCT_H2,SCCT_Blend,CCCT_Blend,
                  SCCT, CCCT_CCS,CCCT, Hydro, Other, Wind, Solar, Stor_B,Stor_HP,Stor_CA,Coal,Cogen)
    
    case$ID <- factor(case$ID, levels=c(
      "AB_NGCONV","AB_SCCT_0NG100H2","AB_CCCT_0NG100H2","AB_SCCT_Blended","AB_CCCT_Blended",
      "AB_SCCT_noncogen","AB_CC90CCS_noncogen","AB_CCCT_noncogen",
      "LTO_Hydro","LTO_Other","LTO_Wind","LTO_Solar","Battery","HydroPumped","CompressedAir","LTO_Coal","LTO_Cogen"))
    
    levels(case$ID) <- c("Coal-to-Gas", "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                         "Blended  Simple Cycle","Blended  Combined Cycle",
                         "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle", 
                         "Hydro", "Other","Wind", 
                         "Solar","Storage - Battery", "Storage - Pumped Hydro", "Storage - Compressed Air",
                         "Coal", "Cogeneration") }
  return(case)  
}

################################################################################
## FUNCTION: sim_filt6
## This function filters for EVERYTHING - based on Primary Fuel Type
################################################################################

sim_filt6 <- function(inputdata) {
  
  # Straight foreward part
  Coal <- inputdata %>%
    filter(Primary_Fuel=="Coal Canada West") 
  Cogen  <- inputdata %>%
    filter(Primary_Fuel=="WECC-AECO Hub NaturalGas-COGEN_oilsands_Alberta")
  
  # Other
  Other <- inputdata %>%
    filter(Primary_Fuel %in% c('Other, ZZ, WC, WH','Other, Bio, ZZ, WC, WH','Biomass'))%>%
    mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"Other"))
  
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
  SCCT <- inputdata %>%
    filter(Primary_Fuel=="WECC-Alberta NaturalGas-Peaking")%>%
    mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"NG-SCCT")) 
  
  # Get coal-to-gas
  NGConv <- inputdata %>%
    filter(Primary_Fuel=="WECC-Alberta NaturalGas-Boiler")%>%
    mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"NGConv"))
  
  CCCT <- inputdata %>%
    filter(Primary_Fuel=="WECC-Alberta NaturalGas")
  
  # Units as defined by New Resources Table
  #First split up the fuel types
  CCC_CCS <- inputdata %>%
    filter(Primary_Fuel=="Alberta Natural Gas with CCS") 
  
  CCS_R <- inputdata %>%
    filter(Primary_Fuel=="Alberta Natural Gas CCS Retrofit")
  
  H2 <- inputdata %>%
    filter(Primary_Fuel=="Hydrogen")         
  
  # H2 Combined cycle
  CC_H2 <-H2[H2$Name %like% "%CC_0NG100H2%",] %>%
    mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"H2-CC"))  
  
  # H2 Simple cycle
  SC_H2 <-H2 %>%
    filter(grepl( 'Frame_0NG100H2|Aeroderivative_0NG100H2',Name)) %>%
    mutate(Primary_Fuel=ifelse(is.na(Primary_Fuel),NA,"H2-SC")) 
  
  # Storage Types
  Stor_B <-    Storage %>%    
    filter(Primary_Fuel=="Storage - Battery")
  
  Stor_HP <- Storage %>%    
    filter(Primary_Fuel=="Storage - HydroPumped")
  
  Stor_CA <- Storage  %>%    
    filter(Primary_Fuel=="Storage - CompressedAir") 
  
  
  
  
  # Combine the grouped data
  { case <- rbind(Coal,NGConv, SC_H2,CC_H2,SC_Blend,CC_Blend,
                  SCCT, CCC_CCS,CCS_R,CCCT, Hydro, Other, Solar, Wind, Stor_B,Stor_CA, Stor_HP, Cogen)
    
    case$Primary_Fuel <- factor(case$Primary_Fuel, levels=c(
      "Coal Canada West","NGConv","H2-SC","H2-CC","Blend-SC","Blend-CC",
      "NG-SCCT","Alberta Natural Gas with CCS","Alberta Natural Gas CCS Retrofit","WECC-Alberta NaturalGas",
      "Water", "Other",
      "Wind", "Solar", 
      "Storage - Battery", "Storage - CompressedAir","Storage - HydroPumped",
      "WECC-AECO Hub NaturalGas-COGEN_oilsands_Alberta"))
    
    levels(case$Primary_Fuel) <- c("Coal","Coal-to-Gas", "Hydrogen Simple Cycle","Hydrogen Combined Cycle",
                                   "Blended  Simple Cycle","Blended  Combined Cycle",
                                   "Natural Gas Simple Cycle", "Natural Gas Combined Cycle + CCS","Natural Gas Combined Cycle CCS Retrofit",
                                   "Natural Gas Combined Cycle", 
                                   "Hydro", "Other",
                                   "Wind", "Solar", 
                                   "Storage - Battery", "Storage - Compressed Air", "Storage - Pumped Hydro", 
                                   "Cogeneration") }
  return(case)  
}

################################################################################
## FUNCTION: sim_filtEm
## This function filters for emission releasing resources. 
## For all cogen emissions use LTO_Cogen
################################################################################

sim_filtEm <- function(inputdata) {
  # Filter the data by resource
  Coal <- inputdata %>%
    filter(ID=="LTO_Coal")
  Cogen  <- inputdata %>%
    filter(ID=="NAICS221112_Cogen")
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
  
  # Combine the grouped data
  { case <- rbind(Other,CCCT_CCS,SCCT_Blend, CCCT_Blend,
                  SCCT, CCCT,NGConv, Coal, Cogen  )
    
    case$ID <- factor(case$ID, levels=c("LTO_Other", "AB_CC90CCS_noncogen",
                                        "AB_SCCT_Blended","AB_CCCT_Blended",
                                        "AB_SCCT_noncogen","AB_CCCT_noncogen","AB_NGCONV",
                                        "LTO_Coal","NAICS221112_Cogen"))
    
    levels(case$ID) <- c("Other", "Natural Gas Combined Cycle + CCS",
                         "Blended  Simple Cycle","Blended  Combined Cycle",
                         "Natural Gas Simple Cycle","Natural Gas Combined Cycle", "Coal-to-Gas", 
                         "Coal", "Cogeneration")  }
  return(case)
}

################################################################################
## FUNCTION: sim_filtFuel
## This function filters for the data that will be evaluated.  
## Same as 3, however only includes Aurora build options and is based on Fuel Type
################################################################################

sim_filtFuel <- function(inputdata) {
  
  # Straight up parts
  Other <- inputdata %>%
    filter(ID %in% c("OT","BIO"))%>%
    mutate(ID=ifelse(is.na(ID),NA,"OT"))
  
  Hydro <- inputdata %>%
    filter(ID=="WAT")
  Solar <- inputdata %>%
    filter(ID=="SUN")
  Wind <- inputdata %>%
    filter(ID=="WND")  
  Nuclear <-inputdata %>%
    filter(ID=="UR") 
  
  # Storage Types
  Stor_B <-    inputdata %>%    
    filter(ID=="PS")
  Stor_HP <- inputdata %>%    
    filter(ID=="PS3")
  Stor_CA <- inputdata  %>%    
    filter(ID=="PS2") 
  
  # Gas
  HHub<-inputdata %>%    
    filter(ID=="NGHenry")
  NGPeak <-inputdata %>%
    filter(ID=="NG2AB")
  NGBase <-inputdata %>%
    filter(ID=="NG1AB")
  NGConv <-inputdata %>%
    filter(ID=="NG3AB")
  NG_CCS <-inputdata %>%
    filter(ID=="NG_CCS")
  
  # Hydrogen
  H2 <- inputdata %>%
    filter(ID == "H2AB")
  
  # Coal
  Coal <- inputdata %>%
    filter(ID == "CoalWCA")
  
  # Hydrogen Blends
  Blend1 <- inputdata %>%
    filter(ID == "20CC80H2")
  Blend2 <- inputdata %>%
    filter(ID == "50CC50H2")
  Blend3 <- inputdata %>%
    filter(ID == "70CC30H2")
  
  Blend4 <- inputdata %>%
    filter(ID == "20SC80H2")
  Blend5 <- inputdata %>%
    filter(ID == "50SC50H2")
  Blend6 <- inputdata %>%
    filter(ID == "70SC30H2")
  
  # Combine the grouped data
  { case <- rbind(H2,
                  Blend1,Blend2,Blend3,Blend4,Blend5,Blend6,
                  NGPeak,NGBase,NGConv,NG_CCS,HHub,Coal
                  # ,Hydro, Other, Wind, Solar,
                  # Stor_B,Stor_CA, Stor_HP
  )
    
    case$ID <- factor(case$ID, levels=c(
      "H2AB",
      "20CC80H2","50CC50H2","70CC30H2","20SC80H2","50SC50H2","70SC30H2",
      "NG2AB","NG1AB","NG3AB","NG_CCS","NGHenry","CoalWCA"
      # ,"WAT", "OT","WND", "SUN", 
      # "PS", "PS2","PS3"
    ))
    
    levels(case$ID) <- c("Hydrogen",
                         "NG H2 Blend CC - 20/80","NG H2 Blend CC - 50/50","NG H2 Blend CC - 70/30",
                         "NG H2 Blend SC - 20/80","NG H2 Blend SC - 50/50","NG H2 Blend SC - 70/30",
                         "Natural Gas - Peaking","Natural Gas - Base","Natural Gas - Boiler","Natural Gas","Henry Hub Natural Gas","Coal"
                         # ,"Water", "Other","Wind", "Sun", 
                         # "Storage - Battery", "Storage - Compressed Air", "Storage - Pumped Hydro"
    ) }
  return(case)  
}